{-# LANGUAGE TypeFamilies #-}

module HQP.QOp.MatrixSemantics where

import QubitOperators
import Numeric.LinearAlgebra hiding(normalize,step,(<>)) -- the hmatrix library
--import Numeric.LinearAlgebra.HMatrix(mul) 
import Data.Bits

type CMat = Matrix ComplexT
type RMat = Matrix RealT
type StateT = CMat
type OpT    = CMat



instance Operator CMat where
{-|The tensor product of two operators in matrix representation a :: (m><n) and b :: (p><q) is the kronecker product c :: (m*p >< n*q) with entries c_{i*p+k, j*q+l} = a_{ij}*b_{kl} -}    
    tensorProd = kronecker

{-| The direct sum of two operators in matrix representation a :: (m><n) and b :: (p><q) is the block-diagonal matrix c :: ((m+p) >< (n+q)) with a and b as blocks. Note that the additive dimension necessitates that a and b have the same dimensions for it to be realizable on qubits.
-}
    directSum a b = fromBlocks [[a, zeros (rows a) (cols b)],
                                [zeros (rows b) (cols a), b]]

{-| The operator adjoint in matrix representation is the conjugate transpose. -}
    adj = tr -- HMatrix confusingly defines conjugate transpose as 'tr' (standard trace notation)

{-| We define a VectorSpace typeclass, which we will use for states -}
class VectorSpace v where
  type Scalar v
  type Realnum   v 
  (.*)  :: Scalar v -> v -> v -- Scalar-vector multiplication
  (.+)  :: v -> v -> v        -- Vector-vector addition
  inner :: v -> v -> Scalar v -- Inner product 
  norm  :: v -> Realnum v     -- Vector 2-norm
  

{-| We implement ket-states as (n >< 1) matrices (= column vectors), and bra-states as (1 >< n) matrices (= row vectors). Thus StateT inherits the operator operations (recall that states
are also linear operators by Riesz representation), but we can also treat them like vectors,
so we implement the VectorSpace operations. -}
instance VectorSpace StateT where
    type Scalar  StateT = ComplexT 
    type Realnum StateT = RealT
    
    (.*) = scale
    (.+) a b = a+b

    -- | inner a b is the usual dot product, with the adjoint coefficients complex conjugated
    inner a b = let 
            (m,n,p,q) = (rows a, cols a, rows b, cols b) 
        in
            if (m,n) == (p,q) then 
                sumElements (conj a * b)        
            else 
                error $ "inner: incompatible shapes " ++ (show (m,n)) ++ " != " ++ (show (p,q))
        
    -- | norm a = ||a||_2 = sqrt(<a|a>)
    norm a = sqrt(realPart $ inner a a)
    


{-| 
 evalOp :: Op -> CMat
 (evalOp op) evaluates the pure quantum operator op (defined in Circuit.hs) in the matrix semantics, and produces a complex matrix of dimension 2^n x 2^n (where n is the qubit-length of op)
-}
evalOp  :: Op -> CMat
evalOp op = case op of
    I -> (2 >< 2) [1,0,
                   0,1]

    X -> (2 >< 2) [0,1,
                   1,0]

    SX -> (2 >< 2) [1:+  1,  1:+(-1),
                    1:+(-1),1:+  1]/2

    Y -> (2 >< 2) [-ii, 0,
                    0, ii]

    Z -> (2 >< 2) [1,0,
                   0,-1]

    H -> let s = 1/sqrt 2
         in  s * ((2><2) [1, 1,
                         1,-1])

    R axis theta' -> 
        let mat  = evalOp axis
            theta = theta' :+ 0 -- Haskell won't multiply real and complex numbers
        in
            matFunc exp ( (-ii*theta/2) .* mat )

    SWAP -> (4 >< 4) [1,0,0,0,
                      0,0,1,0,
                      0,1,0,0,
                      0,0,0,1]

    C op1          ->  let
                            mop = evalOp op1
                            mI  = ident (rows mop)
                        in
                            mI `directSum` mop

    op1 `DirectSum` op2 -> (evalOp op1) ⊕ (evalOp op2)
    op1 `Tensor`    op2 -> (evalOp op1) ⊗ (evalOp op2)
    op1 `Compose`   op2 -> (evalOp op2) <> (evalOp op1) 
    Inverse op1         -> adj $ evalOp op1


--  State contruction: An n-qubit state can be represented by a vector in C^{2^n} (we'll later see potentially more compact representations). 

bra, ket :: [Int] -> CMat

{-| bra [v0,...,v_{n-1}] (where v_j is 0 or 1) is the adjoint state <v0 v1 ... v_{n-1}|. In the matrix representation, this is a row-vector in C^{2^n}, i.e. of dimension (1 >< 2^n). -}
bra []     = (1><1) [1]
bra (v':vs) = let 
        v = fromIntegral v' 
    in 
        (1><2) [1-v,v] ⊗ (bra vs)

{-| ket [v0,...,v_{n-1}] (where v_j is 0 or 1) is the state |v0 v1 ... v_{n-1}>. In the matrix representation, this is a column-vector in C^{2^n}, i.e. of dimension (2^n >< 1). -}
ket vs     = adj $ bra vs


{-| Measurements require random numbers, which are non-deterministic by nature. Everything here is kept purely functional except, with side effects restricted to program mains, so random numbers are simply given as a list of Doubles between 0 and 1. -}
type RNG = [Double]
{-| 'evalStep rng step state' evaluates a computational step (unitary or measurement) given the current state and returns the modified quantum state. 

{- | 'evalStep rng step state' evaluates a computational step (unitary or measurement) given the current state and returns the modified quantum state.

To deal with random measurements in a pure functional setting, we treat a random number generator rng as an infinite list of numbers between 0 and 1. This allows the library to remain pure - IO comes in from program mains.

We take a random number generator as the first parameter, read off the first element, and return
the remainder together with the updated quantum state.
-} 
evalStep :: RNG -> Step -> StateT -> (StateT,RNG)
evalStep [] _ _ = error "No more random numbers. This never happens."

evalStep (r:rng) step state  = case step of
    Unitary op   -> ((evalOp op) <> state, r:rng)
    Measure []   -> (state, r:rng)
    Measure (k:ks) -> let
        n = ilog2 (rows state)        
        proj0 = measureProjection n k 0
        proj1 = measureProjection n k 1

      prob0 = realPart $ inner state s0
      prob1 = realPart $ inner state s1

        prob0 = realPart $ inner state s0
        prob1 = realPart $ inner state s1
                    
        collapsed_state = normalize $ if(r<prob0) then s0 else s1
        in
            if (abs(prob1+prob0-1)>tol) then
                error $ "Probabilities don't sum to 1: " ++ (show (prob0,prob1))
            else
                evalStep rng (Measure ks) collapsed_state 

evalStepOp :: Int -> [Bool] -> Step -> (CMat, [Bool])
evalStepOp _ [] _ = error "Please provide a path."
evalStepOp n (p:path) step = case step of
    Unitary op -> (evalOp op, p:path) -- TODO: assert n = dim op?
    Measure [] -> (ident (2^n) :: CMat, p:path)
    Measure (k:ks) -> let
            proj_k          = measureProjection n k (if p then 1 else 0)
            (proj_ks,path') = evalStepOp n path (Measure ks)
         in
            (proj_ks <> proj_k, path') 


evalProg :: RNG -> [Step] -> StateT -> (StateT, RNG)
evalProg rng steps psi0 =
  foldl apply_step (psi0, rng) steps
  where
    apply_step :: (StateT, RNG) -> Step -> (StateT, RNG)
    apply_step (psi, rng') step = evalStep rng' step psi

{-|
 measureProjection n k result 
 
Builds the projection operator onto the subspace of C^{2^n} in which qubit k has value 'result'.

Construction of the operator:
I ⨷ ... ⨷ I ⨷ P ⨷ I ⨷ ... ⨷ I
\-- k-1 --/                   /
 \------------ n ------------/
-}
measureProjection :: Int -> Int -> Int -> CMat
measureProjection n k result =
  let p =
        if k == 0
          then
            let v = fromIntegral result :: ComplexT
             in (2 >< 2)
                  [ 1 - v
                  , 0
                  , 0
                  , v
                  ]
          else
            evalOp I
   in if n > 1
        then
          p ⊗ (measureProjection (n - 1) (k - 1) result)
        else
          p


--  State contruction: An n-qubit state can be represented by a vector in C^{2^n} (we'll later see potentially more compact representations). 

bra, ket :: [Int] -> CMat

{-| bra [v0,...,v_{n-1}] (where v_j is 0 or 1) is the adjoint state <v0 v1 ... v_{n-1}|. In the matrix representation, this is a row-vector in C^{2^n}, i.e. of dimension (1 >< 2^n). -}
bra []     = (1><1) [1]
bra (v':vs) = let 
        v = fromIntegral v' 
    in 
        (1><2) [1-v,v] ⊗ (bra vs)

{-| ket [v0,...,v_{n-1}] (where v_j is 0 or 1) is the state |v0 v1 ... v_{n-1}>. In the matrix representation, this is a column-vector in C^{2^n}, i.e. of dimension (2^n >< 1). -}
ket vs     = adj $ bra vs


{-| (directSum a b) computes the direct sum of a and b. The direct sum of two operators in matrix  representation is the block diagonal matrix with a and b as diagonal blocks. -}



-- Auxiliary definitions




tol :: RealT
tol = 1e-14

ii, one :: ComplexT
ii = 0 :+ 1
one = 1

zeros :: Int -> Int -> CMat
zeros m n = konst 0 (m, n)

normalize :: StateT -> StateT
normalize s =
  let
    n = (norm_2 (flatten s))
   in
    if (n < tol)
      then s
      else s / scalar (n :+ 0)

-- apply :: Op -> Vector -> Vector -- How do define abstract state instead, for easy backend switching?

realM, imagM :: CMat -> RMat
realM m = cmap realPart m
imagM m = cmap imagPart m

realMI, imagMI :: CMat -> [[Int]]
realMI = map (map (floor . realPart)) . toLists
imagMI = map (map (floor . imagPart)) . toLists

-- | sparseMat takes an (m >< n) matrix and returns ((m,n), nonzeros) where nonzeros is a list of every nonzero index paired with the corresponding value.
sparseMat :: CMat -> ((Int, Int), [((Int, Int), ComplexT)])
sparseMat mat =
  let
    (m, n) = (rows mat, cols mat)
    full_list = case (m, n) of
      (1, 1) -> []
      (_, 1) -> [((i, 0), mat `atIndex` (i, 0)) | i <- [0 .. m - 1]]
      (1, _) -> [((0, j), mat `atIndex` (0, j)) | j <- [0 .. n - 1]]
      _ -> error $ show "Use sparseOp for operators"
   in
    ((m, n), filter (\(_, v) -> (magnitude v > tol)) full_list)

ilog2 :: (FiniteBits a, Integral a) => a -> Int
ilog2 = countTrailingZeros



