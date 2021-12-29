{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module AD where

import Prelude

import           Control.Monad (when)
import           Control.Monad.ST.Strict (ST)
import           Control.Monad.Trans.State.Strict
import qualified Data.Vector
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Unboxed

-- Tagless final doesn't seem to work well, because we need to gather
-- @Delta@ while doing @DualDelta@ operations, but evaluate on concrete
-- vectors that correspond to only the second component of dual numbers.
-- Also, initial encoding gives us full control over
-- when to evaluate. With final, we'd need to be careful
-- about laziness to ensure the optimal evaluation order is chosen
-- (whatever it is for a given differentiated program).
data Delta r =
    Zero
  | Scale r (Delta r)
  | Add (Delta r) (Delta r)
  | Var DeltaId

newtype DeltaId = DeltaId Int
  deriving (Show, Eq, Ord, Enum)

-- This can't be environment in a Reader, because subtrees add their own
-- identifiers for sharing, instead of parents naming their subtrees.
-- This must be the "evaluate Let backwards" from SPJ's talk.
-- This and the need to control evaluation order contribute to
-- the difficulty of applying any HOAS concept instead of the monad
-- with bindings accumulated in state.
-- Note that each variable is created only once, but the subexpression
-- it's a part of can get duplicated grossly.
data DeltaState r = DeltaState
  { deltaCounter  :: DeltaId
  , deltaBindings :: [(DeltaId, Delta r)]
  }

buildVector :: forall s v r. (Eq r, Num r, VM.MVector (V.Mutable v) r)
            => Int -> DeltaState r -> Delta r
            -> ST s (V.Mutable v s r)
buildVector dim st d0 = do
  let DeltaId storeSize = deltaCounter st
  store <- VM.replicate storeSize 0
  let eval :: r -> Delta r -> ST s ()
      eval scale = \case
        Zero -> return ()
        Scale k d -> eval (k * scale) d
        Add d1 d2 -> eval scale d1 >> eval scale d2
        Var (DeltaId i) -> VM.modify store (+ scale) i
  eval 1 d0  -- dt is 1 or hardwired in f
  let evalUnlessZero :: (DeltaId, Delta r) -> ST s ()
      evalUnlessZero (DeltaId i, d) = do
        scale <- store `VM.read` i
        when (scale /= 0) $  -- TODO: dodgy for reals?
          eval scale d
  mapM_ evalUnlessZero (deltaBindings st)
  return $! VM.slice 0 dim store

evalBindingsV :: (Eq r, Num r, V.Vector v r)
              => VecDualDeltaR i -> DeltaState r -> Delta r -> v r
evalBindingsV ds st d0 = V.create $ buildVector (V.length ds) st d0

newtype DeltaImplementationR r a = DeltaImplementation
  { runDeltaImplementation :: State (DeltaState r) a }
  deriving (Monad, Functor, Applicative)

type DeltaImplementation = DeltaImplementationR Float

dlet :: Delta r -> DeltaImplementationR r DeltaId
dlet v = DeltaImplementation $ do
  i <- gets deltaCounter
  modify $ \s ->
    s { deltaCounter = succ i
      , deltaBindings = (i, v) : deltaBindings s
      }
  return i

data DualDeltaR r = D r (Delta r)

type DualDelta = DualDeltaR Float

type VecDualDeltaR r = Data.Vector.Vector (DualDeltaR r)

type VecDualDelta = VecDualDeltaR Float

generalDf :: (s -> (VecDualDeltaR r, Int))
          -> (VecDualDeltaR r -> DeltaState r -> Delta r -> ds)
          -> (VecDualDeltaR r -> DeltaImplementationR r (DualDeltaR r))
          -> s
          -> (ds, r)
{-# INLINE generalDf #-}
generalDf initVars evalBindings f deltaInput =
  let (ds, dim) = initVars deltaInput
      initialState = DeltaState
        { deltaCounter = DeltaId dim
        , deltaBindings = []
        }
      (D value d, st) = runState (runDeltaImplementation (f ds)) initialState
      res = evalBindings ds st d
  in (res, value)

type Domain r = Data.Vector.Unboxed.Vector r  -- s

type Domain' r = Domain r  -- ds

df :: forall r . (Eq r, Num r, Data.Vector.Unboxed.Unbox r)
   => (VecDualDeltaR r -> DeltaImplementationR r (DualDeltaR r))
   -> Domain r
   -> (Domain' r, r)
df =
  let initVars :: Domain r -> (VecDualDeltaR r, Int)
      initVars deltaInput =
        let dualizeInput i xi = D xi (Var $ DeltaId i)
        in ( V.fromList $ zipWith dualizeInput [0 ..] (V.toList deltaInput)
           , V.length deltaInput )
  in generalDf initVars evalBindingsV

gradDesc :: forall r . (Eq r, Num r, Data.Vector.Unboxed.Unbox r)
         => r
         -> (VecDualDeltaR r -> DeltaImplementationR r (DualDeltaR r))
         -> Int
         -> Domain r
         -> Domain' r
gradDesc gamma f = go where
  go :: Int -> Domain r -> Domain' r
  go 0 !vecInitial = vecInitial
  go n vecInitial =
    let res = fst $ df f vecInitial
        v = V.zipWith (\i r -> i - gamma * r) vecInitial res
    in go (pred n) v

(*\) :: DualDelta -> DualDelta -> DeltaImplementation DualDelta
(*\) (D u u') (D v v') = do
  d <- dlet $ Add (Scale v u') (Scale u v')
  return $! D (u * v) (Var d)

(+\) :: DualDelta -> DualDelta -> DeltaImplementation DualDelta
(+\) (D u u') (D v v') = do
  d <- dlet $ Add u' v'
  return $! D (u + v) (Var d)

(-\) :: DualDelta -> DualDelta -> DeltaImplementation DualDelta
(-\) (D u u') (D v v') = do
  d <- dlet $ Add u' (Scale (-1) v')
  return $! D (u - v) (Var d)

(**\) :: DualDelta -> DualDelta -> DeltaImplementation DualDelta
(**\) (D u u') (D v v') = do
  d <- dlet $ Add (Scale (v * (u ** (v - 1))) u')
                  (Scale ((u ** v) * log u) v')
  return $! D (u ** v) (Var d)

scalar :: Float -> DualDelta
scalar k = D k Zero

_scale :: Float -> DualDelta -> DeltaImplementation DualDelta
_scale k (D u u') = do
  d <- dlet $ Scale k u'
  return $! D (k * u) (Var d)

tanhAct :: DualDelta -> DeltaImplementation DualDelta
tanhAct (D u u') = do
  let y = tanh u
  d <- dlet $ Scale (1 - y * y) u'
  return $! D y (Var d)

reluAct :: DualDelta -> DeltaImplementation DualDelta
reluAct (D u u') = do
  d <- dlet $ Scale (if u > 0 then 1 else 0) u'
  return $! D (max 0 u) (Var d)













-- higher order types of vars
-- recursion and recursive types
-- selective fusion of delta (for individual subfunctions: pre-computing,
--   inlining results and simplifying delta-expressions; the usual inlining
--   considerations apply)
-- checkpointing (limiting space usage?)
