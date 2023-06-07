{-# LANGUAGE OverloadedLists, UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module HordeAd.Core.ADValTensor
   where


import Prelude

import qualified Data.Array.DynamicS as OT
import qualified Data.Array.Ranked as ORB
import qualified Data.Array.RankedS as OR
import qualified Data.Strict.IntMap as IM
import qualified Data.Strict.Vector as Data.Vector
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, type (+))

import HordeAd.Core.Ast
import HordeAd.Core.AstSimplify
import HordeAd.Core.AstVectorize ()
import HordeAd.Core.DualClass
import HordeAd.Core.DualNumber hiding (build1)
import HordeAd.Core.SizedIndex
import HordeAd.Core.TensorClass
import HordeAd.Internal.SizedList
import HordeAd.Internal.TensorOps

-- Not that this instance doesn't do vectorization. To enable it,
-- use the Ast instance, which vectorizes and finally interpret in ADVal.
-- In principle, this instance is only useful for comparative tests,
-- though for code without build/map/etc., it should be equivalent
-- to going via Ast.
instance ADModeAndNum d Double => Tensor (ADVal d Double) where
  type TensorOf n (ADVal d Double) = ADVal d (OR.Array n Double)
  type IntOf (ADVal d Double) = Int

  -- Here and elsewhere I can't use methods of the @r@ instance of @Tensor@
  -- (the one implemented as @OR.Array n r@). Therefore, I inline them
  -- manually. There is probably no solution to that (2 parameters to Tensor
  -- would solve this, but we'd need infinitely many instances
  -- for @ADVal d (OR.Array n r)@ and @OR.Array n r@). As a workaround,
  -- the methods are defined as calls to tensor functions provided elsewhere,
  -- so there is no code duplication.
--  tshape = shape
--  tminIndex0 (D u _) = tminIndexR u
--  tmaxIndex0 (D u _) = tmaxIndexR u
--  tfloor (D u _) = floor $ tunScalarR u

--  tindex = indexZ  -- for simplicity, out of bounds indexing permitted
-- tsum = sum'
--  tsum0 = tscalar . sum0
--  tdot0 u v = tscalar $ dot0 u v
--  tscatter = scatterNClosure

--  tfromList = fromList
--  tfromList0N = fromList0N
--  tfromVector = fromVector
--  tfromVector0N = fromVector0N
--  tkonst = konst
--  tkonst0N sh = konst0N sh . unScalar
--  tappend = append
--  tslice = slice
--  treverse = reverse'
--  ttranspose = transpose
--  treshape = reshape
  tbuild1 k f =
    let g i = let D u _ = f i in u
        h i = let D _ u' = f i in u'
    in dD (tbuild1R k g) (dBuild1 k h)
      -- uses the implementation that stores closures on tape to test against
      -- the elementwise implementation used by fallback from vectorizing Ast
--  tgather = gatherNClosure  -- for simplicity, out of bounds indexing permitted

--  tscalar = scalar
--  tunScalar = unScalar


instance ADModeAndNum d Double => HasPrimal (ADVal d Double) where
  type ScalarOf (ADVal d Double) = Double
  type Primal (ADVal d Double) = Double
  type DualOf n (ADVal d Double) = Dual d (OR.Array n Double)


-- * ADVal combinators generalizing ranked tensor operations

shape :: (ADModeAndNum d r, KnownNat n)
      => ADVal d (TensorOf n r) -> ShapeInt n
shape (D u _) = tshape u

-- TODO: speed up by using tindex0R and dIndex0 if the codomain is 0
-- and dD (u `tindex1R` ix) (dIndex1 u' ix (tlengthR u)) if only outermost
-- dimension affected.
--
-- First index is for outermost dimension; empty index means identity,
-- index ouf of bounds produces zero (but beware of vectorization).
indexZ :: forall m n d r.
          (ADModeAndNum d r, IsPrimal d (TensorOf n r), KnownNat m, KnownNat n)
       => ADVal d (TensorOf (m + n) r) -> IndexOf m r
       -> ADVal d (TensorOf n r)
indexZ (D u u') ix =
  let sh = tshape u
  in if undefined (indexToList ix) (shapeToList sh)
     then dD (tindex u ix) (dIndexN u' ix sh)
     else dD (tkonst0N (dropShape @m sh) 0) dZero

--sum' :: (ADModeAndNum d r, IsPrimal d (TensorOf n r), KnownNat n)
--     => ADVal d (TensorOf (1 + n) r) -> ADVal d (TensorOf n r)
--sum' (D u u') = dD (tsum u) (dSum1 (tlength u) u')

--sum0 :: (ADModeAndNum d r, KnownNat n)
--     => ADVal d (TensorOf n r) -> ADVal d r
--sum0 (D u u') = dD (tunScalar $ tsum0 u) (dSum0 (tshape u) u')

--dot0 :: (ADModeAndNum d r, KnownNat n)
--     => ADVal d (TensorOf n r) -> ADVal d (TensorOf n r) -> ADVal d r
--dot0 (D u u') (D v v') = dD (tunScalar $ tdot0 u v)
--                            (dAdd (dDot0 v u') (dDot0 u v'))

{-
scatterNClosure :: ( ADModeAndNum d r, IsPrimal d (TensorOf (p + n) r)
                   , KnownNat m, KnownNat p, KnownNat n )
                => ShapeInt (p + n) -> ADVal d (TensorOf (m + n) r)
                -> (IndexOf m r -> IndexOf p r)
                -> ADVal d (TensorOf (p + n) r)
--scatterNClosure sh (D u u') f =
--  dD (tscatter sh u f) (dScatterN sh u' f (tshape u))
--  -}

fromList :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
         => [ADVal d (TensorOf n r)]
         -> ADVal d (TensorOf (1 + n) r)
fromList lu =
  -- TODO: if lu is empty, crash if n =\ 0 or use List.NonEmpty.
  dD (tfromList $ map (\(D u _) -> u) lu)
     (dFromList1 $ map (\(D _ u') -> u') lu)

--fromList0N :: (ADModeAndNum d r, KnownNat n)
--           => ShapeInt n -> [ADVal d r]
--           -> ADVal d (TensorOf n r)
--fromList0N sh l =
--  dD (tfromList0N sh $ map (\(D u _) -> u) l)  -- I hope this fuses
--     (dFromList01 sh $ map (\(D _ u') -> u') l)

fromVector :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
           => Data.Vector.Vector (ADVal d (TensorOf n r))
           -> ADVal d (TensorOf (1 + n) r)
fromVector lu =
  dD (tfromVector $ V.map (\(D u _) -> u) lu)
     (dFromVector1 $ V.map (\(D _ u') -> u') lu)

--fromVector0N :: (ADModeAndNum d r, KnownNat n)
--             => ShapeInt n -> Data.Vector.Vector (ADVal d r)
--             -> ADVal d (TensorOf n r)
--fromVector0N sh l =
--  dD (tfromVector0N sh $ V.convert $ V.map (\(D u _) -> u) l)  -- hope it fuses
--     (dFromVector01 sh $ V.map (\(D _ u') -> u') l)

konst :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
      => Int -> ADVal d (TensorOf n r) -> ADVal d (TensorOf (1 + n) r)
konst k (D u u') = dD (tkonst k u) (dKonst1 k u')

--konst0N :: (ADModeAndNum d r, KnownNat n)
--        => ShapeInt n -> ADVal d r -> ADVal d (TensorOf n r)
--konst0N sh (D u u') = dD (tkonst0N sh u) (dKonst01 sh u')

append :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
       => ADVal d (TensorOf (1 + n) r) -> ADVal d (TensorOf (1 + n) r)
       -> ADVal d (TensorOf (1 + n) r)
append (D u u') (D v v') = dD (tappend u v) (dAppend1 u' (tlength u) v')

slice :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
      => Int -> Int -> ADVal d (TensorOf (1 + n) r)
      -> ADVal d (TensorOf (1 + n) r)
slice i k (D u u') = dD (tslice i k u) (dSlice1 i k u' (tlength u))

reverse' :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
         => ADVal d (TensorOf (1 + n) r) -> ADVal d (TensorOf (1 + n) r)
reverse' (D u u') = dD (treverse u) (dReverse1 u')

transpose :: (ADModeAndNum d r, IsPrimal d (TensorOf n r), KnownNat n)
          => Permutation -> ADVal d (TensorOf n r) -> ADVal d (TensorOf n r)
transpose perm (D u u') = dD (ttranspose perm u) (dTranspose1 perm u')

reshape :: (ADModeAndNum d r, IsPrimal d (TensorOf m r), KnownNat m, KnownNat n)
        => ShapeInt m -> ADVal d (TensorOf n r) -> ADVal d (TensorOf m r)
reshape sh (D u u') = dD (treshape sh u) (dReshape1 (tshape u) sh u')

-- The element-wise (POPL) version, but only one rank at a time.
build1 :: (ADModeAndNum d r, IsPrimal d (TensorOf (1 + n) r), KnownNat n)
       => Int -> (Int -> ADVal d (TensorOf n r))
       -> ADVal d (TensorOf (1 + n) r)
build1 k f = fromList $ map f [0 .. k - 1]

-- Note that if any index is out of bounds, the result of that particular
-- projection is defined and is 0 (but beware of vectorization).
{-
gatherNClosure :: ( ADModeAndNum d r, IsPrimal d (TensorOf (m + n) r)
                  , KnownNat m, KnownNat p, KnownNat n )
               => ShapeInt (m + n) -> ADVal d (TensorOf (p + n) r)
               -> (IndexOf m r -> IndexOf p r)
               -> ADVal d (TensorOf (m + n) r)
               -}
--gatherNClosure sh (D u u') f =
--  dD (tgather sh u f) (dGatherN sh u' f (tshape u))

scalar :: ADModeAndNum d r => ADVal d r -> ADVal d (TensorOf 0 r)
scalar (D u u') = dD (tscalar u) (dScalar1 u')

--unScalar :: ADModeAndNum d r => ADVal d (TensorOf 0 r) -> ADVal d r
--unScalar (D u u') = dD (tunScalar u) (dUnScalar0 u')


-- * Interpretation of Ast in ADVal

-- We are very close to being able to interpret Ast in any Tensor
-- and HasPrimal instance.
-- However, this proves impossible, because we'd need to adorn interpretAst
-- with constraints like RealFloat (Tensor n r) for all @n@ in use,
-- which includes, e.g., @m + p@, where @m@ and @p@ are not mentioned
-- nor can be deduced from the signature of interpretAst.
-- I don't know if we could hack around by creating and explicitly
-- passing the relevant dictionaries. Users of the library may find
-- similar problems in large enough programs, so developing a technique
-- for that would be useful.
-- For now, we interpret only in the concrete ADVal instance,
-- which is the only interpretation needed for anything apart from tests.

type AstEnv (d :: ADMode) r = IM.IntMap (AstVar (ADVal d (OT.Array r)))

data AstVar a =
    AstVarR a
  | AstVarI Int
 deriving Show

extendEnvR :: (ADModeAndNum d r, KnownNat n, TensorOf n r ~ OR.Array n r)
           => AstVarName (OR.Array n r) -> ADVal d (OR.Array n r)
           -> AstEnv d r -> AstEnv d r
extendEnvR v@(AstVarName var) d =
  IM.insertWithKey (\_ _ _ -> error $ "extendEnvR: duplicate " ++ show v)
                   var (AstVarR $ from1X d)

extendEnvI :: AstVarName Int -> Int
           -> AstEnv d r -> AstEnv d r
extendEnvI v@(AstVarName var) i =
  IM.insertWithKey (\_ _ _ -> error $ "extendEnvI: duplicate " ++ show v)
                   var (AstVarI i)

interpretLambdaI
  :: (AstEnv d r -> Ast n r -> b)
  -> AstEnv d r -> (AstVarName Int, Ast n r) -> Int
  -> b
{-# INLINE interpretLambdaI #-}
interpretLambdaI f env (var, ast) =
  \i -> f (extendEnvI var i env) ast

interpretLambdaIndexToIndex
  :: (AstEnv d r -> AstInt r -> Int)
  -> AstEnv d r -> (AstVarList m, AstIndex p r) -> IndexInt m
  -> IndexInt p
{-# INLINE interpretLambdaIndexToIndex #-}
interpretLambdaIndexToIndex f env (vars, asts) =
  \ix -> let assocs = zip (sizedListToList vars) (indexToList ix)
             env2 = foldr (uncurry extendEnvI) env assocs
         in fmap (f env2) asts

class InterpretAst r where
  interpretAst
    :: forall n d. (ADModeAndNum d r, KnownNat n)
    => AstEnv d r -> Ast n r -> ADVal d (OR.Array n r)

instance InterpretAst Double where
 interpretAst = undefined

interpretAstOp :: RealFloat b
               => (c -> b) -> OpCode -> [c] -> b
{-# INLINE interpretAstOp #-}
interpretAstOp f PlusOp [u, v] = f u + f v
interpretAstOp f MinusOp [u, v] = f u - f v
interpretAstOp f TimesOp [u, v] = f u * f v
interpretAstOp f NegateOp [u] = negate $ f u
interpretAstOp f AbsOp [u] = abs $ f u
interpretAstOp f SignumOp [u] = signum $ f u
interpretAstOp f DivideOp [u, v] = f u / f v
interpretAstOp f RecipOp [u] = recip $ f u
interpretAstOp f ExpOp [u] = exp $ f u
interpretAstOp f LogOp [u] = log $ f u
interpretAstOp f SqrtOp [u] = sqrt $ f u
interpretAstOp f PowerOp [u, v] = f u ** f v
interpretAstOp f LogBaseOp [u, v] = logBase (f u) (f v)
interpretAstOp f SinOp [u] = sin $ f u
interpretAstOp f CosOp [u] = cos $ f u
interpretAstOp f TanOp [u] = tan $ f u
interpretAstOp f AsinOp [u] = asin $ f u
interpretAstOp f AcosOp [u] = acos $ f u
interpretAstOp f AtanOp [u] = atan $ f u
interpretAstOp f SinhOp [u] = sinh $ f u
interpretAstOp f CoshOp [u] = cosh $ f u
interpretAstOp f TanhOp [u] = tanh $ f u
interpretAstOp f AsinhOp [u] = asinh $ f u
interpretAstOp f AcoshOp [u] = acosh $ f u
interpretAstOp f AtanhOp [u] = atanh $ f u
interpretAstOp f Atan2Op [u, v] = atan2 (f u) (f v)
interpretAstOp f MaxOp [u, v] = max (f u) (f v)
interpretAstOp f MinOp [u, v] = min (f u) (f v)
interpretAstOp _ opCode args =
  error $ "interpretAstOp: wrong number of arguments"
          ++ show (opCode, length args)

interpretAstIntOp :: (AstInt r -> Int) -> OpCodeInt -> [AstInt r] -> Int
{-# INLINE interpretAstIntOp #-}
interpretAstIntOp f PlusIntOp [u, v] = f u + f v
interpretAstIntOp f MinusIntOp [u, v] = f u - f v
interpretAstIntOp f TimesIntOp [u, v] = f u * f v
interpretAstIntOp f NegateIntOp [u] = negate $ f u
interpretAstIntOp f AbsIntOp [u] = abs $ f u
interpretAstIntOp f SignumIntOp [u] = signum $ f u
interpretAstIntOp f MaxIntOp [u, v] = max (f u) (f v)
interpretAstIntOp f MinIntOp [u, v] = min (f u) (f v)
interpretAstIntOp f QuotIntOp [u, v] = quot (f u) (f v)
interpretAstIntOp f RemIntOp [u, v] = rem (f u) (f v)
interpretAstIntOp _ opCodeInt args =
  error $ "interpretAstIntOp: wrong number of arguments"
          ++ show (opCodeInt, length args)

interpretAstBoolOp :: (AstBool r -> Bool) -> OpCodeBool -> [AstBool r]
                   -> Bool
{-# INLINE interpretAstBoolOp #-}
interpretAstBoolOp f NotOp [u] = not $ f u
interpretAstBoolOp f AndOp [u, v] = f u && f v
interpretAstBoolOp f OrOp [u, v] = f u || f v
interpretAstBoolOp _ opCodeBool args =
  error $ "interpretAstBoolOp: wrong number of arguments"
          ++ show (opCodeBool, length args)

interpretAstRelOp :: Ord b => (a -> b) -> OpCodeRel -> [a] -> Bool
{-# INLINE interpretAstRelOp #-}
interpretAstRelOp f EqOp [u, v] = f u == f v
interpretAstRelOp f NeqOp [u, v] = f u /= f v
interpretAstRelOp f LeqOp [u, v] = f u <= f v
interpretAstRelOp f GeqOp [u, v] = f u >= f v
interpretAstRelOp f LsOp [u, v] = f u < f v
interpretAstRelOp f GtOp [u, v] = f u > f v
interpretAstRelOp _ opCodeRel args =
  error $ "interpretAstRelOp: wrong number of arguments"
          ++ show (opCodeRel, length args)
