{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | 'Tensor' class instances for 'Ast' terms. Some of these instances
-- vectorize any terms starting with 'build1' and so eliminated the constructor.
module HordeAd.Core.TensorAst
  (
  ) where

import Prelude

import qualified Data.Array.DynamicS as OD
import qualified Data.Array.RankedS as OR
import           Data.Bifunctor.Flip
import           Data.Proxy (Proxy (Proxy))
import qualified Data.Strict.Vector as Data.Vector
import           Data.Type.Equality ((:~:) (Refl))
import qualified Data.Vector.Generic as V
import           GHC.TypeLits (KnownNat, sameNat, type (+))

import HordeAd.Core.Ast
import HordeAd.Core.AstFreshId
import HordeAd.Core.AstSimplify
import HordeAd.Core.AstTools
import HordeAd.Core.AstVectorize
import HordeAd.Core.Domains
import HordeAd.Core.SizedIndex
import HordeAd.Core.TensorClass

instance ShowAstSimplify r
         => Tensor (Ast0 r) where
  type Ranked (Ast0 r) = AstRanked r
  type IntOf (Ast0 r) = AstInt r

  tlet a f = astLetFun a f

  tshape = shapeAst
  tminIndex0 = AstMinIndex1 . AstPrimalPart
  tmaxIndex0 = AstMaxIndex1 . AstPrimalPart
  tfloor = AstIntFloor . AstPrimalPart

  tindex = AstIndexZ
  tsum = AstSum
  tfromIndex0 i = AstConstant $ AstPrimalPart
                  $ AstIndexZ AstIota (singletonIndex i)
    -- toInteger is not defined for Ast, hence a special implementation
  tscatter sh t f = astScatter sh t (funToAstIndex f)  -- introduces new vars

  tfromList = AstFromList
  tfromVector = AstFromVector
  tkonst = AstKonst
  tappend = AstAppend
  tslice = AstSlice
  treverse = AstReverse
  ttranspose = AstTranspose
  treshape = astReshape
  tbuild1 = astBuild1Vectorize
  tgather sh t f = AstGatherZ sh t (funToAstIndex f)  -- introduces new vars

  tscalar = unAst0
  tunScalar = Ast0
    -- due to injective type families, we have to distinguish Ast0 and Ast 0

  tsumOfList l = AstSumOfList l

  type Primal (Ast0 r) = AstPrimalPart 0 r
  type DualOf n (Ast0 r) = AstDualPart n r
  tconst = AstConstant . AstPrimalPart . AstConst
  tconstant = astConstant
  tscale0 (AstPrimalPart r) (Ast0 t) = Ast0 (r * t)
  tprimalPart = AstPrimalPart
  tdualPart = AstDualPart
  tD = AstD
  tScale (AstPrimalPart s) (AstDualPart t) = AstDualPart $ s `tmult` t

  tregister = astRegisterFun
  tletWrap = AstLetADShare
    -- We can't use astLet here, because it may inline a let that is
    -- present at the top level of the dual number and so we'd lose
    -- sharing that is not visible in this restricted context.
    -- To make sure astLet is not used on these, we mark them with
    -- a special constructor that also makes comparing lets cheap.

  tfromD = astFromDynamic
  dfromR r = AstDynamic r

instance DynamicTensor (Ast0 r) where
  type DTensorOf (Ast0 r) = AstDynamic r
  ddummy = AstDynamic AstIota
  disDummy t = case t of
    AstDynamic AstIota -> True
    _ -> False

-- This is used for domains composed of variables only, to adapt them
-- into more complex types and back again. This is not used for
-- vectors of large terms, since they'd share values, so we'd need
-- AstDomainsLet, but these would make adapting the vector costly.
-- DomainsOf is used for that and the only reasons DomainsOf exists
-- is to prevent mixing up the two (and complicating the definition
-- below with errors in the AstDomainsLet case).
instance ShowAstSimplify r
         => DomainsCollection (Ast0 r) where
  type Domains (Ast0 r) = Data.Vector.Vector (AstDynamic r)
  doms0 v = v V.! 0
  domsR v = V.slice 1 (V.length v - 1) v
  mkDoms = V.cons
  emptyDoms0 = AstDynamic @1 (AstFromList [])
  isEmptyDoms params = V.null (domsR params) && case doms0 params of
    AstDynamic t -> sizeShape (shapeAst t) == 0
  uncons0 params =
    let v = tfromD $ doms0 params
    in case tuncons v of
      Nothing -> Nothing
      Just (h, rest) ->
        Just (tunScalar h, mkDoms (dfromR rest) (domsR params))
  unconsR params =
    let v = domsR params
    in case V.uncons v of
      Nothing -> Nothing
      Just (h, rest) ->
        Just (h, mkDoms (doms0 params) rest)
  concatDoms0 = dfromR @(Ast0 r) @1 . foldr1 tappend . map tfromD
  concatDomsR = V.concat
  fromListDoms = V.fromList
  toListDoms = V.toList
  fromVectorDoms = id
  toVectorDoms = id

instance ShowAstSimplify r
         => AdaptableDomains (Ast0 r) where
  type Scalar (Ast0 r) = Ast0 r  -- TODO: or AstPrimalPartRanked r 0 ?
  type Value (Ast0 r) = r
  toDomains = undefined
  fromDomains _aInit = uncons0
  nParams = undefined
  nScalars = undefined

instance ( Tensor r, ShowAstSimplify r, KnownNat n
         , Ranked r ~ Flip OR.Array r )
         => AdaptableDomains (Ast n r) where
  type Scalar (Ast n r) = Ast0 r
  type Value (Ast n r) = OR.Array n r
  toDomains = undefined
  fromDomains aInit params = case unconsR params of
    Just (a, rest) -> Just (ttoRankedOrDummy (tshape $ Flip aInit) a, rest)
    Nothing -> Nothing
  nParams = undefined
  nScalars = undefined

instance AdaptableDomains (AstDynamic r) where
  type Scalar (AstDynamic r) = Ast0 r
  type Value (AstDynamic r) = OD.Array r
  toDomains = undefined
  fromDomains = undefined
  nParams = undefined
  nScalars = undefined

instance AdaptableDomains (AstPrimalPartRanked r n) where
  type Scalar (AstPrimalPartRanked r n) = AstPrimalPartRanked r 0
  type Value (AstPrimalPartRanked r n) = r
  toDomains = undefined
  fromDomains = undefined
  nParams = undefined
  nScalars = undefined

instance AdaptableDomains (AstNoVectorize r n) where
  type Scalar (AstNoVectorize r n) = AstNoVectorize r 0
  type Value (AstNoVectorize r n) = r
  toDomains = undefined
  fromDomains = undefined
  nParams = undefined
  nScalars = undefined

instance AdaptableDomains (AstNoSimplify r n) where
  type Scalar (AstNoSimplify r n) = AstNoSimplify r 0
  type Value (AstNoSimplify r n) = r
  toDomains = undefined
  fromDomains = undefined
  nParams = undefined
  nScalars = undefined

instance ShowAst r
         => DomainsTensor (Ast0 r) where
  daddR :: forall n q. (KnownNat n, q ~ (Ast0 r))
        => TensorOf n q -> DTensorOf q -> DTensorOf q
  daddR r (AstDynamic AstIota) = AstDynamic r
  daddR r (AstDynamic @n2 (AstSumOfList l)) =
    case sameNat (Proxy @n) (Proxy @n2) of
      Just Refl -> AstDynamic (AstSumOfList (r : l))
      _ -> error "daddR: type mismatch"
  daddR r (AstDynamic @n2 v) =
    case sameNat (Proxy @n) (Proxy @n2) of
      Just Refl -> AstDynamic (AstSumOfList [r, v])
      _ -> error "daddR: type mismatch"
  dshape (AstDynamic v) = shapeToList $ shapeAst v
  type DomainsOf (Ast0 r) = AstDomains r
  tletDomains = astLetDomainsFun
  dmkDomains = AstDomains
  dlet = astDomainsLetFun

astLetFun :: (KnownNat n, KnownNat m, ShowAst r)
          => Ast n r -> (Ast n r -> Ast m r) -> Ast m r
astLetFun a@AstVar{} f = f a
astLetFun a f =
  let sh = shapeAst a
      (AstVarName var, ast) = funToAstR sh f
  in astLet var a ast  -- safe, because subsitution ruled out above

astLetDomainsFun
  :: forall m r. ShowAst r
  => AstDomains r
  -> (AstDomains r -> Ast m r)
  -> Ast m r
astLetDomainsFun a f =
  let genVar :: AstDynamic r -> (AstVarId, AstDynamic r)
      genVar (AstDynamic t) =
        let sh = shapeAst t
            (AstVarName var, ast) = funToAstR sh id
        in (var, AstDynamic ast)
      (vars, asts) = V.unzip $ V.map genVar (unwrapAstDomains a)
  in AstLetDomains vars a (f $ AstDomains asts)

astDomainsLetFun :: (KnownNat n, ShowAst r)
                 => Ast n r -> (Ast n r -> AstDomains r)
                 -> AstDomains r
astDomainsLetFun a@AstVar{} f = f a
astDomainsLetFun a f =
  let sh = shapeAst a
      (AstVarName var, ast) = funToAstR sh id
  in astDomainsLet var a (f ast)  -- safe, because subsitution ruled out above

-- This is a vectorizing combinator that also simplifies
-- the terms touched during vectorization, but not any others.
-- Due to how the Ast instance of Tensor is defined above, vectorization
-- works bottom-up, which removes the need to backtrack in the vectorization
-- pass or repeat until a fixed point is reached.
-- This combinator also introduces new variable names.
astBuild1Vectorize :: (KnownNat n, ShowAstSimplify r)
                   => Int -> (AstInt r -> Ast n r) -> Ast (1 + n) r
astBuild1Vectorize k f = build1Vectorize k $ funToAstI f

instance ShowAstSimplify r
         => Tensor (AstPrimalPart 0 r) where
  type Ranked (AstPrimalPart 0 r) = AstPrimalPartRanked r
  type IntOf (AstPrimalPart 0 r) = AstInt r

  tlet a f =
    AstPrimalPart
    $ astLetFun (unAstPrimalPart a) (unAstPrimalPart . f . AstPrimalPart)

  tshape = shapeAst . unAstPrimalPart
  tminIndex0 = AstMinIndex1
  tmaxIndex0 = AstMaxIndex1
  tfloor = AstIntFloor

  tindex v ix = AstPrimalPart $ AstIndexZ (unAstPrimalPart v) ix
  tsum = AstPrimalPart . AstSum . unAstPrimalPart
  tfromIndex0 i = AstPrimalPart
                  $ AstIndexZ AstIota (singletonIndex i)
    -- toInteger is not defined for Ast, hence a special implementation
  tscatter sh t f = AstPrimalPart $ astScatter sh (unAstPrimalPart t)
                    $ funToAstIndex f  -- this introduces new variable names

  tfromList = AstPrimalPart . AstFromList . map unAstPrimalPart
  tfromVector = AstPrimalPart . AstFromVector . V.map unAstPrimalPart
  tkonst k = AstPrimalPart . AstKonst k . unAstPrimalPart
  tappend u v =
    AstPrimalPart $ AstAppend (unAstPrimalPart u) (unAstPrimalPart v)
  tslice i k = AstPrimalPart . AstSlice i k . unAstPrimalPart
  treverse = AstPrimalPart . AstReverse . unAstPrimalPart
  ttranspose perm = AstPrimalPart . AstTranspose perm . unAstPrimalPart
  treshape sh = AstPrimalPart . astReshape sh . unAstPrimalPart
  tbuild1 k f = AstPrimalPart $ astBuild1Vectorize k (unAstPrimalPart . f)
  tgather sh t f = AstPrimalPart $ AstGatherZ sh (unAstPrimalPart t)
                   $ funToAstIndex f  -- this introduces new variable names

  tscalar = id
  tunScalar = id
    -- identifying AstPrimalPart 0 with primal part scalars lets us avoid
    -- adding a lot of constraints to ADReady

  tsumOfList l = AstPrimalPart . AstSumOfList . map unAstPrimalPart $ l

  type Primal (AstPrimalPart 0 r) = AstPrimalPart 0 r
  type DualOf n (AstPrimalPart 0 r) = ()
  tconst = AstPrimalPart . AstConst
  tconstant = id
  tscale0 r d = r * d
  tprimalPart = id
  tdualPart _ = ()
  tD u _ = u
  tScale _ _ = ()

  tregister = undefined
  tletWrap = undefined

  tfromD = undefined
  dfromR = undefined

instance ShowAstSimplify r
         => Tensor (AstNoVectorize r 0) where
  type Ranked (AstNoVectorize r 0) = AstNoVectorize r
  type IntOf (AstNoVectorize r 0) = AstInt r

  tlet a f =
    AstNoVectorize
    $ astLetFun (unAstNoVectorize a) (unAstNoVectorize . f . AstNoVectorize)

  tshape = shapeAst . unAstNoVectorize
  tminIndex0 = AstMinIndex1 . AstPrimalPart . unAstNoVectorize
  tmaxIndex0 = AstMaxIndex1 . AstPrimalPart . unAstNoVectorize
  tfloor = AstIntFloor . AstPrimalPart . unAstNoVectorize

  tindex v ix = AstNoVectorize $ AstIndexZ (unAstNoVectorize v) ix
  tsum = AstNoVectorize . AstSum . unAstNoVectorize
  tfromIndex0 i = AstNoVectorize $ AstConstant $ AstPrimalPart
                  $ AstIndexZ AstIota (singletonIndex i)
    -- toInteger is not defined for Ast, hence a special implementation
  tscatter sh t f = AstNoVectorize $ astScatter sh (unAstNoVectorize t)
                    $ funToAstIndex f  -- this introduces new variable names

  tfromList = AstNoVectorize . AstFromList . map unAstNoVectorize
  tfromVector = AstNoVectorize . AstFromVector . V.map unAstNoVectorize
  tkonst k = AstNoVectorize . AstKonst k . unAstNoVectorize
  tappend u v =
    AstNoVectorize $ AstAppend (unAstNoVectorize u) (unAstNoVectorize v)
  tslice i k = AstNoVectorize . AstSlice i k . unAstNoVectorize
  treverse = AstNoVectorize . AstReverse . unAstNoVectorize
  ttranspose perm = AstNoVectorize . AstTranspose perm . unAstNoVectorize
  treshape sh = AstNoVectorize . astReshape sh . unAstNoVectorize
  tbuild1 k f = AstNoVectorize $ AstBuild1 k
                $ funToAstI  -- this introduces new variable names
                $ unAstNoVectorize . f
  tgather sh t f = AstNoVectorize $ AstGatherZ sh (unAstNoVectorize t)
                   $ funToAstIndex f  -- this introduces new variable names

  tscalar = id
  tunScalar = id

  tsumOfList l = AstNoVectorize . AstSumOfList . map unAstNoVectorize $ l

  type Primal (AstNoVectorize r 0) = AstNoVectorize r 0
  type DualOf n (AstNoVectorize r 0) = AstDualPart n r
  tconst = AstNoVectorize . AstConstant . AstPrimalPart . AstConst
  tconstant = AstNoVectorize . astConstant . AstPrimalPart . unAstNoVectorize
  tscale0 r d = r * d
  tprimalPart = id
  tdualPart = AstDualPart . unAstNoVectorize
  tD u u' = AstNoVectorize $ AstD (AstPrimalPart $ unAstNoVectorize u) u'
  tScale (AstNoVectorize s) (AstDualPart t) = AstDualPart $ s `tmult` t

  tregister = undefined
  tletWrap = undefined

  tfromD = undefined
  dfromR = undefined

instance ShowAstSimplify r
         => Tensor (AstNoSimplify r 0) where
  type Ranked (AstNoSimplify r 0) = AstNoSimplify r
  type IntOf (AstNoSimplify r 0) = AstInt r

  tlet a f =
    AstNoSimplify
    $ astLetFunUnSimp (unAstNoSimplify a) (unAstNoSimplify . f . AstNoSimplify)

  tshape = shapeAst . unAstNoSimplify
  tminIndex0 = AstMinIndex1 . AstPrimalPart . unAstNoSimplify
  tmaxIndex0 = AstMaxIndex1 . AstPrimalPart . unAstNoSimplify
  tfloor = AstIntFloor . AstPrimalPart . unAstNoSimplify

  tindex v ix = AstNoSimplify $ AstIndexZ (unAstNoSimplify v) ix
  tsum = AstNoSimplify . AstSum . unAstNoSimplify
  tfromIndex0 i = AstNoSimplify $ AstConstant $ AstPrimalPart
                  $ AstIndexZ AstIota (singletonIndex i)
    -- toInteger is not defined for Ast, hence a special implementation
  tscatter sh t f = AstNoSimplify $ AstScatter sh (unAstNoSimplify t)
                    $ funToAstIndex f  -- this introduces new variable names

  tfromList = AstNoSimplify . AstFromList . map unAstNoSimplify
  tfromVector = AstNoSimplify . AstFromVector . V.map unAstNoSimplify
  tkonst k = AstNoSimplify . AstKonst k . unAstNoSimplify
  tappend u v =
    AstNoSimplify $ AstAppend (unAstNoSimplify u) (unAstNoSimplify v)
  tslice i k = AstNoSimplify . AstSlice i k . unAstNoSimplify
  treverse = AstNoSimplify . AstReverse . unAstNoSimplify
  ttranspose perm = AstNoSimplify . AstTranspose perm . unAstNoSimplify
  treshape sh = AstNoSimplify . AstReshape sh . unAstNoSimplify
  tbuild1 k f = AstNoSimplify $ astBuild1Vectorize k (unAstNoSimplify . f)
  tgather sh t f = AstNoSimplify $ AstGatherZ sh (unAstNoSimplify t)
                   $ funToAstIndex f  -- this introduces new variable names

  tscalar = id
  tunScalar = id

  tsumOfList l = AstNoSimplify . AstSumOfList . map unAstNoSimplify $ l

  type Primal (AstNoSimplify r 0) = AstNoSimplify r 0
  type DualOf n (AstNoSimplify r 0) = AstDualPart n r
  tconst = AstNoSimplify . AstConstant . AstPrimalPart . AstConst
  tconstant = AstNoSimplify . astConstant . AstPrimalPart . unAstNoSimplify
    -- exceptionally we do simplify AstConstant to avoid long boring chains
  tscale0 r d = r * d
  tprimalPart = id
  tdualPart = AstDualPart . unAstNoSimplify
  tD u u' = AstNoSimplify $ AstD (AstPrimalPart $ unAstNoSimplify u) u'
  tScale (AstNoSimplify s) (AstDualPart t) = AstDualPart $ s `tmult` t

  tregister = undefined
  tletWrap = undefined

  tfromD = undefined
  dfromR = undefined

astLetFunUnSimp :: (KnownNat n, KnownNat m, ShowAst r)
                => Ast n r -> (Ast n r -> Ast m r) -> Ast m r
astLetFunUnSimp a f =
  let sh = shapeAst a
      (AstVarName var, ast) = funToAstR sh f
  in AstLet var a ast
