-- | Operations that (usually impurely) generate fresh variables.
module HordeAd.Core.AstFreshId
  ( astRegisterFun, astRegisterADShare
  , funToAstR, funToAstD, ADAstVars, funToAstAll
  , funToAstIIO, funToAstI, funToAstIndexIO, funToAstIndex
  , resetVarCounter
  ) where

import Prelude

import           Control.Monad (replicateM)
import           Data.Array.Internal (valueOf)
import qualified Data.Array.RankedS as OR
import           Data.IORef.Unboxed
  (Counter, atomicAddCounter_, newCounter, writeIORefU)
import           Data.Proxy (Proxy)
import           GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
import           System.IO.Unsafe (unsafePerformIO)

import HordeAd.Core.Ast
import HordeAd.Core.AstTools
import HordeAd.Core.SizedIndex
import HordeAd.Internal.SizedList

-- Impure but in the most trivial way (only ever incremented counter).
unsafeAstVarCounter :: Counter
{-# NOINLINE unsafeAstVarCounter #-}
unsafeAstVarCounter = unsafePerformIO (newCounter 100000001)

-- Only for tests, e.g., to ensure show applied to terms has stable length.
-- Tests using this need to be run with -ftest_seq to avoid variable confusion.
resetVarCounter :: IO ()
resetVarCounter = writeIORefU unsafeAstVarCounter 100000001

unsafeGetFreshAstVarId :: IO AstVarId
{-# INLINE unsafeGetFreshAstVarId #-}
unsafeGetFreshAstVarId =
  intToAstVarId <$> atomicAddCounter_ unsafeAstVarCounter 1

astRegisterFun :: (ShowAst r, KnownNat n)
               => Ast n r -> [(AstVarId, AstDynamic r)]
               -> ([(AstVarId, AstDynamic r)], Ast n r)
{-# NOINLINE astRegisterFun #-}
astRegisterFun !r@AstVar{} !l = (l, r)
astRegisterFun r l = unsafePerformIO $ do
  freshId <- unsafeGetFreshAstVarId
  let !r2 = AstVar (shapeAst r) freshId
  return ((freshId, AstDynamic r) : l, r2)

astRegisterADShare :: (ShowAst r, KnownNat n)
                   => Ast n r -> ADShare r
                   -> (ADShare r, Ast n r)
{-# NOINLINE astRegisterADShare #-}
astRegisterADShare !r@AstVar{} !l = (l, r)
astRegisterADShare r l = unsafePerformIO $ do
  freshId <- unsafeGetFreshAstVarId
  let !l2 = insertADShare freshId (AstDynamic r) l
      !r2 = AstVar (shapeAst r) freshId
  return (l2, r2)

funToAstRIO :: ShapeInt n -> (Ast n r -> Ast m r)
            -> IO (AstVarName (OR.Array n r), Ast m r)
{-# INLINE funToAstRIO #-}
funToAstRIO sh f = do
  freshId <- unsafeGetFreshAstVarId
  return (AstVarName freshId, f (AstVar sh freshId))

funToAstR :: ShapeInt n -> (Ast n r -> Ast m r)
          -> (AstVarName (OR.Array n r), Ast m r)
{-# NOINLINE funToAstR #-}
funToAstR sh f = unsafePerformIO $ funToAstRIO sh f

funToAstRshIO :: IO (AstVarName (OR.Array n r), ShapeInt n -> Ast n r)
{-# INLINE funToAstRshIO #-}
funToAstRshIO = do
  freshId <- unsafeGetFreshAstVarId
  return (AstVarName freshId, \sh -> AstVar sh freshId)

-- The "fun"ction in this case is fixed to be @id@.
funToAstDIO :: forall r. [Int] -> IO (AstDynamicVarName r, AstDynamic r)
{-# INLINE funToAstDIO #-}
funToAstDIO sh = do
  freshId <- unsafeGetFreshAstVarId
  return $! case someNatVal $ toInteger $ length sh of
    Just (SomeNat (_proxy :: Proxy p)) ->
      let shn = listShapeToShape @p sh
          varName = AstVarName @(OR.Array p r) freshId
      in (AstDynamicVarName varName, AstDynamic (AstVar shn freshId))
    Nothing -> error "funToAstD: impossible someNatVal error"

funToAstD :: forall r. [Int] -> (AstDynamicVarName r, AstDynamic r)
{-# NOINLINE funToAstD #-}
funToAstD sh = unsafePerformIO $ funToAstDIO sh

type ADAstVars n r = ( Ast 1 r
                     , ShapeInt n -> Ast n r
                     , [AstDynamic r] )

funToAstAll :: ShapeInt 1 -> [[Int]] -> (ADAstVarNames n r, ADAstVars n r)
{-# NOINLINE funToAstAll #-}
funToAstAll sh shapes1 = unsafePerformIO $ do
  (vn0, v0) <- funToAstRIO sh id
  (vnDt, vDt) <- funToAstRshIO
  (vn1, v1) <- unzip <$> (mapM funToAstDIO shapes1)
  return ((vn0, vnDt, vn1), (v0, vDt, v1))

funToAstIIO :: (AstInt r -> t) -> IO (AstVarId, t)
{-# INLINE funToAstIIO #-}
funToAstIIO f = do
  freshId <- unsafeGetFreshAstVarId
  return (freshId, f (AstIntVar freshId))

funToAstI :: (AstInt r -> t) -> (AstVarId, t)
{-# NOINLINE funToAstI #-}
funToAstI = unsafePerformIO . funToAstIIO

funToAstIndexIO
  :: forall m p r. KnownNat m
  => Int -> (AstIndex m r -> AstIndex p r) -> IO (AstVarList m, AstIndex p r)
{-# INLINE funToAstIndexIO #-}
funToAstIndexIO p f = do
  varList <- replicateM p unsafeGetFreshAstVarId
  return (listToSized varList, f (listToIndex $ map AstIntVar varList))

funToAstIndex
  :: forall m p r. KnownNat m
  => (AstIndex m r -> AstIndex p r) -> (AstVarList m, AstIndex p r)
{-# NOINLINE funToAstIndex #-}
funToAstIndex = unsafePerformIO . funToAstIndexIO (valueOf @m)
