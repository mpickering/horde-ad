{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -freduction-depth=10000 #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Term-simplifying combinators corresponding to the Ast constructors.
-- They simplify only on the basis of inspecting the roots of their
-- argument term trees. If the arguments get modified as a result,
-- the modified forms are again inspected and may be simplified.
-- However, no unbounded depth inspection nor simplification of terms
-- takes place. This limited simplification is enough to uncover redexes
-- for the vectorization rules to fire and to undo some of the complication
-- introduced by vectorization. The intention is to leave as much
-- of the original terms provided by the user as possible while making
-- sure everything introduced by vectorization is maximally simplified.
--
-- The combinator can also be used to simplify a whole term, bottom-up.
module HordeAd.Core.AstSimplify ()
   where

