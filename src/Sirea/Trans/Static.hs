{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | Static - augment a behavior with static information, applying an
-- applicative or monad to the whole arrow. 
module Sirea.Trans.Static
    ( StaticB
    , toStatic, runStatic
    ) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative
import Sirea.Behavior

newtype StaticB f b x y = SB (f (b x y))

toStatic :: f (b x y) -> StaticB f b x y
toStatic = SB

runStatic :: StaticB f b x y -> f (b x y)
runStatic (SB fbxy) = fbxy

instance (Applicative f) => BEmbed b (StaticB f b) where
    bembed = SB . pure

instance (Category b, Applicative f) => Category (StaticB f b) where
    id = SB $ pure id
    (SB f) . (SB g) = SB $ (.) <$> f <*> g





{-
instance (Arrow a, Applicative f) => Arrow (StaticArrow f a) where
	arr f = SA (pure (arr f))
	first (SA f) = SA (first <$> f)

-- The following promotions follow directly from the arrow transformer.

instance (ArrowZero a, Applicative f) => ArrowZero (StaticArrow f a) where
	zeroArrow = lift zeroArrow

instance (ArrowCircuit a, Applicative f) => ArrowCircuit (StaticArrow f a) where
	delay x = lift (delay x)

instance (ArrowError ex a, Applicative f) => ArrowError ex (StaticArrow f a) where
	raise = lift raise
	handle (SA f) (SA h) = SA (handle <$> f <*> h)
	tryInUnless (SA f) (SA s) (SA h) = SA (tryInUnless <$> f <*> s <*> h)

instance (ArrowReader r a, Applicative f) => ArrowReader r (StaticArrow f a) where
	readState = lift readState
	newReader (SA f) = SA (newReader <$> f)

instance (ArrowState s a, Applicative f) => ArrowState s (StaticArrow f a) where
	fetch = lift fetch
	store = lift store

instance (ArrowWriter w a, Applicative f) => ArrowWriter w (StaticArrow f a) where
	write = lift write
	newWriter (SA f) = SA (newWriter <$> f)

-- Classes that are preserved.

instance (ArrowChoice a, Applicative f) => ArrowChoice (StaticArrow f a) where
	left (SA f) = SA (left <$> f)

-- ArrowApply is generally not preserved.

instance (ArrowLoop a, Applicative f) => ArrowLoop (StaticArrow f a) where
	loop (SA f) = SA (loop <$> f)

instance (ArrowPlus a, Applicative f) => ArrowPlus (StaticArrow f a) where
	SA f <+> SA g = SA ((<+>) <$> f <*> g)

-- Other instances

instance (Arrow a, Applicative f) => Functor (StaticArrow f a b) where
	fmap f g = g >>> arr f

instance (Arrow a, Applicative f) => Applicative (StaticArrow f a b) where
	pure x = arr (const x)
	f <*> g = f &&& g >>> arr (uncurry id)

instance (ArrowPlus a, Applicative f) => Alternative (StaticArrow f a b) where
	empty = zeroArrow
	f <|> g = f <+> g

instance (ArrowPlus a, Applicative f) => Monoid (StaticArrow f a b c) where
	mempty = zeroArrow
	mappend f g = f <+> g

-- promotions

instance (ArrowAddStream a a', Applicative f) =>
		ArrowAddStream (StaticArrow f a) (StaticArrow f a') where
	liftStream (SA f) = SA (liftStream <$> f)
	elimStream (SA f) = SA (elimStream <$> f)

instance (ArrowAddState s a a', Applicative f) =>
		ArrowAddState s (StaticArrow f a) (StaticArrow f a') where
	liftState (SA f) = SA (liftState <$> f)
	elimState (SA f) = SA (elimState <$> f)

instance (ArrowAddReader r a a', Applicative f) =>
		ArrowAddReader r (StaticArrow f a) (StaticArrow f a') where
	liftReader (SA f) = SA (liftReader <$> f)
	elimReader (SA f) = SA (elimReader <$> f)

instance (ArrowAddWriter w a a', Applicative f) =>
		ArrowAddWriter w (StaticArrow f a) (StaticArrow f a') where
	liftWriter (SA f) = SA (liftWriter <$> f)
	elimWriter (SA f) = SA (elimWriter <$> f)

instance (ArrowAddError ex a a', Applicative f) =>
		ArrowAddError ex (StaticArrow f a) (StaticArrow f a') where
	liftError (SA f) = SA (liftError <$> f)
	elimError (SA f) (SA h) = SA (elimError <$> f <*> h)

-- | A special case.

type StaticArrowArrow a s = StaticArrow (WrappedArrow a s)

wrapA :: (Arrow a, Arrow a') => a s (a' b c) -> StaticArrowArrow a s a' b c
wrapA x = SA (WrapArrow x)

unwrapA :: (Arrow a, Arrow a') => StaticArrowArrow a s a' b c -> a s (a' b c)
unwrapA (SA (WrapArrow x)) = x

-- | A special case is monads applied to the whole arrow, in contrast to
-- 'Kleisli' arrows, in which the monad is applied to the output.

type StaticMonadArrow m = StaticArrow (WrappedMonad m)

wrapM :: (Monad m, Arrow a) => m (a b c) -> StaticMonadArrow m a b c
wrapM x = SA (WrapMonad x)

unwrapM :: (Monad m, Arrow a) => StaticMonadArrow m a b c -> m (a b c)
unwrapM (SA (WrapMonad x)) = x
-}

