module BubbleT (Air (Air), Bubble, BubbleT, pop, blow)
where
-------------------------------------------------------------------------------
-- The Bubble comonad and the BubbleT comonad transformer. Born of a very silly
-- pun.
--
-- Features:
--   - Blow bubbles with air inside of them!
--   - Use fmap on your bubbles to fill them with *cool stuff*!
--   - Pop bubbles to get whatever's inside!
--   - Create huge nested bubbles!
--   - Make your monads more *bubbly*!
--   - Drink your bubble tea while playing with some BubbleT!

import Data.Functor.Identity (Identity)
import Control.Comonad (Comonad (extract, duplicate))
import Control.Comonad.Trans.Class (ComonadTrans (lower))

-- Bubble type synonym
type Bubble = BubbleT Identity

-- No, this isn't ()
data Air = Air deriving Show

-- BubbleT type
data BubbleT m a = BubbleT (m a)

-- You can pop bubbles (as long as w cooperates)
pop :: (Comonad w) => BubbleT w a -> a
pop (BubbleT w) = extract w

-- You can blow bubbles (as long as f cooperates)
blow :: (Applicative f) => Air -> BubbleT f Air
blow = BubbleT . pure

-- BubbleT is a ComonadTrans
instance ComonadTrans BubbleT where
    lower (BubbleT w) = w

instance (Comonad w) => Comonad (BubbleT w) where
    extract = pop
    duplicate (BubbleT w) = BubbleT $ fmap BubbleT $ duplicate w

instance (Functor w) => Functor (BubbleT w) where
    fmap f (BubbleT w) = BubbleT (f <$> w)

-- We can observe what is inside the bubble
instance (Show (w a)) => Show (BubbleT w a) where
    show (BubbleT w) = "BubbleT (" ++ show w ++ ")"
