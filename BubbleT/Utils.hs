module BubbleT.Utils (observe)
where

import Control.Comonad (Comonad)
import BubbleT

observe :: (Show (w a)) => BubbleT w a -> String
observe = show

-- Unfortunately, BubbleT is not an instance of MonadTrans.

-- Instances of Monad and Applicative that will work for Bubble
instance (Comonad m, Monad m) => Monad (BubbleT m) where
    b1 >>= f = f $ pop b1

instance (Comonad f, Applicative f) => Applicative (BubbleT f) where
    -- You can put anything in a bubble, I guess
    -- but this is totally cheating, you should blow your own bubbles
    -- smh my head
    pure      = (<$ blow Air)
    b1 <*> b2 = pure $ pop b1 $ pop b2
