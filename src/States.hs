module States where

import           Control.Monad
import           Control.Monad.Trans.State

get' :: State s s
get' = get >>= \s -> put s *> return s

put' :: s -> State s ()
put' s = put s *> return ()

exec :: State s a -> s -> s
exec sa s = snd (runState sa s)

eval :: State s a -> s -> a
eval sa s = fst (runState sa s)

modify'' :: (s -> s) -> State s ()
modify'' f = get >>= \s -> put (f s)
