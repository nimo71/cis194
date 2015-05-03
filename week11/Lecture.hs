module Lecture where

import Control.Applicative


--------------------------------------------------------------------
-- Lecture problems
--------------------------------------------------------------------

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = (\_ y -> y) <$> fa <*> fb   


mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA mf = (\l -> case l of
                  []     -> pure []
                  (x:xs) -> (\as bs -> as ++ bs) <$> (pure <$> mf x) <*> (mapA mf xs))


sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA []       = pure []
sequenceA (fa:fas) = (\as bs -> as ++ bs) <$> (pure <$> fa) <*> (sequenceA fas)
  

replicateA :: Applicative f => Int -> f a -> f [a]
--replicateA 1 fa = pure <$> fa
--replicateA i fa = (\as bs -> as ++ bs) <$> (pure <$> fa) <*> (replicateA (i - 1) fa)
replicateA n fa = (replicate n) <$> fa
-- or, replicateA n f = sequenceA (replicate n f)
