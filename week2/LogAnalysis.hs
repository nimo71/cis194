{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
 
import Log
 
parseMessage :: String -> LogMessage
parseMessage message = case (words message) of
  ("I":t:ws) -> LogMessage Info (read t) (unwords ws)
  ("W":t:ws) -> LogMessage Warning (read t) (unwords ws)
  ("E":e:t:ws) -> LogMessage (Error (read e)) (read t) (unwords ws)
  _ -> Unknown message
 
parse :: String -> [LogMessage]
parse ls = [parseMessage l | l <- (lines ls)]
 
insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = (Node Leaf msg Leaf)
insert msg @ (LogMessage _ ti _) (Node l m @ (LogMessage _ t _) r)
         | (t > ti) = (Node l m (insert msg r))
insert msg @ (LogMessage _ _ _) (Node l m r) = (Node (insert msg l) m r)
insert _ tree = tree
 
build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] t = t
build' (m:ms) t = build' ms (insert m t)
 
build :: [LogMessage] -> MessageTree
build ms = build' ms Leaf
 
 
inOrder' :: MessageTree -> [LogMessage] -> [LogMessage]
inOrder' Leaf               ms = ms
inOrder' (Node Leaf m Leaf) ms = m : ms
inOrder' (Node l m r)       ms = (inOrder' l (m : (inOrder' r ms)))
 
inOrder :: MessageTree -> [LogMessage]
inOrder t = inOrder' t []

whatWentWrong' :: [LogMessage] -> [String] -> [String]
whatWentWrong' []                               ws = ws
whatWentWrong' ((LogMessage (Error e) _ m) : t) ws
                 | e > 50                          = whatWentWrong' t (m : ws)
whatWentWrong' ls                               ws = whatWentWrong' (tail ls) ws

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = whatWentWrong' lm []
