{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseTokens :: [String] -> LogMessage
parseTokens ("I":t:ws) = LogMessage Info (read t) (unwords ws)
parseTokens ("W":t:ws) = LogMessage Warning (read t) (unwords ws)
parseTokens ("E":s:t:ws) = LogMessage (Error (read s)) (read t) (unwords ws)
parseTokens ws = Unknown (unwords ws)

parseMessage :: String -> LogMessage
parseMessage = parseTokens . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) t@(Node l v@(LogMessage _ tv _) r )
    | ts < tv = Node (insert m l) v r
    | ts > tv = Node l v (insert m r)
    | otherwise = t 
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf . reverse -- insert in the same order as it's in the given list

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l v r) = (inOrder l) ++ [v] ++ (inOrder r)

seriousError :: Int -> LogMessage -> Bool
seriousError minimumSeverity (LogMessage (Error s) _ _) = s >= minimumSeverity
seriousError _ _ = False

messageText :: LogMessage -> String
messageText (LogMessage _ _ m) = m
messageText (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageText . filter (seriousError 50) . inOrder . build
