{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

joinList :: [String] -> String
joinList l 
    | l == [] = ""
    | length l == 1 = head l
    | otherwise = head l ++ [' '] ++ joinList (tail l)

parseMessage :: String -> LogMessage
parseMessage s 
    | head s == 'W' = LogMessage Warning (read(head w) :: Int) (joinList (tail w))
    | head s == 'E' = LogMessage (Error (read (head  w) :: Int)) (read (head (tail w)) :: Int) (joinList (drop 2 w)) 
    | head s == 'I' = LogMessage Info (read(head w) :: Int) (joinList (tail w))
    | otherwise = Unknown s
        where w = tail (words s)

parse :: String -> [LogMessage]
parse s = map parseMessage l
    where l = lines s

getTimestamp :: LogMessage -> TimeStamp
getTimestamp (Unknown _) = 0
getTimestamp (LogMessage _ t _) = t 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lmToInsert (Node left lm right)
    | (getTimestamp lmToInsert) < (getTimestamp lm) = Node (insert lmToInsert left) lm right -- insert left 
    | otherwise = Node left lm (insert lmToInsert right) -- insert right (admit there are no duplicates)

build :: [LogMessage] -> MessageTree
build lm = buildRec lm Leaf

buildRec :: [LogMessage] -> MessageTree -> MessageTree
buildRec [] mt = mt 
buildRec lm Leaf = buildRec (tail lm) (Node Leaf (head lm) Leaf )
buildRec lm mt = buildRec (tail lm) (insert (head lm) mt)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

filterImportant :: LogMessage -> Bool
filterImportant (LogMessage (Error importance) _ _) = importance >= 50 
filterImportant _ = False

lmToString :: LogMessage -> String
lmToString (Unknown s) = s
lmToString (LogMessage _ _ s) = s 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmList = map lmToString (inOrder (build (filter filterImportant lmList)))
