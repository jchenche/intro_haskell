{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
    case words s of
        ("I":time:xs)           -> LogMessage Info                           (read time :: Int) (unwords xs)
        ("W":time:xs)           -> LogMessage Warning                        (read time :: Int) (unwords xs)
        ("E":severity:time:xs)  -> LogMessage (Error (read severity :: Int)) (read time :: Int) (unwords xs)
        _                       -> Unknown s

parse :: String -> [LogMessage]
parse s =
    let helper []     = []
        helper (x:xs) = parseMessage x : helper xs
    in
        helper (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert newMessage@(LogMessage _ time' _) (Node left oldMessage@(LogMessage _ time _) right)
    | time' < time = Node (insert newMessage left) oldMessage right
    | otherwise    = Node left oldMessage (insert newMessage right)
insert newMessage@(LogMessage _ _ _) Leaf = Node Leaf newMessage Leaf
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build messages =
    let helper [] tree     = tree
        helper (x:xs) tree = helper xs (insert x tree)
    in
        helper messages Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node Leaf message Leaf)  = [message]
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
    let
        relevantErrors :: LogMessage -> Bool
        relevantErrors (LogMessage (Error severity) _ _) = severity > 50
        relevantErrors _                                 = False

        extractMessage :: LogMessage -> String
        extractMessage (LogMessage _ _ message) = message
        extractMessage _                        = "Not supposed to be here!"
    in
        map extractMessage (filter relevantErrors (inOrder (build messages)))
