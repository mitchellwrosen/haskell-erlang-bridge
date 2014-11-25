{-# LANGUAGE OverloadedStrings #-}

module Main where

import Erlang.Interface

main :: IO ()
main = do
    -- Must be called first.
    erl_init

    -- Initilize our own node.
    Right node <- connect_init "my_node_name" "node_cookie" 0

    -- Connect to "other_node@other_host"
    Right fd <- connect node "other_node@other_host"

    -- Send :hi to the "shell" process
    _ <- regSendAtom node fd "shell" "hi"
    return ()
