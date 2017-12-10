# Distributed Chatroom in Haskell

## Introduction
This is a distributed chat system written in Haskell.


## Team member
Jonathan Huang, Xiaozhou Pu


## Usage
0. Open Config.hs and change `ports` to your preferred ones
1. Open Server.hs in ghci, run main to start a server and follow the prompt
2. Run `nc -4u localhost [port]` in another terminal to start a client
3. Open Test.hs in ghci and run main
4. Repeat step 1 and/or 2 if you want to have multiple servers/clients
