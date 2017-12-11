# Distributed Chatroom in Haskell

## Introduction
This is a distributed chat system written in Haskell.


## Team member
Jonathan Huang, Xiaozhou Pu


## Usage
1. Open Config.hs and change `ports` to your preferred ones
2. Open Server.hs in ghci, run main to start a server and follow the prompt
3. Run `nc -4u localhost [port]` in another terminal to start a client
4. Repeat step 1 and/or 2 if you want to have multiple servers/clients


## Client Manual
1. `/join [group number]`
2. `/nick [nick name]`
3. `/part`
4. `$kill` - special command to kill the server


## Tests
1. QuickCheck - Open Test.hs in ghci and run main
2. Stresstest - run `./stresstest -g [group number] -c [client number] -m [message number] [config file]`
