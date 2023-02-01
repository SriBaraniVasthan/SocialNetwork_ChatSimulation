module Main where
-- | Main module initiating the concurrent threads involving 10 users for 100 message exchanges.
import Control.Concurrent
import ThreadProcess
import Types
import Display
import Data.Bifunctor (first)
import Data.Time
import System.Random
import System.IO
import System.Exit

-- | Initializes user messages with  newMVars, creates a thread status with newEmptyMVar for thread control
--   Initializes a random set of message data using mkStdGen and declares 10 unique usernames
--   Declares 10 independent thread using forkIO and prints the final summary of message exchanges
main :: IO ()
main = do
    putStrLn $ "\n<-----Welcome to instaNetwork----->\n\n<-----Starting the Message exchange process----->"
    status <- newEmptyMVar
    displayFlag <- newEmptyMVar
    dataSet <- newMVar (mkStdGen 932497234)
    messagesSent <- newMVar 0
    msg1 <- newMVar []
    msg2 <- newMVar []
    msg3 <- newMVar []
    msg4 <- newMVar []
    msg5 <- newMVar []
    msg6 <- newMVar []
    msg7 <- newMVar []
    msg8 <- newMVar []
    msg9 <- newMVar []
    msg10 <- newMVar []
    let usersNameList = [ User "Alpha" msg1, User "Bravo" msg2, User "Charlie" msg3, User "Delta" msg4, User "Mike" msg5, User "Oscar" msg6, User "Romeo" msg7, User "Victor" msg8, User "John" msg9, User "Steve" msg10 ]
    forkIO (userThread (usersNameList !! 0) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 1) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 2) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 3) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 4) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 5) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 6) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 7) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 8) status dataSet messagesSent usersNameList)
    forkIO (userThread (usersNameList !! 9) status dataSet messagesSent usersNameList)
    done <- takeMVar status
    putStrLn $ "\n<----Message exchanges ends---->\n"
    displayCountResults usersNameList displayFlag
    die("Hope you had a good time with the App! Connecting people socially like never before!\n")