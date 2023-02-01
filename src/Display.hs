-- | Display module for printing the outputs
module Display where
import Control.Concurrent ( MVar, readMVar, putMVar )
import Types

-- | Prints the user and message summary based on the set flag using putMVar
displayCountResults :: [User] ->  MVar Bool -> IO ()
displayCountResults [] displayFlag = do
    putStrLn ""
    putMVar displayFlag True
displayCountResults (x:xs) displayFlag = do
    displayCount x
    displayCountResults xs displayFlag

-- | Displays all 10 usernames along the total count on number of messages they received in 100 simulations.
displayCount :: User -> IO ()
displayCount (User userName msgReceived) = do
    messagesCount <- readMVar msgReceived
    putStrLn $ userName ++ " received " ++ show (length messagesCount) ++ " text messages" 