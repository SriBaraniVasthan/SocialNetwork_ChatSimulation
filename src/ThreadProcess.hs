-- | Module ThreadProcess for processing threads involving random users exchanging random messages at random intervals.
module ThreadProcess where 
import Control.Concurrent
import Data.List
import Data.Time
import System.Random
import System.IO
import Types

-- | Predefining a set of messages for random exchanges between users.
randomMsgs :: [String]
randomMsgs = ["Hi! Great to see you","Have a great day! Bye","Its a pleasure to meet you","Hey! What a surprise","You look cute today","Hows the family","Good luck","Come on in! Have a drink!"," How do you like it?"," Thank you so much! ","Good to see you!","Could you help me pls","Sounds great!"," It would be really nice to have a chat","You alright?","I am sorry!","Thats interesting","Its amazing!","Nice to meet you","Want to see a movie?","I thought I would drop in and say hello"]

-- | Increments and counts the number of messages exchanged during the thread process
--  When the simulation reaches 100, puts the threads to a halt
--  Constructs the message with username, timestamp and the content of the message
--  Sets a thread delay for a smooth synchronisation of the threads.
--  Randomly picks a message and sends to a random user.
userThread :: User -> MVar Bool -> MVar StdGen -> MVar Int -> [User] -> IO ()
userThread user status dataSet messagesSent usersList = do
    messagesCount <- takeMVar messagesSent
    if messagesCount < 100 then do
        randomGen <- takeMVar dataSet
        let (receiverName, rGen) = randomUserSelection randomGen usersList
        putMVar dataSet rGen
        if userName receiverName /= userName user then do
            putMVar messagesSent (messagesCount + 1)
            textMsg <- constructMessage
            utcNow <- getCurrTime
            putStrLn $ utcNow ++"  ::  " ++ userName user ++ " sent this message to " ++ userName receiverName ++ "  ::   <<<< "++ (textMsg) ++ " >>>> "
            concatTextMsg user receiverName textMsg
            tempVar <- readMVar dataSet
            threadDelay (fst (randomR (1000, 10000) (tempVar)))
            userThread user status dataSet messagesSent usersList
        else do  
            putMVar messagesSent (messagesCount)
            textMsg <- constructMessage
            utcNow <- getCurrTime
            putStrLn $ utcNow ++"  ::  " ++ userName user ++" sent a message to himself " ++ "  ::   <<<< "++ (textMsg) ++ " >>>> "
            userThread user status dataSet messagesSent usersList
    else do
        putMVar messagesSent messagesCount
        putMVar status True

-- | Function implentation for picking Users randomly 
randomUserSelection :: StdGen -> [User] -> (User, StdGen)
randomUserSelection generate usersList = do
    let random = randomR (0, length usersList - 1) generate
    let user_id = usersList !! fst (random)
    (user_id, snd(random))

-- | Function implentation for picking the messages randomly from the defined list. 
constructMessage :: IO String
constructMessage  = do
    msg <- randomIO :: IO Int
    if (msg `mod` 10) == 0 then do
        return $ head randomMsgs
    else if (msg `mod` 10) == 8 then do
        return $ randomMsgs !! 8
    else if (msg `mod` 10) == 7 then do
        return $ randomMsgs !! 7
    else if (msg `mod` 10) == 6 then do
        return $ randomMsgs !! 6
    else if (msg `mod` 10) == 5 then do
        return $ randomMsgs !! 5
    else if (msg `mod` 10) == 4 then do
        return $ randomMsgs !! 4
    else if (msg `mod` 10) == 3 then do
        return $ randomMsgs !! 3
    else if (msg `mod` 10) == 2 then do
        return $ randomMsgs !! 2
    else if (msg `mod` 10) == 1 then do
        return $ randomMsgs !! 1
    else do
        return $ last randomMsgs

-- | Function implmentation for concatenation of text messages
concatTextMsg :: User -> User-> String ->IO ()
concatTextMsg user tgt textMsg = do
    msgInbox <- takeMVar (messageReceived tgt)
    putMVar (messageReceived tgt) ((Message textMsg) : msgInbox)

-- | Gets and returns the cuurent time in UTC format 
getCurrTime :: IO String
getCurrTime = do
    timeNow <- getCurrentTime
    return (show timeNow)