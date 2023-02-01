-- | Types module for defining the data model 
module Types
    (Message (..),
     User (..),
    ) where

import Control.Concurrent ( MVar )

-- | Data type created for the Messages
data Message = Message {
-- | Stores the exchanged message
    message :: String
} deriving (Eq)

-- |Data type created for the Users
data User = User {
-- | Name of the user in social network 
    userName :: String,
-- | Holds the Message received by a particular user
    messageReceived :: MVar [Message]
} deriving (Eq)