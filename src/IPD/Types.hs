module IPD.Types 
    ( Move(..)
    , Strategy(..)
    , GameState(..)
    , Score(..)
    , TournamentResult(..)
    , TournamentConfig(..)
    , scoreRound
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)

-- | Represents a move in the Prisoner's Dilemma
data Move = Cooperate | Defect
    deriving (Show, Eq, Generic)

-- | A strategy is a function that takes the game state and returns a move
data Strategy = Strategy
    { name :: Text                    -- ^ Name of the strategy
    , play :: GameState -> Move       -- ^ Function to determine next move
    }

instance Show Strategy where
    show = show . name

instance Eq Strategy where
    s1 == s2 = name s1 == name s2

instance Ord Strategy where
    compare s1 s2 = compare (name s1) (name s2)

-- | The state of the game, including history of moves
data GameState = GameState
    { roundNumber :: Int              -- ^ Current round number
    , ownHistory :: [Move]            -- ^ History of own moves
    , opponentHistory :: [Move]       -- ^ History of opponent's moves
    } deriving (Show, Eq)

-- | Score for a single round
data Score = Score
    { player1Score :: Int
    , player2Score :: Int
    } deriving (Show, Eq)

-- | Configuration for a tournament
data TournamentConfig = TournamentConfig
    { rounds :: Int                   -- ^ Number of rounds per game
    , strategies :: [Strategy]        -- ^ List of strategies to compete
    } deriving (Show)

-- | Results of a tournament
data TournamentResult = TournamentResult
    { strategyName :: Text            -- ^ Name of the strategy
    , totalScore :: Int               -- ^ Total score across all games
    , averageScore :: Double          -- ^ Average score per game
    } deriving (Show, Eq)

-- | Standard scoring matrix for Prisoner's Dilemma
-- Both cooperate: 3 points each
-- Both defect: 1 point each
-- One defects, one cooperates: Defector gets 5, Cooperator gets 0
scoreRound :: Move -> Move -> Score
scoreRound Cooperate Cooperate = Score 3 3
scoreRound Defect Cooperate = Score 5 0
scoreRound Cooperate Defect = Score 0 5
scoreRound Defect Defect = Score 1 1 