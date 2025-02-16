-- | Core types and data structures for the Iterated Prisoner's Dilemma game
module IPD.Types 
    ( Move(..)
    , Strategy(..)
    , GameState(..)
    , Score(..)
    , TournamentResult(..)
    , TournamentConfig(..)
    , GeneticConfig(..)
    , scoreRound
    ) where

import GHC.Generics (Generic)
import Data.Text (Text)

-- | Represents a player's move in the Prisoner's Dilemma
-- Cooperate represents staying silent (3 points if both cooperate)
-- Defect represents betraying the other prisoner (0-5 points depending on other's move)
data Move = Cooperate | Defect
    deriving (Show, Eq, Generic)

-- | A strategy for playing the Iterated Prisoner's Dilemma
-- Contains a name for identification and a function that determines moves
data Strategy = Strategy
    { name :: Text                     -- ^ Unique identifier for the strategy
    , play :: GameState -> Move        -- ^ Function that decides the next move based on game state
    }

-- | Represents the current state of a game between two players
-- Tracks the history of moves made by both players
data GameState = GameState
    { ownHistory :: [Move]            -- ^ List of moves made by the current player (most recent first)
    , opponentHistory :: [Move]       -- ^ List of moves made by the opponent (most recent first)
    , roundNumber :: Int              -- ^ Current round number (1-based)
    } deriving (Show, Eq)

-- | Score for a single round of the game
data Score = Score
    { player1Score :: Int             -- ^ Points earned by player 1 in this round
    , player2Score :: Int             -- ^ Points earned by player 2 in this round
    } deriving (Show, Eq)

-- | Configuration for running a tournament
-- Specifies parameters that control tournament execution
data TournamentConfig = TournamentConfig
    { numRounds :: Int                -- ^ Number of rounds each pair of strategies plays
    , baseScore :: Int               -- ^ Starting score for each strategy
    , cooperateScore :: Int          -- ^ Points awarded for mutual cooperation (typically 3)
    , defectScore :: Int             -- ^ Points awarded for successful defection (typically 5)
    , suckerScore :: Int             -- ^ Points received when cooperating while opponent defects (typically 0)
    , mutualDefectScore :: Int       -- ^ Points when both players defect (typically 1)
    } deriving (Show)

-- | Results from a tournament round
-- Tracks performance metrics for a strategy
data TournamentResult = TournamentResult
    { strategyName :: Text            -- ^ Name of the strategy these results belong to
    , totalScore :: Int              -- ^ Total points accumulated across all games
    , averageScore :: Double         -- ^ Average points per game
    , cooperationRate :: Double      -- ^ Percentage of moves that were cooperative (0.0 to 1.0)
    , wins :: Int                    -- ^ Number of games won against other strategies
    , losses :: Int                  -- ^ Number of games lost against other strategies
    , ties :: Int                    -- ^ Number of games tied with other strategies
    } deriving (Show, Eq)

-- | Configuration for genetic algorithm
-- Specifies parameters for the genetic algorithm
data GeneticConfig = GeneticConfig
    { populationSize :: Int       -- ^ Size of the population in each generation
    , generations :: Int          -- ^ Number of generations to evolve
    , tournamentRounds :: Int     -- ^ Number of rounds per game in tournament
    , mutationRate :: Double      -- ^ Probability of mutation (0.0 to 1.0)
    , survivalCount :: Int        -- ^ Number of top strategies to preserve unchanged
    } deriving (Show)

-- | Standard scoring matrix for Prisoner's Dilemma
-- Implements the classic payoff matrix for the game:
-- Both cooperate: 3 points each
-- Both defect: 1 point each
-- One defects, one cooperates: Defector gets 5, Cooperator gets 0
scoreRound :: Move -> Move -> Score
scoreRound Cooperate Cooperate = Score 3 3    -- Mutual cooperation
scoreRound Defect Cooperate = Score 5 0       -- Player 1 defects, Player 2 cooperates
scoreRound Cooperate Defect = Score 0 5       -- Player 1 cooperates, Player 2 defects
scoreRound Defect Defect = Score 1 1          -- Mutual defection

-- | Show instance for Strategy to enable debugging and tournament reporting
-- Only shows the strategy name for clarity
instance Show Strategy where
    show s = "Strategy " ++ show (name s)

-- | Equality for Strategy based on name
-- Two strategies are considered equal if they have the same name
-- This is important for tournament organization and results tracking
instance Eq Strategy where
    s1 == s2 = name s1 == name s2

-- | Ordering for Strategy based on name
-- Enables sorting strategies consistently
instance Ord Strategy where
    compare s1 s2 = compare (name s1) (name s2) 