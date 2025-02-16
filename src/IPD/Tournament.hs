-- | Tournament implementation for the Iterated Prisoner's Dilemma
module IPD.Tournament
    ( playGame
    , runTournament
    ) where

import IPD.Types
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Map as Map

-- | Play a single game between two strategies
-- Returns a tuple of final scores (player1Score, player2Score)
-- The game continues for the specified number of rounds, accumulating scores
playGame :: Int -> Strategy -> Strategy -> (Int, Int)
playGame numRounds s1 s2 = go 0 [] [] 0 0
  where
    -- Inner recursive function to play rounds
    -- Parameters:
    -- roundNum: Current round number
    -- p1History: List of player 1's moves (most recent first)
    -- p2History: List of player 2's moves (most recent first)
    -- score1: Accumulated score for player 1
    -- score2: Accumulated score for player 2
    go roundNum p1History p2History score1 score2
        | roundNum >= numRounds = (score1, score2)  -- Game is over
        | otherwise = 
            let -- Create game states for both players
                gameState1 = GameState p1History p2History roundNum
                gameState2 = GameState p2History p1History roundNum
                -- Get moves from both strategies
                move1 = play s1 gameState1
                move2 = play s2 gameState2
                -- Score the round
                Score s1' s2' = scoreRound move1 move2
            in go (roundNum + 1)        -- Increment round
                  (move1 : p1History)    -- Add move1 to history
                  (move2 : p2History)    -- Add move2 to history
                  (score1 + s1')         -- Update score1
                  (score2 + s2')         -- Update score2

-- | Run a complete tournament where each strategy plays against all others
-- Each strategy plays against every other strategy (including itself)
-- Returns a list of TournamentResults sorted by total score
runTournament :: [Strategy] -> IO [TournamentResult]
runTournament strategies = do
    -- Generate all possible pairs of strategies (including self-play)
    let pairs = [(s1, s2) | s1 <- strategies, s2 <- strategies]
        -- Play all games and collect results
        games = map playPair pairs
        -- Create a map of strategy to list of scores
        scoreMap = Map.fromListWith (++) $ concatMap (\(s1, s2, score1, score2) -> 
            [(s1, [score1]), (s2, [score2])]) games
        -- Calculate number of games each strategy played
        numGames = length strategies  -- Each strategy plays against all strategies (including self)
        -- Create TournamentResult for each strategy
        makeResult strat = 
            let scores = Map.findWithDefault [] strat scoreMap
                total = sum scores
                avg = fromIntegral total / fromIntegral numGames
            in TournamentResult
                { strategyName = name strat
                , totalScore = total
                , averageScore = avg
                }
    return $ map makeResult strategies
  where
    -- Helper function to play a single pair of strategies
    -- Returns (Strategy1, Strategy2, Score1, Score2)
    playPair (s1, s2) =
        let (score1, score2) = playGame 200 s1 s2  -- Play 200 rounds
        in (s1, s2, score1, score2) 