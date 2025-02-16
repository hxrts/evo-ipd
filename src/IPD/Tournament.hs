module IPD.Tournament
    ( playGame
    , runTournament
    ) where

import IPD.Types
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Map as Map

-- | Play a single game between two strategies
playGame :: Int -> Strategy -> Strategy -> (Int, Int)
playGame numRounds s1 s2 = go 0 [] [] 0 0
  where
    go roundNum p1History p2History score1 score2
        | roundNum >= numRounds = (score1, score2)
        | otherwise = 
            let gameState1 = GameState roundNum p1History p2History
                gameState2 = GameState roundNum p2History p1History
                move1 = play s1 gameState1
                move2 = play s2 gameState2
                Score s1' s2' = scoreRound move1 move2
            in go (roundNum + 1) 
                  (move1 : p1History)
                  (move2 : p2History)
                  (score1 + s1')
                  (score2 + s2')

-- | Run a complete tournament where each strategy plays against all others
runTournament :: [Strategy] -> IO [TournamentResult]
runTournament strategies = do
    let pairs = [(s1, s2) | s1 <- strategies, s2 <- strategies, s1 /= s2]
        results = Map.fromListWith (+) $ concatMap playPair pairs
        numGames = length strategies - 1
        makeResult (strat, score) = TournamentResult
            { strategyName = name strat
            , totalScore = score
            , averageScore = fromIntegral score / fromIntegral numGames
            }
        -- Ensure all strategies have a result, in the same order as input
        allResults = map (\s -> case Map.lookup s results of
            Just score -> (s, score)
            Nothing -> (s, 0)) strategies
    return $ map makeResult allResults
  where
    playPair (s1, s2) =
        let (score1, score2) = playGame 200 s1 s2
        in [(s1, score1), (s2, score2)] 