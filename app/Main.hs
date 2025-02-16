module Main where

import IPD.Types
import IPD.Strategies
import IPD.Tournament
import IPD.Evolution
import System.Random (mkStdGen, StdGen)
import Data.Text (Text, unpack)
import System.Environment (lookupEnv, getArgs)
import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.List (sortOn)

-- | Default seed for reproducible results
defaultSeed :: Int
defaultSeed = 42

-- | Default genetic algorithm configuration
defaultGeneticConfig :: GeneticConfig
defaultGeneticConfig = GeneticConfig
    { populationSize = 50
    , generations = 5
    , tournamentRounds = 200
    , mutationRate = 0.1
    , eliteCount = 5
    }

-- | Get random seed from environment or use default
getRandomSeed :: IO Int
getRandomSeed = do
    mseed <- lookupEnv "IPD_SEED"
    case mseed >>= readMaybe of
        Just seed -> do
            putStrLn $ "Using random seed: " ++ show seed
            return seed
        Nothing -> do
            putStrLn $ "Using default seed: " ++ show defaultSeed
            return defaultSeed

-- | Base strategies to include in all tournaments
baseStrategies :: StdGen -> [Strategy]
baseStrategies gen =
    [ alwaysCooperate
    , alwaysDefect
    , titForTat
    , grudger
    , randomStrategy gen
    ]

main :: IO ()
main = do
    args <- getArgs
    seed <- getRandomSeed
    let gen = mkStdGen seed
    
    case args of
        ["evolve"] -> runEvolution gen
        _ -> runTournamentMode gen

runTournamentMode :: StdGen -> IO ()
runTournamentMode gen = do
    let strategies = baseStrategies gen

    putStrLn ""    
    putStrLn "Running Iterated Prisoner's Dilemma Tournament..."
    putStrLn "-------------------------------------------------"
    
    results <- runTournament strategies
        
    -- Print header
    putStrLn $ formatRow "Strategy" "Total" ""
    putStrLn $ replicate 40 '-'
    
    -- Print results sorted by score
    let sortedResults = sortOn (negate . totalScore) results
    mapM_ (\result -> 
        putStrLn $ formatRow 
            (unpack $ strategyName result)
            (show $ totalScore result)
            ""
        ) sortedResults

runEvolution :: StdGen -> IO ()
runEvolution gen = do
    putStrLn ""
    putStrLn "Running Evolutionary IPD Tournament..."
    putStrLn "------------------------------------"
    putStrLn $ "Population Size: " ++ show (populationSize defaultGeneticConfig)
    putStrLn $ "Generations: " ++ show (generations defaultGeneticConfig)
    putStrLn $ "Mutation Rate: " ++ show (mutationRate defaultGeneticConfig)
    putStrLn $ "Elite Count: " ++ show (eliteCount defaultGeneticConfig)
    putStrLn ""

    evolvedStrategies <- evolveStrategies 
        (populationSize defaultGeneticConfig)
        (generations defaultGeneticConfig)
        (mutationRate defaultGeneticConfig)
        (eliteCount defaultGeneticConfig)
        (baseStrategies gen)
    
    -- Run final tournament with evolved strategies
    let finalConfig = baseStrategies gen ++ evolvedStrategies
    results <- runTournament finalConfig

    putStrLn "\nFinal Tournament Results:"
    putStrLn "========================\n"
    
    -- Print header
    putStrLn $ formatRow "Strategy" "Total" ""
    putStrLn $ replicate 40 '-'
    
    -- Print results sorted by score
    let sortedResults = sortOn (negate . totalScore) results
    mapM_ (\result -> 
        putStrLn $ formatRow 
            (unpack $ strategyName result)
            (show $ totalScore result)
            ""
        ) sortedResults

-- Formatting helpers
formatRow :: String -> String -> String -> String
formatRow col1 col2 col3 = 
    padRight 30 col1 ++ padLeft 10 col2

padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

padLeft :: Int -> String -> String
padLeft n s = take n (reverse (take n (reverse s ++ repeat ' ')))

formatFloat :: Double -> String
formatFloat x = printf "%.1f" x
    