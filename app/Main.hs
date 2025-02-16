-- | Main entry point for the Iterated Prisoner's Dilemma simulation
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
-- Using a fixed seed ensures experiments can be replicated
defaultSeed :: Int
defaultSeed = 42

-- | Default genetic algorithm configuration
-- These parameters were chosen based on empirical testing
defaultGeneticConfig :: GeneticConfig
defaultGeneticConfig = GeneticConfig
    { populationSize = 50      -- Number of strategies in each generation
    , generations = 50         -- Number of generations to evolve
    , tournamentRounds = 200   -- Rounds per game (enough for strategies to establish patterns)
    , mutationRate = 0.1       -- 10% chance of mutation per gene
    , survivalCount = 5        -- Top 5 strategies preserved unchanged
    }

-- | Get random seed from environment or use default
-- Allows for reproducible experiments via environment variable
getRandomSeed :: IO Int
getRandomSeed = do
    mseed <- lookupEnv "IPD_SEED"  -- Check for IPD_SEED environment variable
    case mseed >>= readMaybe of
        Just seed -> do
            return seed
        Nothing -> do
            return defaultSeed

-- | Base strategies to include in all tournaments
-- These strategies provide a baseline for comparison
baseStrategies :: StdGen -> [Strategy]
baseStrategies gen =
    [ alwaysCooperate     -- Naive cooperation
    , alwaysDefect        -- Naive defection
    , titForTat           -- Responsive cooperation
    , grudger             -- Unforgiving retaliation
    , randomStrategy gen  -- Random baseline
    ]

main :: IO ()
main = do
    args <- getArgs
    seed <- getRandomSeed
    let gen = mkStdGen seed
    
    -- Choose mode based on command line arguments
    case args of
        ["evolve"] -> runEvolution gen     -- Run evolutionary simulation
        _ -> runTournamentMode gen         -- Run standard tournament

-- | Run a standard tournament between base strategies
-- This mode compares the performance of predefined strategies
runTournamentMode :: StdGen -> IO ()
runTournamentMode gen = do
    let strategies = baseStrategies gen
    
    -- Run tournament and get results
    results <- runTournament strategies
        
    -- Print final results
    putStrLn "\nTournament Results:\n"
    putStrLn $ formatRow "Strategy" "Total" "Average"
    putStrLn $ replicate 50 '-'
    
    -- Sort and display results
    let sortedResults = sortOn (negate . totalScore) results
    mapM_ (\result -> 
        putStrLn $ formatRow 
            (unpack $ strategyName result)
            (show $ totalScore result)
            (formatFloat $ averageScore result)
        ) sortedResults

-- | Run evolutionary simulation
-- This mode evolves new strategies and compares them with base strategies
runEvolution :: StdGen -> IO ()
runEvolution gen = do
    -- Get the actual seed used
    seed <- getRandomSeed
    
    -- Print configuration
    putStrLn ""
    putStrLn "Running Evolutionary IPD Tournament..."
    putStrLn $ replicate 50 '-'
    putStrLn $ "Random Seed: " ++ show seed
    putStrLn $ "Population Size: " ++ show (populationSize defaultGeneticConfig)
    putStrLn $ "Generations: " ++ show (generations defaultGeneticConfig)
    putStrLn $ "Mutation Rate: " ++ show (mutationRate defaultGeneticConfig)
    putStrLn $ "Survival Count: " ++ show (survivalCount defaultGeneticConfig)
    putStrLn ""

    -- Run evolution and get evolved strategies
    evolvedStrategies <- evolveStrategies 
        (populationSize defaultGeneticConfig)
        (generations defaultGeneticConfig)
        (mutationRate defaultGeneticConfig)
        (survivalCount defaultGeneticConfig)
        (baseStrategies gen)
    
    -- Run final tournament with both base and evolved strategies
    let finalConfig = baseStrategies gen ++ evolvedStrategies
    results <- runTournament finalConfig

    -- Print final results
    putStrLn $ formatRow "Strategy" "Total" "Average"
    putStrLn $ replicate 50 '-'
    
    -- Sort and display results
    let sortedResults = sortOn (negate . totalScore) results
    mapM_ (\result -> 
        putStrLn $ formatRow 
            (unpack $ strategyName result)
            (show $ totalScore result)
            (formatFloat $ averageScore result)
        ) sortedResults

-- | Formatting helpers for output display
formatRow :: String -> String -> String -> String
formatRow col1 col2 col3 = 
    padRight 30 col1 ++ padLeft 10 col2 ++ padLeft 10 col3

-- | Pad a string on the right with spaces
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

-- | Pad a string on the left with spaces
padLeft :: Int -> String -> String
padLeft n s = take n (reverse (take n (reverse s ++ repeat ' ')))

-- | Format floating point numbers consistently
formatFloat :: Double -> String
formatFloat x = printf "%.1f" x
    