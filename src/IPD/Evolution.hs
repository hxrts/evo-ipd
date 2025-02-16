-- | Implementation of evolutionary algorithms for the Iterated Prisoner's Dilemma
module IPD.Evolution
    ( evolveStrategies
    , GeneticStrategy(..)
    , createRandomStrategy
    ) where

import IPD.Types (Strategy(..), Move(..), GameState(..), TournamentConfig(..), TournamentResult(..), GeneticConfig(..))
import IPD.Tournament (runTournament)
import Data.Text (Text, pack, unpack)
import System.Random
import System.Random.Stateful (randomRIO)
import Control.Monad (replicateM, forM_)
import Data.List (sortOn)
import qualified Data.Map as Map
import Text.Printf

-- | A strategy that can be evolved through genetic algorithms
data GeneticStrategy = GeneticStrategy
    { geneticStrategyName :: Text
    , genome :: [Double]          -- ^ List of probabilities for different actions
    , memorySize :: Int          -- ^ Number of past moves to consider when making decisions
    }

-- | Equality based on strategy name
-- Used for tournament organization and results tracking
instance Eq GeneticStrategy where
    s1 == s2 = geneticStrategyName s1 == geneticStrategyName s2

-- | Show instance for debugging and tournament reporting
instance Show GeneticStrategy where
    show s = "GeneticStrategy " ++ show (geneticStrategyName s)

-- | Create a random strategy with given memory size
-- Parameters:
-- gen: Random number generator
-- id: Unique identifier for the strategy
-- memSize: Number of past moves to consider
createRandomStrategy :: RandomGen g => g -> Int -> Int -> (GeneticStrategy, g)
createRandomStrategy gen id memSize = 
    let (genome, gen') = genomeLength memSize gen
    in ( GeneticStrategy 
         { geneticStrategyName = pack $ "Gen-" ++ show id
         , genome = genome
         , memorySize = memSize
         }
       , gen'
       )
  where
    -- Generate random probabilities for each possible history combination
    -- The number of genes needed is 2^(2 * memSize) because:
    -- - Each player has 2 possible moves (Cooperate/Defect)
    -- - We consider moves from both players
    -- - We look back memSize moves
    genomeLength :: RandomGen g => Int -> g -> ([Double], g)
    genomeLength mem g = 
        let numGenes = 2 ^ (2 * mem)  -- Calculate number of genes needed
        in foldr 
            (\_ (probs, g') -> 
                let (prob, g'') = randomR (0.0, 1.0) g'  -- Generate random probability
                in (prob:probs, g'')
            ) 
            ([], g) 
            [1..numGenes]

-- | Convert a genetic strategy to a regular strategy
-- This allows genetic strategies to participate in regular tournaments
geneticToStrategy :: GeneticStrategy -> Strategy
geneticToStrategy gs = Strategy
    { name = geneticStrategyName gs
    , play = makeDecision (genome gs) (memorySize gs)
    }

-- | Make a decision based on genome and game history
-- Uses the genome as a lookup table for different history patterns
makeDecision :: [Double] -> Int -> GameState -> Move
makeDecision genome memSize GameState{..} =
    let history = zip (take memSize ownHistory) 
                     (take memSize opponentHistory)
        index = historyToIndex history memSize  -- Convert history to genome index
        threshold = genome !! index             -- Look up probability in genome
    in if threshold > 0.5 then Cooperate else Defect  -- Convert probability to move

-- | Convert move history to genome index
-- Creates a binary number from the history where:
-- - Cooperate = 0
-- - Defect = 1
historyToIndex :: [(Move, Move)] -> Int -> Int
historyToIndex history memSize =
    let bits = concatMap movePairToBits history
        padding = replicate (2 * memSize - length bits) False  -- Pad with cooperate
    in bitsToInt (padding ++ bits)
  where
    movePairToBits (m1, m2) = [moveTobit m1, moveTobit m2]
    moveTobit Cooperate = False  -- Cooperate = 0
    moveTobit Defect = True      -- Defect = 1
    bitsToInt = foldr (\b acc -> fromEnum b + 2 * acc) 0  -- Convert bits to integer

-- | Perform crossover between two parent strategies
-- Creates two children by splitting and recombining parent genomes
crossover :: RandomGen g => g -> GeneticStrategy -> GeneticStrategy -> (GeneticStrategy, GeneticStrategy, g)
crossover gen parent1 parent2 =
    let genome1 = genome parent1
        genome2 = genome parent2
        (crossPoint, gen') = randomR (0, length genome1 - 1) gen  -- Random crossover point
        -- Create children by splitting and recombining parent genomes
        (child1Genome, child2Genome) = 
            ( take crossPoint genome1 ++ drop crossPoint genome2
            , take crossPoint genome2 ++ drop crossPoint genome1
            )
        -- Generate unique IDs for children
        (nameId1 :: Int, gen'') = randomR (1000, 9999) gen'
        (nameId2 :: Int, gen''') = randomR (1000, 9999) gen''
        -- Create child strategies
        child1 = parent1 { genome = child1Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId1 }
        child2 = parent2 { genome = child2Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId2 }
    in (child1, child2, gen''')

-- | Mutate a strategy
-- Randomly modifies genes based on mutation rate
mutate :: RandomGen g => g -> Double -> GeneticStrategy -> (GeneticStrategy, g)
mutate gen rate strategy =
    let (genome', gen') = 
            foldr (\prob (acc, g) -> 
                let (shouldMutate, g1) = randomR (0.0, 1.0) g
                    (newProb, g2) = if shouldMutate < rate
                                   then randomR (0.0, 1.0) g1  -- Generate new random probability
                                   else (prob, g1)             -- Keep existing probability
                in (newProb : acc, g2)
            ) ([], gen) (genome strategy)
    in (strategy { genome = genome' }, gen')

-- | Create children through crossover and mutation
createChildren :: RandomGen g => g -> Double -> [GeneticStrategy] -> Int -> ([GeneticStrategy], g)
createChildren gen rate parents numChildren = 
    let pairs = take (numChildren `div` 2) $ 
                [(p1, p2) | p1 <- parents, p2 <- parents, p1 /= p2]
        (children, gen') = foldr (\pair (acc, g) -> 
            let (newChildren, g') = crossoverAndMutate g rate pair
            in (newChildren ++ acc, g')
            ) ([], gen) pairs
    in (take numChildren children, gen')

-- | Perform crossover and mutation on a pair of parents
crossoverAndMutate :: RandomGen g => g -> Double -> (GeneticStrategy, GeneticStrategy) -> ([GeneticStrategy], g)
crossoverAndMutate gen rate (parent1, parent2) =
    let genome1 = genome parent1
        genome2 = genome parent2
        (crossPoint, gen1) = randomR (0, length genome1 - 1) gen
        (child1Genome, child2Genome) = 
            ( take crossPoint genome1 ++ drop crossPoint genome2
            , take crossPoint genome2 ++ drop crossPoint genome1
            )
        (nameId1 :: Int, gen2) = randomR (1000, 9999) gen1
        (nameId2 :: Int, gen3) = randomR (1000, 9999) gen2
        child1 = parent1 { genome = child1Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId1 }
        child2 = parent2 { genome = child2Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId2 }
        (child1', gen4) = mutate gen3 rate child1
        (child2', gen5) = mutate gen4 rate child2
    in ([child1', child2'], gen5)

-- | Evolve a new generation
evolveGeneration :: RandomGen g => g -> Double -> Int -> [(GeneticStrategy, TournamentResult)] -> ([GeneticStrategy], g)
evolveGeneration gen mutRate survivalCount results = 
    let sortedResults = sortOn (negate . totalScore . snd) results
        survival = take survivalCount $ map fst sortedResults
        rest = drop survivalCount $ map fst sortedResults
        (children, gen') = createChildren gen mutRate rest (length results - survivalCount)
    in (survival ++ children, gen')

-- | Evolve a population of strategies
-- Main function that runs the evolutionary process
evolveStrategies :: Int -> Int -> Double -> Int -> [Strategy] -> IO [Strategy]
evolveStrategies popSize numGens mutRate survivalCount baseStrategies = do
    -- Get initial random generator
    gen <- newStdGen
    
    -- Initialize population with random strategies
    let (initialPop, gen') = foldr 
            (\_ (acc, g) -> 
                let (strat, g') = generateRandomStrategy g
                in (strat:acc, g')
            ) 
            ([], gen) 
            [1..popSize]
    
    -- Evolution loop
    let loop gen pop g = do
            -- Run tournament with current population
            results <- runTournament (map geneticToStrategy pop)
            let popResults = zip pop results

            if gen < numGens
                then do
                    -- Select and breed next generation
                    let (nextGen, g') = evolveGeneration g mutRate survivalCount popResults
                    loop (gen + 1) nextGen g'
                else return $ map geneticToStrategy pop

    -- Start evolution process
    finalPop <- loop 1 initialPop gen'
    return finalPop

-- | Initialize a random population
-- Creates specified number of random strategies
initializePopulation :: RandomGen g => g -> Int -> Int -> ([GeneticStrategy], g)
initializePopulation gen size startId = 
    let (strategies, gen') = 
            foldr (\id (acc, g) -> 
                let (strat, g') = createRandomStrategy g id 2  -- Use memory size of 2
                in (strat : acc, g')
            ) ([], gen) [startId..startId + size - 1]
    in (strategies, gen')

-- | Format helpers for output
-- Pad strings with spaces for aligned output
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

padLeft :: Int -> String -> String
padLeft n s = take n (repeat ' ' ++ s)

-- | Print a single tournament result
printResult :: (Strategy, TournamentResult) -> IO ()
printResult (strat, result) = do
    let stratName = unpack $ strategyName result
        total = show $ totalScore result
        line = padRight 30 stratName ++ padLeft 10 total
    putStrLn $ replicate 50 '-'
    putStrLn line
    putStrLn $ replicate 50 '-'

-- | Print statistics for current generation
-- Shows scores and performance of each strategy
printGenerationStats :: Int -> [(GeneticStrategy, TournamentResult)] -> IO ()
printGenerationStats gen results = do
    putStrLn $ "\nGeneration " ++ show gen ++ " Results:"
    putStrLn $ replicate 50 '-'
    putStrLn $ padRight 30 "Strategy Name" ++ padLeft 20 "Score"
    putStrLn $ replicate 50 '-'
    let sortedResults = sortOn (negate . totalScore . snd) results
    forM_ sortedResults $ \(strat, result) -> do
        let name = unpack $ geneticStrategyName strat
            total = show $ totalScore result
        putStrLn $ padRight 30 name ++ padLeft 20 total
    putStrLn $ replicate 50 '-'

-- | Run evolution for specified number of generations
evolveGenerations :: RandomGen g => 
                    g -> 
                    GeneticConfig -> 
                    [Strategy] -> 
                    [GeneticStrategy] -> 
                    Int -> 
                    IO [GeneticStrategy]
evolveGenerations gen config baseStrats pop genNum
    | genNum > generations config = return pop
    | otherwise = do
        -- Run tournament
        let allStrategies = baseStrats ++ map geneticToStrategy pop
        results <- runTournament allStrategies
            
        -- Print base strategies results
        putStrLn "\nBase Strategies:"
        putStrLn $ replicate 50 '-'
        putStrLn $ padRight 30 "Strategy Name" ++ padLeft 20 "Score"
        putStrLn $ replicate 50 '-'
        let baseResults = take (length baseStrats) results
        forM_ baseResults $ \result -> do
            let stratName = unpack $ strategyName result
                total = show $ totalScore result
            putStrLn $ padRight 30 stratName ++ padLeft 20 total
        putStrLn $ replicate 50 '-'
        
        -- Print evolved strategies stats
        let evolvedResults = drop (length baseStrats) results
            -- Results are in the same order as the population
            popResults = zip pop evolvedResults
        
        printGenerationStats genNum popResults
            
        -- Select parents and create new population
        let (newPop, gen') = createNewGeneration gen config pop evolvedResults
        
        -- Recurse
        evolveGenerations gen' config baseStrats newPop (genNum + 1)

-- | Create a new generation through selection, crossover, and mutation
createNewGeneration :: RandomGen g => 
                      g -> 
                      GeneticConfig -> 
                      [GeneticStrategy] -> 
                      [TournamentResult] -> 
                      ([GeneticStrategy], g)
createNewGeneration gen config pop results =
    -- Sort population by fitness
    let popWithFitness = zip pop (map totalScore results)
        sortedPop = map fst $ sortOn (negate . snd) popWithFitness
        -- Keep survival strategies
        survival = take (survivalCount config) sortedPop
        -- Create rest through crossover and mutation
        (children, gen') = createChildren gen (mutationRate config) sortedPop 
                            (populationSize config - survivalCount config)
    in (survival ++ children, gen')

-- | Select two parents using tournament selection
selectParents :: RandomGen g => 
                 g -> 
                 [GeneticStrategy] -> 
                 (GeneticStrategy, GeneticStrategy, g)
selectParents gen parents =
    let (p1, gen1) = selectOne gen parents
        (p2, gen2) = selectOne gen1 (filter (/= p1) parents)
    in (p1, p2, gen2)
  where
    selectOne g ps = 
        let (idx, g') = randomR (0, length ps - 1) g
        in (ps !! idx, g') 

-- | Generate a random strategy
generateRandomStrategy :: RandomGen g => g -> (GeneticStrategy, g)
generateRandomStrategy gen = 
    let (nameId :: Int, gen1) = randomR (1, 9999) gen
        (genome, gen2) = foldr 
            (\_ (acc, g) -> 
                let (val, g') = randomR (0.0, 1.0) g
                in (val:acc, g')
            ) 
            ([], gen1) 
            [1..(2 ^ 4)]  -- memory size of 2 (2^(2*2) = 16)
    in (GeneticStrategy 
        { geneticStrategyName = pack $ "Gen-" ++ show nameId
        , genome = genome
        , memorySize = 2
        }, gen2) 