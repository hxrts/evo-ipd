module IPD.Evolution
    ( GeneticConfig(..)
    , evolveStrategies
    , GeneticStrategy(..)
    , createRandomStrategy
    ) where

import IPD.Types (Strategy(..), Move(..), GameState(..), TournamentConfig(..), TournamentResult(..))
import IPD.Tournament (runTournament)
import Data.Text (Text, pack, unpack)
import System.Random
import System.Random.Stateful (randomRIO)
import Control.Monad (replicateM, forM_)
import Data.List (sortOn)
import qualified Data.Map as Map
import Text.Printf

-- | Configuration for genetic algorithm
data GeneticConfig = GeneticConfig
    { populationSize :: Int       -- ^ Size of the population
    , generations :: Int          -- ^ Number of generations to evolve
    , tournamentRounds :: Int     -- ^ Rounds per game in tournament
    , mutationRate :: Double      -- ^ Probability of mutation
    , eliteCount :: Int          -- ^ Number of top strategies to preserve
    }

-- | A strategy that can be evolved
data GeneticStrategy = GeneticStrategy
    { geneticStrategyName :: Text
    , genome :: [Double]          -- ^ Probabilities for different actions
    , memorySize :: Int          -- ^ How many past moves to consider
    }

-- | Equality based on strategy name
instance Eq GeneticStrategy where
    s1 == s2 = geneticStrategyName s1 == geneticStrategyName s2

-- | Show instance for debugging
instance Show GeneticStrategy where
    show s = "GeneticStrategy " ++ show (geneticStrategyName s)

-- | Create a random strategy with given memory size
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
    genomeLength :: RandomGen g => Int -> g -> ([Double], g)
    genomeLength mem g = 
        let numGenes = 2 ^ (2 * mem)  -- 2 possible moves for both players
        in foldr 
            (\_ (probs, g') -> 
                let (prob, g'') = randomR (0.0, 1.0) g'
                in (prob:probs, g'')
            ) 
            ([], g) 
            [1..numGenes]

-- | Convert a genetic strategy to a regular strategy
geneticToStrategy :: GeneticStrategy -> Strategy
geneticToStrategy gs = Strategy
    { name = geneticStrategyName gs
    , play = makeDecision (genome gs) (memorySize gs)
    }

-- | Make a decision based on genome and history
makeDecision :: [Double] -> Int -> GameState -> Move
makeDecision genome memSize GameState{..} =
    let history = zip (take memSize ownHistory) 
                     (take memSize opponentHistory)
        index = historyToIndex history memSize
        threshold = genome !! index
    in if threshold > 0.5 then Cooperate else Defect

-- | Convert move history to genome index
historyToIndex :: [(Move, Move)] -> Int -> Int
historyToIndex history memSize =
    let bits = concatMap movePairToBits history
        padding = replicate (2 * memSize - length bits) False
    in bitsToInt (padding ++ bits)
  where
    movePairToBits (m1, m2) = [moveTobit m1, moveTobit m2]
    moveTobit Cooperate = False
    moveTobit Defect = True
    bitsToInt = foldr (\b acc -> fromEnum b + 2 * acc) 0

-- | Perform crossover between two parent strategies
crossover :: RandomGen g => g -> GeneticStrategy -> GeneticStrategy -> (GeneticStrategy, GeneticStrategy, g)
crossover gen parent1 parent2 =
    let genome1 = genome parent1
        genome2 = genome parent2
        (crossPoint, gen') = randomR (0, length genome1 - 1) gen
        (child1Genome, child2Genome) = 
            ( take crossPoint genome1 ++ drop crossPoint genome2
            , take crossPoint genome2 ++ drop crossPoint genome1
            )
        (nameId1 :: Int, gen'') = randomR (1000, 9999) gen'
        (nameId2 :: Int, gen''') = randomR (1000, 9999) gen''
        child1 = parent1 { genome = child1Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId1 }
        child2 = parent2 { genome = child2Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId2 }
    in (child1, child2, gen''')

-- | Mutate a strategy
mutate :: RandomGen g => g -> Double -> GeneticStrategy -> (GeneticStrategy, g)
mutate gen rate strategy =
    let (genome', gen') = 
            foldr (\prob (acc, g) -> 
                let (shouldMutate, g1) = randomR (0.0, 1.0) g
                    (newProb, g2) = if shouldMutate < rate
                                   then randomR (0.0, 1.0) g1
                                   else (prob, g1)
                in (newProb : acc, g2)
            ) ([], gen) (genome strategy)
    in (strategy { genome = genome' }, gen')

-- | Evolve a population of strategies
evolveStrategies :: Int -> Int -> Double -> Int -> [Strategy] -> IO [Strategy]
evolveStrategies popSize numGens mutRate eliteCount baseStrategies = do
    putStrLn "\nRunning Evolutionary IPD Tournament..."
    putStrLn "------------------------------------"
    putStrLn $ "Population Size: " ++ show popSize
    putStrLn $ "Generations: " ++ show numGens
    putStrLn $ "Mutation Rate: " ++ show mutRate
    putStrLn $ "Elite Count: " ++ show eliteCount
    putStrLn ""

    -- Run tournament with base strategies
    putStrLn "\nBase Strategies:"
    putStrLn "----------------------------------------"
    putStrLn "Strategy Name                Score"
    putStrLn "----------------------------------------"
    baseResults <- runTournament baseStrategies
    let sortedBaseResults = sortOn (negate . totalScore) baseResults
    forM_ (zip baseStrategies sortedBaseResults) $ \(strat, result) -> do
        let name = unpack $ strategyName result
            total = show $ totalScore result
            line = padRight 30 name ++ padLeft 10 total
        putStrLn $ name ++ " -> " ++ total
        putStrLn line
    putStrLn "----------------------------------------"

    -- Initialize population
    initialPop <- replicateM popSize generateRandomStrategy
    
    -- Evolution loop
    let loop gen pop = do
            -- Run tournament
            results <- runTournament (map geneticToStrategy pop)
            let popResults = zip pop results
            
            -- Print stats
            printGenerationStats gen popResults

            if gen < numGens
                then do
                    -- Select and breed next generation
                    nextGen <- evolveGeneration mutRate eliteCount popResults
                    loop (gen + 1) nextGen
                else return $ map geneticToStrategy pop

    -- Start evolution
    finalPop <- loop 1 initialPop
    return finalPop

-- | Initialize a random population
initializePopulation :: RandomGen g => g -> Int -> Int -> ([GeneticStrategy], g)
initializePopulation gen size startId = 
    let (strategies, gen') = 
            foldr (\id (acc, g) -> 
                let (strat, g') = createRandomStrategy g id 2  -- memory size of 2
                in (strat : acc, g')
            ) ([], gen) [startId..startId + size - 1]
    in (strategies, gen')

-- | Pad a string on the right with spaces to reach specified length
padRight :: Int -> String -> String
padRight n s = take n (s ++ repeat ' ')

-- | Pad a string on the left with spaces to reach specified length
padLeft :: Int -> String -> String
padLeft n s = take n (repeat ' ' ++ s)

-- | Print a single result line
printResult :: (Strategy, TournamentResult) -> IO ()
printResult (strat, result) = do
    let stratName = unpack $ strategyName result
        total = show $ totalScore result
        line = padRight 30 stratName ++ padLeft 10 total
    putStrLn $ stratName ++ " -> " ++ total
    putStrLn line

-- | Print generation statistics
printGenerationStats :: Int -> [(GeneticStrategy, TournamentResult)] -> IO ()
printGenerationStats gen results = do
    putStrLn $ "\nGeneration " ++ show gen ++ " Results:"
    putStrLn "----------------------------------------"
    putStrLn "Strategy Name                Score"
    putStrLn "----------------------------------------"
    let sortedResults = sortOn (negate . totalScore . snd) results
    forM_ sortedResults $ \(strat, result) -> do
        let name = unpack $ geneticStrategyName strat
            total = show $ totalScore result
            line = padRight 30 name ++ padLeft 10 total
        putStrLn $ name ++ " -> " ++ total
        putStrLn line
    putStrLn "----------------------------------------"

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
        putStrLn "----------------------------------------"
        putStrLn "Strategy Name                Score"
        putStrLn "----------------------------------------"
        let baseResults = take (length baseStrats) results
        forM_ baseResults $ \result -> do
            let stratName = unpack $ strategyName result
                total = show $ totalScore result
                line = padRight 30 stratName ++ padLeft 10 total
            putStrLn $ stratName ++ " -> " ++ total
            putStrLn line
        putStrLn "----------------------------------------"
        
        -- Print evolved strategies stats
        let evolvedResults = drop (length baseStrats) results
            -- Results are in the same order as the population
            popResults = zip pop evolvedResults
        
        printGenerationStats genNum popResults
            
        -- Select parents and create new population
        newPop <- createNewGeneration gen config pop evolvedResults
        
        -- Recurse
        evolveGenerations gen config baseStrats newPop (genNum + 1)

-- | Create a new generation through selection, crossover, and mutation
createNewGeneration :: RandomGen g => 
                      g -> 
                      GeneticConfig -> 
                      [GeneticStrategy] -> 
                      [TournamentResult] -> 
                      IO [GeneticStrategy]
createNewGeneration gen config pop results = do
    -- Sort population by fitness
    let popWithFitness = zip pop (map totalScore results)
        sortedPop = map fst $ sortOn (negate . snd) popWithFitness
        
        -- Keep elite strategies
        elite = take (eliteCount config) sortedPop
    
    -- Create rest through crossover and mutation
    children <- createChildren (mutationRate config) sortedPop (populationSize config - eliteCount config)
    return $ elite ++ children

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
generateRandomStrategy :: IO GeneticStrategy
generateRandomStrategy = do
    nameId <- randomRIO (1, 9999) :: IO Int
    genome <- replicateM (2 ^ (2 * 2)) (randomRIO (0.0, 1.0))  -- memory size of 2
    return GeneticStrategy
        { geneticStrategyName = pack $ "Gen-" ++ show nameId
        , genome = genome
        , memorySize = 2
        }

-- | Evolve a new generation
evolveGeneration :: Double -> Int -> [(GeneticStrategy, TournamentResult)] -> IO [GeneticStrategy]
evolveGeneration mutRate eliteCount results = do
    let sortedResults = sortOn (negate . totalScore . snd) results
        elite = take eliteCount $ map fst sortedResults
        rest = drop eliteCount $ map fst sortedResults
    
    -- Create children through crossover and mutation
    children <- createChildren mutRate rest (length results - eliteCount)
    return $ elite ++ children

-- | Create children through crossover and mutation
createChildren :: Double -> [GeneticStrategy] -> Int -> IO [GeneticStrategy]
createChildren rate parents numChildren = do
    let pairs = take (numChildren `div` 2) $ 
                [(p1, p2) | p1 <- parents, p2 <- parents, p1 /= p2]
    children <- concat <$> mapM (crossoverAndMutate rate) pairs
    return $ take numChildren children

-- | Perform crossover and mutation on a pair of parents
crossoverAndMutate :: Double -> (GeneticStrategy, GeneticStrategy) -> IO [GeneticStrategy]
crossoverAndMutate rate (parent1, parent2) = do
    let genome1 = genome parent1
        genome2 = genome parent2
    crossPoint <- randomRIO (0, length genome1 - 1)
    let (child1Genome, child2Genome) = 
            ( take crossPoint genome1 ++ drop crossPoint genome2
            , take crossPoint genome2 ++ drop crossPoint genome1
            )
    nameId1 <- randomRIO (1000, 9999) :: IO Int
    nameId2 <- randomRIO (1000, 9999) :: IO Int
    let child1 = parent1 { genome = child1Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId1 }
        child2 = parent2 { genome = child2Genome, geneticStrategyName = pack $ "Evo-" ++ show nameId2 }
    child1' <- mutateStrategy rate child1
    child2' <- mutateStrategy rate child2
    return [child1', child2']

-- | Mutate a strategy
mutateStrategy :: Double -> GeneticStrategy -> IO GeneticStrategy
mutateStrategy rate strategy = do
    genome' <- mapM mutateGene (genome strategy)
    return strategy { genome = genome' }
  where
    mutateGene prob = do
        shouldMutate <- randomRIO (0.0, 1.0)
        if shouldMutate < rate
            then randomRIO (0.0, 1.0)
            else return prob 