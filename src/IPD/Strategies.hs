-- | Implementation of various strategies for the Iterated Prisoner's Dilemma
module IPD.Strategies
    ( alwaysCooperate
    , alwaysDefect
    , titForTat
    , grudger
    , randomStrategy
    ) where

import IPD.Types
import System.Random (RandomGen, random)

-- | The simplest cooperative strategy
-- Always returns Cooperate regardless of opponent's moves
-- This strategy is naive but can do well against other cooperative strategies
alwaysCooperate :: Strategy
alwaysCooperate = Strategy
    { name = "Always Cooperate"
    , play = const Cooperate    -- Ignores game state and always cooperates
    }

-- | The simplest competitive strategy
-- Always returns Defect regardless of opponent's moves
-- This strategy exploits cooperative strategies but performs poorly against itself
alwaysDefect :: Strategy
alwaysDefect = Strategy
    { name = "Always Defect"
    , play = const Defect       -- Ignores game state and always defects
    }

-- | The famous Tit for Tat strategy
-- Cooperates on first move, then copies opponent's last move
-- This strategy won Axelrod's original tournament and is remarkably effective
-- Key features:
-- 1. Nice - starts with cooperation
-- 2. Retaliatory - punishes defection
-- 3. Forgiving - returns to cooperation if opponent does
-- 4. Clear - easy for opponents to learn its pattern
titForTat :: Strategy
titForTat = Strategy
    { name = "Tit for Tat"
    , play = \GameState{..} -> case opponentHistory of
        [] -> Cooperate                 -- Cooperate on first move
        (lastMove:_) -> lastMove        -- Copy opponent's last move
    }

-- | Grudger strategy (also known as Grim Trigger)
-- Cooperates until opponent defects, then always defects
-- This strategy is very unforgiving - a single defection triggers permanent retaliation
-- Effective against exploitative strategies but vulnerable to noise
grudger :: Strategy
grudger = Strategy
    { name = "Grudger"
    , play = \GameState{..} ->
        if any (== Defect) opponentHistory
            then Defect    -- If opponent has ever defected, always defect
            else Cooperate -- Otherwise cooperate
    }

-- | Random strategy
-- Randomly chooses between cooperate and defect
-- This strategy serves as a baseline for comparing other strategies
-- It's unpredictable but generally performs poorly in tournaments
randomStrategy :: RandomGen g => g -> Strategy
randomStrategy gen = Strategy
    { name = "Random"
    , play = \_ -> if fst (random gen) then Cooperate else Defect  -- 50/50 chance of each move
    } 