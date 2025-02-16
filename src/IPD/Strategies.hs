module IPD.Strategies
    ( alwaysCooperate
    , alwaysDefect
    , titForTat
    , grudger
    , randomStrategy
    ) where

import IPD.Types
import System.Random (RandomGen, random)

-- | Always cooperates
alwaysCooperate :: Strategy
alwaysCooperate = Strategy
    { name = "Always Cooperate"
    , play = const Cooperate
    }

-- | Always defects
alwaysDefect :: Strategy
alwaysDefect = Strategy
    { name = "Always Defect"
    , play = const Defect
    }

-- | The famous Tit for Tat strategy
-- Cooperates on first move, then copies opponent's last move
titForTat :: Strategy
titForTat = Strategy
    { name = "Tit for Tat"
    , play = \GameState{..} -> case opponentHistory of
        [] -> Cooperate  -- Cooperate on first move
        (lastMove:_) -> lastMove  -- Copy opponent's last move
    }

-- | Grudger strategy
-- Cooperates until opponent defects, then always defects
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
randomStrategy :: RandomGen g => g -> Strategy
randomStrategy gen = Strategy
    { name = "Random"
    , play = \_ -> if fst (random gen) then Cooperate else Defect
    } 