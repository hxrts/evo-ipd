# Genetic Algorithm for Iterated Prisoner's Dilemma Strategy Optimization

This program implements a genetic algorithm to discover effective strategies for the Iterated Prisoner's Dilemma (IPD) game, competing against classic strategies like Tit-for-Tat, Always Cooperate, Always Defect, and Grudger.

### Iterated Prisoner's Dilemma

The Iterated Prisoner's Dilemma is a fundamental game theory problem where two players must choose to either cooperate or defect, with payoffs determined by their combined choices. In the iterated version, the game is played repeatedly, allowing for the development of complex strategies based on the player's history of interaction.

### The Original Tournament

In 1980, political scientist Robert Axelrod organized a computer tournament to study effective strategies for the IPD. He invited academics to submit strategies, which would complete in a round-robin competition. Surprisingly, the winning strategy was a simple Tit-for-Tat implementation that cooperated on the first move and then copied the opponent's previous move.

The tournament was scored according to the following payoff matrix:

|              | Player 2: Cooperate | Player 2: Defect |
|--------------|:------------------:|:----------------:|
| **Player 1: Cooperate** |     (3,3)          |     (0,5)        |
| **Player 1: Defect**    |     (5,0)          |     (1,1)        |

Key findings from Axelrod's tournament:
- Successful strategies tended to be "nice" (never first to defect)
- Retaliation against defection was important
- Forgiveness after retaliation helped restore mutual cooperation
- Simple strategies often outperformed complex ones

### This Tournament Implementation

This implementation builds on Axelrod's framework, introducing evolutionary optimization:
- Each strategy plays against every other strategy for 200 rounds (matching Axelrod's setup)
- Strategies accumulate points according to the same payoff matrix
- Total scores determine both tournament rankings and evolutionary fitness
- Unlike the original tournament, strategies can evolve and improve round by round

### Genetic Algorithms

Genetic algorithms use variation and selection to guide the search for optimal solutions. They work by maintaining a population of potential solutions (in our case, strategies) and iteratively improving them through an evolutionary process.

1. **Population**: Each individual in the population represents a possible solution, encoded in a "genome"
2. **Fitness Evaluation**: Individuals are evaluated based on how well they solve the problem
3. **Selection**: Better-performing individuals are more likely to be chosen as parents
4. **Reproduction**: New individuals are created through:
   - Crossover: Combining parts of two parent solutions
   - Mutation: Random modifications to maintain diversity
5. **Succession**: New populations replace the old, with some top performers preserved

Genetic algorithms are particularly well-suited for problems like strategy optimization in the IPD because:
- The solution space is too large to search exhaustively
- Small changes in strategy can be meaningful
- Solutions can be naturally encoded as a sequence of numbers (probabilities)
- The fitness function (tournament performance) is well-defined

### Genetic Algorithm Implementation

The program uses a genetic algorithm with the following features:

1. **Strategy Representation**:
   - Each strategy is represented by a genome of probabilities
   - The genome determines the probability of cooperation based on the history of previous moves
   - Strategies have a memory of the last N moves (default: 2 moves)

2. **Genetic Operations**:
   - **Crossover**: Pairs of parent strategies combine their genomes to create new child strategies
   - **Mutation**: Random changes to the genome allow for exploration of new strategies
   - **Positive Selection**: The best performing strategies are preserved across generations

3. **Fitness Evaluation**:
   - Each strategy plays against all others in round-robin tournaments
   - Strategies accumulate scores based on the standard Prisoner's Dilemma payoff matrix
   - Tournament results determine which strategies survive and reproduce

## Base Strategies

The program includes several classic IPD strategies for comparison:
- **Always Cooperate**: Always plays cooperate
- **Always Defect**: Always plays defect
- **Tit-for-Tat**: Cooperates on first move, then copies opponent's last move
- **Grudger**: Cooperates until opponent defects, then always defects
- **Random**: Randomly chooses between cooperate and defect

### Configuration

The evolutionary process can be configured with the following parameters:
- Population Size: Number of strategies in each generation (default: 50)
- Number of Generations: How many generations to evolve (default: 5)
- Mutation Rate: Probability of genome mutations (default: 0.1)
- Survival Count: Number of top strategies preserved (default: 5)
- Tournament Rounds: Number of rounds per game (default: 200)

## Build and Run

Build the program:
```bash
nix build
```

Run a single tournament with base strategies only:
```bash
nix run .#
```

To run the tournament with strategy evolution:
```bash
nix run .# -- evolve
```

## Results

The program outputs detailed statistics for each generation, showing how strategies evolve and improve over time. The final tournament compares the evolved strategies against the classic base strategies to evaluate their effectiveness.