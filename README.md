# Ascension
A metaheuristic optimization framework. See the [project homepage](http://inversed.ru/Ascension.htm) for an overview and notable results (please note that many of the described features had to be omitted from the 2.0 release). 

## How to use the framework
Copy a problem definition module (PDM) from the *problems* into the project folder, rename it to *problem.pas*, then compile (I recommend using [Lazarus IDE](https://www.lazarus-ide.org/)) and run Ascension. Algorithm parameters are specified in the *Config.ini* file.
Several files are created during the optimization process:

File | Description
--- | ---
XX_Status | Status file containing the data about the optimization process (XX is the abbreviated algorithm name)
XX_Best | The best solution found during the optimization.
Runs | Information about the results of multiple optimization runs
Runs_Best | The best solution found during multiple optimization runs

## Included PDMs
File | Problem
--- | ---
Problem_2DHP.pas | Hydrophobic-polar protein folding model
Problem_3DNQ.pas | 3D N queens
Problem_Chess_Covering.pas | Queen (non)domination, knights covering
Problem_NQ.pas | N queens
Problem_No_Subsquares.pas | Maximal density subsquare-free arrangements
Problem_Peaceable_Queens.pas | Peaceable queens
Problem_Still_Life.pas | Maximal density still life

See the description section inside of each file for more details.

## How to create a PDM
You can specify your own problem by creating a *problem.pas* file that supplies the types and routines required by each metaheuristic (full list can be found in *interface.inc*). See *Problem_NQ.pas* from the *problems* folder for an example of a complete PDM. If you don't need a certain algorithm, the corresponding definitions can be effectively omitted by placing a *{$I DummyXX.inc}* line at the end of the file.

## Config parameters
### Global Parameters
Parameter | Description
--- | --- 
Algorithm | Optimization algorithm
⇢"SA" | Simulated Annealing
⇢"GA" | Genetic Algorithm
⇢"LS" | Local Search
⇢"TS" | Tabu Search
⇢"CTS" | Cooperative Tabu Search
NRuns | Number of independent algorithm runs
ScoreToReach | The desired value of the score function, can be used by stopping criteria

### Local Search
Parameter | Description
--- | --- 
Mode | Local search mode.
⇢"First" | Pick the first improving move from a move list
⇢"Best" | Pick the best move from a move list
⇢"Chain" | Sort all moves by score, then try to sequentially apply all improving moves in order from best to worst
StatusIters | Interval between saving the data about optimization process to a status file, nothing is saved if set to zero

### Tabu Search
Parameter | Description 
--- | --- 
Iterations | Total number of iterations
PopSize | Population size (cooperative variant only)
StatusIters | Interval between saving the data about optimization process to a status file, nothing is saved if set to zero

### Simulated Annealing
Parameter | Description 
--- | --- 
Iterations | Total number of iterations
T0 | Initial temperature, used if *T0Mode* is *"Manual"*
Tfin | Final temperature, used if *TfinEBased* is *No*
dEmax | Maximal change of energy, used if *T0Mode*  is *"EBased"*
dEmin | Minimal change of energy, used if *TfinEBased*  is *Yes*
T0Mode | Initial temperature selection
⇢"Manual" | Determined by *T0*
⇢"EBased" | Calculated based on *dEmax*
⇢"AutoLow" | Automatic selection, lower temperature mode
⇢"AutoHigh" | Automatic selection, higher temperature mode
TfinEBased | Final temperature selection
⇢No | Determined by *Tfin*
⇢Yes | Calculated based on *dEmin*
Acceptance | Acceptance function
⇢"Exp" | Exp(-x<sup>p</sup>)
⇢"Power" | 1 / (1 + x<sup>p</sup>)
⇢"Tsallis" | (1 - (1 - p) x)<sup>1 / (1 - p)</sup>
⇢"Threshold" | (1 - Sign(x - 1)) / 2
⇢"Barker" | 1 / (1 + Exp(x))
AcceptanceP | Acceptance function parameter
Schedule | Cooling schedule type
⇢"Zero" | T = 0
⇢"Log" | T ~ 1 / (C + Ln(1 + t / L)) 
⇢"Power" | T ~ (1 + t / L)<sup>p</sup>
⇢"Exp" | T ~ Exp(-(t / L)<sup>p</sup>)
ScheduleP | Cooling schedule parameter
NReheat | Number of reheating stages, no reheating if set to 1
FastReheat | Determines the duration of reheating stages
⇢No | Each reheating stage takes the same number of iterations
⇢Yes | Later reheating stages take less iterations
Smoothing | The amount of smoothing applied when calculating statistics for the status file
StatusIters | Interval between saving the data about optimization process to a status file, nothing is saved if set to zero.

### Genetic Algorithm
Parameter | Description 
--- | --- 
PopulationSize | Number of individuals in the population
Selection | Method of selecting individuals for reproduction
⇢"RankProp" | Selection probability is proportional to (1 - R)<sup>p</sup> if p > 0 or R<sup>-p</sup> if p < 0, where R is the individual's rank normalized to [0 .. 1] and p = *SelectionP*
⇢"Dist" | *SelectionP* individuals are picked at random, then the pair with the largest (smallest) intersolution distance is selected if *SelectionP* > 0 (< 0)
⇢"DistToBest" | *SelectionP* individuals are picked at random, then two individuals with the largest (smallest) distance to the best individual in the population are selected if *SelectionP* > 0 (< 0)
SelectionP | Selection parameter
Replacement | Method of selecting which individual to replace
⇢"Worst" | Worst individual in the population
⇢"InvRank" | Selection probability is proportional to R<sup>p</sup> if p > 0 or (1 - R)<sup>-p</sup> if p < 0, where R is the individual's rank normalized to [0 .. 1] and p = *ReplacementP*
⇢"WorstParent" | Worst parent
⇢"RandParent" | Random parent
⇢"SimilarParent" | Most similar parent
⇢"Influx" | Worst parent. When the child is better than both parents, also replace the other parent with a new solution
ReplacementP | Replacement parameter
Acceptance | Criterion for determining whether the solution picked by *Replacement* method actually gets replaced
⇢"Elitist" | Replace only if the child is better
⇢"Unconditional" | Replace unconditionally
StopCriterion | Criterion for stopping the algorithm
⇢"MaxGens" | Maximal number of generations is reached
⇢"MaxNFE" | Maximal number of function evaluations is reached
⇢"Score" | *ScoreToReach* is reached
StatusGens | Interval in generations between saving the data about optimization process to a status file, nothing is saved if set to zero
SaveGens | Interval in generations between saving the population, nothing is saved if set to zero

## Acknowledgements
This project has been supported by the following patrons via [Patreon](https://www.patreon.com/inversed):
* Brian Bucklew
* Anton Shepelev
* Adam Hill
* John Metcalf
* Tomoyuki Naito
