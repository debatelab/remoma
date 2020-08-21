# Reflective Equilibrium MOdel in MAthematica (REMoMa)

This repository complements the philosophical paper **"Making Reflective Equilibrium Precise. A Formal Model" by Beisbart, Betz and Brun**, to-be-published with **[ERGO](https://www.ergophiljournal.org/)**. It includes all the code used to generate the computational results presented and discussed therein.

## Simulation Results of RE Processes

Results of RE process simulations (equilibration) are stored in the folder `results`. The repo contains four ensembles:

- `four-cases`: RE process simulations of the four illustrative cases discussed in the paper with default parameter settings.
- `ini-com_sample`: RE process simulations with random initial commitments, parameters as in *four_cases*.
- `par-narrow_sample`: RE process simulations with small random variations of the parameter values used in *four_cases*; initial commitments as in *four_cases*. This ensemble us used to check robustness of the precise RE process as simulated in *four_cases*.
- `par-broad_sample`: RE process simulations with full random variations of the parameter values used in *four_cases*; initial commitments as in *four_cases*. This ensemble us used to check robustness of the general claim that RE processes lead to global optima / full RE states.

## Analysis and Visualization

Various notebooks are used to analyze and visualize simulation results (folder `notebooks`). When running these Notebooks, please make sure to *adjust file-paths* to your system.

- `VisualizingResults` is used to plot every step of a RE equilibration process.
- `GlobalOptimization` checks whether the equilibration end points in the four-cases are global optima; and visualizes, in addition, various properties of global optima over the entire parameter space.
- `LocalAndGlobalOptima_RandomSampleIniComs` calculates how many equilibration fixed points in an ensemble with random initial commitments are global optima.
- `LocalAndGlobalOptima_RandomSampleParams` calculates how many equilibration fixed points in an ensemble with random parameter combinations (weights of the achievement function) are global optima.
- `ParameterRobustness` checks the robustness of the simulation of the four illustrative cases to small parameter perturbations.

## Running the Simulations

We provide three scripts to run simulations and generate your own results.

- `run_4simus.m` has been used to simulate four RE processes and to generate the four illustrative cases.
- `run_simus_random_sample_param.m` has been used to generate the two ensemble with parameter perturbations.
- `run_simus_random_sample_inicom.m` has been used to generate the ensemble with random initial commitments.

## Technical Caveats

### Formal Representation of Positions as Integers

We model theories and commitments as positions, that is as subsets of the sentence pool, for example: {1,-4,6} or {2,3,-6,-7}. Such (minimally consistent) positions are represented -- in the code -- as integers. The idea is the following. Every integer n can be written as a ternary number, i.e. 

```
n = (c_0 * 3^0) + (c_1 * 3^1) + (c_2 * 3^2) + ...
```

with 0 ≤ c_i ≤ 2. We now interpret the coefficients c_i for **n-1** (i.e., the digits of the ternary representation of n-1) as an index function over the non-negative sentence pool which determines whether the position accepts / negates / suspends judgment wrt. a given sentence.

As you see for example in `run_4simus.m`, the illustrative initial commitments are represented as {118,121,1090,1333}. Now, 

```
118-1 = 81 + 27 + 9  
      = (1 * 3^4) + (1 * 3^3) + (1 * 3^2)      
```

So, written as a ternary number, 118-1 equals `0011100` (with two `0` padded). 118 hence picks 3,4,5 from the sentence pool. Likewise

```
1333-1  = 729 + 486 + 81 + 27 + 9
        = (1 * 3^6) + (2 * 3^5) + (1 * 3^4) + (1 * 3^3) + (1 * 3^2)
```

Therefore, the integer 1333 represents the position {3,4,5,-6,7}.

Such ternary representation of positions allows for efficient coding.


### Parameters alpha and beta

The simulation runs are initialized with parameters alpha and beta. These parameters determine the relative weight of

- Account vs Systematicity (alpha) in step theory choice,
- Account vs Faithfulness (beta) in step commitment adjustment.

The higher the parameter values, the more weight is given to account.

These parameters alpha and beta are related to the weights w_a, w_s, and w_f of the achievement (as presented in the paper) as follows:

```
WeightAccount[alpha_, beta_] := 
  (alpha*beta)/(alpha + beta - alpha*beta);
WeightSystematicity[alpha_, beta_] := 
  (beta - alpha*beta)/(alpha + beta - alpha*beta);
WeightCloseness[alpha_, beta_] := 
  1 - (WeightAccount[alpha, beta] + WeightSystematicity[alpha, beta]);
```