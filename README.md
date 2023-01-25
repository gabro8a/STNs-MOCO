# STNs-MOCO
Search trajectory networks for multi-objective combinatorial pptimisation

## STNs Construction, Visualisation & Metrics

This repository contains data, code, plots and metrics associated to the article

*Decision/Objective Space Trajectory Networks for Multi-objective Combinatorial Optimisation*
by Gabriela Ochoa, Arnaud Liefooghe, Yuri Lavinas, and Claus Aranha
To appear in EvoCOP 2023, The 23rd European Conference on Evolutionary Computation in Combinatorial Optimisation 

#### Trajectory Data 

- **Algorithms**:  two state-of-the-art multi-objective evolutionary algorithms, MOEA/D and NSGA2. 
- **Benchmark**:  two- and three-objective *ρmnk*-landscapes, producing  multi-objective multi-modal landscapes with various degree of objective correlation.

#### R Scripts 

- **create.R**: reads trajectory data and creates STN models.

- **plot.r**:  reads STN models and produce STN graph visualisations with to alternative layouts: force-directed and objective-space. Nodes and edges are decorated with relevant attributes.

- **metrics.r**:  reads STN models and produce a CSV file containing relevant network metrics.

  

  
