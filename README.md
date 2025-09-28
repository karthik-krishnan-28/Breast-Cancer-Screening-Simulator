# Breast-Cancer-Screening-Simulator
A Shiny app visualizer for agent-based model in R simulating rural breast cancer screening uptake, incorporating race, poverty, insurance, and access-to-care dynamics.  

This repository contains code for an app that visualizes an **agent-based model (ABM)** implemented in R to simulate breast cancer screening behaviors in rural populations. The model explores how race, poverty, insurance status, and geographic access interact to influence screening intentions and uptake over time.  

## Features
- Generates synthetic rural social networks using `igraph`
- Assigns sociodemographic attributes to agents:
  - Race (White vs. Racialized Minority)
  - Poverty status
  - Insurance status
  - Distance from care (>25 miles vs. <25 miles)
- Models initial screening intentions using a logistic regression–style scoring system
- Simulates screening uptake dynamics over **24 time steps**
  - Screening intention rises when ≥3 connected neighbors are screened
  - Agents with screening intention ≥0.9 may get screened (with stochastic noise)
- Supports **multiple model runs** and **scenario-based experimentation** via user-supplied parameter data frames
- Outputs:
  - **Simulation logs** (aggregate counts over time, by race)
  - **Node histories** (agent-level screening status and timing)
 
Full credit for the ABM goes to Dr. Jennifer Cruz who led and owns this project, and who hired me as a collaborator (https://service.harvard.edu/people/jen-cruz). Due to the publication being in progress, Dr. Cruz's code has been witheld from this repository, and only the app code remains.

## Visualization
![home](https://github.com/karthik-krishnan-28/Breast-Cancer-Screening-Simulator/blob/main/app-screens/sim-home.png)
The Shiny app provides a user-friendly interface with sliders and presets to configure simulations. Example outputs include:
- Cumulative screening uptake by time step
  ![total-screened](https://github.com/karthik-krishnan-28/Breast-Cancer-Screening-Simulator/blob/main/app-screens/total-screened.png)
- Proportion of newly screened agents by run
  ![newly-screened](https://github.com/karthik-krishnan-28/Breast-Cancer-Screening-Simulator/blob/main/app-screens/proportion-newly-screened.png)

