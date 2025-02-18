# ElectionModel-AP_Research

**Polling Simulation Analysis for Cruz vs. Allred**
This repository contains R code for simulating election outcomes based on polling data for the 2024 Texas Senate race between Ted Cruz and Colin Allred.

Overview:
The simulation models the probability of victory for each candidate using polling data, logistic regression, and Monte Carlo simulations. It provides insights into win probabilities and average margins of victory.

Key Findings:
Cruz Win Probability: 71% of simulations.
Mean Margin of Victory (Cruz): 4.18 percentage points.

Files:
poll_simulation.R: Main R script with data loading, modeling, and simulation.
pollresult.csv: Example polling dataset (not included here, add separately).
CruzSimulatedWinRate_Results.png: Distribution of Cruz's simulated win rate
TexasSenateMarginofVictory_Results.png: Distribution of election predicted margin of victory

Requirements:
R version 4.0 or higher
Libraries: caret, glmnet, ggplot2
