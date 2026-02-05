# Network Structure of Wikipedia Attention and Financial Market Volatility

This repository contains the research project developed during my Erasmus+ at **Radboud University Nijmegen**. The study explores the relationship between the network topology of information diffusion on Wikipedia and the stock market volatility of NVIDIA (NVDA).



## Research Summary
The project tests whether network metrics (like **Weighted Eigenvector Centrality**) predict market volatility more accurately than simple attention volume (pageviews). 

### Key Findings:
* **Network Centrality**: Successfully predicts increased volatility ($\beta=0.062, p<0.01$).
* **Attention Volume**: Total pageviews showed no significant effect, highlighting that *how* information is structured matters more than *how much* attention it receives.
* **Methodology**: Time-series regression using **Newey-West** standard errors and analysis of Shannon entropy vs. Herfindahl-Hirschman Index.

## Technical Implementation
The analysis was performed using **R** with the following key libraries:
* `igraph`: For network centrality and topology metrics.
* `quantmod`: To retrieve and process financial market data.
* `tidyverse`: For data cleaning and visualization (ggplot2).
* `sandwich` & `lmtest`: For robust statistical modeling.



## Contents
* `script_information.R`: Complete R script from data collection (Wikimedia API) to statistical modeling.
* `information-3.pdf`: The full academic report detailing the theoretical framework (Diffusion Theory) and results.

## Academic Background
Completed in January 2025 as part of the **Diffusion of Information** course. This work bridges behavioral finance with information network theory.
