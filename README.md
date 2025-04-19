# Analysis of persistent social conformity on risk preferences in moral and economic choices

This repository contains the data processing and analysis scripts for
the peer-reviewed paper ["People conform to social norms when gambling
with lives or money"](https://doi.org/10.1038/s41598-023-27462-1) by
Jiang, Marcowski, Ryazanov, and Winkielman (2023) published in
Scientific Reports.

## Table of Contents

-   [Overview](#overview)
-   [Structure](#structure)
-   [Requirements](#requirements)
-   [Usage](#usage)
-   [Data Description](#data-description)
-   [Analysis](#analysis)
-   [Figures](#figures)
-   [Citation](#citation)
-   [Contact](#contact)

## Overview

This project investigates how social norms influence people's risk
preferences in both moral and monetary domains. Using an adapted version
of the Asian Disease Paradigm, we examine whether and how people shift
their risk preferences to conform with group norms, and whether these
conformity effects persist over time.

The study employs a mixed experimental design with: - Between-subjects
factors: framing (gain vs. loss), social norm type (risk-averse vs.
risk-seeking) - Within-subjects factors: temporal measurement (baseline,
immediate post-exposure, 3-day follow-up) - Domain comparison: moral
decisions (Experiment 1: animal lives) vs. monetary decisions
(Experiment 2)

Key findings: - Participants shift their risk preferences to align with
observed social norms - Conformity effects persist after a three-day
delay - In the monetary domain, risk-averse norms have more influence in
loss frames while risk-seeking norms have more influence in gain
frames - In the moral domain, risk-averse norms are more influential in
loss frames, but both norm types are equally effective in gain frames

The analysis uses mixed-effects logistic regression models to capture
these effects while accounting for individual differences.

## Structure

```         
moral-gambles-conformity/
│
├── data/
│   ├── raw/
│   │   ├── experiment1_day0.csv
│   │   ├── experiment1_day3.csv
│   │   ├── experiment2_day0.csv
│   │   └── experiment2_day3.csv
│   └── prepared/
│       ├── experiment1.Rds
│       └── experiment2.Rds
│
├── output/
│   ├── models_day0.Rds
│   ├── models_day3.Rds
│   ├── fig2.pdf  # Experiment 1 (Lives) - Day 0
│   ├── fig3.pdf  # Experiment 1 (Lives) - Day 3
│   ├── fig4.pdf  # Experiment 2 (Money) - Day 0
│   ├── fig5.pdf  # Experiment 2 (Money) - Day 3
│   └── fig6.pdf  # Combined analysis
│
├── prepare_data.R
├── run_analysis_day0.R
├── run_analysis_day3.R
└── README.md
```

## Requirements

The scripts require R and the following packages installed:

``` r
install.packages(c(
  "tidyverse", 
  "haven", 
  "glmmTMB", 
  "emmeans", 
  "easystats", 
  "patchwork", 
  "ggsignif"
  ))
```

## Usage

1.  Clone this repository:

    ```         
    git clone [repository URL]
    ```

2.  Navigate to the project directory:

    ```         
    cd path/to/moral-gambles-conformity
    ```

3.  Run the scripts in the following order:

    ``` r
    # Data preparation
    source("prepare_data.R")

    # Analysis of day 0 data (immediate conformity effects)
    source("run_analysis_day0.R")

    # Analysis of day 3 data (persistence of conformity)
    source("run_analysis_day3.R")
    ```

The scripts will generate processed data files and output files
including fitted models and figures in the respective directories.

## Data Description

The study used a three-phase experimental design:

1.  **Baseline Phase**: Participants' initial risk preferences were
    assessed by having them choose between certain options (e.g.,
    saving/losing 10 lives/dollars for sure) and gamble options (e.g.,
    50% chance of saving/losing different amounts) across multiple
    scenarios.
    
2.  **Learning Phase**: Participants observed and guessed group
    preferences (with feedback) to learn about either risk-averse or
    risk-seeking social norms.
    
3.  **Transfer Phase**: Participants' risk preferences were reassessed
    immediately after the learning phase and again after a three-day
    delay.

Raw data files include:

-   `experiment1_day0.csv`: Moral domain (lives) - data from day 0
-   `experiment1_day3.csv`: Moral domain (lives) data from 3-day follow-up
-   `experiment2_day0.csv`: Monetary domain data from day 0
-   `experiment2_day3.csv`: Monetary domain data from 3-day follow-up

These files contain trial-level choice data, participant demographics,
and experimental condition information.

## Analysis

The analysis follows these steps:

1.  **Data Preparation** (`prepare_data.R`):
    -   Loads raw data from both experiments
    -   Cleans data and applies exclusion criteria
    -   Creates consistent factor levels
    -   Saves processed datasets
    
2.  **Day 0 Analysis** (`run_analysis_day0.R`):
    -   Fits mixed-effects logistic regression models for each
        experiment and combined data
    -   Examines baseline risk preferences
    -   Calculates immediate conformity effects
    -   Compares effects across conditions and domains
    -   Generates figures for day 0 results
    
3.  **Day 3 Analysis** (`run_analysis_day3.R`):
    -   Fits models for follow-up data
    -   Examines persistence of conformity effects after delay
    -   Compares day 3 responses to baseline and day 0
    -   Generates figures for day 3 results

## Figures

The analysis produces the following figures:

-   **Figure 2**: Conformity effects in the moral domain (Experiment 1) on day 0
-   **Figure 3**: Persistence of conformity in the moral domain (Experiment 1) on day 3
-   **Figure 4**: Conformity effects in the monetary domain (Experiment 2) on day 0
-   **Figure 5**: Persistence of conformity in the monetary domain (Experiment 2) on day 3
-   **Figure 6**: Comparisons between moral and monetary domains

## Citation

If you use this code or data in your research, please cite our paper:

> Jiang, Y., Marcowski, P., Ryazanov, A., & Winkielman, P. (2023).
> People conform to social norms when gambling with lives or money.
> Scientific Reports, 13, 853.
> <https://doi.org/10.1038/s41598-023-27462-1>

## Contact

For any questions or feedback, please contact the author directly.
