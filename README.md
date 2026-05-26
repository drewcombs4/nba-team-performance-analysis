# NBA Team Performance and Three-Point Shooting Efficiency

## A Fixed Effects Analysis of the Modern NBA (2014–2024)

This project analyzes the relationship between three-point shooting efficiency and NBA team success using fixed effects regression modeling and team-level NBA data from the 2013–14 through 2023–24 seasons.

The analysis examines whether teams that outperform league-average three-point efficiency consistently win more games after controlling for offensive and defensive variables including turnovers, rebounds, assists, steals, blocks, and personal fouls.

---

## Key Findings

- Weighted three-point efficiency significantly predicts NBA team wins
- Turnovers negatively impact team success
- Defensive rebounds, steals, and blocks remain highly valuable
- The model explains approximately 67% of variation in team wins

---

## Methodology

- Team-level NBA data from Basketball Reference
- Season-demeaned variables
- Fixed effects regression modeling
- Multicollinearity testing using VIF diagnostics
- Data visualization using ggplot2 in R

---

## Technologies Used

- R
- tidyverse
- ggplot2
- plm
- readxl

---

## Repository Structure

```text
data/       -> raw and cleaned datasets
scripts/    -> regression and visualization code
outputs/    -> figures and regression outputs
report/     -> final PDF report
assets/     -> project visuals/logo
