# STAT 4052 Final Project

This repository contains the R code, data files, generated figures, and output tables for my STAT 4052 final project.

## Project Topic

Predicting food waste propensity using consumer-level and product-level characteristics.

## Data Source

The data come from:

Peterson, H. H. (2020). D2D 2016 Food Study. Data Repository for the University of Minnesota. https://doi.org/10.13020/0m0p-tt02

## Repository Structure

- `data/`: dataset and variable definition files
- `scripts/`: main R analysis script
- `figures/`: generated figures used in the report
- `tables/`: output tables used in the report

## Main Analysis Script

The main analysis file is:

`scripts/STAT4052_final_project_analysis.R`

## Notes

The script reads the data from the `data/` folder, fits multiple linear regression, lasso regression, and random forest models, evaluates model performance on a held-out test set, and saves the final figures and tables.
