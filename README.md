# Unemployment and Job Vacancy Analysis

This repository contains R scripts for analyzing unemployment and job vacancy data. The analysis includes data manipulation, visualization, and correlation studies based on historical unemployment rates and job vacancies. The script was created as part of a data analysis practical task for the internship at the Lithuanian Statistics Department

## Table of Contents

- [General Information](#general-information)
- [Unemployment Analysis](#unemployment-analysis)
  - [Summary Statistics](#summary-statistics)
  - [Unemployment Rate Over Time](#unemployment-rate-over-time)
  - [High Unemployment Rates](#high-unemployment-rates)
  - [Gender Disparities in Unemployment](#gender-disparities-in-unemployment)
  - [Quarterly Data Analysis](#quarterly-data-analysis)
- [Job Vacancy Analysis](#job-vacancy-analysis)
  - [Summary Statistics](#summary-statistics-1)
  - [Total Job Vacancies](#total-job-vacancies)
  - [Correlation Analysis](#correlation-analysis)
- [Final Dataset Export](#final-dataset-export)
- [Dependencies](#dependencies)

## General Information

This repository contains R scripts that analyze unemployment and job vacancy data. The datasets are anonymized and contain information about unemployment rates for men and women, as well as job vacancies across various economic activities.


```r
# Load necessary libraries
library(tibble)
library(dplyr)
library(readr)
library(tidyverse)
library(gridExtra)
library(stringr)
library(ggplot2)
library(rio)
library(xlsx)


# Rename column
names(NedarboLygis)[2] <- "laikotarpis"

# Summary statistics
NedarboLygis %>% summary()
