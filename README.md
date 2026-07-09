# Explaining Wage Differences in Indonesia

## Overview

**Explaining Wage Differences in Indonesia** is an empirical research project that examines wage disparities between **STEM (Science, Technology, Engineering, and Mathematics)** and **Non-STEM** bachelor's degree holders in Indonesia.

Using nationally representative labor force data from **SAKERNAS (National Labor Force Survey) August 2021**, the project estimates the extent to which observed wage differences are explained by differences in worker characteristics versus differences in labor market returns.

The analysis combines Ordinary Least Squares (OLS) regression with the **Blinder–Oaxaca decomposition** to quantify the sources of the wage gap.


## Research Objective

The project aims to answer the following research questions:

- Do STEM graduates earn higher wages than Non-STEM graduates in Indonesia?
- How much of the wage gap is explained by observable characteristics such as education, occupation, industry, demographic characteristics, and labor market experience?
- How much of the remaining wage gap is attributable to differences in returns to these characteristics?

## Methodology

The empirical analysis consists of two main approaches:

### 1. Ordinary Least Squares (OLS)

OLS regression is used to estimate the relationship between wages and individual characteristics while controlling for:

- Educational attainment
- Age and labor market experience
- Gender
- Occupation
- Industry
- Employment characteristics
- Geographic controls
- Other demographic variables

### 2. Blinder–Oaxaca Decomposition

The Blinder–Oaxaca decomposition separates the average wage gap into two components:

- **Explained Component**
  - Wage differences attributable to observable characteristics (endowments).

- **Unexplained Component**
  - Wage differences arising from different returns to those characteristics, often interpreted as differences in labor market valuation or other unobserved factors.

This decomposition provides a deeper understanding of the mechanisms underlying wage inequality between STEM and Non-STEM graduates.

---

## Dataset

The analysis uses data from:

**Survei Angkatan Kerja Nasional (SAKERNAS)**
- August 2021
- Statistics Indonesia (Badan Pusat Statistik - BPS)

The dataset is nationally representative of Indonesia's labor force.


## Statistical Methods

- Data cleaning and preprocessing
- Descriptive statistics
- Ordinary Least Squares (OLS)
- Robust standard errors
- Blinder–Oaxaca decomposition
- Wage gap analysis

## Software

The analysis is conducted in **R**, primarily using packages such as:

- dplyr
- tidyverse
- haven
- fixest
- oaxaca
- ggplot2


## Potential Applications

This project may be useful for research on:

- Labor economics
- Returns to education
- Human capital
- Wage inequality
- STEM education policy
- Occupational choice
- Indonesian labor market studies

## Citation

If you use this code or methodology in your own research, please cite this repository together with the original SAKERNAS dataset published by Statistics Indonesia (BPS).

## Disclaimer

The SAKERNAS dataset is owned by **Statistics Indonesia (BPS)**. This repository contains only the code and supporting materials used for empirical analysis and does not redistribute the original survey microdata.
