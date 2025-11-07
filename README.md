# Rising cost of living in Singapore

## Project Overview
This project investigates the drivers behind Singapore’s rising cost of living — a persistent concern highlighted in the 2025 IPS post-election study — by examining how prices, wages, and economic indicators have evolved over the last four decades.

While Goods and Services Tax (GST) is often cited as a key contributor to inflation, this study integrates multiple datasets to assess how much of the price rise can truly be attributed to GST versus broader macroeconomic and structural factors such as housing prices, income dynamics, borrowing costs, and industrial activity.

## Dataset
- Data sourced from publicly available databases
- Data files included in the `Data/` folder:
  - `ConsumerPriceIndex.xlsx` – Consumer Price Index 
  - `CurrentBanksInterestRatesEndOfPeriodMonthly.csv` – Bank Interest (1988 - 2025)
  - `MedianGrossMonthlyIncomeFromEmploymentIncludingEmployerCPFOfFullTimeEmployedResidentsByOccupationsAndSexEndJuneAnnual.csv` - Median Income by Profession (2001 - 2023)
  -  `MedianGrossMonthlyIncomeFromEmploymentofFullTimeEmployedResidentsTotal.csv` - Median Income (2001 - 2023)
  -  `HDBResalePriceIndex1Q2009100Quarterly.csv` - HDB Resale Price Index (1990 - 2025Q1)
  -  `IndexOfIndustrialProduction2015100Monthlybymanufacturingclusters.csv` - Industrial Production Index (1983 - 2019)


## Files in This Repository
- `Cost of living.R` – R script containing all data preparation, analysis, and visualisations
- `Data/` – Folder containing the dataset files used
- `Cost of living.pdf`  – Written report summarizing findings

## Tools 
- **R 4.4.3** – data cleaning, transformation, analysis, and forecasting  
- **tidyr, ggplot2, gganimate, gifski** – R packages used for data wrangling, visualization and animation 

## Key Analyses
- Examined CPI trends across decades and annotated GST revision years
- Compared CPI vs. HDB resale price index to explore potential wealth effects
- Analyzed median income growth (overall and by occupation) against CPI to assess affordability
- Investigated the role of interest rates and borrowing costs on household expenditure
- Explored industrial production trends as an indicator of demand-pull inflation
- Visualized long-term relationships using annotated and animated plots

## How to Use
1. Clone or download the repository.  
2. Open `Cost of living.R` in RStudio to reproduce the analysis and visualisations.  

## License
This project is for educational and portfolio purposes. Data sourced from the Singapore's publicly available databases.
