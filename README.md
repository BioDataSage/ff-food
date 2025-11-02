# ff-food

# Project Information

## Global Food Security Insights: Focus on children under five and women of reproductive age

### Overview: 
Dataset used comes from the United Nation's Food and Agriculture Organization and is titled the "Suite of Food Security Indicators". This dataset presents Food Security Indicators and corresponding values for all countries from 2000-2024. This project is part of a two day hackathon and focusses on the indicators related to children under five and women of reprodcutive age with the aim to uncover actionable insights. 

### Objectives:
* Identify countries which are extreme in the dataset based on the indicators provided
* Identify which indicators have the strongest correlation to the extreme country
* Explore external sources for explanations related to trends found
* Use a predictive model to determine how long it would take for the called out indicator and country to reach global averages

### Data Sources
* UN FAO Suite of Food Security Indicators (2023) <https://www.fao.org/faostat/en/#data/FS>

### Tools and Technologies
* Excel: Global Map visualization
* R: data cleaning, PCA plots, time series analysis (ARIMA)
* Git/Github: Version control and collaboration
* renv: version control of R packages

### Key Features
* Global map of stunting, wasting and anemia for years 2020-2023
* PCA plots
* Time series analysis for stunting, wasting and anemia

### Insights: 

PCA analysis found that India and China were outliers in all years explored in the analysis (2020-2023) - additionally, USA was seen in 2021 and 2022. 

Indicators for India were: percentage of women of reproductive age affected by anemia, percentage of newborns with low birth weight, percentage of children under five affected by wasting, and percentage of children under five affected by stunting.

Indicators for China were: percentage of children under five who are overweight and percentage of obese adults.

* note: percentage of population was used to account for high populations in China and India.

### Getting Started:
1. Clone the repo: git clone https://github.com/BioDataSage/ff-food.git
2. Open R Studio
3. Do renv::restore()
4. Follow the instructions in the notebook to reproduce the analysis

### Team and Credits:
Alraian “Ryan” Abdelrahim: Conceptualization, documentation, dataset visualization and external research
Camille James: Conceptualization, time series analysis and visualization using R
Sudipta Hazra: Conceptualization, data cleaning, PCA and visualization using R
Alexandra Galvan: Conceptualization, presentation and themes
Victoria Nguyen: Conceptualization and storytelling

### License: MIT LICENSE