# ff-food

# Project Information

## Global Food Security Insights: Focus on children under 5 and women of reproductive age

### Overview: 
Dataset used comes from the United Nation's Food and Agriculture Organization and is titled the "Suite of Food Security Indicators". This dataset looks at Food Security Indicators broken into subcategories of (availability, access, stability, utilization and additonal statistics) from 2000-2024 globally. This project is part of a two day hackathon and focusses on the indicators related to children under five and women of reprodcutive age with the aim to uncover actionable insights. 

### Objectives:
* identify countries which are outliers in the dataset based on the indicators provided
* identify which indicators have the strongest correlation to the outlier country
* explore external sources for explanations related to trends found
* use a predictive model to determine how long it would take for the called out indicator and country to reach global averages

### Data Sources
* FAO Suite of Food Security Indicators (2023)

### Tools and Technologies
* Excel: Global Map visualization
* R: data cleaning, PCR plots, time series analysis
* Git/Github: Version control and collaboration

### Key Features
* Global map of stunting, wasting and anemia for years 2020-2023
* PCR plots
* time series analysis for stunting, wasting and anemia

### Insights: 

PCR analysis found that India and China were outliers in all years explored in the analysis (2020-2023) - USA was seen in 2021 and 2022. 

Indicators for India were: percentage of women of reporoductive age affected by anemia, percentage of newborns with low birth weight, percentage of children under five affected by wasting, and percentage of children under five affected by stunting.

Indicators for China were: percentage of children under five affected by wasting, percentage of children under five affected who are overweight and percentage of obese adults.


* note: percentage of population was used to account for high populations in China and India.

### Getting Started:
1. clone the repo: git clone https://github.com/BioDataSage/ff-food.git
2. Open R Studio
3. Follow the instructions in the notebook to reproduce the analysis

### Team and Credits:

### License: 