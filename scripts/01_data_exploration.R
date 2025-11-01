# renv::install("tidyverse")
# renv::install("Factoshiny")
# renv::install("summarytools")

# 1. Libraries needed ---------------------------------
library(tidyverse)
library(summarytools)
library(Factoshiny)

# 2. Importing Data ----------------------------------------------------
data_2020_2024 <- read.csv("data/FAOSTAT_data_en_11-1-2025.csv") %>% 
  mutate(Value = Value %>%
           str_replace_all("<", "") %>%   # remove "<"
           as.numeric()) 
  
# 3. Data Exploration --------------------------------------------------
## 3.1. unique year set -----------------------------------------------------
unique(data_2020_2024$Year)

## 3.2. selecting only individual year from 2020-2024, not bracketed ones ---------------------------
data_2020_unique_years <- data_2020_2024 %>% 
  filter(Year %in% c("2020")) #%>% 
# write.csv(data_2020_unique_years, "data/data_2020_unique_years.csv")

data_2021_unique_years <- data_2020_2024 %>% 
  filter(Year %in% c("2021"))

data_2022_unique_years <- data_2020_2024 %>% 
  filter(Year %in% c("2022"))

data_2023_unique_years <- data_2020_2024 %>% 
  filter(Year %in% c("2023"))

data_2024_unique_years <- data_2020_2024 %>% 
  filter(Year %in% c("2024"))

## 3.3. widening the data for plotting ----------------------------------------------------
data_2020_unique_years_wide <- data_2020_unique_years %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = "Item",
              values_from = "Value")

data_2021_unique_years_wide <- data_2021_unique_years %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = "Item",
              values_from = "Value")

data_2022_unique_years_wide <- data_2022_unique_years %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = "Item",
              values_from = "Value")

data_2023_unique_years_wide <- data_2023_unique_years %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = "Item",
              values_from = "Value")

data_2024_unique_years_wide <- data_2024_unique_years %>% 
  select(Area, Item, Value) %>% 
  pivot_wider(names_from = "Item",
              values_from = "Value")

write.csv(data_2020_unique_years_wide,"data/data_2020_unique_years_wide.csv")
write.csv(data_2021_unique_years_wide,"data/data_2021_unique_years_wide.csv")
write.csv(data_2022_unique_years_wide,"data/data_2022_unique_years_wide.csv")
write.csv(data_2023_unique_years_wide,"data/data_2023_unique_years_wide.csv")
write.csv(data_2024_unique_years_wide,"data/data_2024_unique_years_wide.csv")


## 3.4. data exploration -------------------------------------
### 2020 -------------------------------------------
view(summarytools::dfSummary(data_2020_unique_years_wide))
### 2021 -------------------------------------------
view(summarytools::dfSummary(data_2021_unique_years_wide))
### 2022 -------------------------------------------
view(summarytools::dfSummary(data_2022_unique_years_wide))
### 2023 -------------------------------------------
view(summarytools::dfSummary(data_2023_unique_years_wide))
### 2024 -------------------------------------------
view(summarytools::dfSummary(data_2024_unique_years_wide))


## 3.5. PCA dimensional reduction for visualisation ---------------------------------------------
# Factoshiny(data_2020_unique_years_wide)

### 2020 -------------------------------------------
res.PCA<-PCA(data_2020_unique_years_wide,quali.sup=c(1),graph=FALSE)
loadings <- data.frame(res.PCA$var)
write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2020.csv")
plot.PCA(res.PCA,choix='var', autoLab = "yes" )
plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title = "data_2020_unique_years_wide")
ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2020_unique_years_wide.png")

### 2021 --------------------------------------------
res.PCA<-PCA(data_2021_unique_years_wide,quali.sup=c(1),graph=FALSE)
loadings <- data.frame(res.PCA$var)
write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2021.csv")
plot.PCA(res.PCA,choix='var', autoLab = "yes")
plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "data_2021_unique_years_wide")
ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2021_unique_years_wide.png")

### 2022 --------------------------------------------
res.PCA<-PCA(data_2022_unique_years_wide,quali.sup=c(1),graph=FALSE)
loadings <- data.frame(res.PCA$var)
write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2022.csv")
plot.PCA(res.PCA,choix='var', autoLab = "yes")
plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "data_2022_unique_years_wide")
ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2022_unique_years_wide.png")

### 2023 ----------------------------------------------
res.PCA<-PCA(data_2023_unique_years_wide,quali.sup=c(1),graph=FALSE)
plot.PCA(res.PCA,choix='var', autoLab = "yes")
plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "data_2023_unique_years_wide")
ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2023_unique_years_wide.png")

### 2024 ----------------------------------------------
res.PCA<-PCA(data_2024_unique_years_wide,quali.sup=c(1),graph=FALSE)
plot.PCA(res.PCA,choix='var', autoLab = "yes")
plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "data_2024_unique_years_wide")
ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2024_unique_years_wide.png")