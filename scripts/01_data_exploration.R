# renv::install("tidyverse")
# renv::install("Factoshiny")
# renv::install("summarytools")
# renv::install("FactoMineR")
# renv::install("factoextra")

# 1. Libraries needed ---------------------------------
library(tidyverse)
library(summarytools)
library(Factoshiny)
library(FactoMineR)
library(factoextra)
source("scripts/functions/PCA_Visualise.R")

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

# write.csv(data_2020_unique_years_wide,"data/data_2020_unique_years_wide.csv")
# write.csv(data_2021_unique_years_wide,"data/data_2021_unique_years_wide.csv")
# write.csv(data_2022_unique_years_wide,"data/data_2022_unique_years_wide.csv")
# write.csv(data_2023_unique_years_wide,"data/data_2023_unique_years_wide.csv")
# write.csv(data_2024_unique_years_wide,"data/data_2024_unique_years_wide.csv")


# ## 3.4. data exploration -------------------------------------
# ### 2020 -------------------------------------------
# view(summarytools::dfSummary(data_2020_unique_years_wide))
# ### 2021 -------------------------------------------
# view(summarytools::dfSummary(data_2021_unique_years_wide))
# ### 2022 -------------------------------------------
# view(summarytools::dfSummary(data_2022_unique_years_wide))
# ### 2023 -------------------------------------------
# view(summarytools::dfSummary(data_2023_unique_years_wide))
# ### 2024 -------------------------------------------
# view(summarytools::dfSummary(data_2024_unique_years_wide))


## 3.5. PCA dimensional reduction for visualisation ---------------------------------------------
# Factoshiny(data_2020_unique_years_wide)

### 2020 -------------------------------------------
# 1. Set the 'Country' column as row names and remove the column

res.PCA<-PCA(column_to_rownames(data_2020_unique_years_wide, var= "Area"),quali.sup=c(1),graph=FALSE)
# loadings <- data.frame(res.PCA$var)
# write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2020.csv")

source("scripts/functions/PCA_Visualise.R")
plot_pca_biplot(res.PCA,
                n_top_individuals = 5,
                n_top_variables = 9,
                scale_factor = 25,
                label_nudge = 1,
                x_range = c(-20, 1.5),
                y_range = c(-5, 35),
                individuals_name = "Countries",
                variables_name = "Indicators",
                plot_title = "Food_Insecurity_Data_2020",
                point_size = 6,
                point_color = '#9370DB',
                arrow_color = '#FF6B9D',
                arrow_width = 3,
                fixedrange = FALSE)

# loadings <- data.frame(res.PCA$var)
# write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2021.csv")
# plot.PCA(res.PCA,choix='var', autoLab = "yes")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "Food_Insecurity_Data_2020")
# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2021_unique_years_wide.png")


# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2020_unique_years_wide.png")

### 2021 --------------------------------------------
res.PCA<-PCA(column_to_rownames(column_to_rownames(data_2021_unique_years_wide, var= "Area"), var= "Area"),quali.sup=c(1),graph=FALSE)
plot_pca_biplot(res.PCA,
                n_top_individuals = 5,
                n_top_variables = 15,
                scale_factor = 17,
                label_nudge = 1,
                x_range = c(-20, 1.5),
                y_range = c(-5, 35),
                individuals_name = "Countries",
                variables_name = "Indicators",
                plot_title = "Food_Insecurity_Data_2021",
                point_size = 6,
                point_color = '#9370DB',
                arrow_color = '#FF6B9D',
                arrow_width = 3,
                fixedrange = FALSE)

# loadings <- data.frame(res.PCA$var)
# write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2021.csv")
# plot.PCA(res.PCA,choix='var', autoLab = "yes")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "Food_Insecurity_Data_2021")
# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2021_unique_years_wide.png")

### 2022 --------------------------------------------
res.PCA<-PCA(column_to_rownames(data_2022_unique_years_wide, var= "Area"),quali.sup=c(1),graph=FALSE)
plot_pca_biplot(res.PCA,
                n_top_individuals = 5,
                n_top_variables = 9,
                scale_factor = 25,
                label_nudge = 1,
                x_range = c(-20, 1.5),
                y_range = c(-5, 35),
                individuals_name = "Countries",
                variables_name = "Indicators",
                plot_title = "Food_Insecurity_Data_2022",
                point_size = 6,
                point_color = '#9370DB',
                arrow_color = '#FF6B9D',
                arrow_width = 3,
                fixedrange = FALSE)

# loadings <- data.frame(res.PCA$var)
# write.csv(loadings[, c("coord.Dim.1", "coord.Dim.2")], "data/pca_results/loadings_2022.csv")
# plot.PCA(res.PCA,choix='var', autoLab = "yes")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "data_2022_unique_years_wide")
# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/Food_Insecurity_Data_2022")

### 2023 ----------------------------------------------
res.PCA<-PCA(column_to_rownames(data_2023_unique_years_wide, var= "Area"),quali.sup=c(1),graph=FALSE)
source("scripts/functions/PCA_Visualise.R")
plot_pca_biplot(res.PCA,
                n_top_individuals = 5,
                n_top_variables = 9,
                scale_factor = 25,
                label_nudge = 1,
                x_range = c(-2, 20),
                y_range = c(-1, 30),
                individuals_name = "Countries",
                variables_name = "Indicators",
                plot_title = "Food_Insecurity_Data_2023",
                point_size = 6,
                point_color = '#9370DB',
                arrow_color = '#FF6B9D',
                arrow_width = 3,
                fixedrange = FALSE)
# plot.PCA(res.PCA,choix='var', autoLab = "yes")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "Food_Insecurity_Data_2023")
# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2023_unique_years_wide.png")

### 2024 ----------------------------------------------
res.PCA<-PCA(column_to_rownames(data_2024_unique_years_wide, var= "Area"),quali.sup=c(1),graph=FALSE)
plot_pca_biplot(res.PCA,
                n_top_individuals = 5,
                n_top_variables = 9,
                scale_factor = 25,
                label_nudge = 1,
                x_range = c(-20, 20),
                y_range = c(-5, 35),
                individuals_name = "Countries",
                variables_name = "Indicators",
                plot_title = "Food_Insecurity_Data_2024",
                point_size = 6,
                point_color = '#9370DB',
                arrow_color = '#FF6B9D',
                arrow_width = 3,
                fixedrange = FALSE)

# plot.PCA(res.PCA,choix='var', autoLab = "yes")
# plot.PCA(res.PCA,invisible=c('ind.sup'),select=NULL,label =c('quali'), autoLab = "yes", title= "Food_Insecurity_Data_2024")
# ggplot2::ggsave("assets/data_exploration_dimension_reduction_pca/score_plot_data_2024_unique_years_wide.png")