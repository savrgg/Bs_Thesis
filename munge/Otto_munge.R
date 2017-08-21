# 1) Load libraries ----------------------------------------
set.seed(12345)
library(plyr) # Tools for split, applying and combine data
library(dplyr) # package of grammar of data manipulation
library(tidyr) # package to efficiently "tidy" data
library(FactoMineR) # Package for multivariate analysis
library(ggplot2) # Graphing package "Grammar of Graphics"
library(gridExtra) # Package to work with grid graphics
library(data.table)
library(ProjectTemplate)
# 1) Generate train and test from loan data ---------
base.otto <- read.csv("data/Otto/train_otto.csv") %>% 
  dplyr::rename(grupo = target)

base.reduc <- base.otto %>% 
  group_by(grupo) %>% 
  sample_n(size = 1900, replace = F)

train.id <- base.reduc %>% 
  group_by(grupo) %>% 
  sample_n(size = 950, replace = F) %>% 
  data.frame()

train <- base.reduc %>% 
  filter(id %in% train.id$id)

test <- base.reduc %>% 
  filter(!(id %in% train.id$id))

# 3) Train and test sets -----------------------------------
nrow(train)/(nrow(train) + nrow(test))

train %>% group_by(grupo) %>% tally
test %>% group_by(grupo) %>% tally

otto.test <- test %>%  data.frame()
otto.train <- train %>% data.frame()
cache("otto.train")
cache("otto.test")

