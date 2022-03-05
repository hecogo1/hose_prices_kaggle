#------------
# Author: Hector Cozar
# Data: 2022_02_26
# Purpose: Introduce the two logic variables with 5% NAs.
# Input: Train, test dataset.
# Output: Submissions.
#------------

#---- Libraries
library(dplyr)       # Data Manipulation.
library(data.table)  # Fast Data Manipulation.
library(inspectdf)   # EDAs automatic
library(ranger)      # fast randomForest
library(tictoc)      # Measure execution time
library(magrittr)    # Piping mode
library(ggplot2)     # Very nice charts 
library(forcats)     # Manage factor variables
library(tibble)      # Compact dataframe
library(janitor)
library(h2o)
# library(tidytable)
# library(poorman)

set.seed(1234)

#---- Data Loading.
traintarget <- as.data.frame(fread("./data/train.csv", nThread = 3))

test  <- fread("./data/test.csv", nThread = 3, data.table = FALSE)
names(traintarget) 
names(test)
#change names to avoid erros with ranger
traintarget <- clean_names(traintarget)
test <- clean_names(test)



# #3.---- Confirm "target" distribution
# datTrainOrilab %>%
#   count(status_group) %>%
#   arrange(-n)

#              status_group     n
# 1              functional 32259
# 2          non functional 22824
# 3 functional needs repair  4317

#----- EDA (Exploratory Data Analysis) !!!!!!!

# # Horizontal bar plot for categorical column composition
# x <- inspect_cat(datTrainOri) 
# show_plot(x)
# 
# # Correlation betwee numeric columns + confidence intervals
# x <- inspect_cor(datTrainOri)
# show_plot(x)
# 
# # Bar plot of most frequent category for each categorical column
# x <- inspect_imb(datTrainOri)
# show_plot(x)
# 
# # Bar plot showing memory usage for each column
# x <- inspect_mem(datTrainOri)
# show_plot(x)
# 
# # Occurence of NAs in each column ranked in descending order
# x <- inspect_na(datTrainOri)
# show_plot(x)
# 
# # Histograms for numeric columns
# x <- inspect_num(datTrainOri)
# show_plot(x)
# 
# # Barplot of column types
# x <- inspect_types(datTrainOri)
# show_plot(x)

# Results:
# Categorical Variables:
# extraction_type_class and group are very similar (not equal)
# payment and payment_type, equal??.
# quantity and quantity_group, equal??
# recorded_by is constant.
# source and source_type, very similar.
# Correlation:
# No correlation
# NAs:
# public_meeting, permit ~ 5%.
# Numeric:
#  amount_tsh: has a few outliers.
#  construction_year: 30% are 0s.
#  district_code: No comments.
#  gps_height: 
#      - negative values??.
#      - around 40% are 0s.
#  id: No comments.
#  latitude: suspicious 0s. (5%)
#  longitude: anomalous 0s  (5%)    
#  num_private: a few outliers
#  population: a few outliers
#  region_code: More pumps <25.
#     

# #--- Dplyr - another way
# traintarget <- train  %>%
#   left_join(test)

#--- Removing target from price
train <- traintarget %>% 
  select(-sale_price)

#----- We use just NUMERICAL columns
#-- Now what I have is: train + target. Ready to model...
trainnum <- 
  traintarget %>%
  select(where(is.numeric), sale_price) %>%
  select_if(~ !any(is.na(.)))

#----- Add new variables....
# Veamos cardinalidad de cada variable categorica.
# datTrainOri %>%
#   select(where(is.character)) %>%
#   count()
#-- Function to get number of levels
myfun <- function(x) {
  num_levels <- length(unique(x))
  return(num_levels)
}
#-- Just character columns
trainchar <- traintarget %>%
  select(where(is.character)) %>%
  select_if(~ !any(is.na(.)))

#-- Count levels in character columns.
levels_val <- as.data.frame(apply(trainchar, 2, myfun))
names(levels_val)[1] <- "num_levels"
levels_val$vars <- rownames(levels_val)
rownames(levels_val) <- NULL
levels_val %<>% arrange(num_levels)

#-- We will use all levels
lev_good <- levels_val %>%
  filter( num_levels <= 1000) %>%
  #-- This filter is to remove "recorded_by"
  filter( num_levels > 1) %>%
  #-- This selects just var column
  select(vars) %>%
  #-- Convert to vector
  pull(vars)

# Dataframe with just the columns I want.
# datTrainOrigod <- datTrainOri[ , lev_god ]

#-- With dplyr.
trainchargood <- traintarget %>%
  # select(!!sym(lev_god))
  select(lev_good)

#-- Now put together numeric + the dataframe with the character columns.
#-- And locate target (status_group) at the end of the dataframe.
trainnumchargood <- cbind(trainnum, trainchargood) %>%
  relocate( sale_price, .after = neighborhood)


#----- To include Logic variables : permit / public_meeting
#1-- Pongo junto train y test (original) with out the target
datAll <- rbind(train, test)


# Occurence of NAs in each column ranked in descending order
x <- inspect_na(datAll)

x %>% 
  filter(pcnt > 0) %>%
  show_plot()

# #-- Selecciono las 80 variables que tengo hasta ahora
# col_gd <- names(train)
# col_gd <-  col_gd[ col_gd != "status_group"]
# datGd <- datAll[, col_gd]
# datGd <- cbind(datGd, datLogic)
# 
# 
# #2-- Selecciono las logicas
# datLogic <- datAll %>%
#   select(where(is.logical)) %>%
#   mutate(across(where(is.logical), as.numeric))

#--- Apply library/algorithm to impute NAs on the whole dataset.
library(missRanger)
datGdImputed <- missRanger(datAll, pmm.k = 3, num.trees = 100)

#checking there is no NA
sum(is.na(datGdImputed))

#-- Split train and test once imputed
datGdImpTrain <- datGdImputed[1:nrow(train), ]
datGdImpTest  <- datGdImputed[(nrow(train) + 1):nrow(datGdImputed),]

# #-- Manera alternativa
# datGdImputed %<>%
#   mutate(miindice = 1:nrow(datGdImputed))

#-- Vuelvo a añadir el target al set de training
datGdImpTrain %<>%
  mutate(sale_price = traintarget$sale_price)


# #----- Submission 
# #-- Prediction
# pred_val <- predict( mymodel, data = datGdImpTest)$predictions
# head(pred_val)
# 
# #-- Prepare submission
# sub_df <- data.frame(
#   Id = test$id,
#   SalePrice = pred_val
#   )
# 
# #-- Save submission
# fwrite(sub_df, 
#        paste0("./submissions/file1_ranger_vars_", nrow(varImp),
#               "_acc_", round(accu_val,4), ".csv"), nThread = 3
#        )
# 
# head(sub_df)


### H2O  Categorical variables needs to be factors

datGdImpTrain %<>% mutate_if(is.character, as.factor)
datGdImpTest %<>% mutate_if(is.character, as.factor)



## H2o Variables for csv name

max_models <- 100


## ---------------H2o----------------------

h2o.init(max_mem_size = "8g", nthreads = 8)

#Loading data as a h2o object
train_hex <- as.h2o(datGdImpTrain)
test_hex <- as.h2o(datGdImpTest)


# Split for the first time, train and test just to show we can validate our test. But Ranger was doing this by himself
splits <- h2o.splitFrame(train_hex, ratios = 0.8, seed = 1234)

train <- splits[[1]]
# test to validation
valid <- splits[[2]]

response <- "sale_price"

##  ## Random forest on H2o
##  modh2Orf <- h2o.randomForest(
##                                y = response,
##                                training_frame = train,
##                                nfolds = 3,
##                                seed = 1234
##  )



## Auto ML - Esto es la leche
automl <- h2o.automl(
                      y = response,
                      training_frame = train,
                      project_name = 'house_prices_kaggle',
                      max_models = max_models,
                      seed = 1234,
                      #exclude_algos = c("DeepLearning"),
                      keep_cross_validation_predictions = TRUE # otherwise it returns an error with 100 models
                      )


# Model name
H2Omodel_name <- as.character(automl@leaderboard[1, 1])

# Cogemos el mejor modelo del auto ML
myH2Omodel <- h2o.getModel(automl@leaderboard[1, 1])


# Performance del modelo
mod_perf <- h2o.performance(myH2Omodel, valid)

# Model error - Quita no funciona por no ser de clasificación??
error <- h2o.mean_per_class_error(mod_perf)
acc_val <- 1 - error
acc_val

# Importancia de las variables h2o
var_imp <- h2o.varimp(myH2Omodel)
# Grafico variables importantes
h2o.varimp_plot(myH2Omodel)

# Predict dentro de H2O
pred_val <- h2o.predict(myH2Omodel, test_hex)

# Traemos las predicciones de H2O a R
pred_df <- as.data.frame(pred_val)

# Cerramos H2O
h2o.shutdown()


## Submision
## Creamos dataframe con respuestas finales
sub_df <- data.frame(
                      Id = datGdImpTest$id,
                      SalePrice = pred_df$predict
                      
)



#-- Save submission
fwrite(sub_df, 
       paste0("./submissions/file2.2_H2O_AUTOML_n_models_", max_models, ".csv"),
       nThread = 3
)

# h2o.automl(x = mytrain,
#          y = mytrain$sale_price,
#          stopping_metric = "RMSLE",
#          training_frame = prostate,
#          project_name = 'house_prices_kaggle',
#          max_models = 15)
# 



# #----- Submission Sin H2O
# #-- Prediction
# pred_val <- predict(model1, newdata = datGdImpTest, type="response")
# 
# pred_val
# 
# #-- Prepare submission
# sub_df <- data.frame(
#   Id = test$id,
#   SalePrice = pred_val
# )
# 
# #-- Save submission
# fwrite(sub_df, 
#        paste0("./submissions/file2.1_glm_vars_", nrow(varImp),
#               "_acc_", round(accu_val,4), ".csv"), nThread = 3
# )
# 

## END OF FILE
## Random forest 1 submission score on platform: 0.15015 error
## GLM 2 submission score on platform: 0.14615 error <- better
## H2O random forest: 0.14541 <- better
## H2O automl setting up the option rsml GBM model<- 0.13589
## H2O automl by default StackedEnsemble_BestOfFamily_4<- 0.13014