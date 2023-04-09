
# Necessary modules -------------------------------------------------------

require(readr)
require(dplyr)
require(writexl)

require(random)

require(caret)
#require(randomForest)
require(ranger)

# Working with clusters (optional) ----------------------------------------

library(doParallel)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

# Setting output data path ------------------------------------------------

output_models_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/tuned-models_v1-7-2/"

output_models_results_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/cross-validation-results_v1-7-2/"

# Importing and splitting datasets ----------------------------------------

original_data <- read_csv("01-data/matriz_rf_prediction_v1-7-2.csv")

data_version <- "v1-7-2"
k = NA

# Creating random seeds vector --------------------------------------------

#randomNumbers(n = 100, min = 100, max = 9999, col = 1) %>% as.vector()

random_seeds <- c(6842, 7045, 1359, 4109, 7947, 9122, 2050, 6646, 8143, 8444,
                  6402, 1721, 6955, 3744, 3144, 3681, 9588, 3807, 4464, 1034,
                  950, 8778, 163, 7249, 3181, 9938, 1564, 685, 8560, 8504, 3092,
                  7722, 6351, 2368, 5969, 6367, 3921, 8767, 9040, 1415, 428,
                  4837, 8263, 1631, 4249, 1411, 4747, 3158, 7846, 430, 6366,
                  6428, 1305, 8981, 3461, 6489, 1580, 8997, 8685, 5944, 991,
                  3630, 4472, 9304, 8411, 4961, 6877, 1325, 1507, 6748, 9408,
                  5790, 8395, 6161, 8942, 8907, 329, 2263, 9397, 3317, 6359,
                  8121, 2416, 1121, 9781, 4723, 5186, 3671, 7715, 4939, 4640,
                  9268, 5138, 6258, 8862, 2386, 6146, 879, 6644, 1821)

# Covariables -------------------------------------------------------------

covariables <- c(
  #Soilgrids WRB probability classes
  'Ferralsols',
  'Histosols',
  'Sandysols',
  'Humisols',
  'Thinsols',
  'Wetsols',
  #Soilgrids Soil Properties
  'bdod',
  'cec',
  'cfvo',
  'clay',
  'nitrogen',
  'phh2o',
  'sand',
  'silt',
  'soc',
  'oxides',
  'clayminerals',
  # Black Soil
  'black_soil_prob',
  # Geomorphometry
  'convergence',
  'cti',
  'eastness',
  'northness',
  'pcurv',
  'roughness',
  'slope',
  'spi',
  'elevation',
  # Lat-Long
  'latitude',
  'longitude',
  # Koppen
  'lv1_Humid_subtropical_zone',
  'lv1_Tropical',
  'lv2_monsoon',
  'lv2_oceanic_climate_without_sry_season',
  'lv2_with_dry_summer',
  'lv2_with_dry_winter',
  'lv2_with_dry_winter_1',
  'lv2_without_dry_season',
  'lv3_with_hot_summer',
  'lv3_with_temperate_summer',
  # Indices
  'ndvi',
  'evi',
  'savi',
  # MapBiomas - Col.7.1
  'formacaoCampestre',
  'formacaoFlorestal',
  'formacaoSavanica',
  'mosaicoAgriculturaPastagem',
  'outrasFormacoesNaoFlorestais',
  'pastagem',
  'lavouras',
  'antropico',
  'natural',
  'Area_Estavel',
  # Biomas 
  'Amazonia',
  'Caatinga',
  'Cerrado',
  'Mata_Atalntica',
  'Pampa',
  'Pantanal',
  # Fitofisionomia
  'Floresta_Ombrofila_Aberta',
  'Floresta_Estacional_Decidual',
  'Floresta_Ombrofila_Densa',
  'Floresta_Estacional_Semidecidual',
  'Campinarana',
  'Floresta_Ombrofila_Mista',
  'Formacao_Pioneira',
  'Savana',
  'Savana_Estepica',
  'Contato_Ecotono_e_Encrave',
  'Floresta_Estacional_Sempre_Verde',
  'Estepe',
  # Quarta Comunicação Nacional
  "cagb",
  "cbgb",
  "cdw",
  "clitter",
  "ctotal")

# Statistical metrics -----------------------------------------------------

source("./my_statistical_functions.R")

# Modeling ----------------------------------------------------------------

rf_kFold_cross_validation <- list()
rf_kFold_best_models <- list()

for (i in seq(along.with = random_seeds)) {
  
  ti <- Sys.time()
  print(paste("Start time:", Sys.time(),"; random seed:", random_seeds[i]))
  
  ## Setting randomization seed
  set.seed(random_seeds[i])
  
  ## Preparing k-Fold control object
  cv_control_object <- trainControl(method = "cv", number = 10,
                                    summaryFunction = my_summary_metrics,
                                    returnResamp = 'all')
  
  ## Training model
  tuned_RF_kfold_cv <- train(
    as.formula(paste("estoque", "~",
                     paste(covariables, collapse = '+'))),
    data = original_data,
    method = "ranger",
    num.trees = 1000,
    replace = TRUE,
    sample.fraction = 0.632,
    importance = "permutation",
    trControl = cv_control_object,
    tuneGrid = expand.grid(mtry = 25, min.node.size = 5,
                           splitrule = "variance")
    )
  
  remove(cv_control_object)
  
  ## Getting training metrics
  
  ## Getting cross-validation metrics
  cv <- tuned_RF_kfold_cv[["resample"]] %>% mutate(model = i) %>% 
    mutate(map_version = data_version, n_clusters = k)
  
  rf_kFold_cross_validation[[i]] <- cv
  
  # Getting best models stats
  hyperparameters <- tuned_RF_kfold_cv[["bestTune"]]
  
  result <- tuned_RF_kfold_cv[["results"]] %>%
    filter(mtry == hyperparameters[1, 1])
  
  rf_kFold_best_models[[i]] <- result %>% mutate(model = i) %>% 
    mutate(map_version = data_version, n_clusters = k)
  
  ## Cleaning up memory space
  remove(cv, hyperparameters, result)
  gc() 
  
  ## Saving tuned models
  
  if (!dir.exists(output_models_path)){
    
    dir.create(output_models_path)
    
    save(tuned_RF_kfold_cv,
         file = paste0(output_models_path,
                       "tuned_RF_cv_model_", i, ".RData"))
    
  } else {
    
    save(tuned_RF_kfold_cv,
         file = paste0(output_models_path,
                       "tuned_RF_cv_model_", i, ".RData"))
  }
  
  tf <- Sys.time()
  print(paste0("Time spent training the model ", i, ":"))
  print(tf - ti)
  
  ## Cleaning up memory space
  remove(tuned_RF_kfold_cv, i, ti, tf)
  gc()
  
}

rf_all_cv_results <- bind_rows(rf_kFold_cross_validation)
rf_all_best_models <- bind_rows(rf_kFold_best_models)

remove(rf_kFold_cross_validation, rf_kFold_best_models)

if (dir.exists(output_models_results_path)) {
  
  write_xlsx(rf_all_cv_results,
             paste0(output_models_results_path,
                    "rf-inner-kFold-results.xlsx"),
             col_names = TRUE)
  
  write_xlsx(rf_all_best_models,
             paste0(output_models_results_path,
                    "rf-kFold-results.xlsx"),
             col_names = TRUE)
  
} else {
  
  dir.create(output_models_results_path)
  
  write_xlsx(rf_all_cv_results,
             paste0(output_models_results_path,
                    "rf-inner-kFold-results.xlsx"),
             col_names = TRUE)
  
  write_xlsx(rf_all_best_models,
             paste0(output_models_results_path,
                    "rf-kFold-results.xlsx"),
             col_names = TRUE)  
  
}

remove(rf_all_cv_results, rf_all_best_models, train_sets, random_seeds,
       covariables, j, output_models_path, output_models_results_path)

# Finalizing --------------------------------------------------------------

if (exists("cl")) {
  print("Closing clusters.")
  stopCluster(cl)
  rm(cl)
} else {
  print("Code is not working with clusters.")
}
