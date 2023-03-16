
# Necessary modules -------------------------------------------------------

require(readr)
require(dplyr)
require(writexl)

require(random)

require(caret)
require(randomForest)

# Working with clusters (optional) ----------------------------------------

library(doParallel)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

# Setting output data path ------------------------------------------------

output_models_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/tuned-models/"

output_models_results_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/cross-validation-results/"

# Importing and splitting datasets ----------------------------------------

original_data <- read_csv("01-data/matriz_rf_prediction_v1-5-0.csv")

stable_areas_data <- original_data %>% filter(Area_Estavel == 1)

unstable_areas_data <- original_data %>% filter(Area_Estavel == 0)

train_sets <- list(stable_areas_data, unstable_areas_data)
names(train_sets) <- c("stable_areas_data", "unstable_areas_data")

remove(original_data, stable_areas_data, unstable_areas_data)

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
                  'Ferralsols',
                  'Histosols',
                  'Sandysols',
                  'Humisols',
                  'Thinsols',
                  'Wetsols',
                  'bdod',
                  'cec',
                  'cfvo',
                  'clay',
                  'nitrogen',
                  'phh2o',
                  'sand',
                  'silt',
                  'black_soil_prob',
                  'convergence',
                  'cti',
                  'eastness',
                  'northness',
                  'pcurv',
                  'roughness',
                  'slope',
                  'spi',
                  'elevation',
                  'latitude',
                  'longitude',
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
                  'ndvi',
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
                  'Amazonia',
                  'Caatinga',
                  'Cerrado',
                  'Mata_Atalntica',
                  'Pampa',
                  'Pantanal',
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
                  'Estepe'
                  )

# Statistical metrics -----------------------------------------------------

source("C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/soc_stocks-random_forest-prediction/my_statistical_functions.R")

# Modeling ----------------------------------------------------------------

rf_all_cv_results <- list()
rf_all_best_models <- list()

for (j in seq(along.with = train_sets)) {
  
  training_data <- train_sets[[j]]
  
  rf_kFold_cross_validation <- list()
  rf_kFold_best_models <- list()
  
  for (i in seq(along.with = random_seeds)) {
    
    ti <- Sys.time()
    
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
      data = training_data,
      method = "rf",
      ntree = 790,
      nodesize = 5,
      sampsize = 0.632,
      importance = TRUE,
      trControl = cv_control_object,
      tuneGrid = expand.grid(mtry = 22)
      )
    
    remove(cv_control_object)
    
    ## Getting training metrics
    
    ## Getting cross-validation metrics
    cv <- tuned_RF_kfold_cv[["resample"]] %>% mutate(model = i)
    
    rf_kFold_cross_validation[[i]] <- cv
    
    # Getting best models stats
    hyperparameters <- tuned_RF_kfold_cv[["bestTune"]]
    
    result <- tuned_RF_kfold_cv[["results"]] %>%
      filter(mtry == hyperparameters[1, 1])
    
    rf_kFold_best_models[[i]] <- result %>% mutate(model = i)
    
    ## Cleaning up memory space
    remove(cv, hyperparameters, result)
    gc() 
    
    ## Saving tuned models
    
    if (!dir.exists(output_models_path)){
      
      dir.create(output_models_path)
      
      save(tuned_RF_kfold_cv,
           file = paste0(output_models_path,
                         if(j == 1){"stable_"} else {"unstable_"},
                         "tuned_RF_cv_model_", i, ".RData"))
      
    } else {
      
      save(tuned_RF_kfold_cv,
           file = paste0(output_models_path,
                         if(j == 1){"stable_"} else {"unstable_"},
                         "tuned_RF_cv_model_", i, ".RData"))
    }
    
    tf <- Sys.time()
    print(paste0("Time spent training the model ", i, ":"))
    print(tf - ti)
    
    ## Cleaning up memory space
    remove(tuned_RF_kfold_cv, i, ti, tf)
    gc()
    
  }
  
  rf_all_cv_results[[j]] <- bind_rows(rf_kFold_cross_validation)
  rf_all_best_models[[j]] <- bind_rows(rf_kFold_best_models)
  
  remove(rf_kFold_cross_validation, rf_kFold_best_models)

  if (dir.exists(output_models_results_path)) {
    
    write_xlsx(rf_all_cv_results[[j]],
               paste0(output_models_results_path,
                      if(j == 1){"stable_"} else {"unstable_"},
                      "rf-inner-kFold-results.xlsx"),
               col_names = TRUE)
    
    write_xlsx(rf_all_best_models[[j]],
               paste0(output_models_results_path,
                      if(j == 1){"stable_"} else {"unstable_"},
                      "rf-kFold-results.xlsx"),
               col_names = TRUE)
    
  } else {
    
    dir.create(output_models_results_path)
    
    write_xlsx(rf_all_cv_results[[j]],
               paste0(output_models_results_path,
                      if(j == 1){"stable_"} else {"unstable_"},
                      "rf-inner-kFold-results.xlsx"),
               col_names = TRUE)
    
    write_xlsx(rf_all_best_models[[j]],
               paste0(output_models_results_path,
                      if(j == 1){"stable_"} else {"unstable_"},
                      "rf-kFold-results.xlsx"),
               col_names = TRUE)  
    
  }
  
  remove(training_data)
  
  print("Done.")
  
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
