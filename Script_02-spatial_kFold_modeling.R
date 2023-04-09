number_of_clusters <- c(10, 20, 30)

for (k in seq(along.with = number_of_clusters)) {
  
  # Necessary modules -------------------------------------------------------
  
  require(readr)
  require(dplyr)
  require(writexl)
  
  require(random)
  require(geosphere)
  library(jsonlite)
  
  require(CAST)
  require(caret)
  #require(randomForest)
  require(ranger)
  
  # Setting output data path ------------------------------------------------
  
  output_models_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/tuned-models_v1-5-0/"
  
  output_models_results_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/cross-validation-results_v1-5-0/"
  
  # Importing and splitting datasets ----------------------------------------
  
  original_data <- read_csv("01-data/matriz_rf_prediction_v1-5-0.csv")
  
  data_version <- "v1-5-0"
  
  # Grouping samples by geographical position -------------------------------
  
  extract_latitude <- function(geojson) {
    # parse GeoJSON string as JSON object
    geojson_obj <- parse_json(geojson)
    # extract latitude from JSON object
    return(geojson_obj$coordinates[[2]])
  }
  
  extract_longitude <- function(geojson) {
    # parse GeoJSON string as JSON object
    geojson_obj <- parse_json(geojson)
    # extract longitude from JSON object
    return(geojson_obj$coordinates[[1]])
    
  }
  
  original_data <- original_data%>%
    mutate(LAT = apply(original_data %>% select(.geo), 1, extract_latitude)) %>%
    mutate(LON = apply(original_data %>% select(.geo), 1, extract_longitude))
  
  # compute distance matrix 
  dist_mat <- distm(original_data[, c("LON", "LAT")])
  
  # apply k-means clustering to the distance matrix
  #randomNumbers(n = 1, min = 100, max = 9999, col = 1) %>% as.vector()
  set.seed(3019)
  k <- number_of_clusters[k] # number of clusters
  kmeans_clusters <- kmeans(dist_mat, k)
  
  # add cluster ID to original data frame
  original_data$cluster <- kmeans_clusters$cluster
  
  # Visualizing clusters ----------------------------------------------------
  
  #ggplot2::ggplot(original_data, aes(LON, LAT))+
  #  geom_point(size = 3, alpha = 0.1)
  
  #clustered_samples <- ggplot2::ggplot(original_data,
  #                aes(LON, LAT, color = as.factor(cluster),
  #                    shape = as.factor(cluster)))+
  #  geom_point(size = 3, alpha = 0.3)+
  #  scale_color_manual(aesthetics = "color",
  #                     values = c("#ffd700", "#ffb14e", "#fa8775",
  #                                "#ea5f94", "#ef0000", "#9d02d7",
  #                                "#0000ff", "#04590d", "#00e8ff", "#000000"))+
  #  scale_shape_manual(values = c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12))+
  #  theme(legend.position = "none")
  
  #ggsave("clustered_points.png", plot = clustered_samples,
  #       width = 210, height = 250, units = "mm", dpi = 300)               
  
  remove(dist_mat, kmeans_clusters, clustered_samples,
         extract_latitude, extract_longitude)
  
  # Working with clusters (optional) ----------------------------------------
  
  library(doParallel)
  cl <- makeCluster(detectCores()-2)
  registerDoParallel(cl)
  
  # Splitting dataset -------------------------------------------------------
  
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
  
  source("./my_statistical_functions.R")
  
  # Modeling ----------------------------------------------------------------
  
  rf_all_cv_results <- list()
  rf_all_best_models <- list()
  
  for (j in seq(along.with = train_sets)) {
    
    training_data <- train_sets[[j]]
    
    rf_kFold_cross_validation <- list()
    rf_kFold_best_models <- list()
    
    for (i in seq(along.with = random_seeds)) {
      
      ti <- Sys.time()
      print(paste("Start time:", Sys.time(),"; random seed:", random_seeds[i]))
      
      ## Setting randomization seed
      set.seed(random_seeds[i])
      
      ## Preparing for Space Folds
      LCOCV_obj <- CreateSpacetimeFolds(
        training_data,
        spacevar = "cluster", class = "cluster",
        k = 10)
      
      LCOCV_trCtrl <- trainControl(
        method = "cv",
        number = 10,
        savePredictions = TRUE,
        index = LCOCV_obj$index,
        indexOut = LCOCV_obj$indexOut,
        summaryFunction = my_summary_metrics,
        returnResamp = 'all'
      )
      
      ## Training model
      tuned_RF_spatial_kfold_cv <- train(
        as.formula(paste("estoque", "~",
                         paste(covariables, collapse = '+'))),
        data = training_data,
        method = "ranger",
        num.trees = 790,
        replace = TRUE,
        sample.fraction = 0.632,
        importance = "permutation",
        trControl = LCOCV_trCtrl,
        tuneGrid = expand.grid(mtry = 22, min.node.size = 5,
                               splitrule = "variance")
      )
      
      remove(LCOCV_obj, LCOCV_trCtrl)
  
      ## Getting training metrics
      
      ## Getting cross-validation metrics
      cv <- tuned_RF_spatial_kfold_cv[["resample"]] %>%
        mutate(model = i) %>% mutate(map_version = data_version, n_clusters = k)
      
      rf_kFold_cross_validation[[i]] <- cv
      
      # Getting best models stats
      hyperparameters <- tuned_RF_spatial_kfold_cv[["bestTune"]]
      
      result <- tuned_RF_spatial_kfold_cv[["results"]] %>%
        filter(mtry == hyperparameters[1, 1])
      
      rf_kFold_best_models[[i]] <- result %>% mutate(model = i) %>%
        mutate(map_version = data_version, n_clusters = k)
      
      ## Cleaning up memory space
      remove(cv, hyperparameters, result)
      gc() 
      
      ## Saving tuned models
      
      if (!dir.exists(output_models_path)){
        
        dir.create(output_models_path)
        
        save(tuned_RF_spatial_kfold_cv,
             file = paste0(output_models_path,
                           if(j == 1){"stable_"} else {"unstable_"},
                           "tuned_RF-", k, "_clusters-spatial_cv_model_", i, ".RData"))
        
      } else {
        
        save(tuned_RF_spatial_kfold_cv,
             file = paste0(output_models_path,
                           if(j == 1){"stable_"} else {"unstable_"},
                           "tuned_RF-", k, "_clusters-spatial_cv_model_", i, ".RData"))
      }
      
      tf <- Sys.time()
      print(paste0("Time spent training the model ", i, ":"))
      print(tf - ti)
      
      ## Cleaning up memory space
      remove(tuned_RF_spatial_kfold_cv, i, ti, tf)
      gc()
      
    }
    
    rf_all_cv_results[[j]] <- bind_rows(rf_kFold_cross_validation)
    rf_all_best_models[[j]] <- bind_rows(rf_kFold_best_models)
    
    remove(rf_kFold_cross_validation, rf_kFold_best_models)
    
    if (dir.exists(output_models_results_path)) {
      
      write_xlsx(rf_all_cv_results[[j]],
                 paste0(output_models_results_path,
                        if(j == 1){"stable-"} else {"unstable-"},
                        paste0("rf-inner-", k, "_clusters-spatial-kFold-results.xlsx")),
                 col_names = TRUE)
      
      write_xlsx(rf_all_best_models[[j]],
                 paste0(output_models_results_path,
                        if(j == 1){"stable-"} else {"unstable-"},
                        paste0("rf-", k, "_clusters-spatial-kFold-results.xlsx")),
                 col_names = TRUE)
      
    } else {
      
      dir.create(output_models_results_path)
      
      write_xlsx(rf_all_cv_results[[j]],
                 paste0(output_models_results_path,
                        if(j == 1){"stable-"} else {"unstable-"},
                        paste0("rf-inner-", k, "_clusters-spatial-kFold-results.xlsx")),
                 col_names = TRUE)
      
      write_xlsx(rf_all_best_models[[j]],
                 paste0(output_models_results_path,
                        if(j == 1){"stable-"} else {"unstable-"},
                        paste0("rf-", k, "_clusters-spatial-kFold-results.xlsx")),
                 col_names = TRUE)  
      
    }
    
    remove(training_data)
    
    print("Done.")
    
  }
  
  remove(rf_all_cv_results, rf_all_best_models, train_sets, random_seeds,
         covariables, j, output_models_path, output_models_results_path,
         k, data_version)
  
  # Finalizing --------------------------------------------------------------
  
  if (exists("cl")) {
    print("Closing clusters.")
    stopCluster(cl)
    rm(cl)
  } else {
    print("Code is not working with clusters.")
  }
  
}
