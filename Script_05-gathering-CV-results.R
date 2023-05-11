
# Required packages -------------------------------------------------------

require(dplyr)
require(readxl)
require(writexl)
require(readr)
require(tidyr)
require(tibble)
require(hydroGOF)
require(yardstick)
require(caret)
require(ranger)

# Data --------------------------------------------------------------------

data_version <- "v1-7-3"

cv_results_path <- "C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/2023-04-25-cross_validation-study/cross-validation-results-v1-7-3/"

list.files(path = cv_results_path, pattern = ".xlsx")

# Cross-validation results ------------------------------------------------

cv_results <- list(
  # Resultados da validação cruzada k-Fold comum:
  stable_rf__kFold_results = read_excel(
    paste0(cv_results_path, "/stable_rf-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Stable areas", n_clusters = 0),
  read_excel(
    paste0(cv_results_path, "/unstable_rf-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Unstable areas", n_clusters = 0),
  
  # Resultados da validação cruzada Spatial
  ## 10 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-10_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Stable areas", n_clusters = 10),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-10_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Unstable areas", n_clusters = 10),
  ## 20 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-20_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Stable areas", n_clusters = 20),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-20_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Unstable areas", n_clusters = 20),
  ## 30 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-30_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Stable areas", n_clusters = 30),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-30_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Unstable areas", n_clusters = 30)
  )

cv_results <- bind_rows(cv_results)

# Unique model results ----------------------------------------------------

cv_results <- list(
  # Resultados da validação cruzada k-Fold comum:
  stable_rf__kFold_results = read_excel(
    paste0(cv_results_path, "/rf-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Both stable and unstable", n_clusters = 0),

  # Resultados da validação cruzada Spatial
  ## 10 clusters
  read_excel(
    paste0(cv_results_path, "/rf-10_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Both stable and unstable", n_clusters = 10),
  ## 20 clusters
  read_excel(
    paste0(cv_results_path, "/rf-20_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Both stable and unstable", n_clusters = 20),
  ## 30 clusters
  read_excel(
    paste0(cv_results_path, "/rf-30_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Spatial k-Fold CV", model_for = "Both stable and unstable", n_clusters = 30)
)

cv_results <- bind_rows(cv_results)

# Writting results --------------------------------------------------------

write_xlsx(cv_results,
           path = "C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/2023-04-25-cross_validation-study/02-cross_validation-results/cross_validation-results_v1-7-3.xlsx",
           col_names = TRUE)

# Biome-wise standard cross-validation statistics -------------------------

input_data_path <- "C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/2023-04-25-cross_validation-study/01-data"
original_data <- read_csv(paste0(input_data_path,
                                 "/matriz_rf_prediction_v1-7-3.csv"))
data_version <- "v1-7-3"

biomes <- list(original_data %>% filter(Amazonia == 1),
               original_data %>% filter(Caatinga == 1),
               original_data %>% filter(Cerrado == 1),
               original_data %>% filter(Mata_Atalntica == 1),
               original_data %>% filter(Pampa == 1),
               original_data %>% filter(Pantanal == 1))

names(biomes) <- c("Amazônia", "Caatinga", "Cerrado", "Mata Atlântica", "Pampa", "Pantanal")
  
remove(input_data_path, original_data)

# Auxiliary functions -----------------------------------------------------

remove_tail <- function(x, sep = ".R", del = 1){
  sapply(strsplit(x, split = sep, fixed = TRUE),
         function(i) paste(head(i, -del), collapse = sep))
}

keep_tail <- function(x, sep = "_", del = 4){
  sapply(strsplit(x, split = sep, fixed = TRUE),
         function(i) paste(tail(i, -del), collapse = sep))
}

# Computing statistical metrics for samples from each biome separa --------
# Computing same goodness-of-fit metrics from models tuning

models_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/tuned-models_v1-7-3/"

model_names <- list.files(path = models_path, pattern = "RF_cv_model")

model_number <- keep_tail(remove_tail(model_names)) %>% as.numeric()

check_statistical_results <- function(
    dataset = biomes[["Amazônia"]],
    biome = "Amazônia",
    map_version = data_version, model_for = "Both stable and unstable",
    cross_validation = "Standard k-Fold CV", n_clusters = 0, model_number = 1) {
  
  gof(
    sim = predict.train(tuned_RF_kfold_cv, dataset),
    obs = dataset %>% pull(estoque),
    do.spearman = TRUE, digits = 5) %>%
    as.data.frame() %>% rownames_to_column() %>% 
    rename(.metric = rowname, .estimate = V1) %>%
    bind_rows(
      tibble(
        .metric = "CCC",
        .estimate = ccc_vec(
          truth = dataset %>% pull(estoque),
          estimate = predict.train(tuned_RF_kfold_cv, dataset)
        )
      )
    ) %>%
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    select(c("ME", "MSE", "MAE", "RMSE", "R2", "NSE", "CCC")) %>% 
    rename(Rsquared = R2) %>% 
    mutate(model = model_number, 
           map_version = map_version,
           n_clusters = n_clusters,
           cross_validation = cross_validation,
           model_for = model_for,
           biome = biome)
  
}

amazonia_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  amazonia_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Amazônia"]], biome = "Amazônia", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

caatinga_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  caatinga_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Caatinga"]], biome = "Caatinga", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

cerrado_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  cerrado_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Cerrado"]], biome = "Cerrado", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

mata_atlantica_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  mata_atlantica_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Mata Atlântica"]], biome = "Mata Atlântica", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

pampa_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  pampa_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Pampa"]], biome = "Pampa", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

pantanal_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))
  
  pantanal_cv_results[[i]] <- check_statistical_results(
    dataset = biomes[["Pantanal"]], biome = "Pantanal", model_number = i)
  
  remove(tuned_RF_kfold_cv)
  gc()
  
}

biome_cv_results <- bind_rows(amazonia_cv_results, caatinga_cv_results,
                              cerrado_cv_results, mata_atlantica_cv_results, 
                              pampa_cv_results, pantanal_cv_results)

remove(amazonia_cv_results, caatinga_cv_results,
       cerrado_cv_results, mata_atlantica_cv_results, 
       pampa_cv_results, pantanal_cv_results)

# Writting in disk
write_xlsx(biome_cv_results,
           path = paste0("C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/2023-04-25-cross_validation-study/02-cross_validation-results/",
                         "each_biome-cross_validation-results_v1-7-3.xlsx"),
           col_names = TRUE)
