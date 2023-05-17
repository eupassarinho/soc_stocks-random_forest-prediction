
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

original_data <- original_data %>% mutate(
  biome = ifelse(Amazonia == 1, "Amazônia",
                 ifelse(Caatinga == 1, "Caatinga",
                        ifelse(Mata_Atalntica == 1, "Mata Atlântica",
                               ifelse(Cerrado == 1, "Cerrado",
                                      ifelse(Pampa == 1, "Pampa",
                                             ifelse(Pantanal == 1, "Pantanal", "NA"
                                                    )))))))

remove(input_data_path)

# Auxiliary functions -----------------------------------------------------

remove_tail <- function(x, sep = ".R", del = 1){
  sapply(strsplit(x, split = sep, fixed = TRUE),
         function(i) paste(head(i, -del), collapse = sep))
}

keep_tail <- function(x, sep = "_", del = 4){
  sapply(strsplit(x, split = sep, fixed = TRUE),
         function(i) paste(tail(i, -del), collapse = sep))
}

regression_eval <- function(pred, obs){
  
  # mean error
  ME <- round(mean(pred - obs, na.rm = TRUE), digits = 4)
  
  # mean square error
  MSE <-   round(mean((pred - obs)^2, na.rm = TRUE), digits = 4)
  
  # mean absolute error
  MAE <-   round(mean(abs(pred - obs), na.rm = TRUE), digits = 4)
  
  # root mean square error
  RMSE <-   round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 4)
  
  # Pearson's correlation squared
  r2 <-  round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 4)
  
  # Nash–Sutcliffe model efficiency coefficient
  NSE <- round(hydroGOF::NSE(sim = pred, obs = obs), digits = 4)
  
  # Lin's concordance correlation coefficient
  CCC <- round(yardstick::ccc_vec(truth = obs, estimate = pred), digits = 4)
  
  out <- c(ME, MAE, MSE, RMSE, NSE, r2, CCC)
  names(out) <- c("ME", "MAE", "MSE", "RMSE", "NSE", "Rsquared", "CCC")
  
  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out
  
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
  
  regression_eval(pred = dataset %>% pull(pred_estoque),
                  obs = dataset %>% pull(estoque)) %>%
    as.data.frame() %>% t() %>% as_tibble() %>% 
    mutate(model = model_number, 
           map_version = map_version,
           n_clusters = n_clusters,
           cross_validation = cross_validation,
           model_for = model_for,
           biome = biome)
  
}

amazonia_cv_results <- list()
caatinga_cv_results <- list()
cerrado_cv_results <- list()
mata_atlantica_cv_results <- list()
pampa_cv_results <- list()
pantanal_cv_results <- list()

for (i in 1:100) {
  
  load(paste0(models_path,
              model_names[grep(paste0("_", model_number[i], ".RData"), model_names)]))

  original_data$pred_estoque <- tuned_RF_kfold_cv[["finalModel"]][["predictions"]]
  
  amazonia_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Amazônia"), biome = "Amazônia",
    model_number = i)
  
  caatinga_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Caatinga"), biome = "Caatinga",
    model_number = i)
  
  cerrado_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Cerrado"), biome = "Cerrado",
    model_number = i)
  
  mata_atlantica_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Mata Atlântica"), biome = "Mata Atlântica",
    model_number = i)
  
  pampa_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Pampa"), biome = "Pampa",
    model_number = i)
  
  pantanal_cv_results[[i]] <- check_statistical_results(
    dataset = original_data %>% filter(biome == "Pantanal"), biome = "Pantanal",
    model_number = i)
  
  original_data <- original_data %>% select(-pred_estoque)
  
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
