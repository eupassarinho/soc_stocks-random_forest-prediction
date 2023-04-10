
require(dplyr)
require(readxl)
require(writexl)

data_version <- "v1-7-2"

cv_results_path <- "C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/soc_stocks-random_forest-prediction/cross-validation-results_v1-7-2"

list.files(path = cv_results_path, pattern = ".xlsx")

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
           path = "./02-cross_validation-results/cross_validation-results_v1-7-2.xlsx", col_names = TRUE)
