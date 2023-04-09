
require(dplyr)
require(readxl)
require(writexl)

cv_results_path <- "C:/Users/CODEVASF/Desktop/soc_stocks-random_forest-prediction-main/cross-validation-results"

list.files(path = cv_results_path, pattern = ".xlsx")

inner_cv_results <- list(
  # Resultados da validação cruzada k-Fold comum:
  stable_rf_inner_kFold_results = read_excel(
    paste0(cv_results_path, "/stable_rf-inner-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Stable areas", n_clusters = 0),
  read_excel(
    paste0(cv_results_path, "/unstable_rf-inner-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Unstable areas", n_clusters = 0),
  
  # Resultados da validação cruzada Leave-Cluster-Out
  ## 10 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-inner-10_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Stable areas", n_clusters = 10),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-inner-10_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Unstable areas", n_clusters = 10),
  ## 30 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-inner-30_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Stable areas", n_clusters = 30),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-inner-30_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Unstable areas", n_clusters = 30),
  ## 50 clusters
  read_excel(
    paste0(cv_results_path, "/stable-rf-inner-50_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Stable areas", n_clusters = 50),
  read_excel(
    paste0(cv_results_path, "/unstable-rf-inner-50_clusters-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Unstable areas", n_clusters = 50)
  )

inner_cv_results <- bind_rows(inner_cv_results)

write_xlsx(inner_cv_results,
           path = "./02-cross_validation-results/cross_validation-results_v1-6-0.xlsx", col_names = TRUE)
