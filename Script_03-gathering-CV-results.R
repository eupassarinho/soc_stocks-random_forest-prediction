
require(dplyr)
require(readxl)
require(writexl)

cv_results_path <- "C:/Users/erlis/OneDrive/Área de Trabalho/cross-validation-results"

list.files(path = cv_results_path, pattern = ".xlsx")

inner_cv_results <- list(
  stable_rf_inner_kFold_results = read_excel(
    paste0(cv_results_path, "/stable_rf-inner-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Stable areas"),
  unstable_rf_inner_kFold_results = read_excel(
    paste0(cv_results_path, "/unstable_rf-inner-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Standard k-Fold CV", model_for = "Unstable areas"),
  stable_rf_inner_spatial_kFold_results = read_excel(
    paste0(cv_results_path, "/stable_rf-inner-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Stable areas"),
  unstable_rf_inner_spatial_kFold_results = read_excel(
    paste0(cv_results_path, "/unstable_rf-inner-spatial-kFold-results.xlsx")) %>% 
    mutate(cross_validation = "Leave-Cluster-Out k-Fold CV", model_for = "Unstable areas")
  )

inner_cv_results <- bind_rows(inner_cv_results)

write_xlsx(inner_cv_results,
           path = "./02-cross_validation-results/cross_validation-results.xlsx", col_names = TRUE)
