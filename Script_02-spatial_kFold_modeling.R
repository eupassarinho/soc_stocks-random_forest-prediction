
# Necessary modules -------------------------------------------------------

require(readr)
require(dplyr)
require(writexl)

require(random)
require(geosphere)
library(jsonlite)

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

extract_latitude <- function(geojson) {
  # parse GeoJSON string as JSON object
  geojson_obj <- parse_json(geojson)
  # extract coordinates from JSON object
  return(geojson_obj$coordinates[[2]])
  
}

extract_longitude <- function(geojson) {
  # parse GeoJSON string as JSON object
  geojson_obj <- parse_json(geojson)
  # extract coordinates from JSON object
  return(geojson_obj$coordinates[[1]])
  
}

original_data <- original_data%>%
  mutate(LAT = apply(original_data %>% select(.geo), 1, extract_latitude)) %>%
  mutate(LON = apply(original_data %>% select(.geo), 1, extract_longitude))


# compute distance matrix using distGeo()
dist_matrix <- distGeo(df)

# apply k-means clustering to the distance matrix
k <- 2 # number of clusters
kmeans_clusters <- kmeans(dist_matrix, k)

# add cluster ID to original data frame
df$cluster <- kmeans_clusters$cluster

# print the resulting data frame
print(df)





stable_areas_data <- original_data %>% filter(Area_Estavel == 1)

unstable_areas_data <- original_data %>% filter(Area_Estavel == 0)
