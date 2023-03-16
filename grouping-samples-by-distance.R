# Load required packages
library(geosphere)

# Create a data frame with latitude and longitude variables
df <- data.frame(
  id = 1:10,
  latitude = rnorm(10, 40, 1),
  longitude = rnorm(10, -80, 1)
)

# Compute the distance matrix between all pairs of points
dist_matrix <- distm(df[, c("longitude", "latitude")])

# Run k-means clustering on the distance matrix
k <- 3
clusters <- kmeans(dist_matrix, centers = k)

# Assign cluster IDs to the original data frame
df$cluster <- clusters$cluster

# Print the results
print(df)


require(ggplot2)

ggplot(df)+
  geom_point(aes(longitude, latitude, color = as.factor(cluster)), size = 3)


# Unscrambling GeoJSON ----------------------------------------------------


# load required packages
library(dplyr)
library(jsonlite)

# create sample data frame with GeoJSON column
df <- data.frame(geojson = c("{\"type\":\"Point\",\"coordinates\":[-46.450219341389875,-19.420183266845367]}",
                             "{\"type\":\"Point\",\"coordinates\":[-122.4194,37.7749]}"))

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

# update existing data frame with latitude and longitude columns
df <- df %>%
  mutate(LAT = apply(df %>% select(geojson), 1, extract_latitude)) %>%
  mutate(LON = apply(df %>% select(geojson), 1, extract_longitude))



# Creating clusters -------------------------------------------------------



# load required packages
library(geosphere)
library(dplyr)
library(stats)

# generate sample data frame with latitude and longitude columns
df <- data.frame(latitude = c(-23.55052, -22.90642, -23.55052, -22.90642),
                 longitude = c(-46.63331, -43.17290, -46.63331, -43.17290))

# compute distance matrix between samples
dist_mat <- distm(df[, c("longitude", "latitude")])

# perform k-means clustering on distance matrix
k <- 2  # number of clusters
km_res <- kmeans(dist_mat, k)

# add cluster ID to original data frame
df <- df %>%
  mutate(cluster = km_res$cluster)

# print result
print(df)


