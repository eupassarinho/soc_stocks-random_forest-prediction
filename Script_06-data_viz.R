
require(dplyr)
require(readxl)
require(ggplot2)
require(patchwork)

data_path <- "C:/Users/erlis/Documents/MEGA/Parcerias_Laboratorios/11_MapBiomas_GT-Solos/soc_stocks-random_forest-prediction/02-cross_validation-results/"

list.files(path = data_path, pattern = ".xlsx")

cv_results <- list(
  read_excel(paste0(data_path, "cross_validation-results_v1-5-0.xlsx")),
  read_excel(paste0(data_path, "cross_validation-results_v1-6-0.xlsx")),
  read_excel(paste0(data_path, "cross_validation-results_v1-7-2.xlsx"))) %>% 
  bind_rows() %>%
  mutate(cv_nclusters = ifelse(cross_validation == "Standard k-Fold CV",
                               cross_validation,
                               paste(cross_validation, "with", n_clusters, "clusters")))

## ME e MAE para v1-5-0
(cv_results %>% 
  filter(map_version == "v1-5-0") %>% 
  ggplot(aes(x = model_for, y = ME))+
  geom_errorbar(stat = "boxplot", width = 0.25)+
  geom_boxplot(width = 0.5, size = 0.7)+
  facet_grid(.~cv_nclusters)+
  ylim(-300, 1100)+
  labs(x = NULL, y = expression(ME~(g~m^-2)),
       title = "Simulations for the version data: v1-5-0 (n = 6228)\nStable area n samples = 2970; Unstable area n samples: 3258")+
  theme_classic()+
  theme(text = element_text(family = "serif", size = 12),
        axis.text = element_text(family = "serif", size = 12),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
        axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-5-0") %>% 
     ggplot(aes(x = model_for, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     ylim(1000, 3500)+
     labs(x = NULL, y = expression(MAE~(g~m^-2)))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_text(size = 10),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/me_and_mae-v1-5-0.png",
       width = 250, height = 148, units = "mm", dpi = 300)

## MSE e RMSE para v1-5-0
(cv_results %>% 
    filter(map_version == "v1-5-0") %>% 
    ggplot(aes(x = model_for, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    ylim(9000000, 37000000)+
    labs(x = NULL,
         title = "Simulations for the version data: v1-5-0 (n = 6228)\nStable area n samples = 2970; Unstable area n samples: 3258")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-5-0") %>% 
     ggplot(aes(x = model_for, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     ylim(2800, 5800)+
     labs(x = NULL, y = expression(RMSE~(g~m^-2)))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_text(size = 10),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/mse_and_rmse-v1-5-0.png",
       width = 250, height = 148, units = "mm", dpi = 300)

## NSE para v1-5-0
(cv_results %>% 
    filter(map_version == "v1-5-0") %>% 
    ggplot(aes(x = model_for, y = NSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    labs(x = NULL,
         title = "Simulations for the version data: v1-5-0 (n = 6228)\nStable area n samples = 2970; Unstable area n samples: 3258")+
    ylim(-1.2, 0.5)+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/nse-v1-5-0.png",
       width = 250, height = 74, units = "mm", dpi = 300)

## ME e MAE para v1-6-0
(cv_results %>% 
    filter(map_version == "v1-6-0") %>% 
    ggplot(aes(x = model_for, y = ME))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    ylim(-300, 1100)+
    labs(x = NULL, y = expression(ME~(g~m^-2)),
         title = "Simulations for the version data: v1-6-0 (n = 6389)\nStable area n samples = 3050; Unstable area n samples: 3339")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-6-0") %>% 
     ggplot(aes(x = model_for, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     ylim(1000, 3500)+
     labs(x = NULL, y = expression(MAE~(g~m^-2)))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_text(size = 10),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/me_and_mae-v1-6-0.png",
       width = 250, height = 148, units = "mm", dpi = 300)

## MSE e RMSE para v1-6-0
(cv_results %>% 
    filter(map_version == "v1-6-0") %>% 
    ggplot(aes(x = model_for, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    ylim(9000000, 37000000)+
    labs(x = NULL,
         title = "Simulations for the version data: v1-6-0 (n = 6389)\nStable area n samples = 3050; Unstable area n samples: 3339")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-6-0") %>% 
     ggplot(aes(x = model_for, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     ylim(2800, 5800)+
     labs(x = NULL, y = expression(RMSE~(g~m^-2)))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_text(size = 10),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/mse_and_rmse-v1-6-0.png",
       width = 250, height = 148, units = "mm", dpi = 300)

## NSE para v1-6-0
(cv_results %>% 
    filter(map_version == "v1-6-0") %>% 
    ggplot(aes(x = model_for, y = NSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    labs(x = NULL,
         title = "Simulations for the version data: v1-6-0 (n = 6389)\nStable area n samples = 3050; Unstable area n samples: 3339")+
    ylim(-1.2, 0.5)+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_text(size = 10),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/nse-v1-6-0.png",
       width = 250, height = 74, units = "mm", dpi = 300)

## ME e MAE para v1-7-2
(cv_results %>% 
    filter(map_version == "v1-7-2", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = model_for, y = ME))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100),
                       labels = seq(0, 700, 100)/1000)+
    labs(x = NULL, y = expression(atop("ME", (kg~m^-2))),
         title = "Simulations for the version data: v1-7-2 (n = 9633)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-7-2", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = MAE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(1500, 2500), breaks = seq(1500, 2500, 200),
                        labels = seq(1500, 2500, 200)/1000)+
     labs(x = NULL, y = expression(atop("MAE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/me_and_mae-v1-7-2.png",
       width = 148, height = 148, units = "mm", dpi = 600)

## MSE, RMSE e NSE para v1-7-2
library(scales)

(cv_results %>% 
    filter(map_version == "v1-7-2", n_clusters %in% c(0, 30)) %>% 
    ggplot(aes(x = model_for, y = MSE))+
    geom_errorbar(stat = "boxplot", width = 0.25)+
    geom_boxplot(width = 0.5, size = 0.7)+
    facet_grid(.~cv_nclusters)+
    scale_y_continuous(limits = c(13000000, 25000000), labels = scientific)+
    labs(x = NULL,
         title = "Simulations for the version data: v1-7-2 (n = 9633)")+
    theme_classic()+
    theme(text = element_text(family = "serif", size = 12),
          axis.text = element_text(family = "serif", size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
          axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
     filter(map_version == "v1-7-2", n_clusters %in% c(0, 30)) %>% 
     ggplot(aes(x = model_for, y = RMSE))+
     geom_errorbar(stat = "boxplot", width = 0.25)+
     geom_boxplot(width = 0.5, size = 0.7)+
     facet_grid(.~cv_nclusters)+
     scale_y_continuous(limits = c(3000, 5000),
                        breaks = round(seq(3000, 5000, 400), 3),
                        labels = round(seq(3000, 5000, 400)/1000, 3))+
     labs(x = NULL, y = expression(atop("RMSE", (kg~m^-2))))+
     theme_classic()+
     theme(text = element_text(family = "serif", size = 12),
           axis.text = element_text(family = "serif", size = 12),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
           axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))/
  (cv_results %>% 
      filter(map_version == "v1-7-2", n_clusters %in% c(0, 30)) %>% 
      ggplot(aes(x = model_for, y = NSE))+
      geom_errorbar(stat = "boxplot", width = 0.25)+
      geom_boxplot(width = 0.5, size = 0.7)+
      facet_grid(.~cv_nclusters)+
      labs(x = NULL)+
      scale_y_continuous(limits = c(-0.2, 0.6), breaks = round(seq(-0.2, 0.6, 0.2), 1),
                         labels = round(seq(-0.2, 0.6, 0.2), 1))+
      theme_classic()+
      theme(text = element_text(family = "serif", size = 12),
            axis.text = element_text(family = "serif", size = 12),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_text(family = "serif", size = 12, colour = "#000000"),
            axis.title.y = element_text(angle = 0, hjust = 1, vjust = 0.5)))

ggsave(filename = "./03-resultant_figures/mse_rmse_nse-v1-7-2.png",
       width = 148, height = 220, units = "mm", dpi = 300)

# Viewing clusters --------------------------------------------------------

require(readr)
require(dplyr)
require(geosphere)
library(jsonlite)

# Importing and splitting datasets ----------------------------------------

original_data <- read_csv("01-data/matriz_rf_prediction_v1-7-2.csv")

data_version <- "v1-7-2"

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
k <- 30 # number of clusters
kmeans_clusters <- kmeans(dist_mat, k)

# add cluster ID to original data frame
original_data$cluster <- kmeans_clusters$cluster

# Visualizing clusters ----------------------------------------------------

clustered_samples <- ggplot2::ggplot(original_data,
                aes(LON, LAT, color = as.factor(cluster),
                    shape = as.factor(cluster)))+
  geom_point(size = 3, alpha = 0.3)+
  #scale_color_manual(aesthetics = "color",
  #                   values = c("#ffd700", "#ffb14e", "#fa8775",
  #                              "#ea5f94", "#ef0000", "#9d02d7",
  #                              "#0000ff", "#04590d", "#00e8ff", "#000000"))+
  #scale_shape_manual(values = c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12))+
  #scale_color_manual(values = sample(col_vector, 20))+
  #scale_shape_manual(values = rep(c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12), 2))+
  scale_color_manual(values = sample(col_vector, 30))+
  scale_shape_manual(values = rep(c(15, 18, 17, 16, 3, 8, 0, 6, 9, 12), 3))+
  labs(y = "Lat (º)", x = "Long (º)",
       title = "Point data from version v1-7-2 (n = 9633): 30 clusters of points")+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(family = "serif", size = 12, color = "#000000"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(family = "serif", size = 12, color = "#000000"),
        title = element_text(family = "serif", size = 12, color = "#000000"),
        panel.background = element_rect(fill = "#B9B9B9"),
        plot.background = element_rect(fill = "#B9B9B9"))

ggsave("./03-resultant_figures/clustered_points-v1_7_2-30_clusters.png",
       plot = clustered_samples,
       width = 210, height = 250, units = "mm", dpi = 300) 


library(RColorBrewer)
n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
