
## functions for the validation statistics

my_summary_metrics <- function(data, lev = NULL, model = NULL) {
  
  regression_eval <- function(pred, obs){
    
    # mean error
    ME <- round(mean(pred - obs, na.rm = TRUE), digits = 3)
    
    # mean square error
    MSE <-   round(mean((pred - obs)^2, na.rm = TRUE), digits = 3)
    
    # mean absolute error
    MAE <-   round(mean(abs(pred - obs), na.rm = TRUE), digits = 3)
    
    # root mean square error
    RMSE <-   round(sqrt(mean((pred - obs)^2, na.rm = TRUE)), digits = 3)
    
    # Pearson's correlation squared
    r2 <-  round((cor(pred, obs, method = 'spearman', use = 'pairwise.complete.obs')^2), digits = 2)
    
    # Lin's concordance correlation coefficient
    CCC <- yardstick::ccc_vec(truth = obs, estimate = pred)
    
    out <- c(ME, MAE, MSE, RMSE, r2, CCC)
    names(out) <- c("ME", "MAE", "MSE", "RMSE", "Rsquared", "CCC")
    
    if (any(is.nan(out))) 
      out[is.nan(out)] <- NA
    out
    
  }
  
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  regression_eval(data[, "pred"], data[, "obs"])

}

