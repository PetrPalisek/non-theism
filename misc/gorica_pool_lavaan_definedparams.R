

gorica_pool_lavaan_definedparams <- function(imputed_data_list, model.syntax, estimator, ordered, hypothesis, hypothesis_elements) {
  library(lavaan)
  library(restriktor)
  
  gorica_df <- data.frame() # Dataframe to store the results
  
  # Loop over each imputed dataset
  for (dataset in 1:length(imputed_data_list)) {
    
    # Fit the specified SEM to each dataset
    fit <- lavaan::sem(model = model.syntax, data = data.frame(imputed_data_list[[dataset]]),
                       estimator = estimator, ordered = ordered, parameterization = "theta", 
                       meanstructure = T,  missing = "pairwise")
    
    indices <- rep(NA, length(hypothesis_elements))
    
    # Extract the parameters from hypothesis_elements
    for (i in seq_along(hypothesis_elements)) {
      indices[i] <- which(standardizedSolution(fit)[, 'label'] == hypothesis_elements[i])
    }
    
    est <- standardizedSolution(fit)[indices, 'est.std'] # estimates
    VCOV <- lavInspect(fit, "vcov.def.std.all") # covariance matrix
    
    names(est) <- hypothesis_elements
    
    gorica_df <- rbind(gorica_df, restriktor::goric(est, VCOV = VCOV,
                                       hypotheses = list(hypothesis), comparison = "complement")[["result"]][1,])
  }
  
  gorica_df$model <- 1:length(imputed_data_list)
  print(gorica_df)
  return(gorica_df) 
}

# Example:
# base_goricas <- gorica_pool_lavaan_definedparams(mice.imp, base, "WLSMV", ordered = c("BiG1","BiG2", "BiG3", "BiG4"),
#                                hypothesis = "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0",
#                                 hypothesis_elements = c("h1a_", "h1b_", "h1c_", "ms1_big3", "ms1_big4"))
