extract_defined_params_lavaan <- function(lavaan_object) {
  
  library(lavaan)
  
  # Extract labels
  hypothesis_elements <-  data.frame(lavaan::standardizedSolution(lavaan_object))
  hypothesis_elements <- hypothesis_elements[hypothesis_elements$op == ":=","label"]
  # Set up indices vector
  indices <- rep(NA, length(hypothesis_elements))
  
  # Extract std.est
  for (i in seq_along(hypothesis_elements)) {
    param <- hypothesis_elements[i]
    indices[i] <- which(lavaan::standardizedSolution(lavaan_object)[, 'label'] == param)
  }
  
  est <- lavaan::standardizedSolution(lavaan_object)[indices, 'est.std'] # estimates
  
  names(est) <- hypothesis_elements
  
  VCOV <- lavaan::lavInspect(lavaan_object, what = "vcov.def.std.all") # vcov
  
  param_list <- return(list(est = est, VCOV = VCOV))
}
