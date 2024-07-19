extract_defined_params_lavaanmi <- function(lavaan.mi_object) {
  
  library(lavaan.mi)
  
  # Extract labels
  hypothesis_elements <-  data.frame(lavaan.mi::standardizedSolution.mi(lavaan.mi_object))
  hypothesis_elements <- hypothesis_elements[hypothesis_elements$op == ":=","label"]
  # Set up indices vector
  indices <- rep(NA, length(hypothesis_elements))
  
  # Extract std.est
  for (i in seq_along(hypothesis_elements)) {
    param <- hypothesis_elements[i]
    indices[i] <- which(lavaan.mi::standardizedSolution.mi(lavaan.mi_object)[, 'label'] == param)
  }
  
  est <- lavaan.mi::standardizedSolution.mi(lavaan.mi_object)[indices, 'est.std'] # estimates
  
  names(est) <- hypothesis_elements
  
  VCOV <- standardizedSolution.mi(lavaan.mi_object, return.vcov = TRUE, type = "def.std.all") # vcov
  
  param_list <- return(list(est = est, VCOV = VCOV))
}
