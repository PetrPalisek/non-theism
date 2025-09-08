## Revisions 2

library(MASS)
library(lavaan)
library(semTools)
library(lavaan.mi)
library(purrr)
library(tidyverse)


load("~/Documents/GitHub/non-theism/imputed_data_list.RData")

## Polrs


# Unadjusted
unadj_polr <- map(mice.imp, ~ polr(as.factor(BiG4) ~ scale(MS1), 
                   data = .x, Hess = TRUE, method = "probit"))

# This is the baseline adjustment model

base_polr <- map(mice.imp, ~ polr(as.factor(BiG4) ~ scale(MS1) + BlackE + LatinxE + 
                                       OtherE + ParEd_ord, 
                                     data = .x, Hess = TRUE, method = "probit"))

# And this is the fully adjusted model

adj_polr <- map(mice.imp, ~ polr(as.factor(BiG4) ~ scale(MS1) + BlackE + LatinxE +
                                   OtherE + ParEd_ord +  BlackProt + 
                                   Catholic + MainProt + Age + Male + ParRit + 
                                   Inc3 + College + AAVOC + PST, 
                                 data = .x, Hess = TRUE, method = "probit"))

# fully adjusted model, incl. AR

ar_adj_polr <- map(mice.imp, ~ polr(as.factor(BiG4) ~ as.factor(BiG3) + scale(MS1) + BlackE + LatinxE +
                                   OtherE + ParEd_ord +  BlackProt + 
                                   Catholic + MainProt + Age + Male + ParRit + 
                                   Inc3 + College + AAVOC + PST, 
                                 data = .x, Hess = TRUE, method = "probit"))

# Define a custom pooling function for polr models
pool_polr <- function(models) {
  # Number of imputations
  m <- length(models)
  
  # Extract coefficient summaries (ignoring intercept/threshold rows)
  coef_list <- lapply(models, function(model) {
    coefs <- coef(summary(model))
    # Keep only coefficient rows that are not thresholds (those containing a "|")
    coefs[!grepl("\\|", rownames(coefs)), , drop = FALSE]
  })
  
  # Get all unique parameter names from the imputations
  all_params <- unique(unlist(lapply(coef_list, rownames)))
  
  pooled_results <- lapply(all_params, function(param) {
    # Extract the estimate for param from each imputation
    estimates <- sapply(coef_list, function(coef_mat) {
      if (param %in% rownames(coef_mat)) coef_mat[param, "Value"] else NA
    })
    estimates <- na.omit(estimates)
    
    # Extract standard errors and compute within-imputation variance (square of SE)
    ses <- sapply(coef_list, function(coef_mat) {
      if (param %in% rownames(coef_mat)) coef_mat[param, "Std. Error"] else NA
    })
    ses <- na.omit(ses)
    
    U_bar <- mean(ses^2)     # average within-imputation variance
    B <- var(estimates)      # between-imputation variance
    Q_bar <- mean(estimates)  # pooled estimate
    
    # Total variance: within + (1 + 1/m)*between variance
    T_var <- U_bar + (1 + 1/m) * B
    pooled_se <- sqrt(T_var)
    
    # t-statistic and approximate degrees of freedom
    t_val <- Q_bar / pooled_se
    df <- if (B == 0) Inf else (m - 1) * (1 + U_bar / ((1 + 1/m) * B))^2
    p_val <- 2 * pt(-abs(t_val), df)
    
    # Return a data frame with pooled results on the log scale
    data.frame(
      term = param,
      estimate = Q_bar,
      se = pooled_se,
      t_value = t_val,
      df = df,
      p_value = p_val,
      stringsAsFactors = FALSE
    )
  })
  
  pooled_df <- do.call(rbind, pooled_results)
  return(pooled_df)
}

# Pool the polr models
pooled_polrs <- bind_rows(
  pool_polr(unadj_polr) %>%
    mutate(model = "POLR: Unadjusted") %>%
    slice(1),
  
  pool_polr(base_polr) %>%
    mutate(model = "POLR: Baseline") %>%
    slice(1),
  
  pool_polr(adj_polr) %>%
    mutate(model = "POLR: Adjusted") %>%
    slice(1),
  
  pool_polr(ar_adj_polr) %>%
    mutate(model = "POLR: Adjusted + AR") %>%
    slice(3)
)
  
sem_specs <- list(
  "SEM: Unadjusted" = "
    BiG4 ~ MS1
  ",
  
  "SEM: Baseline" = "
    BiG4 ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord
  ",
  
  "SEM: Fully adjusted" = "
    BiG4 ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord + 
           BlackProt + Catholic + MainProt + Age + Male + ParRit + 
           Inc3 + College + AAVOC + PST
  ",
  
  "SEM: Fully adjusted + AR" = "
    BiG4 ~ BiG3 + MS1 + BlackE + LatinxE + OtherE + ParEd_ord + 
           BlackProt + Catholic + MainProt + Age + Male + ParRit + 
           Inc3 + College + AAVOC + PST
  ",
  
  "SEM: Fully adjusted + all AR" = "
    BiG4 ~ BlackE + LatinxE + OtherE + ParEd_ord + 
           BlackProt + Catholic + MainProt + Age + Male + ParRit + 
           Inc3 + College + AAVOC + PST

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4
  ",
  
  "SEM: Fully adjusted + all AR + unit effect eta" = "
    BiG4 ~  BlackE + LatinxE + OtherE + ParEd_ord + 
           BlackProt + Catholic + MainProt + Age + Male + ParRit + 
           Inc3 + College + AAVOC + PST

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4
  ",
  
  "SEM: Fully adjusted + all AR + unit effect eta + invariant thresholds" = "
    BiG4 ~  BlackE + LatinxE + OtherE + ParEd_ord + 
           BlackProt + Catholic + MainProt + Age + Male + ParRit + 
           Inc3 + College + AAVOC + PST

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4

    # Now enforce threshold invariance across BiG1…BiG4
    BiG1 | l*t1
    
    BiG2 | k*t1
    BiG2 | l*t2
    
    BiG3 | k*t1
    BiG3 | l*t2
    
    BiG4 | k*t1
    BiG4 | l*t2

    BiG1 ~ 0*1
    BiG2 ~ NA*1
    BiG3 ~ NA*1
    BiG4 ~ NA*1
  ",
  
  
  "SEM: Not adjusted + all AR + unit effect eta" = "
  

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4
  ",
  
  "SEM: Not adjusted + all AR + unit effect eta + invariant thresholds" = "

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4

    # Now enforce threshold invariance across BiG1…BiG4
    BiG1 | l*t1
    
    BiG2 | k*t1
    BiG2 | l*t2
    
    BiG3 | k*t1
    BiG3 | l*t2
    
    BiG4 | k*t1
    BiG4 | l*t2

    BiG1 ~ 0*1
    BiG2 ~ NA*1
    BiG3 ~ NA*1
    BiG4 ~ NA*1
  ",
  
  
  "SEM: Baseline adjustment + all AR + unit effect eta + invariant thresholds" = "
    BiG4 ~ BlackE + LatinxE + OtherE + ParEd_ord

    BiG1 ~ 0*MS1
    BiG2 ~ BiG1 + h1a*MS1
    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1

    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

    ## Defined parameters
    ms1_big3 := h1a*ar3
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4

    # Now enforce threshold invariance across BiG1…BiG4
    BiG1 | l*t1
    
    BiG2 | k*t1
    BiG2 | l*t2
    
    BiG3 | k*t1
    BiG3 | l*t2
    
    BiG4 | k*t1
    BiG4 | l*t2

    BiG1 ~ 0*1
    BiG2 ~ NA*1
    BiG3 ~ NA*1
    BiG4 ~ NA*1
  "
)

# sem.mi requires specifying which variables are ordered. 
# For the first three models, only BiG4 is ordered; thereafter we add BiG1–BiG4.
ordered_vars_list <- list(
  "SEM: Unadjusted"                              = "BiG4",
  "SEM: Baseline"                                = "BiG4",
  "SEM: Fully adjusted"                          = "BiG4",
  "SEM: Fully adjusted + AR"                     = "BiG4",
  "SEM: Fully adjusted + all AR"                 = c("BiG1", "BiG2", "BiG3", "BiG4"),
  "SEM: Fully adjusted + all AR + unit effect eta" = c("BiG1", "BiG2", "BiG3", "BiG4"),
  "SEM: Fully adjusted + all AR + unit effect eta + invariant thresholds" = c("BiG1", "BiG2", "BiG3", "BiG4"),
  "SEM: Not adjusted + all AR + unit effect eta" = c("BiG1", "BiG2", "BiG3", "BiG4"),
  "SEM: Not adjusted + all AR + unit effect eta + invariant thresholds" = c("BiG1", "BiG2", "BiG3", "BiG4"),
  "SEM: Baseline adjustment + all AR + unit effect eta + invariant thresholds" = c("BiG1", "BiG2", "BiG3", "BiG4")
  )

# Fit each lavaan.mi model (returns a named list of lavaan.mi‐objects)
fitted_sem_list <- imap(sem_specs, function(model_syntax, lbl) {
  sem.mi(model_syntax,
         data = mice.imp,
         ordered = ordered_vars_list[[lbl]],
         meanstructure = TRUE,
         estimator = "WLSMV",
         missing = "pairwise",
         parameterization = "theta",
         std.lv = TRUE)
})

# EXTRACT “BiG4 ~ MS1” FROM EACH SEM ----

extract_path <- function(sem_fit, lhs_var, rhs_var, model_label) {
  standardizedSolution.mi(sem_fit) %>%
    filter(lhs == lhs_var, op == "~", rhs == rhs_var) %>%
    mutate(model = model_label)
}

sem_path_dfs <- imap(fitted_sem_list, function(sem_obj, lbl) {
  extract_path(sem_obj, "BiG4", "MS1", lbl)
})


# 9) EXTRACT “sumH1” DEFINED PARAMETER WHEN IT EXISTS ----

extract_defined <- function(sem_fit, def_label, model_label) {
  standardizedSolution.mi(sem_fit) %>%
    filter(label == def_label) %>%
    mutate(model = model_label)
}

# Only some of the SEM specs define “sumH1,” so we test existence of that label.
sem_sum_dfs <- imap(fitted_sem_list, function(sem_obj, lbl) {
  stdsol <- standardizedSolution.mi(sem_obj)
  if ("sumH1" %in% stdsol$label) {
    extract_defined(sem_obj, "sumH1", lbl)
  } else {
    NULL
  }
}) %>%
  compact()   # drops any NULL entries

pooled_sems_sum <- purrr::imap_dfr(
  fitted_sem_list,
  function(sem_obj, model_name) {
    stdsol <- standardizedSolution.mi(sem_obj)
    if ("sumH1" %in% stdsol$label) {
      stdsol %>%
        filter(label == "sumH1") %>%
        mutate(
          across(where(~inherits(.x, "lavaan.vector")), as.numeric),
          model = model_name
        )
    } else {
      # Return an empty tibble with the right columns
      empty <- stdsol[0, ]
      empty$model <- character(0)
      empty
    }
  }
)

pooled_sems_sum %>% data.frame() %>% select(model, est.std) %>% 
  mutate(est.std = round(est.std, 2) ) %>% 
  rename(summed_effect = est.std) %>% remove_rownames() %>% print()


# 1) Define a helper that extracts "BiG4 ~ MS1" (if present) and coerces lavaan.vector → numeric
extract_path_safe <- function(sem_fit, model_label) {
  stdsol <- standardizedSolution.mi(sem_fit)
  
  # Check if "BiG4 ~ MS1" is in there
  row_exists <- any(
    stdsol$lhs == "BiG4" &
      stdsol$op  == "~"  &
      stdsol$rhs == "MS1"
  )
  
  if (row_exists) {
    stdsol %>%
      filter(lhs == "BiG4", op == "~", rhs == "MS1") %>%
      # Convert any lavaan.vector columns to numeric
      mutate(across(where(~ inherits(.x, "lavaan.vector")), as.numeric)) %>%
      mutate(model = model_label)
  } else {
    # Return an empty tibble with the same columns + model
    empty <- stdsol[0, ]
    empty$model <- character(0)
    empty
  }
}

# 2) Use imap_dfr() to apply extract_path_safe() to each fitted SEM
pooled_sems_paths <- imap_dfr(
  fitted_sem_list,
  function(sem_obj, model_name) {
    extract_path_safe(sem_obj, model_name)
  }
)

# 3) Now select model & est.std, round est.std to 2 decimals, rename to path_effect
pooled_sems_paths %>%
  data.frame() %>%
  select(model, est.std) %>%
  mutate(est.std = round(est.std, 2)) %>%
  rename(`MS->BiG4` = est.std) %>%
  remove_rownames() %>%
  print()
