# Initial checks ----------------------------------------------------------
library(MASS)      # for polr() and other functions
library(sjPlot)    # for model diagnostic plots
library(mice)      # for pooling models (for supported models)
library(dplyr)     # for data manipulation
library(purrr)     # for iterating over imputations
library(ggplot2)   # for plotting

# Assume df is your original data and mice.imp is your list of imputed datasets

# ========================
# 1. Fit LINEAR REGRESSIONS & Pooling (Supported by mice)
# ========================

# Using the with() function to apply the linear model to each imputed dataset
lm_fit <- with(df_imp, lm(as.numeric(BiG4) ~ as.numeric(MS1) + BlackE + LatinxE + OtherE + ParEd_ord))

# Pool the results automatically using Rubin's rules
pooled_lm <- pool(lm_fit)
summary(pooled_lm)

# Optionally, if you want a second variant of the linear model:
lm_fit_stb <- with(df_imp, lm(as.numeric(BiG4) ~ as.numeric(MS1) + BiG3 +
                                  BlackE + LatinxE + OtherE + ParEd_ord))
pooled_lm_stb <- pool(lm_fit_stb)
summary(pooled_lm_stb)

# For diagnostic plots on each imputed dataset:
walk(lm_fit$analyses, sjPlot::plot_model, type = "diag")
walk(lm_fit_stb$analyses, sjPlot::plot_model, type = "diag")

# ========================
# 2. Fit POLR MODELS and Pooling (Manual Pooling via Rubin's Rules)
# ========================

# For comparing different link functions via AIC, we still loop over the imputations
link_methods <- c("logistic", "probit", "cloglog", "loglog")

# Helper function to extract AIC values from one imputed dataset for given link functions
get_aic <- function(link_method, mice.imp) {
  # Model without BiG3
  mod1 <- polr(as.factor(BiG4) ~ MS1, data = mice.imp, method = link_method, Hess = TRUE)
  # Model with controls (without BiG3)
  mod2 <- polr(as.factor(BiG4) ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord,
               data = mice.imp, method = link_method, Hess = TRUE)
  # Model with BiG3 and controls
  mod3 <- polr(as.factor(BiG4) ~ MS1 + BiG3 + BlackE + LatinxE + OtherE + ParEd_ord,
               data = mice.imp, method = link_method, Hess = TRUE)
  
  tibble(
    method = link_method,
    AIC = c(AIC(mod1), AIC(mod2), AIC(mod3)),
    model = c("No BiG3", "With Controls", "With BiG3 + Controls")
  )
}

# Run AIC comparisons across imputations and link functions
aic_results <- map_dfr(mice.imp, function(imp_data) {
  map_dfr(link_methods, function(link) {
    get_aic(link, imp_data)
  })
}, .id = "imputation")

# -------------
# Pooling AIC values across imputations
# -------------

# Note: Because there is no standard pooling method for AIC in multiple imputation,
# a common approach is to average the AIC values across the m imputations.
pooled_aic <- aic_results %>%
  group_by(method, model) %>%
  summarise(
    pooled_AIC = mean(AIC),
    aic_sd = sd(AIC),
    n = n(),
    .groups = "drop"
  )

print(pooled_aic)

# Plot the pooled AIC (averaged over imputations) for each link function and model type
ggplot(pooled_aic, aes(x = method, y = pooled_AIC, group = model, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pooled_AIC - aic_sd, ymax = pooled_AIC + aic_sd), width = 0.2) +
  labs(
    title = "Pooled AIC by Link Function",
    subtitle = "Mean AIC and standard deviation across imputations",
    x = "Link Function",
    y = "Pooled (Average) AIC",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)


# -------------------------------
# Fit polr models across all imputations
# -------------------------------

# Ordinal regression without BiG3 and controls
rev_fit_ord <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1, data = .x, Hess = TRUE, method = "cloglog"))
# Ordinal regression with controls (without BiG3)
rev_fit_ord_cont <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord,
                                         data = .x, Hess = TRUE, method = "cloglog"))
# Ordinal regression with BiG3 and controls
rev_fit_ord_stb <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1 + BiG3 + BlackE + LatinxE + OtherE + ParEd_ord,
                                        data = .x, Hess = TRUE, method = "cloglog"))

# -------------------------------
# Define a custom pooling function for polr models
# -------------------------------
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

# -------------------------------
# Pool the polr model estimates for each model type
# -------------------------------
pooled_or_df_1 <- pool_polr(rev_fit_ord) %>% 
  mutate(model = "POLR: No BiG3, no controls")
pooled_or_df_2 <- pool_polr(rev_fit_ord_cont) %>% 
  mutate(model = "POLR: No BiG3, with controls")
pooled_or_df_3 <- pool_polr(rev_fit_ord_stb) %>% 
  mutate(model = "POLR: With BiG3 and controls")

# For easier interpretation, compute Odds Ratios and 95% Confidence Intervals
# (Exponentiating the pooled estimate and its confidence bounds)
pooled_or_df_1 <- pooled_or_df_1 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))
pooled_or_df_2 <- pooled_or_df_2 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))
pooled_or_df_3 <- pooled_or_df_3 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))

# ========================
# 3. Combine all pooled polr results and Plot Odds Ratios
# ========================

plot_df <- bind_rows(
  pooled_or_df_1,
  pooled_or_df_2,
  pooled_or_df_3
)

# Plotting the Odds Ratios with their Confidence Intervals
ggplot(plot_df, aes(x = term, y = odds_ratio, color = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_text(aes(label = paste0("p=", signif(p_value, 2))), 
            position = position_dodge(width = 0.9), hjust = -0.2, size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Pooled Model Comparison: Odds Ratios (exp(β))",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)
