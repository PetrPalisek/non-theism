# Load packages
library(tidyverse)
library(sjPlot)
library(performance)
library(MASS)

# Load your data
load("~/non-theism/nontheism.RData")

# ========================
# 1. Fit LINEAR REGRESSIONS
# ========================

# Reviewer's linear model
rev_fit <- lm(as.numeric(BiG4) ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord, data = df)
rev_fit_stb <- lm(as.numeric(BiG4) ~ MS1 + as.numeric(BiG3) + BlackE + LatinxE + OtherE + ParEd_ord, data = df)

sjPlot::plot_model(rev_fit, "diag")
sjPlot::plot_model(rev_fit_stb, "diag")


# Extract estimates, p-values, and adjusted R²
lm_df_1 <- broom::tidy(rev_fit) |>
  filter(term != "(Intercept)") |>
  mutate(
    estimate = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    model = "LM: No BiG3",
    p_label = paste0("p = ", signif(p.value, 2))
  )

lm_df_1$adj_r2 <- signif(summary(rev_fit)$adj.r.squared, 3)

lm_df_2 <- broom::tidy(rev_fit_stb) |>
  filter(term != "(Intercept)") |>
  mutate(
    estimate = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error),
    model = "LM: With BiG3",
    p_label = paste0("p = ", signif(p.value, 2))
  )

lm_df_2$adj_r2 <- signif(summary(rev_fit_stb)$adj.r.squared, 3)

# ========================
# 2. Fit POLR MODELS
# ========================

# -------------------------------
# AIC comparison across link functions
# for both ordinal models (with and without BiG3)
# -------------------------------

# Define link methods to test
link_methods <- c("logistic", "probit", "cloglog", "loglog")

# Helper function to extract AIC for both models
get_aic <- function(link_method) {
  # Model without BiG3
  mod1 <- polr(
    as.factor(BiG4) ~ MS1,
    data = df,
    method = link_method,
    Hess = TRUE
  )
  
  # Model with BiG3
  mod2 <- polr(
    as.factor(BiG4) ~ MS1,
    data = df,
    method = link_method,
    Hess = TRUE
  )
  
  tibble(
    method = link_method,
    AIC = c(AIC(mod1), AIC(mod2)),
    model = c("No BiG3", "With BiG3")
  )
}

# Run across all link methods and bind results
aic_results <- map_dfr(link_methods, get_aic)

# -------------------------------
# Plot AICs
# -------------------------------

ggplot(aic_results, aes(x = method, y = AIC, group = model, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.8, size = 3.5) +
  labs(
    title = "Ordinal Regression AIC by Link Function",
    subtitle = "Comparing models with and without BiG3",
    x = "Link Function",
    y = "AIC",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)



# Ordinal regression without BiG3
rev_fit_ord <- polr(
  as.factor(BiG4) ~ MS1,
  data = df,
  Hess = TRUE,
  method = "cloglog"
)

# Ordinal regression with BiG3
rev_fit_ord_stb <- polr(
  as.factor(BiG4) ~ MS1 + BiG3,
  data = df,
  Hess = TRUE,
  method = "cloglog"
)

# Function to extract exp(β), CIs, and p-values from polr
extract_or_ci <- function(model) {
  coef_est <- coef(summary(model))
  p_vals <- pnorm(abs(coef_est[, "t value"]), lower.tail = FALSE) * 2
  estimates <- coef_est[, "Value"]
  se <- coef_est[, "Std. Error"]
  
  lower <- estimates - 1.96 * se
  upper <- estimates + 1.96 * se
  
  data.frame(
    term = rownames(coef_est),
    estimate = exp(estimates),
    conf.low = exp(lower),
    conf.high = exp(upper),
    p_label = paste0("p = ", signif(p_vals, 2))
  )
}

or_df_1 <- extract_or_ci(rev_fit_ord) |>
  mutate(model = "POLR: No BiG3")

or_df_2 <- extract_or_ci(rev_fit_ord_stb) |>
  mutate(model = "POLR: With BiG3")

# ========================
# 3. Combine all results
# ========================

# Bind all model results
plot_df <- bind_rows(
  or_df_1,
  or_df_2
) |>
  filter(!grepl("\\|", term))  # remove intercepts from polr

# ========================
# 4. Plot
# ========================

ggplot(plot_df, aes(x = term, y = estimate, color = model)) +
  geom_point(
    aes(alpha = grepl("^LM", model)),  # LM models shaded
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, alpha = grepl("^LM", model)),
    position = position_dodge(width = 0.6),
    width = 0.2
  ) +
  geom_text(
    aes(label = p_label, group = model),
    position = position_dodge(width = 0.9),
    hjust = -0.2,
    size = 3
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_log10() +
  coord_flip() +
  scale_alpha_manual(values = c("TRUE" = 0.4, "FALSE" = 1), guide = "none") +
  labs(
    title = "Model Comparison: Odds Ratios (exp(β))",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)
