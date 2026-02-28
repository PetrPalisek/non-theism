# ---- 1) Base model: all hypothesis paths fixed to zero (your spec) ----
base_nest_ord <- "
PST_l =~ PST
PST ~~ 0*PST

PR2_l =~ PR2
PR2 ~~ 0*PR2

CR2_l =~ CR2
CR2 ~~ 0*CR2

eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

BiG1 ~ 0* MS1
BiG2 ~ BiG1 + 0*MS1
BiG3 ~ ar3*BiG2 + 0*MS1 + 0*H2 + 0*T2 + PR2_l + CR2_l 
BiG4 ~ ar4*BiG3 + 0*MS1 + 0*H2 + 0*T2 + 0*H3 + 0*PR3 + 0*CR3 

MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
BlackProt ~ BlackE
Catholic ~ LatinxE
BlackProt + MainProt + Catholic ~ OtherE
College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
Inc3 ~ MS1
ParRit ~ ParCollege + ParAAVOC

PR3 ~ PR2_l + h3.1a*H2 + T2
CR3 ~ CR2_l + H2 + h3.2a*T2

H2 ~ MS1
H3 ~ arH*H2
T2 ~ MS1

BiG1 | l*t1
BiG2 | k*t1
BiG2 | l*t2
BiG3 | k*t1
BiG3 | l*t2
BiG4 | k*t1
BiG4 | l*t2

PR2 | pr1*t1 + pr2*t2 + pr3*t3 + pr4*t4 + pr5*t5 + pr6*t6
PR3 | pr1*t1 + pr2*t2 + pr3*t3 + pr4*t4 + pr5*t5 + pr6*t6

CR2 | cr1*t1 + cr2*t2 + cr3*t3 + cr4*t4 + cr5*t5 + cr6*t6
CR3 | cr1*t1 + cr2*t2 + cr3*t3 + cr4*t4 + cr5*t5 + cr6*t6

H2 | hth1*t1 + hth2*t2 + hth3*t3 + hth4*t4
H3 | hth1*t1 + hth2*t2 + hth3*t3 + hth4*t4

Catholic ~~ MainProt + BlackProt
MainProt ~~ BlackProt
Inc3 ~~ College
ParRit ~~ CR2_l + CR3 + PR2_l + PR3
"

# ---- 2) Activator: free blocks by name ----
free_blocks <- function(model, blocks = character(0)) {
  m <- model
  if ("H1"   %in% blocks) m <- gsub("0\\*MS1","MS1", m)
  if ("H2.1" %in% blocks) m <- gsub("0\\*H2","H2", m) |> gsub("0\\*H3","H3", x = _)
  if ("H2.2" %in% blocks) m <- gsub("0\\*T2","T2", m)
  if ("H3.1" %in% blocks) m <- gsub("0\\*PR3","PR3", m)
  if ("H3.2" %in% blocks) m <- gsub("0\\*CR3","CR3", m)
  m
}

# ---- 3) The 11 combinations ----
combos <- list(
  "H1"                                 = c("H1"),
  "H1 + H2.1"                          = c("H1","H2.1"),
  "H1 + H2.1 + H2.2"                   = c("H1","H2.1","H2.2"),
  "H1 + H2.1 + H2.2 + H3.1"            = c("H1","H2.1","H2.2", "H3.1"),
  "H1 + H2.1 + H2.2 + H3.1 + H3.2"     = c("H1","H2.1","H2.2", "H3.1", "H3.2"),
  "H2.1 + H2.2 + H3.1 + H3.2"          = c("H2.1","H2.2", "H3.1", "H3.2"),
  "H2.1 + H2.2 + H3.2"                 = c("H2.1","H2.2", "H3.2"),
  "H2.1 + H3.1 + H3.2"                 = c("H2.1", "H3.1", "H3.2"),
  "H2.2 + H3.1 + H3.2"                 = c("H2.1","H2.2", "H3.1", "H3.2"),
  "H2.1 + H3.2"                        = c("H2.1", "H3.2")
)

# ---- 4) Fit them all (same estimator/sample across models) ----
fit_one <- function(model_string) lavaan.mi::sem.mi(
  model_string, mice.imp,
  estimator="WLSMV", parameterization="theta",
  meanstructure=TRUE,
  ordered=c("BiG1","BiG2","BiG3","BiG4","ParRit","PR2","PR3","CR2","CR3","H2","H3","T2","PST"),
  missing="listwise", control=list(iter.max=10e5)
)

fits <- lapply(combos, function(b) fit_one(free_blocks(base_nest_ord, b)))

# ---- 5) Pull scaled fit indices & make the table ----
`%or%` <- function(a,b) if(!is.na(a)) a else b
get_scaled <- function(fit){
  fm <- fitmeasures(fit)
  data.frame(
    chisq = fm[["chisq.scaled"]] %or% fm[["chisq"]],
    df    = fm[["df.scaled"]]    %or% fm[["df"]],
    RMSEA = fm[["rmsea.scaled"]] %or% fm[["rmsea"]],
    TLI   = fm[["tli.scaled"]]   %or% fm[["tli"]],
    SRMR  = if(!is.na(fm[["srmr"]])) fm[["srmr"]] else fm[["srmr"]],
    row.names=NULL
  )
}

tab <- do.call(rbind, lapply(names(fits), function(nm){
  out <- get_scaled(fits[[nm]])
  cbind(Model = nm, out)
}))

# pretty formatting
tab$chisq <- round(tab$chisq,0)
tab$df    <- round(tab$df,0)
tab$RMSEA <- sprintf("%.3f", tab$RMSEA)
tab$TLI   <- sprintf("%.3f", tab$TLI)
tab$SRMR  <- sprintf("%.3f", tab$SRMR)

tab
# openxlsx::write.xlsx(tab, "nested_11_models_scaled_fits.xlsx")

library(dplyr)
library(purrr)
library(tibble)

# Robustly fetch a parameter table from a lavaan.mi fit
get_par_table <- function(fit) {
  tryCatch({
    as_tibble(lavaan::lavInspect(fit, "parTable"))
  }, error = function(e1) {
    tryCatch({
      as_tibble(lavaan::parameterTable(fit))
    }, error = function(e2) {
      obj <- NULL
      # try common slots in lavaan.mi
      if (!is.null(fit@lavList)) obj <- fit@lavList[[1]]
      if (is.null(obj) && !is.null(fit@fitList)) obj <- fit@fitList[[1]]
      if (is.null(obj)) stop("Couldn't locate an underlying lavaan object.")
      as_tibble(lavaan::parameterTable(obj))
    })
  })
}

# 1) All user-defined (':=') parameters per model in `fitst`
defined_by_model <- imap(
  fits,
  ~ parameterEstimates.mi(.x, asymptotic = TRUE) %>%
    filter(lhs %in% c("BiG1", "BiG2", "BiG3", "BiG4")) %>% filter(op == "~") %>%
    mutate(model_name = .y, .before = 1) %>%
    select(model_name, lhs, op, rhs, est, se, z, pvalue, label)
) %>% list_rbind()

# 2) All parameters fixed to 0 per model in `fitst`
fixed_zero_by_model <- imap(
  fits,
  ~ get_par_table(.x) %>%
    filter(op %in% c("~")) %>%
    mutate(ustart = dplyr::coalesce(ustart, 0)) %>%
    filter(free == 0, ustart == 0) %>%
    mutate(model_name = .y, .before = 1) %>%
    transmute(model_name, lhs, op, rhs, fixed_value = ustart, free, label)
) %>% list_rbind()

# --- Results ---
defined_by_model %>% View()
fixed_zero_by_model

