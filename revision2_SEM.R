library(lavaan)


ar_stable <- '

  MS1    ~ -0.5*white + 0.5*parEd
  
  BiG1 ~ 0*MS1 + -0.5*white + 0.25*parEd

  BiG2   ~  .7*BiG1   +  -0.1*MS1   +
            -0.5*white + 0.25*parEd
  BiG3   ~   .7*BiG2  + -0.1*MS1 +
            -0.5*white +  -0.25*parEd

  BiG4   ~   .7*BiG3  +  -0.1*MS1
           +  -0.5*white +  -0.25*parEd
          

 
'

# Target total effect:
-.1 + -.1*.7 + -.1*.7**2

# -.219

simData_stable <- simulateData(
  model        = ar_stable,
  sample.nobs  = 50000)

summary(lm(BiG4 ~ MS1 + white + parEd, data = simData_stable))

model <- '

  MS1    ~ white + parEd
  
  BiG1 ~ 0*MS1 + white + parEd

  BiG2   ~  ar2*BiG1   +  h1a*MS1   + white + parEd
  BiG3   ~  ar3*BiG2  + h1b*MS1 + white +  parEd

  BiG4   ~  ar4*BiG3  +  h1c*MS1  +  white +  parEd
  
    ## MS1 -> BiG2 -> BiG3

ms1.big2.big3.big4 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1.big3.big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

sumH1 := h1c_ + 
         ms1.big2.big3.big4 + 
         ms1.big3.big4
         
'

fit_ar_stable <- sem(model, simData_stable)

summary(fit_ar1, std = T)

###

true_model_ar_vary <- '

  MS1    ~ -0.5*white + 0.5*parEd
  
  BiG1 ~ 0*MS1 + -0.5*white + 0.25*parEd

  BiG2   ~  .6*BiG1   +  -0.1*MS1   +
            -0.5*white + 0.25*parEd
  BiG3   ~   .8*BiG2  + -0.1*MS1 +
            -0.5*white +  -0.25*parEd

  BiG4   ~   .5*BiG3  +  -0.1*MS1
           +  -0.5*white +  -0.25*parEd
          

 
'

# Target total effect:
-.1 + 
  -.1*.5 +
  -.1*.5*.8

# -.19

simData_ar_vary <- simulateData(
  model        = true_model_ar_vary,
  sample.nobs  = 50000)

summary(lm(BiG4 ~ MS1 + white + parEd, data = simData_ar_vary))

ar_vary <- '

  MS1    ~ white + parEd
  
  BiG1 ~ 0*MS1 + white + parEd

  BiG2   ~  ar2*BiG1   +  h1a*MS1   + white + parEd
  BiG3   ~  ar3*BiG2  + h1b*MS1 + white +  parEd

  BiG4   ~  ar4*BiG3  +  h1c*MS1  +  white +  parEd
  
    ## MS1 -> BiG2 -> BiG3

ms1.big2.big3.big4 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1.big3.big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

sumH1 := h1c_ + 
         ms1.big2.big3.big4 + 
         ms1.big3.big4
         
'

fit_ar_vary <- sem(model, simData_ar_vary)

summary(fit_ar_vary, std = T)

###

ar_stable_bias_unstable <- '

  MS1    ~ -0.5*white + 0.5*parEd
  
  BiG1 ~ 0*MS1 + -0.5*white + 0.25*parEd

  BiG2   ~  .7*BiG1   +  -0.1*MS1   +
            -0.4*white + 0.45*parEd
  BiG3   ~   .7*BiG2  + -0.1*MS1 +
            -0.5*white +  -0.05*parEd

  BiG4   ~   .7*BiG3  +  -0.1*MS1
           +  -0.7*white +  -0.25*parEd
          

 
'

# Target total effect:
-.1 + -.1*.7 + -.1*.7**2

# -.219

simData_stable_bias_unstable <- simulateData(
  model        = ar_stable_bias_unstable,
  sample.nobs  = 50000)

summary(lm(BiG4 ~ MS1 + white + parEd, data = simData_stable_bias_unstable))

model <- '

  MS1    ~ white + parEd
  
  BiG1 ~ 0*MS1 + white + parEd

  BiG2   ~  ar2*BiG1   +  h1a*MS1   + white + parEd
  BiG3   ~  ar3*BiG2  + h1b*MS1 + white +  parEd

  BiG4   ~  ar4*BiG3  +  h1c*MS1  +  white +  parEd
  
    ## MS1 -> BiG2 -> BiG3

ms1.big2.big3.big4 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1.big3.big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

sumH1 := h1c_ + 
         ms1.big2.big3.big4 + 
         ms1.big3.big4
         
'

fit_ar_stable_bias_unstable <- sem(model, simData_stable_bias_unstable)

summary(fit_ar_stable_bias_unstable, std = T)

###

###

ar_unstable_bias_unstable <- '

  MS1    ~ -0.5*white + 0.5*parEd
  
  BiG1 ~ 0*MS1 + -0.5*white + 0.25*parEd

  BiG2   ~  .6*BiG1   +  -0.1*MS1   +
            -0.4*white + 0.45*parEd
  BiG3   ~   .8*BiG2  + -0.1*MS1 +
            -0.5*white +  -0.05*parEd

  BiG4   ~   .5*BiG3  +  -0.1*MS1
           +  -0.7*white +  -0.25*parEd
          

 
'

# Target total effect:
-.1 + 
  -.1*.5 +
  -.1*.5*.8

# -.19

simData_unstable_bias_unstable <- simulateData(
  model        = ar_unstable_bias_unstable,
  sample.nobs  = 50000)

summary(lm(BiG4 ~ MS1 + white + parEd, data = simData_unstable_bias_unstable))

model <- '

  MS1    ~ white + parEd
  
  BiG1 ~ 0*MS1 + white + parEd

  BiG2   ~  ar2*BiG1   +  h1a*MS1   + white + parEd
  BiG3   ~  ar3*BiG2  + h1b*MS1 + white +  parEd

  BiG4   ~  ar4*BiG3  +  h1c*MS1  +  white +  parEd
  
    ## MS1 -> BiG2 -> BiG3

ms1.big2.big3.big4 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1.big3.big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

sumH1 := h1c_ + 
         ms1.big2.big3.big4 + 
         ms1.big3.big4
         
'

fit_ar_unstable_bias_unstable <- sem(model, simData_unstable_bias_unstable)

summary(fit_ar_unstable_bias_unstable, std = T)

###

library(dplyr)
library(knitr)


#–– Helpers ––
get_unstd_sumH1 <- function(fit) {
  parameterEstimates(fit, standardized=FALSE) %>%
    filter(label=="sumH1") %>%
    pull(est)
}

get_stdall_sumH1 <- function(fit) {
  parameterEstimates(fit, standardized=TRUE) %>%
    filter(label=="sumH1") %>%
    pull(std.all)
}

get_lmMS1 <- function(simData) {
  coef(lm(BiG4 ~ MS1 + white + parEd, data = simData))["MS1"]
}

#–– Build the results table ––
results <- tibble(
  model = c(
    "ar_stable",
    "ar_vary",
    "ar_stable_bias_unstable",
    "ar_unstable_bias_unstable"
  ),
  unstd_sumH1 = c(
    get_unstd_sumH1(fit_ar_stable),
    get_unstd_sumH1(fit_ar_vary),
    get_unstd_sumH1(fit_ar_stable_bias_unstable),
    get_unstd_sumH1(fit_ar_unstable_bias_unstable)
  ),
  lavaan_std_sumH1 = c(
    get_stdall_sumH1(fit_ar_stable),
    get_stdall_sumH1(fit_ar_vary),
    get_stdall_sumH1(fit_ar_stable_bias_unstable),
    get_stdall_sumH1(fit_ar_unstable_bias_unstable)
  ),
  lm_MS1 = c(
    get_lmMS1(simData_stable),
    get_lmMS1(simData_ar_vary),
    get_lmMS1(simData_stable_bias_unstable),
    get_lmMS1(simData_unstable_bias_unstable)
  ),
  true = c(
    -.219,
    -.19,
    -.219,
    -.19),
  delta_lm = true-lm_MS1,
  delta_sem = true-unstd_sumH1
  )


#–– Print as markdown table ––
kable(
  results,
  col.names = c(
    "Model",
    "Unstd. sumH1",
    "Std.all sumH1",
    "OLS",
    "True ",
    "Delta regression",
    "Delta SEM"
  ),
  digits = 3,
  format = "markdown"
)

