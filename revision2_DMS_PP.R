# The simulation is the same but with AR paths < 1.

### Try simulating some similar data, to test out methods

set.seed(32123)
n <- 500000 # Using a massive number here, just to remove any random variation

# Ethnicity and parental education
white <- rbinom(n = n, size = 1, prob = 0.9)
parEd <- rnorm(n = n, mean = 0, sd = 1)

# Material security 1
MS1 <- (white * -0.5) + (parEd * 0.5) + rnorm(n = n, mean = 0, sd = 1)
summary(MS1)
summary(lm(MS1 ~ white + parEd))

# Belief in God 2 (treat as standardised normal variable, just for simplicity [like on latent scale])
BiG2 <- (white * -0.5) + (parEd * -0.25) + (MS1 * -0.1) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG2)
summary(lm(BiG2 ~ MS1 + white + parEd))

# Belief in god 3
BiG3 <- (white * -0.5) + (parEd * -0.25) + (MS1 * -0.1) + (BiG2 * .7) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG3)
summary(lm(BiG3 ~ MS1 + BiG2 + white + parEd))

# Belief in god 4
BiG4 <- (white * -0.5) + (parEd * -0.25) + (MS1 * -0.1) + (BiG3 * .7) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG4)
summary(lm(BiG4 ~ MS1 + BiG2 + BiG3 + white + parEd))


## Combine into data frame
df <- as.data.frame(cbind(white, parEd, MS1, BiG2, BiG3, BiG4))
head(df)
str(df)


## Total effect of MS1 on BiG4 (via all direct and indirect routes) - Is -0.3, comprised of direct effect of -0.1 of MS1 on BiG4, plus -0.1 indirect effect of MS1 on BiG2, and -0.1 indirect effect of MS1 on BiG4
# After accounting for AR paths it's -.1*.7*.7 + -.1*.7 + -.1 = -.219
summary(lm(BiG4 ~ MS1 + white + parEd, data = df))

# Unadjusted effect (-0.40)
summary(lm(BiG4 ~ MS1, data = df))

# SEM unadjusted - No auto-regressive paths
sem_unadj_mod <- "BiG4 ~ MS1"
sem_unadj <- sem(sem_unadj_mod, df)
summary(sem_unadj)
parameterestimates(sem_unadj)

# SEM adjusted - No auto-regressive paths
sem_adj_mod <- "BiG4 ~ MS1 + white + parEd"
sem_adj <- sem(sem_adj_mod, df)
summary(sem_adj)
parameterestimates(sem_adj)

# SEM adjusted - Auto-regressive paths
sem_adj_mod_AR <- "
    BiG4 ~ h1a*MS1 + white + parEd
    BiG3 ~ h1b*MS1 + white + parEd
    BiG2 ~ h1c*MS1 + white + parEd

    BiG3 ~ ar3*BiG2 
    BiG4 ~ ar4*BiG3 

    ## Defined parameters
    ms1_big4 := h1a*ar4
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big4
"
sem_adj_AR <- sem(sem_adj_mod_AR, df)
summary(sem_adj_AR)
parameterestimates(sem_adj_AR)


# DMJ's original spec

sem_adj_mod_AR_mis <- "
    BiG4 ~ MS1 + white + parEd
    BiG3 ~ MS1 
    BiG2 ~ MS1

    BiG2 ~ 0*MS1
    BiG3 ~ BiG2 + h1a*MS1
    BiG4 ~ ar4*BiG3 + h1b*MS1

    ## Defined parameters
    ms1_big4 := h1a*ar4
    h1a_ := h1a
    h1b_ := h1b
    sumH1 := h1a_ + h1b_ + ms1_big4
"
sem_adj_AR_mis <- sem(sem_adj_mod_AR_mis, df)
summary(sem_adj_AR_mis)
parameterestimates(sem_adj_AR_mis)

### 
# (1) MS1 - 0 -> BiG1, not BiG2

# corrected

sem_adj_mod_AR_mis_v1 <- "
    BiG4 ~ MS1 + white + parEd
    BiG3 ~ MS1 
    BiG2 ~ MS1

    BiG3 ~ BiG2 + h1a*MS1
    BiG4 ~ ar4*BiG3 + h1b*MS1

    ## Defined parameters
    ms1_big4 := h1a*ar4
    h1a_ := h1a
    h1b_ := h1b
    sumH1 := h1a_ + h1b_ + ms1_big4
"
sem_adj_AR_mis_v1 <- sem(sem_adj_mod_AR_mis_v1, df)
summary(sem_adj_AR_mis_v1)
parameterestimates(sem_adj_AR_mis_v1)

### 
# (2) AR paths should be counted twice for MS1 -> BiG2 -> BiG3 -> BiG4!

# corrected

sem_adj_mod_AR_mis_v2 <- "
    BiG4 ~ MS1 + white + parEd
    BiG2 ~ h1a*MS1

    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1



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
"
sem_adj_AR_mis_v2 <- sem(sem_adj_mod_AR_mis_v2, df)
summary(sem_adj_AR_mis_v2)
parameterestimates(sem_adj_AR_mis_v2)

### 
# (3) Controls for all waves

# corrected

sem_adj_mod_AR_mis_v3 <- "
    BiG4 ~ MS1 + white + parEd
    BiG2 ~ h1a*MS1+ white + parEd

    BiG3 ~ ar3*BiG2 + h1b*MS1 + white + parEd
    BiG4 ~ ar4*BiG3 + h1c*MS1+ white + parEd



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
"
sem_adj_AR_mis_v3 <- sem(sem_adj_mod_AR_mis_v3, df)
summary(sem_adj_AR_mis_v3, std = T)
parameterestimates(sem_adj_AR_mis_v3)


# The simulation is the same but with AR paths < 1.
# We also add "non-invariant" biases, will it change anything?

### Try simulating some similar data, to test out methods

set.seed(32123)
n <- 500000 # Using a massive number here, just to remove any random variation

# Ethnicity and parental education
white <- rbinom(n = n, size = 1, prob = 0.9)
parEd <- rnorm(n = n, mean = 0, sd = 1)

# Material security 1
MS1 <- (white * -0.5) + (parEd * 0.5) + rnorm(n = n, mean = 0, sd = 1)
summary(MS1)
summary(lm(MS1 ~ white + parEd))

# Belief in God 2 (treat as standardised normal variable, just for simplicity [like on latent scale])
BiG2 <- (white * -0.5) + (parEd * -0) + (MS1 * -0.1) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG2)
summary(lm(BiG2 ~ MS1 + white + parEd))

# Belief in god 3
BiG3 <- (white * -0.3) + (parEd * -0.35) + (MS1 * -0.1) + (BiG2 * .7) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG3)
summary(lm(BiG3 ~ MS1 + BiG2 + white + parEd))

# Belief in god 4
BiG4 <- (white * -0.1) + (parEd * -0.25) + (MS1 * -0.1) + (BiG3 * .7) + rnorm(n = n, mean = 0, sd = 1)
summary(BiG4)
summary(lm(BiG4 ~ MS1 + BiG2 + BiG3 + white + parEd))


## Combine into data frame
df <- as.data.frame(cbind(white, parEd, MS1, BiG2, BiG3, BiG4))
head(df)
str(df)


## Total effect of MS1 on BiG4 (via all direct and indirect routes) - Is -0.3, comprised of direct effect of -0.1 of MS1 on BiG4, plus -0.1 indirect effect of MS1 on BiG2, and -0.1 indirect effect of MS1 on BiG4
# After accounting for AR paths it's -.1*.7*.7 + -.1*.7 + -.1 = -.219
summary(lm(BiG4 ~ MS1 + white + parEd, data = df))

# Unadjusted effect (-0.40)
summary(lm(BiG4 ~ MS1, data = df))

# SEM unadjusted - No auto-regressive paths
sem_unadj_mod <- "BiG4 ~ MS1"
sem_unadj <- sem(sem_unadj_mod, df)
summary(sem_unadj)
parameterestimates(sem_unadj)

# SEM adjusted - No auto-regressive paths
sem_adj_mod <- "BiG4 ~ MS1 + white + parEd"
sem_adj <- sem(sem_adj_mod, df)
summary(sem_adj)
parameterestimates(sem_adj)

# SEM adjusted - Auto-regressive paths
sem_adj_mod_AR <- "
    BiG4 ~ h1a*MS1 + white + parEd
    BiG3 ~ h1b*MS1 + white + parEd
    BiG2 ~ h1c*MS1 + white + parEd

    BiG3 ~ ar3*BiG2 
    BiG4 ~ ar4*BiG3 

    ## Defined parameters
    ms1_big4 := h1a*ar4
    ms1_big4 := h1b*ar4
    h1a_ := h1a
    h1b_ := h1b
    h1c_ := h1c
    sumH1 := h1a_ + h1b_ + h1c_ + ms1_big4
"
sem_adj_AR <- sem(sem_adj_mod_AR, df)
summary(sem_adj_AR)
parameterestimates(sem_adj_AR)


# DMJ's original spec

sem_adj_mod_AR_mis <- "
    BiG4 ~ MS1 + white + parEd
    BiG3 ~ MS1 
    BiG2 ~ MS1

    BiG2 ~ 0*MS1
    BiG3 ~ BiG2 + h1a*MS1
    BiG4 ~ ar4*BiG3 + h1b*MS1

    ## Defined parameters
    ms1_big4 := h1a*ar4
    h1a_ := h1a
    h1b_ := h1b
    sumH1 := h1a_ + h1b_ + ms1_big4
"
sem_adj_AR_mis <- sem(sem_adj_mod_AR_mis, df)
summary(sem_adj_AR_mis)
parameterestimates(sem_adj_AR_mis)

### 
# (1) MS1 - 0 -> BiG1, not BiG2

# corrected

sem_adj_mod_AR_mis_v1 <- "
    BiG4 ~ MS1 + white + parEd
    BiG3 ~ MS1 
    BiG2 ~ MS1

    BiG3 ~ BiG2 + h1a*MS1
    BiG4 ~ ar4*BiG3 + h1b*MS1

    ## Defined parameters
    ms1_big4 := h1a*ar4
    h1a_ := h1a
    h1b_ := h1b
    sumH1 := h1a_ + h1b_ + ms1_big4
"
sem_adj_AR_mis_v1 <- sem(sem_adj_mod_AR_mis_v1, df)
summary(sem_adj_AR_mis_v1)
parameterestimates(sem_adj_AR_mis_v1)

### 
# (2) AR paths should be counted twice for MS1 -> BiG2 -> BiG3 -> BiG4!

# corrected

sem_adj_mod_AR_mis_v2 <- "
    BiG4 ~ MS1 + white + parEd
    BiG2 ~ h1a*MS1

    BiG3 ~ ar3*BiG2 + h1b*MS1
    BiG4 ~ ar4*BiG3 + h1c*MS1



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
"
sem_adj_AR_mis_v2 <- sem(sem_adj_mod_AR_mis_v2, df)
summary(sem_adj_AR_mis_v2)
parameterestimates(sem_adj_AR_mis_v2)

### 
# (3) Controls for all waves

# corrected

sem_adj_mod_AR_mis_v3 <- "
    BiG4 ~ MS1 + white + parEd
    BiG2 ~ h1a*MS1+ white + parEd

    BiG3 ~ ar3*BiG2 + h1b*MS1 + white + parEd
    BiG4 ~ ar4*BiG3 + h1c*MS1+ white + parEd



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
"
sem_adj_AR_mis_v3 <- sem(sem_adj_mod_AR_mis_v3, df)
summary(sem_adj_AR_mis_v3, std = T)
parameterestimates(sem_adj_AR_mis_v3)
