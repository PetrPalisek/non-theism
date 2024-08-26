ef2 <- read.csv("efekt_BiG2.csv", stringsAsFactors = FALSE)
ef4 <- read.csv("efekt_BiG4.csv", stringsAsFactors = FALSE)


###########
# Eefekt BiG4

model <- "

          # AR paths
          
          # AR paths: BiG
          BiG4 ~ BiG3
          BiG3 ~ BiG2
          BiG4 ~ BiG1
          
          
          # Explanatory paths
          BiG4 ~ h1*MS
          
          
          # Unit effects
          eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
          
          "

# Check the result
fit <- lavaan::sem(model, ef4, meanstructure = T)
lavaan::summary(fit, std = T, rsquare = T)


###########
# Eefekt BiG2

model <- "

          # AR paths
          
          # AR paths: BiG
          BiG4 ~ BiG3
          BiG3 ~ BiG2
          BiG4 ~ BiG1
          
          
          # Explanatory paths
          BiG4 ~ h1*MS
          
          # Unit effects
          eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
          
          "

# Check the result
fit <- lavaan::sem(model, ef2, meanstructure = T)
lavaan::summary(fit, std = T, rsquare = T)

