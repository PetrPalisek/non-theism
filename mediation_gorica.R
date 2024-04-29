library(lavaan)
library(semTools)
library(semPlot)
library(restriktor)

# Initial simulation ------------------------------------------------------


# Specify the true model
model_true <- "

# BiG
          
BiG1 ~ .1*Gender + .1*RelTrad + .1*Ethn + .2*RelEd

BiG2 ~ .1*Gender + .1*RelTrad + -.1*PST + .1*Ethn + .1*RelEd
BiG2 ~ .6*BiG1 + -.3*MS1

BiG3 ~ .1*Gender + .1*RelTrad + -.1*PST + .1*Ethn + .1*RelEd
BiG3 ~ .6*BiG2 + -.2*MS1 + .3*PR2 + .3*CR2 + -.2*H2 + -.2*T2

BiG4 ~ .1*Gender + .1*RelTrad + -.1*PST + .1*Ethn + .1*RelEd + -.25*Inc3 + .05*Age
BiG4 ~ .6*BiG3 + -.3*MS1 +  .3*PR3 + .3*CR3 + -.2*H2 + -.2*T2 + -.2*Edu

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ -.1*MS1
CR3 ~ .5*CR2 + -.1*MS1 + -.05*H2 + .15*T2
CR4 ~ .5*CR3 + -.1*MS1

# PR

PR3 ~ .5*PR2 + -.1*H2 + 0*T2

# H

H2 ~ .1*MS1
H3 ~ .8*H2

# T

T2 ~ .1*MS1
T3 ~ .5*T2 + .1*MS1

# Misc

MS1 ~ -.2*Ethn + .4*ParEd
Inc3 ~ .3*MS1          
RelTrad ~ .3*Ethn
Edu ~ .2*ParEd + .3*Age + .4*MS1
ParRit ~ -.2*ParEd
RelEd ~ .2*ParRit

         "

# Use lavaan's simulateData function
sim_data <- lavaan::simulateData(model_true, standardized = F, sample.nobs = 3000)

# Specify the blind model
model <- "

# BiG

BiG1 ~ Gender + RelTrad + Ethn + RelEd

BiG2 ~ Gender + RelTrad + PST + Ethn + RelEd
BiG2 ~ BiG1 + MS1

BiG3 ~ Gender + RelTrad + PST + Ethn + RelEd
BiG3 ~ BiG2 + MS1 + PR2 + CR2 + H2 + T2

BiG4 ~ Gender + RelTrad + PST + Ethn + RelEd + Inc3 + Age
BiG4 ~ BiG3 + h1*MS1 +  h3.1b*PR3 + h3.2b*CR3 + h2.1*H2 + h2.2*T2 + h4*Edu

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ MS1
CR3 ~ CR2 + MS1 + H2 + h3.2a*T2
CR4 ~ CR3 + h5.1*MS1

# PR

PR3 ~ PR2 + h3.1a*H2 + T2

# H

H2 ~ MS1
H3 ~ H2

# T

T2 ~ MS1
T3 ~ T2 + MS1

# Misc

MS1 ~ Ethn + ParEd
Inc3 ~ MS1          
RelTrad ~ Ethn
Edu ~ ParEd + Age + MS1
ParRit ~ ParEd
RelEd ~ ParRit

# Mediation paths  

## H2 -> PR3 -> BiG4

h3.1indirect := h3.1a*h3.1b
h3.1direct := h2.1

## T2 -> CR3 -> BiG4

h3.2indirect := h3.2a*h3.2b
h3.2direct := h2.2
"

# Check the result
fit <- lavaan::sem(model, sim_data, meanstructure = T)
lavaan::summary(fit, std = T, rsquare = T)

H3 <- "h3.1indirect > 0"

fit.std <- standardizedSolution(fit)
which(fit.std[, 'label'] == "h3.1indirect")
which(fit.std[, 'label'] == "h3.1direct")
which(fit.std[, 'label'] == "h3.2indirect")
which(fit.std[, 'label'] == "h3.2direct")
indices <- 133:136 # e.g, 10:11 # Base this on results above
est <- fit.std[indices, 'est.std'] # estimates
VCOV <- lavInspect(fit, "vcov.def.std.all") # cov. matrix

names(est)[1] <- "h3.1indirect"

restriktor::goric(est, hypotheses = list(H3), 
                  VCOV = VCOV)
