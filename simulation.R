library(lavaan)
library(semTools)
library(semPlot)
library(restriktor)
library(gorica)
library(qpcR)

# Initial simulation ------------------------------------------------------
set.seed(44)

# Specify the true model
model_true <- "

# BiG

BiG1 ~ .2*Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other

BiG2 ~ .05*Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other
BiG2 ~ .7*BiG1 + -.05*MS1

BiG3 ~ .05*Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other
BiG3 ~ .6*BiG2 + .1*PR2 + .1*CR2 + -.12*H2 + -.14*T2 + -.05*MS1

BiG4 ~ .05*Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other + Inc3 + Age1 + PST + .1*ParRit
BiG4 ~ .6*BiG3 + -.3*MS1 +  .1*PR3 + .1*CR3 + -.12*H2 + -.11*T2 + -.25*Edu4 + .05*H3

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ -.1*MS1
CR3 ~ .6*CR2 + MS1 + H2 + -.2*T2
CR4 ~ .6*CR3 + MS1

# PR

PR2 ~ -.1*MS1
PR3 ~ .6*PR2 + MS1 + .2*H2 + T2
PR4 ~ .6*PR3 + MS1

# H

H2 ~ .2*MS1
H3 ~ .7*H2
H4 ~ .7*H3

# T

T2 ~ .2*MS1

# Misc

MS1 ~ -.15*Black + -.05*Latinx + -.05*eth_other + .2*ParEd
Inc3 ~ .25*MS1  
Inc4 ~ .6*Inc3
BlackProt + ConProt + Cath + M_LDS + trad_other ~ eth_other
BlackProt ~ .7*Black
Cath ~ .2*Latinx
Edu4 ~ ParEd + Age1 + MS1
ParRit ~ ParEd

         "

# Use lavaan's simulateData function
sim_data <- lavaan::simulateData(model_true, standardized = F, sample.nobs = 3000)

# Specify the blind model
model <- "

# BiG

BiG1 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 

BiG2 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG2 ~ BiG1 + h1a*MS1

BiG3 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG3 ~ ar3*BiG2 + PR2 + CR2 + h1b*MS1 + h2.1a*H2 + h2.2a*T2

BiG4 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other + Inc3 + Age1 + PST + ParRit
BiG4 ~ ar4*BiG3 + h1c*MS1 + h3.1b*PR3 + h3.2b*CR3 + h2.1b*H2 + h2.2b*T2 + Edu4 + h2.1d*H3

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ MS1
CR3 ~ CR2 + MS1 + H2 + h3.2a*T2
CR4 ~ CR3 + MS1

# PR

PR2 ~ MS1
PR3 ~ PR2 + MS1 + h3.1a*H2 + T2
PR4 ~ PR3 + MS1

# H

H2 ~ MS1
H3 ~ arH*H2
H4 ~ H3

# T

T2 ~ MS1

# Misc

MS1 ~ Black + Latinx + eth_other + ParEd
Inc3 ~ MS1  
Inc4 ~ Inc3
BlackProt + ConProt + Cath + M_LDS + trad_other ~ eth_other
BlackProt ~ Black
Cath ~ Latinx
Edu4 ~ ParEd + Age1 + MS1
ParRit ~ ParEd

# Mediation paths  


## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

## H2 -> BiG3 -> BiG4

h2_big4 := h2.1a*ar4

## T2 -> BiG3 -> BiG4

t2_big4 := h2.2a*ar4

## H2 -> H3 -> BiG4

h2_h3_big4 := arH*h2.1d

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

## H2 -> PR3 -> BiG4

h3.1indirect := h3.1a*h3.1b
h3.1direct := h2.1b

## T2 -> CR3 -> BiG4

h3.2indirect := h3.2a*h3.2b
h3.2direct := h2.2b

"

# Check the result
fit <- lavaan::sem(model, sim_data, meanstructure = T)
lavaan::summary(fit, std = T, rsquare = T)
semPlot::semPaths(fit, what = "std", layout = "spring", intercepts = F)

lavaanExtra::nice_lavaanPlot(fit)


# Hypothesis tests ---------------------------------------------------------


# Test the hypotheses using GORICA

# H1: The (total) effect of MS1 on BiG4 is negative
H1 <- "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

indices <- c(which(standardizedSolution(fit)[, 'label'] == "h1a_"),
             which(standardizedSolution(fit)[, 'label'] == "h1b_"),
             which(standardizedSolution(fit)[, 'label'] == "h1c_"),
             which(standardizedSolution(fit)[, 'label'] == "ms1_big3"),
             which(standardizedSolution(fit)[, 'label'] == "ms1_big4"),
             which(standardizedSolution(fit)[, 'label'] == "h2_big4"),
             which(standardizedSolution(fit)[, 'label'] == "t2_big4"),             
             which(standardizedSolution(fit)[, 'label'] == "h3.1direct"),
             which(standardizedSolution(fit)[, 'label'] == "h3.1indirect"),
             which(standardizedSolution(fit)[, 'label'] == "h3.2direct"),
             which(standardizedSolution(fit)[, 'label'] == "h3.2indirect"),
             which(standardizedSolution(fit)[, 'label'] == "h2_h3_big4"))


est <- standardizedSolution(fit)[indices, 'est.std'] # estimates
VCOV <- lavInspect(fit, "vcov.def.std.all") # cov. matrix

names(est) <- c("h1a_", "h1b_", "h1c_", 
                "ms1_big3", "ms1_big4",
                "h2_big4", "t2_big4",
                "h3.1direct", "h3.1indirect", "h3.2direct", "h3.2indirect",
                "h2_h3_big4")


restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(H1), comparison = "complement")

# H2.1: 
# H2 -> BiG4
# If H1+, then:

H2.1 <- "h3.1direct + h2_big4 + h2_h3_big4 < 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H2.1), comparison = "complement")
# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H2.2), comparison = "complement")

# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.1any = H3.1any), comparison = "complement")


# Is there partial mediation 
H3.1part <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) > 0"

# Is there full mediation
H3.1full <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) = 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.1part = H3.1part,
                    H3.1full = H3.1full), comparison = "unconstrained")

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.1partneg <- "h3.2indirect < 0 ; abs(h3.2direct) > 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.1partneg = H3.1partneg), comparison = "complement")

# H3.2: 
# T2 -> CR3 -> BiG4

# Is there any indirect effect?
H3.2any <- "abs(h3.2indirect) > 0"

# First as complement

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.2any = H3.2any), comparison = "complement")


# Is there partial mediation 
H3.2part <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"

# Is there full mediation
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"


restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(H3.2part = H3.2part,
                                    H3.2full = H3.2full), comparison = "unconstrained")

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.2partneg <- "h3.2indirect < 0 ; abs(h3.2direct) > 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.2partneg = H3.2partneg), comparison = "complement")


# Testing using nested models

EST1 <- "

# BiG

BiG1 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 

BiG2 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG2 ~ BiG1 + 0*MS1

BiG3 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG3 ~ ar3*BiG2 + PR2 + CR2 + 0*MS1 + h2.1a*H2 + h2.2a*T2

BiG4 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other + Inc3 + Age1 + PST + ParRit
BiG4 ~ ar4*BiG3 + 0*MS1 + h3.1b*PR3 + h3.2b*CR3 + h2.1b*H2 + h2.2b*T2 + Edu4 + h2.1d*H3

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ MS1
CR3 ~ CR2 + MS1 + H2 + h3.2a*T2
CR4 ~ CR3 + MS1

# PR

PR2 ~ MS1
PR3 ~ PR2 + MS1 + h3.1a*H2 + T2
PR4 ~ PR3 + MS1

# H

H2 ~ MS1
H3 ~ arH*H2
H4 ~ H3

# T

T2 ~ MS1

# Misc

MS1 ~ Black + Latinx + eth_other + ParEd
Inc3 ~ MS1  
Inc4 ~ Inc3
BlackProt + ConProt + Cath + M_LDS + trad_other ~ eth_other
BlackProt ~ Black
Cath ~ Latinx
Edu4 ~ ParEd + Age1 + MS1
ParRit ~ ParEd



"


fit_est1 <- lavaan::sem(EST1, sim_data, meanstructure = T)
summary(fit_est1, std = T, fit = T)

EST2 <- "

# H1 and H2 paths fixed to 0

# BiG

BiG1 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 

BiG2 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG2 ~ BiG1 + 0*MS1

BiG3 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other 
BiG3 ~ ar3*BiG2 + PR2 + CR2 + 0*MS1 + 0*H2 + 0*T2

BiG4 ~ Female + BlackProt + ConProt + Cath + trad_other + Black + Latinx + eth_other + Inc3 + Age1 + PST + ParRit
BiG4 ~ ar4*BiG3 + 0*MS1 + h3.1b*PR3 + h3.2b*CR3 + 0*H2 + 0*T2 + Edu4 + h2.1d*H3

eta_BiG =~ 1*BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

# CR

CR2 ~ MS1
CR3 ~ CR2 + MS1 + H2 + h3.2a*T2
CR4 ~ CR3 + MS1

# PR

PR2 ~ MS1
PR3 ~ PR2 + MS1 + h3.1a*H2 + T2
PR4 ~ PR3 + MS1

# H

H2 ~ MS1
H3 ~ arH*H2
H4 ~ H3

# T

T2 ~ MS1

# Misc

MS1 ~ Black + Latinx + eth_other + ParEd
Inc3 ~ MS1  
Inc4 ~ Inc3
BlackProt + ConProt + Cath + M_LDS + trad_other ~ eth_other
BlackProt ~ Black
Cath ~ Latinx
Edu4 ~ ParEd + Age1 + MS1
ParRit ~ ParEd



"

fit_est2 <- lavaan::sem(EST2, sim_data, meanstructure = T)
summary(fit_est2, std = T, fit = T)

# Compare full vs. EST model
anova(fit_est1, fit_est2, fit) 

qpcR::akaike.weights(anova(fit_est1, fit_est2, fit)$AIC) # get AIC weights
