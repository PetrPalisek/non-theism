library(lavaan)
library(semTools)
library(semPlot)
library(restriktor)
library(gorica)
library(qpcR)
library(readxl)
library(tidyverse)
library(fastDummies)
library(Amelia)
library(psych)

# Load data
w1 <- readxl::read_excel("nsyr1.xlsx")
w2 <- readxl::read_excel("nsyr2.xlsx")
w3 <- readxl::read_excel("nsyr3.xlsx")
w4 <- readxl::read_excel("nsyr4.xlsx")


# Selecting relevant vars
w1 <- w1 %>%
      select(IDS, I_GENDER, PRACE, ETHRACE, AGECATS,
             PINCOME, GOD, PEDUC1, PSPEDUC1, PEDUC3,
             PATTEND, PSTRESS, RELTRAD, PSPEDUC3)

# Adding suffixes to distinguish waves
names(w1)[2:ncol(w1)] <- paste(names(w1)[2:ncol(w1)], "_W1", sep = "")

# Selecting relevant vars
w2 <- w2 %>%
      select(IDS, GOD, HEALTH, ATTREG, ATTEND1, 
             PRAYALON, TRUST)

# Adding suffixes to distinguish waves
names(w2)[2:ncol(w2)] <- paste(names(w2)[2:ncol(w2)], "_W2", sep = "")


# Selecting relevant vars
w3 <- w3 %>%
      select(IDS, GOD, HEALTH, ATTREG, ATTEND1, 
             PRAYALON, EARNINGS)

# Adding suffixes to distinguish waves
names(w3)[2:ncol(w3)] <- paste(names(w3)[2:ncol(w3)], "_W3", sep = "")

w4 <- w4 %>%
  select(IDS, GOD_W4, HEALTH_W4, ATTREG_W4, ATTEND1_W4, 
         PRAYALON_W4, EARNINGS_W4, EDATT_W4)

# Merging

df <- merge(w1, w2, by = "IDS", all = T)
df <- merge(df, w3, by = "IDS", all = T)
df <- merge(df, w4, by = "IDS", all = T)

# Coding NAs
table(df$GOD_W4)

df$PRACE_W1 <- ifelse(df$PRACE_W1 == 888, NA, df$PRACE_W1)
df$ETHRACE_W1 <- ifelse(df$ETHRACE_W1 > 776, NA, df$ETHRACE_W1)
df$PINCOME_W1 <- ifelse(df$PINCOME_W1 > 776, NA, df$PINCOME_W1)


df$PEDUC1_W1 <- ifelse(df$PEDUC1_W1 > 4, NA, df$PEDUC1_W1)
df$PSPEDUC1_W1 <- ifelse(df$PSPEDUC1_W1 > 4, NA, df$PSPEDUC1_W1)
df$PEDUC3_W1 <- ifelse(df$PEDUC3_W1 > 12, NA, df$PEDUC3_W1)

df$PATTEND_W1 <- ifelse(df$PATTEND_W1 == 888, NA, df$PATTEND_W1)
df$PSTRESS_W1 <- ifelse(df$PSTRESS_W1 > 5, NA, df$PSTRESS_W1)

df$HEALTH_W2 <- ifelse(df$HEALTH_W2 == 666, NA, df$HEALTH_W2)

df$ATTREG_W2 <- ifelse(df$ATTREG_W2 == 666, NA, df$ATTREG_W2)

df$ATTEND1_W2 <- ifelse(df$ATTEND1_W2 > 7, NA, df$ATTEND1_W2)
df$ATTEND1_W3 <- ifelse(df$ATTEND1_W3 > 7, NA, df$ATTEND1_W3)
df$ATTEND1_W4 <- ifelse(df$ATTEND1_W4 == -99, NA, df$ATTEND1_W4)


df$PRAYALON_W2 <- ifelse(df$PRAYALON_W2 > 7, NA, df$PRAYALON_W2)
df$PRAYALON_W3 <- ifelse(df$PRAYALON_W3 > 7, NA, df$PRAYALON_W3)


df$TRUST_W2 <- ifelse(df$TRUST_W2 > 4, NA, df$TRUST_W2)

df$HEALTH_W3 <- ifelse(df$HEALTH_W3 == 999, NA, df$HEALTH_W3)

df$EARNINGS_W3 <- ifelse(df$EARNINGS_W3 > 27, NA, df$EARNINGS_W3)
df$EARNINGS_W4 <- ifelse(df$EARNINGS_W4 == 17, NA, df$EARNINGS_W4)


# Making sure all vars have the same scales over waves

# df$GOD_W1 yes / no / unsure ( 1, 2, 777)
# df$GOD_W2 yes / no / unsure ( 1, 2, 3)
# df$GOD_W3 yes / no / unsure ( 1, 2, 3)
# df$GOD_W4 no / yes / unsure ( 0, 1, 2)

# Check all NAs  

table(df$GOD_W1)
table(df$GOD_W2)
table(df$GOD_W3)
table(df$GOD_W4)

df <- df %>%
  mutate(GOD_W1 = case_when(
    GOD_W1 == 1 ~ "1",
    GOD_W1 == 2 ~ "0",
    GOD_W1 == 777 ~ ".5",
    .default = NA
  )) %>%
  mutate(GOD_W2 = case_when(
    GOD_W2 == 1 ~ "1",
    GOD_W2 == 2 ~ "0",
    GOD_W2 == 3 ~ ".5",
    .default = NA
  )) %>%
  mutate(GOD_W3 = case_when(
    GOD_W3 == 1 ~ "1",
    GOD_W3 == 2 ~ "0",
    GOD_W3 == 3 ~ ".5",
    .default = NA
  )) %>%
  mutate(GOD_W4 = case_when(
    GOD_W4 == 1 ~ "1",
    GOD_W4 == 0 ~ "0",
    GOD_W4 == 2 ~ ".5",
    .default = NA
  )) %>%
  mutate(TRUST_W2 = case_when(
    TRUST_W2 == 2 ~ "0", 
    TRUST_W2 == 3 ~ "0.5",
    TRUST_W2 == 1 ~ "1"
  ))


# Recoding factor variable values to characters

df <- df %>%
  mutate(I_GENDER_W1 = case_when(
    I_GENDER_W1 == 1 ~ "Female",
    I_GENDER_W1 == 2 ~ "Male",
    TRUE ~ as.character(I_GENDER_W1)  # Keep other values unchanged
  )) %>%
  mutate(PRACE_W1 = case_when(
    PRACE_W1 == 1 ~ "White",
    PRACE_W1 == 2 ~ "Black",
    PRACE_W1 == 3 ~ "Black",
    PRACE_W1 == 4 ~ "Latinx",
    PRACE_W1 == 5 ~ "Asian",
    PRACE_W1 == 6 ~ "Asian",
    PRACE_W1 == 7 ~ "Other",
    PRACE_W1 == 8 ~ "Native",
    PRACE_W1 == 9 ~ "Native",
    PRACE_W1 == 10 ~ "Other",
    PRACE_W1 == 11 ~ "Other",
  )) %>%
  mutate(ETHRACE_W1 = case_when (
    ETHRACE_W1 == 1 ~ "White",
    ETHRACE_W1 == 2 ~ "White",
    ETHRACE_W1 == 3 ~ "White",
    ETHRACE_W1 == 4 ~ "Black",
    ETHRACE_W1 == 5 ~ "Black",
    ETHRACE_W1 == 6 ~ "Latinx",
    ETHRACE_W1 == 7 ~ "Latinx",
    ETHRACE_W1 == 8 ~ "Other",
    ETHRACE_W1 == 9 ~ "Other",
    ETHRACE_W1 == 10 ~ "Other",
    ETHRACE_W1 == 11 ~ "Other",
    ETHRACE_W1 == 12 ~ "Other",
    ETHRACE_W1 == 14 ~ "Other",
    ETHRACE_W1 == 15 ~ "Other",
  )) %>%
  mutate(RELTRAD_W1 = case_when(
    RELTRAD_W1 == 1 ~ "ConProt",
    RELTRAD_W1 == 2 ~ "MainProt",
    RELTRAD_W1 == 3 ~ "BlackProt",
    RELTRAD_W1 == 4 ~ "Catholic",
    RELTRAD_W1 == 5 ~ "Jewish",
    RELTRAD_W1 == 6 ~ "Other",
    RELTRAD_W1 == 7 ~ "None",
    RELTRAD_W1 == 8 ~ "Other",
    RELTRAD_W1 == 9 ~ "Other",
  ))

# Create education vars

# Education at W4
df <- df %>%
      mutate(EDATT_W4 = case_when(
        EDATT_W4 == 1 ~ "0",
        EDATT_W4 == 2 ~ "1",
        EDATT_W4 == 3 ~ "2",
        EDATT_W4 == 4 ~ "3",
        EDATT_W4 == 5 ~ "3"
      ))

df$College <- ifelse(df$EDATT_W4 == "3", 1, 0)
df$HighSchool <- ifelse(df$EDATT_W4 == "2", 1, 0)

# Parental education at W1
df <- df %>% 
      mutate(PEDUC1_W1 = case_when(
        PEDUC1_W1 == 1 ~ "0", # < HS
        PEDUC1_W1 == 2 ~ "1", # HS
        PEDUC1_W1 == 3 ~ "2", # > HS
        PEDUC1_W1 == 777 ~ NA,
        PEDUC1_W1 == 888 ~ NA,
        PEDUC1_W1 == 999 ~ NA
      )) %>% 
  mutate(PSPEDUC1_W1 = case_when(
    PSPEDUC1_W1 == 1 ~ "0", # < HS
    PSPEDUC1_W1 == 2 ~ "1", # HS
    PSPEDUC1_W1 == 3 ~ "2", # > HS
    PSPEDUC1_W1 == 777 ~ NA,
    PSPEDUC1_W1 == 888 ~ NA,
    PSPEDUC1_W1 == 999 ~ NA 
  ))

# Treat DiS, BA, MA, PhD and prof degree as college, others as HS
df$FirstParEd <- ifelse(df$PEDUC1_W1 == "2" & df$PEDUC3_W1 %in% c(5, 6, 8, 10, 11), "3", df$PEDUC1_W1)
df$SecondParEd <- ifelse(df$PSPEDUC1_W1 == "2" & df$PSPEDUC3_W1 %in% c(5, 6, 8, 10, 11), "3", df$PSPEDUC1_W1)


# Maybe we want both college, only one college, both HS, one HS, rest?
df$ParEd <- paste0(df$FirstParEd, df$SecondParEd)
df <- df %>% 
      mutate(ParEd_ord = case_when(
        ParEd == "33" ~ 5,
        ParEd %in% c("03", "13", "23", "30", "31", "32", "3NA") ~ 4,
        ParEd == "22" ~ 3,
        ParEd %in% c("02", "12", "20", "21", "2NA") ~ 2,
        .default = 1
      ))

df$ParCollege <- ifelse(df$ParEd_ord %in% c(4, 5), 1, 0)
df$ParHighSchool <- ifelse(df$ParEd_ord %in% c(2, 3), 1, 0)


# Creating dummy variables
df[, c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")] <- lapply(
  df[, c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")], factor
  )

dummies <- fastDummies::dummy_cols(df[,
                                      c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")],
                                   remove_most_frequent_dummy = T, ignore_na = T, 
                                   remove_selected_columns = T)

names(dummies) <- c("Male", "AsianPR", "BlackPR", "LatinxPR", "NativePR", "OtherPR", 
                    "BlackE", "LatinxE", "OtherE",
                    "BlackProt", "Catholic", "Jewish","MainProt", "None", "OtherRel")

dummies_cors <- round(cor(dummies, use = "pairwise"),2)

psych::describe(dummies)

df <- cbind(df, dummies)

# Deleting non-Christian participants (WHAT TO DO WITH Unaffiliated, other and indeterminate?)

table(df$RELTRAD_W1)

df <- df[!(df$RELTRAD_W1 %in% c("Jewish", "Other", "None")),]
df <- df[df$GOD_W1 != "0",] # Remove W1 non-believers

# Renaming vars to match preregistration

names(df) <- c("id", "Gender", "PRACE", "ETHRACE", "Age", "MS1", "BiG1",
               "PEDUC1", "PSPEDUC", "PEDUC3", "ParRit", "PST", "RELTRAD", "PSPEDUC3", "BiG2",
               "H2", "CR2_1", "CR2_2", "PR2", "T2", "BiG3", "H3", "CR3_1", "CR3_2",
               "PR3", "Inc3", "BiG4", "H4", "CR4_1", "CR4_2", "PR4", "Inc4", "EDATT_W4", names(df)[34:ncol(df)])


df$CR2 <- ifelse(df$CR2_1 == 0, 0,  df$CR2_2)
df$CR3 <- ifelse(df$CR3_1 == 0, 0,  df$CR3_2)
df$CR4 <- ifelse(df$CR4_1 == 0, 0,  df$CR4_2)

df$MS1 <- as.numeric(df$MS1)
df$ParRit <- ifelse(df$ParRit == 777, NA, df$ParRit)

# Descriptives ------------------------------------------------------------

Amelia::missmap(df, rank.order = F)

na_sum <- data.frame(lapply(df, function(x) sum(is.na(x))))
na_sum[2,] <- round(na_sum[1,]/nrow(df)*100,2)
na_sum <- t(na_sum) # % missing per var

# missing in BiG
Amelia::missmap(df[,c("BiG1", "BiG2", "BiG3", "BiG4")], rank.order = F)
df$god_na_sum <- rowSums(is.na(df[,c("BiG1", "BiG2", "BiG3", "BiG4")]))

psych::describe(df[,c("BiG1", "BiG2", "BiG3", "BiG4")])


#M: Can we have a look at how much missing data do we have?
# make sure we do with missing data what we promised to do
# I know we had some worries that those who are likely to drop out may be also likely to gave-up belief

#M: I am thinking about plotting the overall trends in beliefs across waves, can explain in person
# a plot that would be nicely descriptive and would give a lot of information about the demographics and missingess

base <- "
  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   BiG2 ~*~ 1*BiG2
"

base_fit <- sem(base, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4"), meanstructure = T,
                   estimator = "WLSMV", missing = "pairwise", parameterization = "theta")

summary(base_fit, std = T, fit = T)

base_h1 <- "h1a + h1b + h1c < 0"

restriktor::goric(base_fit,
                  hypotheses = list(base_h1), comparison = "complement")

controls <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE + Inc3 + ParRit

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ College + HighSchool
  
"

# P: What was this supposed to do?
# BlackProt + ConProt + Cath + M_LDS + trad_other ~ eth_other


controls_fit <- sem(controls, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit"), meanstructure = T,
                   estimator = "WLSMV", missing = "pairwise", parameterization = "theta")

summary(controls_fit, std = T, fit = T)

restriktor::goric(controls_fit,
                  hypotheses = list(base_h1), comparison = "complement")


full <- "
  BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE + Inc3 + ParRit

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 + h2.1a*H2 + h2.2a*T2 + PR2 + CR2
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h3.1b*PR3 + h3.2b*CR3 + h2.1d*H3
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
   
  # PR

   PR2 ~ MS1
   PR3 ~ PR2 + MS1 + h3.1a*H2 + T2
   
   # H

   H2 ~ MS1
   H3 ~ arH*H2
   
   # T

   T2 ~ MS1
   
   # CR

   CR2 ~ MS1
   CR3 ~ CR2 + MS1 + H2 + h3.2a*T2

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ College + HighSchool
  
     PR2 ~ 0
   PR3 ~ 0
  

"

full <- "# PR

   PR2 ~ MS1
   PR3 ~ PR2 + MS1 + h3.1a*H2 + T2
   
   PR2 ~ 0
   PR3 ~ 0
   
   # H

   H2 ~ MS1
   H3 ~ arH*H2
   
   # T

   T2 ~ MS1
   
   # CR

   CR2 ~ MS1
   CR3 ~ CR2 + MS1 + H2 + h3.2a*T2"

full_fit <- lavaan(full, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit",
                                      "CR2", "CR3", "H2", "H3", "T2"), meanstructure = T,
                estimator = "WLSMV", missing = "pairwise", parameterization = "theta")


summary(full_fit, std = T, fit = T)

modificationindices(full_fit, sort. = T)

" 

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
<<<<<<< Updated upstream
h3.2direct := h2.2b

"

fit <- sem(base, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4",
                                      "CR2", "CR3", "PR2", "PR3", "H2", "H3", "T2"), meanstructure = T,
                estimator = "WLSMV", missing = "pairwise", parameterization = "theta")

lavaanExtra::nice_lavaanPlot(base_fit, stand = T)

summary(base_fit, std = T, fit = T)

modificationindices(base_fit, sort. = T)

=======
h3.2direct := h2.2b"
>>>>>>> Stashed changes

# Hypothesis tests --------------------------------------------------------
fit <- base_fit

  
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
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) = 0"


restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(H3.2part = H3.2part,
                                    H3.2full = H3.2full), comparison = "unconstrained")

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.2partneg <- "h3.2indirect < 0 ; abs(h3.2direct) > 0"

restriktor::goric(est, VCOV = VCOV,
                  hypotheses = list(
                    H3.2partneg = H3.2partneg), comparison = "complement")

