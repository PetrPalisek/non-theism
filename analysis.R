library(lavaan)
library(semTools)
library(semPlot)
library(restriktor)
library(gorica)
library(qpcR)
library(readxl)
library(tidyverse)
library(fastDummies)

# Load data
w1 <- readxl::read_excel("nsyr1.xlsx")
w2 <- readxl::read_excel("nsyr2.xlsx")
w3 <- readxl::read_excel("nsyr3.xlsx")
w4 <- readxl::read_excel("nsyr4.xlsx")


# Selecting relevant vars
w1 <- w1 %>%
      select(IDS, I_GENDER, PRACE, ETHRACE, AGECATS,
             PINCOME, GOD, PEDUC1, PSPEDUC1, PEDUC3,
             PATTEND, PSTRESS, RELTRAD)

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
             PRAYALON)

# Adding suffixes to distinguish waves
names(w3)[2:ncol(w3)] <- paste(names(w3)[2:ncol(w3)], "_W3", sep = "")

w4 <- w4 %>%
  select(IDS, GOD_W4, HEALTH_W4, ATTREG_W4, ATTEND1_W4, 
         PRAYALON_W4, EARNINGS_W4)

# Merging

df <- merge(w1, w2, by = "IDS")
df <- merge(df, w3, by = "IDS")
df <- merge(df, w4, by = "IDS")

# Coding NAs
table(df$GOD_W4)

df$PRACE_W1 <- ifelse(df$PRACE_W1 == 888, NA, df$PRACE_W1)
df$ETHRACE_W1 <- ifelse(df$ETHRACE_W1 > 776, NA, df$ETHRACE_W1)
df$PINCOME_W1 <- ifelse(df$PINCOME_W1 > 776, NA, df$PINCOME_W1)

df$GOD_W1 <- ifelse(df$GOD_W1 == 888, NA, df$GOD_W1)
df$GOD_W2 <- ifelse(df$GOD_W2 > 4, NA, df$GOD_W2)
df$GOD_W3 <- ifelse(df$GOD_W3 > 4, NA, df$GOD_W3)
df$GOD_W4 <- ifelse(df$GOD_W4 > 4, NA, df$GOD_W4)

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

df$HEALTH_W3 <- ifelse(df$HEALTH_W3 == 999, NA, df$TRUST_W2)

df$EARNINGS_W4 <- ifelse(df$EARNINGS_W4 == 17, NA, df$EARNINGS_W4)


# Making sure all vars have the same scales over waves

# df$GOD_W1 yes / no / unsure ( 1, 2, 777)
# df$GOD_W2 yes / no / unsure ( 1, 2, 3)
# df$GOD_W3 yes / no / unsure ( 1, 2, 3)
# df$GOD_W4 no / yes / unsure ( 0, 1, 2)

df$GOD_W1 <- ifelse(df$GOD_W1 == 777, 1.5, df$GOD_W1)
df$GOD_W2 <- ifelse(df$GOD_W2 == 3, 1.5, df$GOD_W2)
df$GOD_W3 <- ifelse(df$GOD_W3 == 3, 1.5, df$GOD_W3)


df$GOD_W4 <- ifelse(df$GOD_W4 == 0, 2,
                    ifelse(df$GOD_W4 == 1, 1,
                           ifelse(df$GOD_W4 == 2, 1.5, df$GOD_W4)))

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
    ETHRACE_W1 == 8 ~ "Asian",
    ETHRACE_W1 == 9 ~ "Asian",
    ETHRACE_W1 == 10 ~ "Other",
    ETHRACE_W1 == 11 ~ "Native",
    ETHRACE_W1 == 12 ~ "Native",
    ETHRACE_W1 == 14 ~ "Other",
    ETHRACE_W1 == 15 ~ "Other",
  )) %>%
  mutate(RELTRAD_W1 = case_when(
    RELTRAD_W1 == 1 ~ "ConProt",
    RELTRAD_W1 == 2 ~ "MainProt",
    RELTRAD_W1 == 3 ~ "BlackProt",
    RELTRAD_W1 == 4 ~ "Catholic",
    RELTRAD_W1 == 5 ~ "Jewish",
    RELTRAD_W1 == 6 ~ "Mormon",
    RELTRAD_W1 == 7 ~ "None",
    RELTRAD_W1 == 8 ~ "Other",
    RELTRAD_W1 == 9 ~ "Indeter",
  ))

# Creating dummy variables
df[, c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")] <- lapply(df[, c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")], factor)

dummies <- fastDummies::dummy_cols(df[,c("I_GENDER_W1", "PRACE_W1", "ETHRACE_W1", "RELTRAD_W1")],
                                   remove_most_frequent_dummy = T, ignore_na = T, 
                                   remove_selected_columns = T)

names(dummies) <- c("Female", "AsianPR", "BlackPR", "LatinxPR", "NativePR", "OtherPR", 
                    "AsianE", "BlackE", "LatinxE", "NativeE", "OtherE",
                    "BlackProt", "Catholic", "Indeter", "Jewish","MainProt", "Mormon", "None", "OtherRel")

df <- cbind(df, dummies)

# Deleting non-Christian participants (WHAT TO DO WITH Unaffiliated, other and indeterminate?)

table(df$RELTRAD_W1)

df <- df[df$RELTRAD_W1 != "Jewish",]

# Renaming vars to match preregistration

names(df) <- c("id", "Gender", "PRACE", "ETHRACE", "Age", "MS1", "BiG1",
               "PEDUC1", "PSPEDUC", "PEDUC3", "ParRit", "PST", "RELTRAD", "BiG2",
               "H2", "CR2_1", "CR2_2", "PR2", "T2", "BiG3", "H3", "CR3_1", "CR3_2",
               "PR3", "BiG4", "H4", "CR4_1", "CR4_2", "PR4", "Inc4", names(df)[31:ncol(df)])

# Descriptives ------------------------------------------------------------
table(df$BiG1)

# H3?

base <- "
   BiG3 ~ Female + BlackProt + Catholic + MainProt + Mormon + None + OtherRel + AsianE + BlackE + LatinxE + NativeE + OtherE
   BiG4 ~ Age + Female + BlackProt + Catholic + MainProt + Mormon + None + OtherRel + AsianE + BlackE + LatinxE + NativeE + OtherE
   BiG3 ~ BiG2 + MS1 + H2 + T2
   BiG4 ~ BiG3 + MS1 + H2 + T2
   
"

base_fit <- sem(base, df, ordered = T, estimator = "WLSMV")

summary(base_fit, std = T, fit = T)
