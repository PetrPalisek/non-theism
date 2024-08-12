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

source("https://raw.githubusercontent.com/PetrPalisek/gorica_helpers/main/extract_defined_params_lavaanmi.R")
source("https://raw.githubusercontent.com/PetrPalisek/gorica_helpers/main/extract_defined_params_lavaan.R")


set.seed(3333)

# Load data
w1 <- readxl::read_excel("nsyr1.xlsx")
w2 <- readxl::read_excel("nsyr2.xlsx")
w3 <- readxl::read_excel("nsyr3.xlsx")
w4 <- readxl::read_excel("nsyr4.xlsx")


# Selecting relevant vars
w1 <- w1 %>%
  select(IDS, I_GENDER, PRACE, ETHRACE, AGECATS,
         PINCOME, GOD, PEDUC1, PSPEDUC1, PEDUC3,
         PATTEND, PSTRESS, RELTRAD, PSPEDUC3, BNPRLCAT, BNPRLPRT)

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

table(df$GOD_W1)
table(df$GOD_W2)
table(df$GOD_W3)
table(df$GOD_W4)

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
    RELTRAD_W1 == 9 ~ "INDE",
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

# Participant's education
df$College <- ifelse(df$EDATT_W4 == "3", 1, 0) # Highest education == some college
df$HighSchool <- ifelse(df$EDATT_W4 == "2", 1, 0) # Highest education == high school

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
                    "BlackProt", "Catholic", "INDE", "Jewish","MainProt", "None", "OtherRel")

dummies_cors <- round(cor(dummies, use = "pairwise"),2)

psych::describe(dummies)

df <- cbind(df, dummies)



table(df$RELTRAD_W1)

# Deleting non-Christian participants 
df <- df[!(df$RELTRAD_W1 %in% c("Jewish", "Other")),] 

# Deleting unaffiliates with no Christian parent
df <- df[!(df$RELTRAD_W1 == "INDE" & df$BNPRLCAT_W1 == 0 & df$BNPRLPRT_W1 == 0),] 

# Remove W1 non-believers
df <- df[df$GOD_W1 != "0",] 

# Renaming vars to match preregistration

names(df) <- c("id", "Gender", "PRACE", "ETHRACE", "Age", "MS1", "BiG1",
               "PEDUC1", "PSPEDUC", "PEDUC3", "ParRit", "PST", "RELTRAD", "PSPEDUC3", "ParCatholic", "ParProtestant","BiG2",
               "H2", "CR2_1", "CR2_2", "PR2", "T2", "BiG3", "H3", "CR3_1", "CR3_2",
               "PR3", "Inc3", "BiG4", "H4", "CR4_1", "CR4_2", "PR4", "Inc4", "EDATT_W4", names(df)[36:ncol(df)])


df$CR2 <- ifelse(df$CR2_1 == 0, 0,  df$CR2_2)
df$CR3 <- ifelse(df$CR3_1 == 0, 0,  df$CR3_2)
df$CR4 <- ifelse(df$CR4_1 == 0, 0,  df$CR4_2)

df$MS1 <- as.numeric(df$MS1)
df$ParRit <- ifelse(df$ParRit == 777, NA, 8-df$ParRit) # revert ParRit

df$BiG1 <- factor(df$BiG1, levels = c("0", ".5", "1"))
df$BiG2 <- factor(df$BiG2, levels = c("0", ".5", "1"))
df$BiG3 <- factor(df$BiG3, levels = c("0", ".5", "1"))
df$BiG4 <- factor(df$BiG4, levels = c("0", ".5", "1"))

df$T2 <- factor(df$T2)

df$H2 <- factor(df$H2)
df$H3 <- factor(df$H3 )
df$H4 <- factor(df$H4 )

df$PR2 <- factor(df$PR2)
df$PR3 <- factor(df$PR3)
df$PR4 <- 8-df$PR4 # Revert PR at T4
df$PR4 <- factor(df$PR4)

df$CR2 <- factor(df$CR2)
df$CR3 <- factor(df$CR3)
df$CR4 <- factor(df$CR4)

df$ParRit <- factor(df$ParRit )
df$PST <- factor(df$PST )

df$Age <- as.vector(scale(ifelse(df$Age == 888, NA, df$Age)))
df$Inc3 <-  as.vector(scale(df$Inc3))

str(df)


# Descriptives ------------------------------------------------------------

Amelia::missmap(df, rank.order = F)

na_sum <- data.frame(lapply(df, function(x) sum(is.na(x))))
na_sum[2,] <- round(na_sum[1,]/nrow(df)*100,2)
na_sum <- t(na_sum) # % missing per var

# missing in BiG
Amelia::missmap(df[,c("BiG1", "BiG2", "BiG3", "BiG4")], rank.order = F)
df$god_na_sum <- rowSums(is.na(df[,c("BiG1", "BiG2", "BiG3", "BiG4")]))

table(df$god_na_sum)

psych::describe(df[,c("BiG1", "BiG2", "BiG3", "BiG4")])

df$focal_na <- rowSums(is.na(df[,c("BiG1", "BiG2", "BiG3", "BiG4",
                                   "MS1", "H2", "T2", "PR3", "CR3")]))

sum(df$focal_na > 0)

Amelia::missmap(df[,c("BiG1", "BiG2", "BiG3", "BiG4",
                      "MS1", "H2", "T2", "PR3", "CR3")], rank.order = F)

psych::describe(df[,c("BiG1", "BiG2", "BiG3", "BiG4",
                      "MS1", "H2", "T2", "PR3", "CR3")])

table(df$focal_na)


# Plots -------------------------------------------------------------------

library(ggalluvial)

long_df <- df %>%
  pivot_longer(cols = starts_with("BiG"), names_to = "TimePoint", values_to = "Belief") %>%
  mutate(TimePoint = as.integer(gsub("BiG", "", TimePoint)),
         Belief = ifelse(is.na(Belief), "NA", as.character(Belief)),
         Belief = factor(Belief, ordered = TRUE, levels = c("NA", "0", ".5", "1")))

# Plot the data using stacked bar plot
ggplot(long_df, aes(x = TimePoint, fill = Belief)) +
  geom_bar(position = "stack", width = 0.7) +
  labs(title = "Changes in Belief in God over Time",
       x = "Time Point",
       y = "Count",
       fill = "Belief") +
  theme_minimal()



# Imputation --------------------------------------------------------------


# Number of imputed df 
N.Imp <-  50

# Create a column that contain the appropreate model for each variable - this only works if the variable is of the correct data type in the first place 
names(df)

type <- c("", "", "", "", "","pmm", "polr",
          "", "", "", "polr", "polr", "", "",
          "", "", "polr", "polr", "", "", "polr",
          "polr", "polr", "polr", "", "", "polr", "pmm",
          "polr", "polr", "", "", "polr", "pmm", "",
          "", "", "", "", "", "", "",
          "", "", "", "", "", "", "",
          "", "", "", "", "", "", "",
          "", "", "", "polr", "polr", "polr", "",
          "")

df_imp <- mice::mice(df, m = N.Imp, method = type)

# Exract each DF
mice.imp <- list()
for(i in 1:N.Imp) {
  
  mice.imp[[i]] <- mice::complete(df_imp, action= i, inc=FALSE)
  
  
  # remove spaces and other charecters on colnames
  colnames( mice.imp[[i]]) <- gsub(" ", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("/", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("-", "_", colnames( mice.imp[[i]]), fixed = TRUE)
}



#M: I am thinking about plotting the overall trends in beliefs across waves, can explain in person
# a plot that would be nicely descriptive and would give a lot of information about the demographics and missingess


base <- "


   BiG1 ~ 0*MS1  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
  
   BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1
   
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c"



summary(base, std = T)

base_mi <- lavaan.mi::sem.mi(base, mice.imp, ordered = c("BiG1","BiG2", "BiG3", "BiG4"), meanstructure = T,
                           estimator = "WLSMV", 
                           missing = "pairwise", 
                           parameterization = "theta", std.lv = T)

fitmeasures(base_mi)

base_mi_params <- extract_defined_params_lavaanmi(base_mi)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base <- restriktor::goric(base_mi_params[["est"]], VCOV = base_mi_params[["VCOV"]],
                  hypotheses = list(hypothesis), comparison = "complement")

benchmark(H1_base)


### Controls model

controls <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool
  
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

      BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1
   
  
"

controls_mi <- lavaan.mi::sem.mi(controls, mice.imp,
                               ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit", "PST"), 
                               meanstructure = T,
                               estimator = "WLSMV", missing = "pairwise", 
                               parameterization = "theta", std.lv = T)

fitmeasures(controls_mi)

controls_mi_params <- extract_defined_params_lavaanmi(controls_mi)


hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls <- restriktor::goric(controls_mi_params[["est"]], VCOV = controls_mi_params[["VCOV"]],
                  hypotheses = list(hypothesis), comparison = "complement")

benchmark(H1_controls)


## Full ordinal (slightly cut)


full_ordinal <- "

PST_l =~ PST
PST ~~ 0*PST

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST_l
 
     eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ h2.1a*H2 +h2.2a*T2 + PR2_l + CR2_l 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h2.1d*H3 + h3.2b*PR3 +  h3.1b*CR3 

  # Misc
 
  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
 
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool


  # PR

PR2_l =~ PR2
PR2 ~~ 0*PR2
  
PR3 ~  PR2_l + h3.1a*H2 + T2

# CR
CR2_l =~ CR2
CR2 ~~ 0*CR2

CR3 ~ CR2_l + H2 + h3.2a*T2




    # H
 
   H2 ~ MS1
   H3 ~ arH*H2
      # T
 
  T2 ~ MS1
 
 
  
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


      BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
      
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1

   PR2 | pr1*t1
   PR2 | pr2*t2
   PR2 | pr3*t3
   PR2 | pr4*t4
   PR2 | pr5*t5
   PR2 | pr6*t6
   
   PR3 | pr1*t1
   PR3 | pr2*t2
   PR3 | pr3*t3
   PR3 | pr4*t4
   PR3 | pr5*t5
   PR3 | pr6*t6

   
   CR2 | cr1*t1
   CR2 | cr2*t2
   CR2 | cr3*t3
   CR2 | cr4*t4
   CR2 | cr5*t5
   CR2 | cr6*t6
   
   CR3 | cr1*t1
   CR3 | cr2*t2
   CR3 | cr3*t3
   CR3 | cr4*t4
   CR3 | cr5*t5
   CR3 | cr6*t6

   
   H2 | hth1*t1
   H2 | hth2*t2
   H2 | hth3*t3
   H2 | hth4*t4
   
   H3 | hth1*t1
   H3 | hth2*t2
   H3 | hth3*t3
   H3 | hth4*t4
   


"

full_ordinal_fit <- lavaan.mi::sem.mi(full_ordinal, mice.imp, 
                                      estimator = "WLSMV", parameterization = "theta",
                                      meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                     "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                      missing = "listwise",  control = list(iter.max = 10e5))

s <- lavaan.mi::standardizedSolution.mi(full_ordinal_fit)
compareFit(base_mi, controls_mi, full_ordinal_fit)

standardizedSolution.mi(full_ordinal_fit, return.vcov = F, type = "cov.lv")

sqrt((15475.243-171)/(171*1387))

full_ordinal_prereg <- "
PST_l =~ PST
PST ~~ 0*PST 

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST_l

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ PR2 + CR2 + h2.1a*H2 + h2.2a*T2 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h3.1b*PR3 + h3.2b*CR3 + h2.1d*H3
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE

  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool
  
  # PR
 PR2 ~ MS1
 PR3 ~  PR2 + MS1 + h3.1a*H2 + T2

 
 # CR

 CR2 ~ MS1
  CR3 ~ CR2 + MS1 + H2 + h3.2a*T2

 
  
    # H

   H2 ~ MS1
   H3 ~ arH*H2

  
      # T

  T2 ~ MS1

  
  
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


   BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
      
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1

   PR2 | pr1*t1
   PR2 | pr2*t2
   PR2 | pr3*t3
   PR2 | pr4*t4
   PR2 | pr5*t5
   PR2 | pr6*t6
   
   PR3 | pr1*t1
   PR3 | pr2*t2
   PR3 | pr3*t3
   PR3 | pr4*t4
   PR3 | pr5*t5
   PR3 | pr6*t6
   


   
   CR2 | cr1*t1
   CR2 | cr2*t2
   CR2 | cr3*t3
   CR2 | cr4*t4
   CR2 | cr5*t5
   CR2 | cr6*t6
   
   CR3 | cr1*t1
   CR3 | cr2*t2
   CR3 | cr3*t3
   CR3 | cr4*t4
   CR3 | cr5*t5
   CR3 | cr6*t6

   H2 | hth1*t1
   H2 | hth2*t2
   H2 | hth3*t3
   H2 | hth4*t4
   
   H3 | hth1*t1
   H3 | hth2*t2
   H3 | hth3*t3
   H3 | hth4*t4
   

"


# Hypothesis tests --------------------------------------------------------
#H1

full_ordinal_mi_params <- extract_defined_params_lavaanmi(full_ordinal_fit)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

full_ordinal_mi_eval <- restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                                         hypotheses = list(hypothesis), comparison = "complement")
full_ordinal_mi_eval

full_ordinal_mi_eval_ben <- benchmark(full_ordinal_mi_eval)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

H2.1 <- "h3.1direct + h2_big4 + h2_h3_big4 < 0" # correct direction?

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H2.1), comparison = "complement")
# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0" # correct direction?

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H2.2), comparison = "complement")

# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.1any = H3.1any), comparison = "complement")


# Is there partial mediation 
H3.1part <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) > 0"

# Is there full mediation
H3.1full <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) = 0"

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.1part = H3.1part,
                    H3.1full = H3.1full), comparison = "unconstrained")

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.1partneg <- "h3.1indirect > 0 ; abs(h3.1direct) > 0"

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.1partneg = H3.1partneg), comparison = "complement")

# H3.2: 
# T2 -> CR3 -> BiG4

# Is there any indirect effect?
H3.2any <- "abs(h3.2indirect) > 0"

# First as complement

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.2any = H3.2any), comparison = "complement")


# Is there partial mediation 
H3.2part <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"

# Is there full mediation
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) = 0"


restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(H3.2part = H3.2part,
                                    H3.2full = H3.2full), comparison = "unconstrained")

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.2partneg <- "h3.2indirect < 0 ; abs(h3.2direct) > 0"

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.2partneg = H3.2partneg), comparison = "complement")



H2_BiG4 <- "h3.1direct + h2_big4 > 0"

restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H2_BiG4 = H2_BiG4), comparison = "complement")

save.image("nontheism.RData")


# Sensitivity checks ------------------------------------------------------

full_fit_ofiml <- lavaan::sem(full_linear, df_ml, meanstructure = T, estimator = "MLR", missing = "fiml",
                              fixed.x = F)


summary(full_fit_ofiml, std = T, fit = T)

hypothesis_elements <-  rownames(standardizedSolution.mi(full_ordinal_fit, return.vcov = TRUE, type = "def"))

indices <- rep(NA, length(hypothesis_elements))

for (i in seq_along(hypothesis_elements)) {
  param <- hypothesis_elements[i]
  indices[i] <- which(lavaan.mi::standardizedSolution.mi(full_ordinal_fit)[, 'label'] == param)
}

est <- lavaan.mi::standardizedSolution.mi(full_ordinal_fit)[indices, 'est.std'] # estimates

names(est) <- hypothesis_elements

VCOV <- standardizedSolution.mi(full_ordinal_fit, return.vcov = TRUE, type = "def.std.all")



# MLE sensitivity-------------------------------------------------------------------------
df_ml <- df


df_ml[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
         "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")] <- lapply(df_ml[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                                               "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")], FUN = as.numeric)
mice.imp_lin <- mice.imp

mice.imp_lin <- lapply(mice.imp, function(df) {
  df[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
        "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")] <- 
    lapply(df[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                 "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")], 
           FUN = as.numeric)
  return(df)
})

# Baseline ML model
base_ml <- "
  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   ## MS1 -> BiG2 -> BiG3
                             
     ms1_big3 := h1a*ar3
                           
  ## MS1 -> BiG3 -> BiG4
                           
    ms1_big4 := h1b*ar4
                           
    h1a_ := h1a
    h1b_ := h1b
 h1c_ := h1c
"

base_mi_ml <- lavaan.mi::sem.mi(base_ml, mice.imp_lin, meanstructure = T,
                           estimator = "MLR", missing = "fiml")



base_mi_ml_params <- extract_defined_params_lavaanmi(base_mi_ml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base_ml <- restriktor::goric(base_mi_ml_params[["est"]], VCOV = base_mi_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")
H1_base_ml

H1_base_ml_ben <- benchmark(H1_base_ml)


# Controls model ML

controls_ml <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool
  
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

  
"

controls_mi_ml <- lavaan.mi::sem.mi(controls_ml, mice.imp_lin, meanstructure = T,
                                 estimator = "MLR", missing = "fiml")

controls_mi_ml_params <- extract_defined_params_lavaanmi(controls_mi_ml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls_ml <- restriktor::goric(controls_mi_ml_params[["est"]], VCOV = controls_mi_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")
H1_controls_ml

H1_controls_ml_ben <- benchmark(H1_controls_ml)

# Full ML model

# Test the hypotheses using GORICA

## FULL LINEAR MODEL


full_linear <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ PR2 + CR2 + h2.1a*H2 + h2.2a*T2 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h3.1b*PR3 + h3.2b*CR3 + h2.1d*H3
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE

  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool
  
  # PR
 
PR2_l =~ PR2
PR2 ~~ 0*PR2

 PR3 ~  PR2_l + MS1 + h3.1a*H2 + T2

 # CR

CR2_l =~ CR2
CR2 ~~ 0*CR2

CR3 ~ CR2_l + H2 + h3.2a*T2
  
    # H

   H2 ~ ms1_h2*MS1
   H3 ~ arH*H2

      # T

  T2 ~ MS1

  
  
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
h3.2direct := h2.2b"


full_fiml_ml <- sem(full_linear, df_ml, meanstructure = T,
                                estimator = "MLR", missing = "fiml")


full_fiml_ml_params <- extract_defined_params_lavaan(full_fiml_ml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_full_fiml <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                 hypotheses = list(hypothesis), comparison = "complement")
H1_full_fiml

H1_full_fiml_ben <- benchmark(H1_full_fiml)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

H2.1 <- "h3.1direct + h2_big4 + h2_h3_big4 > 0"

H2.1_eval_fiml <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                  hypotheses = list(
                    H2.1), comparison = "complement")

H2.1_eval_fiml_ben <- benchmark(H2.1_eval_fiml)
# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0"

H2.2_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(
                    H2.2), comparison = "complement")

H2.2_eval_ben <- benchmark(H2.2_eval)


# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

H3.1_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(
                    H3.1any = H3.1any), comparison = "complement")
H3.1_eval_ben <- benchmark(H3.1_eval)

# Is there partial mediation 
H3.1part <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) > 0"

# Is there full mediation
H3.1full <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) = 0"

H3.1full_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(
                    H3.1part = H3.1part,
                    H3.1full = H3.1full), comparison = "unconstrained")

benchmark(H3.1full_eval)

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.1partneg <- "h3.1indirect > 0 ; abs(h3.1direct) > 0"

H3.1full_eval_c <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(
                    H3.1full = H3.1full), comparison = "complement")

benchmark(H3.1full_eval_c)

# H3.2: 
# T2 -> CR3 -> BiG4

# Is there any indirect effect?
H3.2any <- "abs(h3.2indirect) > 0"

# First as complement

H3.2eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(
                    H3.2any = H3.2any), comparison = "complement")

benchmark(H3.2eval)

# Is there partial mediation 
H3.2part <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"

# Is there full mediation
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) = 0"


H3.2part_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                  hypotheses = list(H3.2part = H3.2part,
                                    H3.2full = H3.2full), comparison = "unconstrained")
H3.2full_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                   hypotheses = list(
                                                     H3.2full = H3.2full), comparison = "complement")

benchmark(H3.2full_eval)

# FIML

# baseline
base_fiml <- lavaan::sem(base_ml, df_ml, estimator = "MLR", 
                         missing = "fiml", meanstructure = T)
                       
base_mi_fiml_params <- extract_defined_params_lavaan(base_fiml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base_fiml <- restriktor::goric(base_mi_fiml_params[["est"]], VCOV = base_mi_fiml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")
H1_base

H1_base_ben_fiml <- benchmark(H1_base_fiml)

# controls

controls_fiml <- lavaan::sem(controls_ml, df_ml, estimator = "MLR", 
                         missing = "fiml", meanstructure = T)

controls_mi_fiml_params <- extract_defined_params_lavaan(controls_fiml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls_fiml <- restriktor::goric(controls_mi_fiml_params[["est"]], VCOV = controls_mi_fiml_params[["VCOV"]],
                                  hypotheses = list(hypothesis), comparison = "complement")
H1_controls_fiml

H1_controls_fiml_ben <- benchmark(H1_controls_fiml)

# full

full_fiml <- lavaan::sem(full_linear, df_ml, estimator = "MLR", 
                             missing = "fiml", meanstructure = T)

full_fiml_ml_params <- extract_defined_params_lavaan(full_fiml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_full_fiml <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                      hypotheses = list(hypothesis), comparison = "complement")
H1_full_fiml

H1_full_fiml_ben <- benchmark(H1_full_fiml)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

H2.1 <- "h3.1direct + h2_big4 + h2_h3_big4 > 0"

H2.1_eval_fiml <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                    hypotheses = list(
                                      H2.1), comparison = "complement")

H2.1_eval_fiml_ben <- benchmark(H2.1_eval_fiml)
# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0"

H2.2_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                               hypotheses = list(
                                 H2.2), comparison = "complement")

H2.2_eval_ben <- benchmark(H2.2_eval)


# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

H3.1_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                               hypotheses = list(
                                 H3.1any = H3.1any), comparison = "complement")
H3.1_eval_ben <- benchmark(H3.1_eval)

# Is there partial mediation 
H3.1part <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) > 0"

# Is there full mediation
H3.1full <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) = 0"

H3.1full_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                   hypotheses = list(
                                     H3.1part = H3.1part,
                                     H3.1full = H3.1full), comparison = "unconstrained")

benchmark(H3.1full_eval)

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.1partneg <- "h3.1indirect > 0 ; abs(h3.1direct) > 0"

H3.1full_eval_c <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                     hypotheses = list(
                                       H3.1full = H3.1full), comparison = "complement")

benchmark(H3.1full_eval_c)

# H3.2: 
# T2 -> CR3 -> BiG4

# Is there any indirect effect?
H3.2any <- "abs(h3.2indirect) > 0"

# First as complement

H3.2eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                              hypotheses = list(
                                H3.2any = H3.2any), comparison = "complement")

benchmark(H3.2eval)

# Is there partial mediation 
H3.2part <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"

# Is there full mediation
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) = 0"


H3.2part_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                   hypotheses = list(H3.2part = H3.2part,
                                                     H3.2full = H3.2full), comparison = "unconstrained")
H3.2full_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                   hypotheses = list(
                                     H3.2full = H3.2full), comparison = "complement")


# Pairwise WLSMV


base <- "


   BiG1 ~ 0*MS1  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
  
   BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1
   
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c"



base_pw <- lavaan::sem(base, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4"), meanstructure = T,
                               estimator = "WLSMV", parameterization = "theta", missing = "pairwise",
                          std.lv = F)
                        
                         

summary(base_pw, fit = T, std = T)
base_pw_ml_params <- extract_defined_params_lavaan(base_pw)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base_pw <- restriktor::goric(base_pw_ml_params[["est"]], VCOV = base_pw_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")
H1_base_pw

H1_base_pw_ben <- benchmark(H1_base_pw)

controls <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool
  
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

      BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1
   
  
"


controls_pw <- lavaan::sem(controls, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit", "PST"), meanstructure = T,
                       estimator = "WLSMV", parameterization = "theta", missing = "pairwise")

summary(controls_pw, fit = T, std = T)

controls_pw_params <- extract_defined_params_lavaan(controls_pw)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls_pw <- restriktor::goric(controls_pw_params[["est"]], VCOV = controls_pw_params[["VCOV"]],
                                hypotheses = list(hypothesis), comparison = "complement")
H1_controls_pw

H1_controls_pw_ben <- benchmark(H1_controls_pw)

full_ordinal <- "

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + HighSchool + PST
 
     eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ h2.1a*H2 +h2.2a*T2 + PR2_l + CR2_l 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h2.1d*H3 + h3.2b*PR3 +  h3.1b*CR3 

  # Misc
 
  MS1 ~ ParCollege + ParHighSchool + BlackE + LatinxE + OtherE
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
 
  College + HighSchool ~ ParCollege + ParHighSchool + Age + MS1
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParHighSchool


  # PR

PR2_l =~ PR2
PR2 ~~ 0*PR2
  
PR3 ~  PR2_l + h3.1a*H2 + T2

# CR
CR2_l =~ CR2
CR2 ~~ 0*CR2

CR3 ~ CR2_l + H2 + h3.2a*T2




    # H
 
   H2 ~ MS1
   H3 ~ arH*H2
      # T
 
  T2 ~ MS1
 
 
  
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


      BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2
   
      
   BiG1 ~ 0*1
   BiG2 ~ NA*1
   BiG3 ~ NA*1
   BiG4 ~ NA*1

   PR2 | pr1*t1
   PR2 | pr2*t2
   PR2 | pr3*t3
   PR2 | pr4*t4
   PR2 | pr5*t5
   PR2 | pr6*t6
   
   PR3 | pr1*t1
   PR3 | pr2*t2
   PR3 | pr3*t3
   PR3 | pr4*t4
   PR3 | pr5*t5
   PR3 | pr6*t6

   
   CR2 | cr1*t1
   CR2 | cr2*t2
   CR2 | cr3*t3
   CR2 | cr4*t4
   CR2 | cr5*t5
   CR2 | cr6*t6
   
   CR3 | cr1*t1
   CR3 | cr2*t2
   CR3 | cr3*t3
   CR3 | cr4*t4
   CR3 | cr5*t5
   CR3 | cr6*t6

   
   H2 | hth1*t1
   H2 | hth2*t2
   H2 | hth3*t3
   H2 | hth4*t4
   
   H3 | hth1*t1
   H3 | hth2*t2
   H3 | hth3*t3
   H3 | hth4*t4
   


"


part_ordinal_fit <- lavaan::sem(model = full_ordinal, data = mice.imp[[1]], 
                                estimator = "WLSMV", parameterization = "theta",
                                meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                 "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                missing = "pairwise",  control = list(iter.max = 10e5))

summary(part_ordinal_fit, std = T, fit = T)

full_ordinal_pw_params <- extract_defined_params_lavaan(part_ordinal_fit)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

full_ordinal_pw_ord <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                    hypotheses = list(hypothesis), comparison = "complement")
full_ordinal_pw_ord

full_ordinal_pw_ord_ben <- benchmark(full_ordinal_pw_ord)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

H2.1 <- "h3.1direct + h2_big4 + h2_h3_big4 > 0"

H2.1_eval_ford <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                    hypotheses = list(
                                      H2.1), comparison = "complement")

H2.1_eval_ford_ben <- benchmark(H2.1_eval_ford)
# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0"

H2.2_eval_ford <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                               hypotheses = list(
                                 H2.2), comparison = "complement")

H2.2_eval_ben <- benchmark(H2.2_eval_ford)


# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

H3.1_eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                               hypotheses = list(
                                 H3.1any = H3.1any), comparison = "complement")
H3.1_eval_ben <- benchmark(H3.1_eval)

# Is there partial mediation 
H3.1part <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) > 0"

# Is there full mediation
H3.1full <- "abs(h3.1indirect) > 0 ; abs(h3.1direct) = 0"

H3.1full_eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                   hypotheses = list(
                                     H3.1part = H3.1part,
                                     H3.1full = H3.1full), comparison = "unconstrained")

benchmark(H3.1full_eval)

# If par. med supported, then:
# Is there par. med with negative indirect eff?
H3.1partneg <- "h3.1indirect > 0 ; abs(h3.1direct) > 0"

H3.1full_eval_c <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                     hypotheses = list(
                                       H3.1partneg = H3.1partneg), comparison = "complement")

benchmark(H3.1full_eval_c)

# H3.2: 
# T2 -> CR3 -> BiG4

# Is there any indirect effect?
H3.2any <- "abs(h3.2indirect) > 0"

# First as complement

H3.2eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                              hypotheses = list(
                                H3.2any = H3.2any), comparison = "complement")

benchmark(H3.2eval)

# Is there partial mediation 
H3.2part <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) > 0"

# Is there full mediation
H3.2full <- "abs(h3.2indirect) > 0 ; abs(h3.2direct) = 0"


H3.2part_eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                   hypotheses = list(H3.2part = H3.2part,
                                                     H3.2full = H3.2full), comparison = "unconstrained")
H3.2full_eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                   hypotheses = list(
                                     H3.2part = H3.2part), comparison = "complement")



# Checks ------------------------------------------------------------------

check <- "



  # PR

PR2_l =~ PR2
PR2 ~~ 0*PR2
  
PR3 ~  PR2_l + h3.1a*H2 + T2

# CR
CR2_l =~ CR2
CR3 ~ CR2_l + H2 + h3.2a*T2

    # H
 
   H2 ~ MS1
   H3 ~ arH*H2
      # T
 
  T2 ~ MS1


"

check_fit <- lavaan::sem(model = check, data = df, 
                                estimator = "WLSMV", parameterization = "delta",
                                meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                               "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                control = list(iter.max = 10e5))

summary(check_fit, fit = T, std = T)
