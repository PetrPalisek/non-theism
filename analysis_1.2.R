################################3###############################################
## The relationship between exogenous pressures and belief in God in young American Christians
################################################################################


# Load libraries
library(lavaan)
library(semTools)
library(semPlot)
library(restriktor)
library(qpcR)
library(readxl)
library(fastDummies)
library(Amelia)
library(psych)
library(lavaan.mi)
library(summarytools)
library(dplyr)
library(mice)
library(tidyverse)
library(summarytools)
library(openxlsx)

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
  dplyr::select(IDS, I_GENDER, PRACE, ETHRACE, AGECATS,
         PINCOME, GOD, PEDUC1, PSPEDUC1, PEDUC3,
         PATTEND, PSTRESS, RELTRAD, PSPEDUC3, BNPRLCAT, BNPRLPRT)

# Adding suffixes to distinguish waves
names(w1)[2:ncol(w1)] <- paste(names(w1)[2:ncol(w1)], "_W1", sep = "")

# Selecting relevant vars
w2 <- w2 %>%
  dplyr::select(IDS, GOD, HEALTH, ATTREG, ATTEND1, 
         PRAYALON, TRUST)

# Adding suffixes to distinguish waves
names(w2)[2:ncol(w2)] <- paste(names(w2)[2:ncol(w2)], "_W2", sep = "")


# Selecting relevant vars
w3 <- w3 %>%
  dplyr::select(IDS, GOD, HEALTH, ATTREG, ATTEND1, 
         PRAYALON, EARNINGS)

# Adding suffixes to distinguish waves
names(w3)[2:ncol(w3)] <- paste(names(w3)[2:ncol(w3)], "_W3", sep = "")

w4 <- w4 %>%
  dplyr::select(IDS, GOD_W4, HEALTH_W4, ATTREG_W4, ATTEND1_W4, 
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
    RELTRAD_W1 == 9 ~ "INDE"
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
df$AAVOC <- ifelse(df$EDATT_W4 == "2", 1, 0) # Highest education == high school

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
    ParEd %in% c("03", "13", "23", "30", "31", "32", "3NA", "NA3") ~ 4,
    ParEd == "22" ~ 3,
    ParEd %in% c("02", "12", "20", "21", "2NA", "NA2") ~ 2,
    ParEd == "NANA" ~ NA_real_,
    .default = 1
  ))

df$ParEd_ord <- factor(df$ParEd_ord, ordered = T)

df$ParCollege <- ifelse(df$ParEd_ord %in% c(4, 5), 1, 0)
df$ParAAVOC <- ifelse(df$ParEd_ord %in% c(2, 3), 1, 0)


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

df$BiG1 <- factor(df$BiG1, levels = c("0", ".5", "1"), ordered = T)
df$BiG2 <- factor(df$BiG2, levels = c("0", ".5", "1"), ordered = T)
df$BiG3 <- factor(df$BiG3, levels = c("0", ".5", "1"), ordered = T)
df$BiG4 <- factor(df$BiG4, levels = c("0", ".5", "1"), ordered = T)

df$T2 <- factor(df$T2)

df$H2 <- factor(6-df$H2) # Revert H at T2
df$H3 <- factor(6-df$H3 ) # Revert H at T3
df$H4 <- factor(6-df$H4 ) # Revert H at T4

df$PR2 <- factor(df$PR2, ordered = T)
df$PR3 <- factor(df$PR3, ordered = T)
df$PR4 <- 8-df$PR4 # Revert PR at T4
df$PR4 <- factor(df$PR4, ordered = T)

df$CR2 <- factor(df$CR2, ordered = T)
df$CR3 <- factor(df$CR3, ordered = T)
df$CR4 <- factor(df$CR4, ordered = T)

df$ParRit <- factor(df$ParRit, ordered = T)
df$PST <- factor(df$PST, ordered = T)

df$Age <- as.vector(scale(ifelse(df$Age == 888, NA, df$Age)))
df$Inc3 <-  as.vector(scale(df$Inc3))

str(df)


# Excluding participants --------------------------------------------------


# First maintain the full copy for imputation
df_pre <- df

# Removing non-Christian participants 
df[df$RELTRAD %in% c("Jewish", "Other"),]  %>% nrow()
# 274
df <- df[!(df$RELTRAD %in% c("Jewish", "Other")),] 

# Removing unaffiliated and indeterminate RELTRADs with no Christian parent

df[(df$RELTRAD == "INDE" & df$ParCatholic == 0 & df$ParProtestant == 0),] %>% nrow()
# 29
df <- df[!(df$RELTRAD == "INDE" & df$ParCatholic == 0 & df$ParProtestant == 0),] 

df[(df$RELTRAD == "None" & df$ParCatholic == 0 & df$ParProtestant == 0),] %>% nrow() 
# 176
df <- df[!(df$RELTRAD == "None" & df$ParCatholic == 0 & df$ParProtestant == 0),] 



# Recode BiG_NA_Wave based on missing patterns in BiG2, BiG3, and BiG4:
df$BiG_NA_Wave <- ifelse(
  is.na(df$BiG2) & !is.na(df$BiG3) & !is.na(df$BiG4), "100", 
  ifelse(is.na(df$BiG2) & is.na(df$BiG3) & !is.na(df$BiG4), "110",
         ifelse(is.na(df$BiG2) & is.na(df$BiG3) & is.na(df$BiG4), "111",
                ifelse(is.na(df$BiG2) & !is.na(df$BiG3) & is.na(df$BiG4), "101",
                       ifelse(!is.na(df$BiG2) & is.na(df$BiG3) & is.na(df$BiG4), "011",
                              ifelse(!is.na(df$BiG2) & is.na(df$BiG3) & !is.na(df$BiG4), "010",
                                     ifelse(!is.na(df$BiG2) & !is.na(df$BiG3) & is.na(df$BiG4), "001",
                                            ifelse(!is.na(df$BiG2) & !is.na(df$BiG3) & !is.na(df$BiG4), "000", NA))))))))
# Convert the outcome to a factor
df$BiG_NA_Wave <- factor(df$BiG_NA_Wave, 
                         levels = c("000", "111", "011", "001", "101", "110", "100"))

# Check frequencies
table(df$BiG_NA_Wave)

plot(df$BiG_NA_Wave, df$MS1, xlab = "BiG_NA_Wave", ylab = "MS1", main = "BiG_NA_Wave vs MS1")

# Create a faceted plot (one panel per BiG1 level)
ggplot(df, aes(x = BiG_NA_Wave, y = MS1)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray") +  # Boxplot for summary statistics
  geom_jitter(width = 0.2, alpha = 0.6, color = "blue") +   # Individual data points with some jitter
  facet_wrap(~ BiG1) +                                      # Facet by BiG1 levels
  labs(x = "BiG_NA_Wave", 
       y = "MS1", 
       title = "MS1 vs BiG_NA_Wave by BiG1 Levels") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12))

# Remove non-believers in W1
df[df$BiG1 == "0",] %>% nrow()
# 59

df <- df[df$BiG1 != "0",] 

table(df$RELTRAD, df$BiG1)
# Descriptives (after exclusions) ------------------------------------------------------------

# Check missing values
Amelia::missmap(df, rank.order = F)

na_sum <- data.frame(lapply(df, function(x) sum(is.na(x))))
na_sum[2,] <- round(na_sum[1,]/nrow(df)*100,2)
na_sum <- t(na_sum) # % missing per var

table(df$BiG1, useNA = "ifany")
table(df$BiG2, useNA = "ifany") 
table(df$BiG3, useNA = "ifany") 
table(df$BiG4, useNA = "ifany") 

table(df$BiG1) %>% prop.table() %>% round(2)
table(df$BiG2) %>% prop.table() %>% round(2)
table(df$BiG3) %>% prop.table() %>% round(2)
table(df$BiG4) %>% prop.table() %>% round(2)

table(df$BiG1, useNA = "ifany") %>% prop.table() %>% round(2)
table(df$BiG2, useNA = "ifany") %>% prop.table() %>% round(2)
table(df$BiG3, useNA = "ifany") %>% prop.table() %>% round(2)
table(df$BiG4, useNA = "ifany") %>% prop.table() %>% round(2)



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

df_pre$BiG4NA <- ifelse(is.na(df_pre$BiG4), 1, 0)
df_pre$BiG1 <- factor(df_pre$BiG1,ordered = F)

# Predicting missingness in BiG4 by MS1
na_fit <- glm(BiG4NA ~ BiG1*MS1, df_pre, family = binomial(link = "logit"))

effects::allEffects(na_fit)

summary(na_fit)

### Plot raw data_______________________________________________________________________________

# Create a subset of data for the plot
df_plot <- subset(df_pre, select = c(BiG1,BiG2,BiG3,BiG4,MS1, id))
df_plot <- df_plot[!is.na(df_plot$MS1) & !is.na(df_plot$BiG1),]

# Cut data into 3 roughly equivalent groups based on material security
df_plot$MS <- as.numeric(ggplot2::cut_number(df_plot$MS1,n = 3))
df_plot <- subset(df_plot, select = -c(MS1))

# Melt data into a long format
df_plot <- reshape2::melt(df_plot,id.vars = c("MS","id"), variable.name = "T")
levels(df_plot$T) <- c("T1","T2","T3","T4")

# Recode data for BiG
df_plot$value[is.na(df_plot$value)] <- "Missing"
df_plot$value[df_plot$value==1] <- "Yes"
df_plot$value[df_plot$value==".5"] <- "Uncertain"
df_plot$value[df_plot$value==0] <- "No"


# Define colors
col_vector = c(   '#E7298A',  'purple','#9DD1D1','grey')



total_n1 <- nrow(df_plot[df_plot$MS==1,] %>% distinct(id))
# Three subplots based on levels of material security
p1 <- easyalluvial::alluvial_long(df_plot[df_plot$MS==1,]
                                  , key = T
                                  , value = value
                                  , id = id
                                  , verbose = F
                                  , stratum_labels = T
                                  , stratum_label_size = 3.5
                                  , fill_by = 'value'
                                  , NA_label = 'None'
                                  , col_vector_value = col_vector
                                  , col_vector_flow = col_vector
) +
  labs(title = 'Low material security') + 
  scale_x_discrete(name = "", labels = c("Wave1", "Wave2","Wave3","Wave4"),expand = c(0.05,0.05)) + 
  scale_y_continuous(name = "Percentage of participants",
                     labels = function(x) paste0(round(x / total_n1 * 100), "%"), breaks = seq(0,total_n1+10,total_n1/5),
                     limits = c(0,total_n1+10)) +
  theme_bw()  +
  theme(
    #panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(2)),
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    legend.justification = c("right", "top"),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.key.size = unit(0.8, "cm"),
    axis.title = element_text(size = rel(1.5)),
    axis.text= element_text(size = rel(1.5)),
    plot.margin=unit(c(0.4,0.4,-1,0.4),"cm"),
    strip.text.x = element_text(size = rel(2)))

total_n2 <- nrow(df_plot[df_plot$MS==2,] %>% distinct(id))
p2 <- easyalluvial::alluvial_long(df_plot[df_plot$MS==2,]
                                  , key = T
                                  , value = value
                                  , id = id
                                  , stratum_label_size = 3.5
                                  , fill_by = 'value'
                                  , col_vector_value = col_vector
                                  , col_vector_flow = col_vector
) +
  labs(title = 'Medium material security') +
  scale_x_discrete(name = "", labels = c("Wave1", "Wave2","Wave3","Wave4"),expand = c(0.05,0.05)) + 
  scale_y_continuous(name = "Percentage of participants",
                     labels = function(x) paste0(round(x / total_n2 * 100), "%"),
                     breaks = seq(0,total_n2+10,total_n2/5),
                     limits = c(0,total_n2+10)) +
  theme_bw()   +
  theme(
    #panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(2)),
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    legend.justification = c("right", "top"),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.key.size = unit(0.8, "cm"),
    axis.title = element_text(size = rel(1.5)),
    axis.text= element_text(size = rel(1.5)),
    plot.margin=unit(c(0.4,0.4,-1,0.4),"cm"),
    strip.text.x = element_text(size = rel(2)))

total_n3 <- nrow(df_plot[df_plot$MS==3,] %>% distinct(id))
p3 <- easyalluvial::alluvial_long(df_plot[df_plot$MS==3,]
                                  , key = T
                                  , value = value
                                  , id = id
                                  , stratum_label_size = 3.5
                                  , fill_by = 'value'
                                  , NA_label = 'None'
                                  , col_vector_value = col_vector
                                  , col_vector_flow = col_vector
) +
  labs(title = 'High material security') + 
  scale_x_discrete(name = "", labels = c("Wave1", "Wave2","Wave3","Wave4"),expand = c(0.05,0.05)) + 
  scale_y_continuous(name = "Percentage of participants",
                     labels = function(x) paste0(round(x / total_n3 * 100), "%"),
                     breaks = seq(0,total_n3+10,total_n3/5),
                     limits = c(0,total_n3+10)) +
  
  theme_bw()  +
  theme(
    #panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = rel(2)),
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    legend.justification = c("right", "top"),
    legend.text = element_text(size = rel(1.5)),
    legend.title = element_text(size = rel(1.5)),
    legend.key.size = unit(0.8, "cm"),
    axis.title = element_text(size = rel(1.5)),
    axis.text= element_text(size = rel(1.5)),
    plot.margin=unit(c(0.4,0.4,-1,0.4),"cm"),
    strip.text.x = element_text(size = rel(2)))

# Arrange subplots into the final plot and save
gx <- ggpubr::ggarrange(p1, p2, p3, ncol=1, nrow = 3, 
                        labels = c('A.', 'B.', 'C.'),
                        font.label = list(size = 18, face = "bold", color ="black"),
                        common.legend = T)

gx

print(c(total_n1,
      total_n2,
      total_n3))

ggplot2::ggsave("Alluvial_plot.png", plot = gx, width = 12, height = 16,
                dpi = 600)

table(df_plot[df_plot$MS == 1,"value"], df_plot[df_plot$MS == 1,"T"]) %>% prop.table() %>% round(2)
table(df_plot[df_plot$MS == 3,"value"], df_plot[df_plot$MS == 3,"T"]) %>% prop.table() %>% round(2)

df_plot %>%
  filter(T %in% c("T1","T4")) %>%
  select(id, MS, T, value) %>%
  # Keep only Yes/No for the transition metric; drop Missing/Uncertain
  filter(value %in% c("Yes","No")) %>%
  pivot_wider(names_from = T, values_from = value) %>%
  # Require non-missing at both waves
  filter(!is.na(T1), !is.na(T4)) %>%
  group_by(MS) %>%
  summarize(
    N_T1_yes      = sum(T1 == "Yes"),
    T1Yes_T4No    = sum(T1 == "Yes" & T4 == "No"),
    pct_T1Yes_to_T4No = round(100 * T1Yes_T4No / N_T1_yes, 0),
    .groups = "drop"
  ) %>%
  arrange(MS)

## Summaries -------------------------------------------------------------



# Summarize the data
dfSummary(df)
quantile(df$MS1, na.rm = T)

paste0("% protestants ", 14.1+36.6+12.1)

# Imputation --------------------------------------------------------------

# Number of imputed df 
N.Imp <-  70
max.it <- 20
seed <- 3333

# --- Step 1: Pre-clean the dataset ---

df_forimp <- df_pre
# Remove ID variable or other non-informative columns
df_forimp$id <- NULL  

# Remove constant variables (no variation)
constant_vars <- names(df_forimp)[sapply(df_forimp, function(x) length(unique(x)) == 1)]
df_forimp <- df_forimp[ , !(names(df_forimp) %in% constant_vars)]

# Remove perfectly collinear dummy variables (if categorical vars were already dummy-coded)
df_forimp <- df_forimp[ , !duplicated(t(df_forimp))]  # Remove duplicate columns


df_forimp <- df_forimp %>%
  mutate(across(
    c(EDATT_W4, FirstParEd, SecondParEd, ETHRACE,
      CR2_1, CR2_2, CR3_1, CR3_2, CR4_1, CR4_2, MS1, H2, H3, H4, T2, PR2, PR3, PR4, ParRit, PST),
    ~ factor(.x, ordered = TRUE)
  ))

df_forimp <- df_forimp %>%
  mutate(across(
    c(ETHRACE, ParCatholic, ParProtestant, BlackProt, Catholic, MainProt,INDE, None, OtherRel, Jewish),
    ~ factor(.x, ordered = FALSE)
  ))
# --- Step 2: Imputation model setup ---

# List of variables to impute 
vars_to_impute <- c(
  "BiG2","BiG3","BiG4",
  "MS1","Age","Male",
  "ParEd_ord","EDATT_W4",
  "Inc3","Inc4",
  "PR2","PR3","PR4","CR2","CR3","CR4",
  "H2","H3","H4",
  "RELTRAD","T2","PST","ParRit","ETHRACE"
)

# Set method defaults
method <- mice::make.method(df_forimp)

# Convert all character variables to factors
df_forimp <- df_forimp %>%
  dplyr::mutate(dplyr::across(where(is.character), as.factor))

# Assign methods by type
for (v in vars_to_impute) {
  if (v %in% names(df_forimp)) {
    if (is.ordered(df_forimp[[v]])) {
      method[v] <- "polr"
    } else if (is.factor(df_forimp[[v]]) && nlevels(df_forimp[[v]]) > 2) {
      method[v] <- "polyreg"
    } else if (is.factor(df_forimp[[v]]) && nlevels(df_forimp[[v]]) == 2) {
      method[v] <- "logreg"
    } else if (is.numeric(df_forimp[[v]])) {
      method[v] <- "pmm"
    } else {
      method[v] <- ""
    }
  }
}




passive_map <- c(
  College    = "~I(as.integer(EDATT_W4 == '3'))",
  AAVOC      = "~I(as.integer(EDATT_W4 == '2'))",
  ParCollege = "~I(as.integer(as.numeric(ParEd_ord) %in% c(4,5)))",
  ParAAVOC   = "~I(as.integer(as.numeric(ParEd_ord) %in% c(2,3)))",
  BlackE     = "~I(as.integer(ETHRACE == 'Black'))",
  LatinxE    = "~I(as.integer(ETHRACE == 'Latinx'))",
  OtherE     = "~I(as.integer(ETHRACE == 'Other'))",
  ConProt    = "~I(as.integer(RELTRAD == 'ConProt'))",
  MainProt   = "~I(as.integer(RELTRAD == 'MainProt'))",
  BlackProt  = "~I(as.integer(RELTRAD == 'BlackProt'))",
  Catholic   = "~I(as.integer(RELTRAD == 'Catholic'))",
  Jewish     = "~I(as.integer(RELTRAD == 'Jewish'))",
  OtherRel     = "~I(as.integer(RELTRAD == 'Other'))",
  None       = "~I(as.integer(RELTRAD == 'None'))",
  INDE       = "~I(as.integer(RELTRAD == 'INDE'))")



for (nm in names(passive_map)) {
  if (nm %in% names(df_forimp)) {
    method[nm] <- passive_map[[nm]]
  }
}

# Set non-imputed variables (that are not passives) to ""
method_names <- names(method)
non_targets <- setdiff(method_names, c(vars_to_impute, names(passive_map)))
method[non_targets] <- ""

# Set up predictor matrix
predictorMatrix <- mice::make.predictorMatrix(df_forimp)

# Exclude non-imputed vars from being imputed as targets
predictorMatrix[!(rownames(predictorMatrix) %in% c(vars_to_impute, names(passive_map))), ] <- 0

# Don't use as predictors
vars <- c("CR2_1","CR2_2","CR3_1","CR3_2","CR4_2","CR4_1",
          "PEDUC1","PSPEDUC","PEDUC3","PSPEDUC3",
          "College","AAVOC","ParCollege","ParAAVOC", "BiG4NA",
          "FirstParEd",  "SecondParEd", "ParEd",
          "ParCollege",  "ParAAVOC",    "Male",   "AsianPR",     "OtherRel",
          "BlackPR",  "LatinxPR",    "NativePR",  "OtherPR",  "BlackE", "LatinxE", "OtherE")


# Drop as targets
predictorMatrix[intersect(vars, rownames(predictorMatrix)), ] <- 0

# Drop as predictors
predictorMatrix[, intersect(vars, colnames(predictorMatrix))] <- 0

parents_present <- intersect(c("ETHRACE","EDATT_W4","ParEd_ord","RELTRAD"), colnames(predictorMatrix))
dummy_candidates <- intersect(c("BlackE","LatinxE","OtherE",
                                "College","AAVOC","ParCollege","ParAAVOC",
                                "ConProt","MainProt","BlackProt","Catholic","Jewish","Other","None","INDE"),
                              colnames(predictorMatrix))
if (length(parents_present)) {
  predictorMatrix[, dummy_candidates] <- 0
}

# --- Summary Table of Imputation Settings ---
missing_pct <- sapply(df_forimp, function(x) sum(is.na(x)) / length(x)) * 100
var_class <- sapply(df_forimp, function(x) class(x)[1])
assigned_method <- method
used_as_predictor <- colSums(predictorMatrix != 0) > 0
imputation_summary <- data.frame(
  Variable = names(df_forimp),
  MissingPercent = round(missing_pct, 1),
  Class = var_class,
  Method = assigned_method,
  UsedAsPredictor = used_as_predictor,
  row.names = NULL
)

imputation_summary[imputation_summary$UsedAsPredictor == TRUE,]
imputation_summary[imputation_summary$Method != "",]
imputation_summary

# --- Step 3: Run the imputation ---
df_imp <- mice::mice(
  df_forimp, m = N.Imp, method = method, predictorMatrix = predictorMatrix,
  maxit = max.it, seed = seed
)
log <- df_imp$loggedEvents

# helper: force all 2-level unordered factors to numeric
postprocess_completed <- function(d, force_numeric = c("MS1","PST")) {
  # 1. Turn 2-level unordered factors into numeric (0/1)
  for (nm in names(d)) {
    if (is.factor(d[[nm]]) && !is.ordered(d[[nm]]) && nlevels(d[[nm]]) == 2) {
      d[[nm]] <- as.integer(d[[nm]]) - 1
    }
  }
  
  # 2. Force specific ordered vars (like MS1, PST) into numeric
  for (nm in intersect(force_numeric, names(d))) {
    d[[nm]] <- as.numeric(as.character(d[[nm]]))
  }
  
  d
}

imputed_data_list <- lapply(complete(df_imp, "all"), postprocess_completed)

subset_post <- function(d) {
  d %>%
    # drop non-Christian participants
    dplyr::filter(!(RELTRAD %in% c("Jewish", "Other"))) %>%
    # drop unaffiliated with no Christian parent
    dplyr::filter(!(RELTRAD == "INDE" & ParCatholic == 0 & ParProtestant == 0)) %>%
    dplyr::filter(!(RELTRAD == "None" & ParCatholic == 0 & ParProtestant == 0)) %>%
    # drop W1 non-believers from imputed datasets
    dplyr::filter(BiG1 != "0")
}

imputed_data_list <- lapply(imputed_data_list, subset_post)


mice.imp <- imputed_data_list


saveRDS(df_imp, file = "df_imp.rds")
saveRDS(mice.imp, file = "imputed_data_list.rds")

plot(df_imp) 

table(mice.imp[[1]]$EDATT_W4)
table(mice.imp[[1]]$ParEd_ord)
table(mice.imp[[1]]$ETHRACE)
table(mice.imp[[1]]$RELTRAD)

mice.imp[[1]] %>%
  filter(is.na(MS1)) %>%
  select(all_of(imputation_summary %>%
                  filter(UsedAsPredictor) %>%
                  pull(Variable)))

# Initial checks ----------------------------------------------------------

library(MASS)      # for polr() and other functions
library(sjPlot)    # for model diagnostic plots
library(purrr)     # for iterating over imputations

# 1. Fit LINEAR REGRESSIONS & Pooling (Supported by mice)

# Using the with() function to apply the linear model to each imputed dataset
lm_fit <- with(df_imp, lm(as.numeric(BiG4) ~ as.numeric(MS1) + BlackE + LatinxE + OtherE + ParEd_ord))

# Pool the results automatically using Rubin's rules
pooled_lm <- mice::pool(lm_fit)
summary(pooled_lm)

# With BiG stability:
lm_fit_stb <- with(df_imp, lm(as.numeric(BiG4) ~ as.numeric(MS1) + BiG3 +
                                BlackE + LatinxE + OtherE + ParEd_ord))
pooled_lm_stb <- pool(lm_fit_stb)
summary(pooled_lm_stb)

# 2. Fit POLR MODELS and Pooling (Manual Pooling via Rubin's Rules)

# For comparing different link functions via AIC, we  loop over the imputations
link_methods <- c("logistic", "probit", "cloglog", "loglog")

# Helper function to extract AIC values from one imputed dataset for given link functions
get_aic <- function(link_method, mice.imp) {
  # Model without BiG3
  mod1 <- polr(as.factor(BiG4) ~ MS1, data = mice.imp, method = link_method, Hess = TRUE)
  # Model with controls (without BiG3)
  mod2 <- polr(as.factor(BiG4) ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord,
               data = mice.imp, method = link_method, Hess = TRUE)
  # Model with BiG3 and controls
  mod3 <- polr(as.factor(BiG4) ~ MS1 + BiG3 + BlackE + LatinxE + OtherE + ParEd_ord,
               data = mice.imp, method = link_method, Hess = TRUE)
  
  tibble(
    method = link_method,
    AIC = c(AIC(mod1), AIC(mod2), AIC(mod3)),
    model = c("No BiG3", "With Controls", "With BiG3 + Controls")
  )
}

# Run AIC comparisons across imputations and link functions
aic_results <- map_dfr(mice.imp, function(imp_data) {
  map_dfr(link_methods, function(link) {
    get_aic(link, imp_data)
  })
}, .id = "imputation")

# Pooling AIC values across imputations

# Note: Because there is no standard pooling method for AIC in multiple imputation,
# a common approach is to average the AIC values across the m imputations.
pooled_aic <- aic_results %>%
  group_by(method, model) %>%
  summarise(
    pooled_AIC = mean(AIC),
    aic_sd = sd(AIC),
    n = n(),
    .groups = "drop"
  )

print(pooled_aic)

# Plot the pooled AIC (averaged over imputations) for each link function and model type
ggplot(pooled_aic %>% filter(model == "With BiG3 + Controls"), aes(x = method, y = pooled_AIC, group = model, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = pooled_AIC - aic_sd, ymax = pooled_AIC + aic_sd), width = 0.2) +
  labs(
    title = "Pooled AIC by Link Function",
    subtitle = "Mean AIC and standard deviation across imputations",
    x = "Link Function",
    y = "Pooled (Average) AIC",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)


# Fit polr models across all imputations

# Ordinal regression without BiG3 and controls
rev_fit_ord <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1, data = .x, Hess = TRUE, method = "cloglog"))
# Ordinal regression with controls (without BiG3)
rev_fit_ord_cont <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1 + BlackE + LatinxE + OtherE + ParEd_ord,
                                         data = .x, Hess = TRUE, method = "cloglog"))
# Ordinal regression with BiG3 and controls
rev_fit_ord_stb <- map(mice.imp, ~ polr(as.factor(BiG4) ~ MS1 + BiG3 + BlackE + LatinxE + OtherE + ParEd_ord,
                                        data = .x, Hess = TRUE, method = "cloglog"))

# Define a custom pooling function for polr models
pool_polr <- function(models) {
  # Number of imputations
  m <- length(models)
  
  # Extract coefficient summaries (ignoring intercept/threshold rows)
  coef_list <- lapply(models, function(model) {
    coefs <- coef(summary(model))
    # Keep only coefficient rows that are not thresholds (those containing a "|")
    coefs[!grepl("\\|", rownames(coefs)), , drop = FALSE]
  })
  
  # Get all unique parameter names from the imputations
  all_params <- unique(unlist(lapply(coef_list, rownames)))
  
  pooled_results <- lapply(all_params, function(param) {
    # Extract the estimate for param from each imputation
    estimates <- sapply(coef_list, function(coef_mat) {
      if (param %in% rownames(coef_mat)) coef_mat[param, "Value"] else NA
    })
    estimates <- na.omit(estimates)
    
    # Extract standard errors and compute within-imputation variance (square of SE)
    ses <- sapply(coef_list, function(coef_mat) {
      if (param %in% rownames(coef_mat)) coef_mat[param, "Std. Error"] else NA
    })
    ses <- na.omit(ses)
    
    U_bar <- mean(ses^2)     # average within-imputation variance
    B <- var(estimates)      # between-imputation variance
    Q_bar <- mean(estimates)  # pooled estimate
    
    # Total variance: within + (1 + 1/m)*between variance
    T_var <- U_bar + (1 + 1/m) * B
    pooled_se <- sqrt(T_var)
    
    # t-statistic and approximate degrees of freedom
    t_val <- Q_bar / pooled_se
    df <- if (B == 0) Inf else (m - 1) * (1 + U_bar / ((1 + 1/m) * B))^2
    p_val <- 2 * pt(-abs(t_val), df)
    
    # Return a data frame with pooled results on the log scale
    data.frame(
      term = param,
      estimate = Q_bar,
      se = pooled_se,
      t_value = t_val,
      df = df,
      p_value = p_val,
      stringsAsFactors = FALSE
    )
  })
  
  pooled_df <- do.call(rbind, pooled_results)
  return(pooled_df)
}

# Pool the polr model estimates for each model type
pooled_or_df_1 <- pool_polr(rev_fit_ord) %>% 
  mutate(model = "POLR: No BiG3, no controls")
pooled_or_df_2 <- pool_polr(rev_fit_ord_cont) %>% 
  mutate(model = "POLR: No BiG3, with controls")
pooled_or_df_3 <- pool_polr(rev_fit_ord_stb) %>% 
  mutate(model = "POLR: With BiG3 and controls")

# For easier interpretation, compute Odds Ratios and 95% Confidence Intervals
# (Exponentiating the pooled estimate and its confidence bounds)
pooled_or_df_1 <- pooled_or_df_1 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))
pooled_or_df_2 <- pooled_or_df_2 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))
pooled_or_df_3 <- pooled_or_df_3 %>% 
  mutate(odds_ratio = exp(estimate),
         conf.low = exp(estimate - 1.96 * se),
         conf.high = exp(estimate + 1.96 * se))

# 3. Combine all pooled polr results and Plot Odds Ratios

plot_df <- bind_rows(
  pooled_or_df_1,
  pooled_or_df_2,
  pooled_or_df_3
)

# Plotting the Odds Ratios with their Confidence Intervals
ggplot(plot_df, aes(x = term, y = odds_ratio, color = model)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.6), width = 0.2) +
  geom_text(aes(label = paste0("p=", signif(p_value, 2))), 
            position = position_dodge(width = 0.9), hjust = -0.2, size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Pooled Model Comparison: Odds Ratios (exp(β))",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Model"
  ) +
  theme_minimal(base_size = 13)


# Models --------------------------------------------------------

## Baseline --------------------------------------------------- 
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

   
  ## MS1 -> BiG2 -> BiG3

ms1.big2.big3.big4 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1.big3.big4 := h1b*ar4


h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

sumH1 := h1c_ + 
         ms1.big2.big3.big4 + 
         ms1.big3.big4 "


base_mi <- lavaan.mi::sem.mi(base, mice.imp, ordered = c("BiG1","BiG2", "BiG3", "BiG4"), meanstructure = T,
                             estimator = "WLSMV", 
                             missing = "pairwise", 
                             parameterization = "theta", std.lv = T)

summary(base_mi)
parameterEstimates.mi(base_mi, asymptotic = TRUE)

parameterEstimates.mi(base_mi, asymptotic = TRUE) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
parameterEstimates.mi(base_mi, asymptotic = TRUE) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))
parameterEstimates.mi(base_mi, asymptotic = TRUE) %>% filter(op == "~") %>% writexl::write_xlsx("base_bs.xlsx")

standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(op == ":=")
standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))

fitmeasures(base_mi)

lavResiduals.mi(base_mi, type = "cor") %>% pluck("res.cov") %>%
  corrplot::corrplot(method = "number")

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
   
   
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3*ar4

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c"


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

b <- standardizedSolution.mi(base_mi)
b[,5:10] <-  b[,5:10] %>% round(3)
b %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
b %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))



## Controls ---------------------------------------------------

controls <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE+ ParRit
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE+ ParRit
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE+ ParRit
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  
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

      BiG1 | l*t1
   
   BiG2 | k*t1
   BiG2 | l*t2

   BiG3 | k*t1
   BiG3 | l*t2
   
   BiG4 | k*t1
   BiG4 | l*t2

   Catholic ~~ MainProt + BlackProt
   MainProt ~~ BlackProt
      AAVOC ~~ College

Inc3 ~~ College"

controls_mi <- lavaan.mi::sem.mi(controls, mice.imp ,
                                 ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit"), 
                                 meanstructure = T,
                                 estimator = "WLSMV", missing = "pairwise", 
                                 parameterization = "theta", std.lv = T)

controls_resid <- lavResiduals.mi(controls_mi, type = "cor")

controls_resid %>% pluck("res.cov") %>%
  corrplot::corrplot(method = "number")


summary(controls_mi)
parameterEstimates.mi(controls_mi, asymptotic = TRUE) %>% filter(op == ":=")
parameterEstimates.mi(controls_mi, asymptotic = TRUE) %>% filter(op == "~~")

parameterEstimates.mi(controls_mi, asymptotic = TRUE) %>% filter(op == "~") %>% writexl::write_xlsx("controls_bs.xlsx")

parameterEstimates.mi(controls_mi, asymptotic = TRUE) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
parameterEstimates.mi(controls_mi, asymptotic = TRUE) %>% data.frame() %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))

standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(op == ":=")
standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(op == "~~", se > 0)
fitmeasures(controls_mi)

bind_rows(
standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(lhs == "sumH1") %>% 
  select(est.std) %>% mutate(model = "baseline (original)", path = "summed_H1"), 

standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(lhs == "sumH1") %>% 
  select(est.std) %>% mutate(model = "controls (original)", path = "summed_H1"),

standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(label == "ms1_big4") %>% 
  select(est.std) %>% mutate(model = "baseline (original)", path = "MS1->BiG4"), 

standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(label == "h1c") %>% 
  select(est.std) %>% mutate(model = "controls (original)", path = "MS1->BiG4"),

standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(label == "h1b") %>% 
  select(est.std) %>% mutate(model = "baseline (original)", path = "MS1->BiG3"), 

standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(label == "h1b") %>% 
  select(est.std) %>% mutate(model = "controls (original)", path = "MS1->BiG3"),

standardizedSolution.mi(base_mi) %>% data.frame() %>% filter(label == "h1a") %>% 
  select(est.std) %>% mutate(model = "baseline (original)", path = "MS1->BiG2"), 

standardizedSolution.mi(controls_mi) %>% data.frame() %>% filter(label == "h1a") %>% 
  select(est.std) %>% mutate(model = "controls (original)", path = "MS1->BiG2")
) %>%  mutate(est.std = round(est.std, 2))

estimate_sample_size_from_rmsea <- function(fit) {
  # Get fit measures
  fit_measures <- fitMeasures(fit)
  
  # Extract required values
  chisq <- fit_measures["chisq"]
  df <- fit_measures["df"]
  rmsea <- fit_measures["rmsea"]
  
  # Compute estimated sample size
  N_estimated <- (chisq - df) / (rmsea^2 * df) + 1
  
  return(N_estimated)
}


estimate_sample_size_from_rmsea(controls_mi)

# RMSEA_null by hand
sqrt ( ( 1080.404-66)/(66*2829) )

controls <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE+ ParRit
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE+ ParRit
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE+ ParRit
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  
  ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar3*ar4

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
   
   
  
   Catholic ~~ MainProt + BlackProt
   MainProt ~~ BlackProt
      AAVOC ~~ College

Inc3 ~~ College

"


controls_mi <- lavaan.mi::sem.mi(controls, mice.imp,
                               ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit"), 
                               meanstructure = T,
                               estimator = "WLSMV", missing = "pairwise", 
                               parameterization = "theta", std.lv = T)


fitmeasures(controls_mi)

c <- standardizedSolution.mi(controls_mi)
c[,5:10] <-  c[,5:10] %>% round(3)
c %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
c %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))

c %>% filter(op == "~") %>% writexl::write_xlsx("controls_betas.xlsx")

controls_mi_params <- extract_defined_params_lavaanmi(controls_mi)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls <- restriktor::goric(controls_mi_params[["est"]], VCOV = controls_mi_params[["VCOV"]],
                  hypotheses = list(hypothesis), comparison = "complement")

benchmark(H1_controls)


## Full ordinal (slightly cut) ---------------------------------------------------


full_ordinal <- "

PST_l =~ PST
PST ~~ 0*PST

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE + ParRit
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE+ ParRit
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE+ ParRit
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST_l
 
    
    eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ h2.1a*H2 +h2.2a*T2 + PR2_l + CR2_l 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h2.1d*H3 + h3.1b*PR3 +  h3.2b*CR3 

  # Misc
 
  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
 
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  


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
 
 
  
## MS1 -> BiG2 -> BiG3 -> BiG4
 
ms1_big3 := h1a*ar3*ar4
 
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
   


   
     sumH1 := h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4
  sumH2_1 := h3.1direct + h2_big4 + h2_h3_big4
  sumH2_2 := h3.2direct + t2_big4
  
     Catholic ~~ MainProt + BlackProt
   MainProt ~~ BlackProt

Inc3 ~~ College

ParRit ~~ CR2_l + CR3 + PR2_l + PR3
   "


full_ordinal_fit <- lavaan.mi::sem.mi(full_ordinal, mice.imp, 
                                      estimator = "WLSMV", parameterization = "theta",
                                      meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                     "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                      missing = "pairwise",  control = list(iter.max = 10e5))
full_resid <- lavResiduals.mi(full_ordinal_fit, type = "cor")

full_resid %>% pluck("res.cov") %>%
  corrplot::corrplot(method = "number")

summary(full_ordinal_fit)
fitmeasures(full_ordinal_fit)

parameterEstimates.mi(full_ordinal_fit, asymptotic = T) %>% filter(op == ":=")
parameterEstimates.mi(full_ordinal_fit, asymptotic = T) %>% filter(op == "~")
parameterEstimates.mi(full_ordinal_fit, asymptotic = T) %>% filter(op == "~~")

parameterEstimates.mi(full_ordinal_fit, asymptotic = T) %>% filter(op == "~") %>%
  filter(rhs == "MS1")


parameterEstimates.mi(full_ordinal_fit, asymptotic = T) %>% filter(op == "~") %>%
  openxlsx::write.xlsx("full_bs.xlsx")

s <- standardizedSolution.mi(full_ordinal_fit) %>% data.frame() 
s[,5:10] <-  s[,5:10] %>% round(3)
s %>% filter(op == "~") %>% filter(grepl("BiG", lhs))
s %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("BiG", rhs))
s %>% filter(op == "~") %>% filter(grepl("BiG", lhs)) %>% filter(grepl("MS", rhs))
s %>% filter(op == "~") %>% filter(grepl("H2", lhs)) %>% filter(grepl("MS", rhs))
s %>% filter(op == "~") %>% filter(grepl("T2", lhs)) %>% filter(grepl("MS", rhs))

s %>% filter(op == "~") %>% filter(grepl("H2", rhs)) %>% filter(grepl("PR", lhs))
s %>% filter(op == "~") %>% filter(grepl("PR", rhs))
s %>% filter(op == "~") %>% filter(grepl("CR", lhs))
s %>% filter(op == "~") %>% filter(grepl("CR", rhs))
s %>% filter(op == ":=") %>% filter(grepl("indirect", label))
s %>% filter(op == ":=") %>% filter(grepl("sum", label))
s %>% filter(op == ":=") 
s %>% filter(op == "~~") 




estimate_sample_size_from_rmsea(full_ordinal_fit)

sqrt ( (  2583.904  -190)/(190*2929) )


full_ordinal <- "

PST_l =~ PST
PST ~~ 0*PST

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE + ParRit
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE+ ParRit
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE+ ParRit
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST_l
 
     eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ h2.1a*H2 +h2.2a*T2 + PR2_l + CR2_l 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h2.1d*H3 + h3.1b*PR3 +  h3.2b*CR3 

  # Misc
 
  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
 
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC


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
 
ms1_big3 := h1a*ar3*ar4
 
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
   
   
     Catholic ~~ MainProt + BlackProt
   MainProt ~~ BlackProt

Inc3 ~~ College

ParRit ~~ CR2_l + CR3 + PR2_l + PR3
   "

full_ordinal_fit <- lavaan.mi::sem.mi(full_ordinal, mice.imp, 
                                      estimator = "WLSMV", parameterization = "theta",
                                      meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                     "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                      missing = "pairwise",  control = list(iter.max = 10e5))



standardizedSolution.mi(full_ordinal_fit) %>% data.frame() %>% filter(op == ":=")

fitmeasures(full_ordinal_fit)


## Full ordinal (preregistered) ---------------------------------------------------


full_ordinal_prereg <- "

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ PR2 + CR2 + h2.1a*H2 + h2.2a*T2 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h3.1b*PR3 + h3.2b*CR3 + h2.1d*H3
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE

  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  
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
   
   PR2 ~ 0*1
   PR3 ~ NA*1
   
   CR2 ~ 0*1
   CR3 ~ NA*1
   
   H2 ~ 0*1
   H3 ~ NA*1

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
full_ordinal_fit_prereg <- lavaan.mi::sem.mi(full_ordinal_prereg, mice.imp, 
                                      estimator = "WLSMV", parameterization = "delta",
                                      meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                     "PR3", "CR2", "CR3", "H2", "H3", "T2"),
                                      missing = "pairwise",  control = list(iter.max = 10e5))

summary(full_ordinal_fit_prereg)
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

full_ordinal_mi_eval_h2_1 <-  restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H2.1), comparison = "complement")

full_ordinal_mi_eval_h2_1_ben <- benchmark(full_ordinal_mi_eval_h2_1)

# H2.2: 
# T2 -> BiG4
# If H1+, then:

H2.2 <- "h3.2direct + t2_big4 < 0" # correct direction?

full_ordinal_mi_eval_h2_2 <- restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H2.2), comparison = "complement")

full_ordinal_mi_eval_h2_2_ben <- benchmark(full_ordinal_mi_eval_h2_2)

# Explanatory paths

# H3.1: 
# H2 -> PR3 -> BiG4

# Is there any indirect effect?
H3.1any <- "abs(h3.1indirect) > 0" 

# First as complement

full_ordinal_mi_eval_h3_1 <- restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.1any = H3.1any), comparison = "complement")

full_ordinal_mi_eval_h3_1_ben <- benchmark(full_ordinal_mi_eval_h3_1)

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
H3.1partneg <- "h3.1indirect < 0 ; abs(h3.1direct) > 0" 

H3.1partneg_mi_eval <- restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                  hypotheses = list(
                    H3.1partneg = H3.1partneg), comparison = "complement")

benchmark(H3.1partneg_mi_eval)

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



H3.2partneg_mi_eval <- restriktor::goric(full_ordinal_mi_params[["est"]], VCOV = full_ordinal_mi_params[["VCOV"]],
                                         hypotheses = list(
                                           H3.2partneg = H3.2partneg), comparison = "complement")
benchmark(H3.2partneg_mi_eval)


# Sensitivity checks ------------------------------------------------------
## MLE sensitivity-------------------------------------------------------------------------
df_ml <- df


df_ml[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
         "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")] <- lapply(df_ml[,c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
         "PR3", "PR4", "CR2", "CR3", "CR4", "H2", "H3", "H4", "T2", "PST")], FUN = as.numeric)

dfSummary(df_ml[,c("BiG1", "BiG2", "BiG3", "BiG4", "MS1")])


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
   BiG1 ~ 0*MS1  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4
   
h1a_ := h1a
h1b_ := h1b
h1c_ := h1c

# MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4
"

base_mi_ml <- lavaan.mi::sem.mi(base_ml, mice.imp_lin, meanstructure = T,
                           estimator = "MLR", missing = "fiml", std.lv = F)
fitmeasures(base_mi_ml)

base_mi_ml_params <- extract_defined_params_lavaanmi(base_mi_ml)

H1_base_ml <- restriktor::goric(base_mi_ml_params[["est"]], VCOV = base_mi_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")

H1_base_ml_ben <- benchmark(H1_base_ml)

# Controls model ML

controls_ml <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE + ParRit
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE+ ParRit
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE+ ParRit
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1 
   BiG4 ~ ar4*BiG3 + h1c*MS1 
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  
     Catholic ~~ MainProt + BlackProt
   MainProt ~~ BlackProt
      AAVOC ~~ College

Inc3 ~~ College

    ## MS1 -> BiG2 -> BiG3

ms1_big3 := h1a*ar3

## MS1 -> BiG3 -> BiG4

ms1_big4 := h1b*ar4

h1a_ := h1a
h1b_ := h1b
h1c_ := h1c
"

controls_mi_ml <- lavaan.mi::sem.mi(controls_ml, mice.imp_lin, meanstructure = T,
                                 estimator = "MLR", missing = "fiml", std.lv = F)

controls_mi_ml_params <- extract_defined_params_lavaanmi(controls_mi_ml)


H1_controls_ml <- restriktor::goric(controls_mi_ml_params[["est"]], VCOV = controls_mi_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")

H1_controls_ml_ben <- benchmark(H1_controls_ml)

full_linear <- "
   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST

  
   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ PR2 + CR2 + h2.1a*H2 + h2.2a*T2 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h3.1b*PR3 + h3.2b*CR3 + h2.1d*H3
   
   eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

  # Misc

  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE

  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC
  
  # PR


 PR3 ~  PR2 + MS1 + h3.1a*H2 + T2

 # CR

CR3 ~ CR2 + H2 + h3.2a*T2
  
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
h3.2direct := h2.2b

"

full_mi_ml <- lavaan.mi::sem.mi(full_linear, mice.imp_lin, meanstructure = T,
                                    estimator = "MLR", missing = "fiml", std.lv = F)


full_mi_ml_params <- extract_defined_params_lavaanmi(full_mi_ml)

#H1

full_mi_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                    hypotheses = list(hypothesis), comparison = "complement")

full_mi_eval_ben <- benchmark(full_mi_eval)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

full_mi_eval_h2_1 <-  restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                          hypotheses = list(
                                            H2.1), comparison = "complement")

full_mi_eval_h2_1_ben <- benchmark(full_mi_eval_h2_1)

# H2.2: 
# T2 -> BiG4
# If H1+, then:

full_mi_eval_h2_2 <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                         hypotheses = list(
                                           H2.2), comparison = "complement")

full_mi_eval_h2_2_ben <- benchmark(full_mi_eval_h2_2)

# H3.1: 
# H2 -> PR3 -> BiG4

H3.1partneg_mi_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                           hypotheses = list(
                                             H3.1partneg = H3.1partneg), comparison = "complement")

full_mi_eval_h3_1_ben <- benchmark(H3.1partneg_mi_eval)

# H3.2: 
# T2 -> CR3 -> BiG4

H3.2partneg_mi_eval <- restriktor::goric(full_mi_ml_params[["est"]], VCOV = full_mi_ml_params[["VCOV"]],
                                           hypotheses = list(
                                             H3.2partneg = H3.2partneg), comparison = "complement")
full_mi_eval_h3_2_ben <- benchmark(H3.2partneg_mi_eval)
## FIML ----------------------------------------------------------

# baseline
base_fiml <- lavaan::sem(base_ml, df_ml, estimator = "MLR", 
                         missing = "fiml", meanstructure = T, 
                         std.lv = F, std.ov = F)
summary(base_fiml, std = T, fit = T)
lavInspect(base_fiml, what = "sampstat")
lavInspect(base_fiml, what = "mean.ov")


base_mi_fiml_params <- extract_defined_params_lavaan(base_fiml)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base_fiml <- restriktor::goric(base_mi_fiml_params[["est"]], VCOV = base_mi_fiml_params[["VCOV"]],
                                  hypotheses = list(hypothesis), comparison = "complement")

H1_base_ben_fiml <- benchmark(H1_base_fiml)

# controls

controls_fiml <- lavaan::sem(controls_ml, df_ml, estimator = "MLR", 
                             missing = "fiml", meanstructure = T, 
                             std.lv = F, std.ov = F)

lavInspect(controls_fiml, what = "sampstat")
lavInspect(controls_fiml, what = "mean.ov")

summary(controls_fiml, std = T, fit = T)


controls_mi_fiml_params <- extract_defined_params_lavaan(controls_fiml)


H1_controls_fiml <- restriktor::goric(controls_mi_fiml_params[["est"]], VCOV = controls_mi_fiml_params[["VCOV"]],
                                      hypotheses = list(hypothesis), comparison = "complement")
H1_controls_fiml_ben <- benchmark(H1_controls_fiml)

# full

full_fiml <- lavaan::sem(full_linear, df_ml, estimator = "MLR", 
                         missing = "fiml", 
                         meanstructure = T, std.lv = F, std.ov = F)

summary(full_fiml, std = T, fit = T)

full_fiml_ml_params <- extract_defined_params_lavaan(full_fiml)

#H1

full_fiml_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                          hypotheses = list(hypothesis), comparison = "complement")

full_fiml_eval_ben <- benchmark(full_fiml_eval)

# H2.1: 
# H2 -> BiG4
# If H1+, then:

full_fiml_eval_h2_1 <-  restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                                hypotheses = list(
                                                  H2.1), comparison = "complement")

full_fiml_eval_h2_1_ben <- benchmark(full_fiml_eval_h2_1)

# H2.2: 
# T2 -> BiG4
# If H1+, then:

full_fiml_eval_h2_2 <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                               hypotheses = list(
                                                 H2.2), comparison = "complement")

full_fiml_eval_h2_2_ben <- benchmark(full_fiml_eval_h2_2)

# H3.1: 
# H2 -> PR3 -> BiG4

H3.1partneg_fiml_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                         hypotheses = list(
                                           H3.1partneg = H3.1partneg), comparison = "complement")

full_fiml_eval_h3_1_ben <- benchmark(H3.1partneg_fiml_eval)

# H3.2: 
# T2 -> CR3 -> BiG4

H3.2partneg_fiml_eval <- restriktor::goric(full_fiml_ml_params[["est"]], VCOV = full_fiml_ml_params[["VCOV"]],
                                         hypotheses = list(
                                           H3.2partneg = H3.2partneg), comparison = "complement")
full_fiml_eval_h3_2_ben <- benchmark(H3.2partneg_fiml_eval)

## Pairwise ordinal --------------------------------------------------------


base_pw <- lavaan::sem(base, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4"), meanstructure = T,
                               estimator = "WLSMV", parameterization = "theta", missing = "pairwise",
                          std.lv = T)
                        
summary(base_pw, fit = T, std = T)
base_pw_ml_params <- extract_defined_params_lavaan(base_pw)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_base_pw <- restriktor::goric(base_pw_ml_params[["est"]], VCOV = base_pw_ml_params[["VCOV"]],
                             hypotheses = list(hypothesis), comparison = "complement")
H1_base_pw

H1_base_pw_ben <- benchmark(H1_base_pw)



controls_pw <- lavaan::sem(controls, df, ordered = c("BiG1","BiG2", "BiG3", "BiG4", "ParRit", "PST"), meanstructure = T,
                       estimator = "WLSMV", parameterization = "theta", missing = "pairwise",
                       std.lv = T)

summary(controls_pw, fit = T, std = T)

controls_pw_params <- extract_defined_params_lavaan(controls_pw)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

H1_controls_pw <- restriktor::goric(controls_pw_params[["est"]], VCOV = controls_pw_params[["VCOV"]],
                                hypotheses = list(hypothesis), comparison = "complement")
H1_controls_pw

H1_controls_pw_ben <- benchmark(H1_controls_pw)

full_ordinalx <- "

PST_l =~ PST
PST ~~ 0*PST

   BiG1 ~ Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE + OtherE
   BiG2 ~ Male + BlackProt + Catholic + MainProt   + BlackE + LatinxE + OtherE
   BiG3 ~ Male + BlackProt + Catholic + MainProt + BlackE + LatinxE + OtherE
   BiG4 ~ Age + Male + BlackProt + Catholic + MainProt  + BlackE + LatinxE  + OtherE +
   Inc3 + ParRit + College + AAVOC + PST_l
 
     eta_BiG =~ BiG1 + 1*BiG2 + 1*BiG3 + 1*BiG4

   BiG2 ~ BiG1 + h1a*MS1
   BiG3 ~ ar3*BiG2 + h1b*MS1+ h2.1a*H2 +h2.2a*T2 + PR2_l + CR2_l 
   BiG4 ~ ar4*BiG3 + h1c*MS1 + h2.1b*H2 + h2.2b*T2 + h2.1d*H3 + h3.2b*PR3 +  h3.1b*CR3 

  # Misc
 
  MS1 ~ ParCollege + ParAAVOC + BlackE + LatinxE + OtherE
  BlackProt ~ BlackE
  Catholic ~ LatinxE
  BlackProt + MainProt + Catholic ~ OtherE
 
  College + AAVOC ~ ParCollege + ParAAVOC + Age + MS1
  Inc3 ~ MS1
  ParRit ~ ParCollege + ParAAVOC


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

part_ordinal_fit <- lavaan::sem(model = full_ordinal, data = df, 
                                estimator = "WLSMV", parameterization = "theta",
                                meanstructure = T, ordered = c("BiG1", "BiG2", "BiG3", "BiG4", "ParRit", "PR2", 
                                                                 "PR3", "CR2", "CR3", "H2", "H3", "T2", "PST"),
                                missing = "pairwise",  control = list(iter.max = 10e5), std.lv = T)

summary(part_ordinal_fit, std = T, fit = T)

full_ordinal_pw_params <- extract_defined_params_lavaan(part_ordinal_fit)

hypothesis <-  "h1a_ + h1b_ + h1c_ + ms1_big3 + ms1_big4 < 0"

full_ordinal_pw_ord <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                    hypotheses = list(hypothesis), comparison = "complement")
full_ordinal_pw_ord

full_ordinal_pw_ord_ben <- benchmark(full_ordinal_pw_ord)


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


H3.1full_eval <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                   hypotheses = list(
                                     H3.1part = H3.1part,
                                     H3.1full = H3.1full), comparison = "unconstrained")

benchmark(H3.1full_eval)

# If par. med supported, then:
# Is there par. med with negative indirect eff?

H3.1full_eval_c <- restriktor::goric(full_ordinal_pw_params[["est"]], VCOV = full_ordinal_pw_params[["VCOV"]],
                                     hypotheses = list(
                                       H3.1partneg = H3.1partneg), comparison = "complement")

H3.1full_eval_c_ben <- benchmark(H3.1full_eval_c)

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

H3.2full_eval_ben <- benchmark(H3.2full_eval)




# Nested models -----------------------------------------------------------

# ---- 1) Base model: all hypothesis paths fixed to zero ----
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

# 1) All user-defined (':=') parameters per model in `fits`
defined_by_model <- imap(
  fits,
  ~ parameterEstimates.mi(.x, asymptotic = TRUE) %>%
    filter(lhs %in% c("BiG1", "BiG2", "BiG3", "BiG4")) %>% filter(op == "~") %>%
    mutate(model_name = .y, .before = 1) %>%
    select(model_name, lhs, op, rhs, est, se, z, pvalue, label)
) %>% list_rbind()


# --- Results ---
defined_by_model %>% View()


