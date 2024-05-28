set.seed(44)

df_model <- df[,c("BiG1", "BiG2", "BiG3", "BiG4",
                  "MS1", "H2", "T2", "PR3", "CR3")]

# Number of imputed df 
N.Imp <-  10

names(df_model)
str(df_model)

# Create a column that contain the appropreate model for each variable - this only works if the variable is of the correct data type in the first place 
type <- c(rep("polr", 4),
           "rf", rep("polr", 4))

DF3_imp <- mice::mice(df_model, m = N.Imp, method = type)

# Exract each DF
mice.imp <- list()
for(i in 1:N.Imp) {
  
  mice.imp[[i]] <- mice::complete(DF3_imp, action= i, inc=FALSE)

  
  # remove spaces and other charecters on colnames
  colnames( mice.imp[[i]]) <- gsub(" ", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("/", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("-", "_", colnames( mice.imp[[i]]), fixed = TRUE)
}

