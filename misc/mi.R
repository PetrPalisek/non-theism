set.seed(44)

# Number of imputed df 
N.Imp <-  10

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

DF3_imp <- mice::mice(df, m = N.Imp, method = type)

# Exract each DF
mice.imp <- list()
for(i in 1:N.Imp) {
  
  mice.imp[[i]] <- mice::complete(DF3_imp, action= i, inc=FALSE)
  
  
  # remove spaces and other charecters on colnames
  colnames( mice.imp[[i]]) <- gsub(" ", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("/", "", colnames( mice.imp[[i]]), fixed = TRUE)
  colnames( mice.imp[[i]]) <- gsub("-", "_", colnames( mice.imp[[i]]), fixed = TRUE)
}

