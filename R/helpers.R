################################################################################
# USER SHOULD DEFINE VALUES HERE

# DEFINE DEFAULT VARIABLES
default.x.col <- "AGE"
default.y.col <- "TOTALBRAIN_VOLUME"
default.z.col <- "CS1"

# DEFINE AVAILABLE BIOCOVARIATES
biological.covariates <- c("AGE", "SEX", "DIAGNOSIS")

# DEFINE DEFAULT FILTERS HERE
default.studies <- c()
default.diagnosis <- "CN"
default.sex <- c("M", "F")

# DEFINE PATH TO DATA
PATH.DATA <- "data/DUMMY_DATASET.csv"

# DO NOT MODIFY BELOW THIS LINE
################################################################################

library(mgcv)
library(ggplot2)
library(visreg)

theme_set(theme_classic(base_size=18))

# READ INPUT FILE
data <- read.csv(PATH.DATA, stringsAsFactors=F)

# READ COLUMN DICTIONARY
dict <- read.csv("column_dictionary/column_dictionary.csv", stringsAsFactors=F)

# CONVERT COLUMNS
original.colnames <- colnames(data)
keep.columns <- rep(TRUE, length(original.colnames))
for (i in seq_along(original.colnames)){
  colname <- original.colnames[i]
  if (colname%in%dict$COL_NAME){
    key.index <- which(colname==dict$COL_NAME)
    keyname <- dict$KEY_NAME[key.index]
    if (keyname=="REMOVE"){
      keep.columns[i] <- FALSE
    }
    colnames(data)[i] <- keyname
  }
}
data <- data[, keep.columns]

# DEFINE SELECTABLE VARIABLES
selectable.variables <- colnames(data)

# FIND GROUPS
studies <- unique(data$STUDY)
diagnosis.groups <- unique(data$DIAGNOSIS)
sex.groups <- unique(data$SEX)


################################################################################
# HELPER FUNCTION FOR CONSTRUCTING GAM FORMULA STRINGS
construct_gam_formula_string <- function(X.COL, Y.COL, Z.COL,
                                         COVARIATES=c(),
                                         GAM.K=5,
                                         Y.CATEGORICAL=FALSE,
                                         SMOOTH.CONSTRUCTION="te(x, y)"){
  gam.formula.string <- paste0(Z.COL)
  if (Y.CATEGORICAL){
    gam.formula.string <- paste0(gam.formula.string, " ~ s(")
    gam.formula.string <- paste0(gam.formula.string, X.COL)
    gam.formula.string <- paste0(gam.formula.string, ", k=", GAM.K)
    gam.formula.string <- paste0(gam.formula.string, ")")
    gam.formula.string <- paste0(gam.formula.string, " + ", Y.COL)
  } else if (SMOOTH.CONSTRUCTION=="s(x) + s(y)"){
    gam.formula.string <- paste0(gam.formula.string, " ~ s(")
    gam.formula.string <- paste0(gam.formula.string, X.COL)
    gam.formula.string <- paste0(gam.formula.string, ", k=", GAM.K)
    gam.formula.string <- paste0(gam.formula.string, ")")
    gam.formula.string <- paste0(gam.formula.string, " + s(")
    gam.formula.string <- paste0(gam.formula.string, Y.COL)
    gam.formula.string <- paste0(gam.formula.string, ", k=", GAM.K)
    gam.formula.string <- paste0(gam.formula.string, ")")
  } else if (SMOOTH.CONSTRUCTION=="s(x, y)"){
    gam.formula.string <- paste0(gam.formula.string, " ~ s(")
    gam.formula.string <- paste0(gam.formula.string, X.COL)
    gam.formula.string <- paste0(gam.formula.string, ", ")
    gam.formula.string <- paste0(gam.formula.string, Y.COL)
    gam.formula.string <- paste0(gam.formula.string, ", k=", GAM.K)
    gam.formula.string <- paste0(gam.formula.string, ")")
  } else {
    gam.formula.string <- paste0(gam.formula.string, " ~ te(")
    gam.formula.string <- paste0(gam.formula.string, X.COL)
    gam.formula.string <- paste0(gam.formula.string, ", ")
    gam.formula.string <- paste0(gam.formula.string, Y.COL)
    gam.formula.string <- paste0(gam.formula.string, ", k=", GAM.K)
    gam.formula.string <- paste0(gam.formula.string, ")")
  }
  for (covar in COVARIATES){
    gam.formula.string <- paste0(gam.formula.string, " + ", covar)
  }
  
  return(gam.formula.string)
}





















