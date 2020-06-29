# function to fit models and return anonomized output for plotting

library(mgcv)
options(warn=-1)  # suppresses plot warnings

fit.gam <- function(data, x.column.name, y.column.name, z.column.name,
                    z.transformation=NULL, include.study=F, include.education=F,
                    include.sex=F, age.range=c(40, 90), gam.family=gaussian(),
                    X.LIM.CUSTOM=NULL, Y.LIM.CUSTOM=NULL, Z.LEVELS=NULL){
  # remove rows where variables are missing
  selected.data <- data
  selected.data <- selected.data[!is.na(selected.data[, x.column.name]), ]
  selected.data <- selected.data[!is.na(selected.data[, y.column.name]), ]
  selected.data <- selected.data[!is.na(selected.data[, z.column.name]), ]
  # remove rows where subjects are not controls
  selected.data <- subset(selected.data, DIAGNOSIS%in%c("CN"))
  # remove rows where subjects are outside age range
  selected.data <- subset(selected.data, AGE>=age.range[1] & AGE<=age.range[2])
  # remove rows where subjects do not have education
  if (include.education){
    selected.data <- selected.data[!is.na(selected.data$EDUCATION), ]
  }
  # check number of rows removed
  print(paste(nrow(data) - nrow(selected.data), "rows removed from data."))
  # transform Y to cubic root scale if lesions, AND REMOVE ZEROS
  if (y.column.name=="L701"){
    lesions.zeros.index <- selected.data[, y.column.name]==0
    selected.data <- selected.data[!lesions.zeros.index, ]
    print(paste(sum(lesions.zeros.index), "rows removed for zero lesions."))
    selected.data[, y.column.name] <- selected.data[, y.column.name] ^ (1/3)
  }
  # variable transformations
  if (!is.null(z.transformation)){
    if (z.transformation=="zscore"){
      selected.data[, z.column.name] <- scale(selected.data[, z.column.name])
    } else if (z.transformation=="cubicroot"){
      original.values <- selected.data[, z.column.name]
      selected.data[, z.column.name] <- sign(original.values)*abs(original.values)^(1/3)
    } else if (z.transformation=="naturallog"){
      selected.data[, z.column.name] <- log(selected.data[, z.column.name] + 1)
    }
  }
  # fix study to most commonly occurring study
  fixed.study <- names(table(selected.data$STUDY))[which.max(table(selected.data$STUDY))]
  # fix education to median
  fixed.education <- median(selected.data$EDUCATION)
  # fix sex to female
  fixed.sex <- "F"
  # construct gam formula string
  gam.formula.str <- paste0(z.column.name, " ~ te(",
                            x.column.name, ", ",
                            y.column.name, ", k=5)")
  # handle additional covariates
  if (include.study){
    gam.formula.str <- paste0(gam.formula.str, " + STUDY")
  }
  if (include.education){
    gam.formula.str <- paste0(gam.formula.str, " + EDUCATION")
  }
  if (include.sex){
    gam.formula.str <- paste0(gam.formula.str, " + SEX")
  }
  # fit gam
  gam.fit <- gam(as.formula(gam.formula.str),
                 data=selected.data,
                 method="REML",
                 family=gam.family,
                 gamma=1)
  # create list of conditionally fixed covariates
  cond.list <- data.frame(STUDY=fixed.study, EDUCATION=fixed.education, SEX=fixed.sex)
  # find x and y limits, if not specified
  if (is.null(X.LIM.CUSTOM)){
    X.LIM.CUSTOM <- range(selected.data[, x.column.name])
  }
  if (is.null(Y.LIM.CUSTOM)){
    Y.LIM.CUSTOM <- range(selected.data[, y.column.name])
  }
  # create 3D surface with 100x100 resolution
  x <- seq(X.LIM.CUSTOM[1], X.LIM.CUSTOM[2], length.out=100)
  y <- seq(Y.LIM.CUSTOM[1], Y.LIM.CUSTOM[2], length.out=100)
  df.pred <- expand.grid(x=x, y=y)
  colnames(df.pred) <- c(x.column.name, y.column.name)
  df.pred <- cbind(df.pred, cond.list)
  df.pred$z <- predict(gam.fit, newdata=df.pred)
  exclude.points <- exclude.too.far(
    df.pred[, x.column.name], df.pred[, y.column.name],
    selected.data[, x.column.name],selected.data[, y.column.name], dist=0.05)
  z.masked <- df.pred$z; z.masked[exclude.points]<-NA
  Z <- matrix(data=z.masked, nrow=100, ncol=100)
  # return all elements required for plotting
  return(list("data"=df.pred, "x"=x, "y"=y, "Z"=Z, "levels"=Z.LEVELS))
}
