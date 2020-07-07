# compare the new set contours (EDIC) with those from iSTAGING

source("fit_function.R")
options(stringsAsFactors=F)

DIR.IN <- "/cbica/home/pomponir/all_personal_projects/117_istaging_visualization/shiny_app_v4/data/"

data <- read.csv(paste0(DIR.IN, "ISTAGING_reducedOutputTable.csv"))
data <- subset(data, PROCESSING_BATCH==0)

# 2A
fit.2a <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_FullSet_Pred_Age_Adj",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(2.4, 2.6, 3.0))
save(fit.2a, file="../presaved_fits/fit_2A.rdata")

# 2B
fit.2b <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_ABSet_SPARE_AD",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(2.6, 2.8, 3.0),
  age.range=c(55, 90))
save(fit.2b, file="../presaved_fits/fit_2B.rdata")

# 2C
fit.2c <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_ABSet_SPARE_AD",
  z.column.name="CVLT_long", z.transformation="inverse",
  include.study=T, include.education=T, Z.LEVELS=c(-12, -10.5, -9),
  age.range=c(55, 90))
save(fit.2c, file="../presaved_fits/fit_2C.rdata")

# 4A
fit.4a <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_FullSet_Pred_Age_Adj",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(3.5, 4, 4.5),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4a, file="../presaved_fits/fit_4A.rdata")

### demonstrate usage
# image(fit.4a$x, fit.4a$y, fit.4a$Z, col=cm.colors(10))
# contour(fit.4a$x, fit.4a$y, fit.4a$Z, nlevels=10, add=T, levels=fit.4a$levels)

