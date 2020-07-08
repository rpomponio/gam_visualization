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

# 3A
fit.3a <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="sMRI_FullSet_Pred_Age_Adj",
  include.study=T, Z.LEVELS=c(50, 60, 70, 80),
  age.range=c(40, 90))
save(fit.3a, file="../presaved_fits/fit_3A.rdata")

# 3B
fit.3b <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="sMRI_ABSet_SPARE_AD",
  include.study=T, Z.LEVELS=c(-2, -1, 0),
  age.range=c(55, 90))
save(fit.3b, file="../presaved_fits/fit_3B.rdata")

# 3C
fit.3c <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(2.4, 2.8, 3.2),
  age.range=c(40, 90))
save(fit.3c, file="../presaved_fits/fit_3C.rdata")

# 3D
fit.3d <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="CVLT_long", z.transformation="inverse",
  include.study=T, include.education=T, Z.LEVELS=c(-12.5, -11, -9.5),
  age.range=c(40, 90))
save(fit.3d, file="../presaved_fits/fit_3D.rdata")

# 3E
fit.3e <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="ABeta_STATUS",
  include.study=T, Z.LEVELS=c(0.2, 0.4, 0.6), gam.family=binomial(),
  age.range=c(55, 90))
save(fit.3e, file="../presaved_fits/fit_3E.rdata")

# 4A
fit.4a <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_FullSet_Pred_Age_Adj",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(3.5, 4, 4.5),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4a, file="../presaved_fits/fit_4A.rdata")

# 4B
fit.4b <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_FullSet_Pred_Age_Adj",
  z.column.name="MMSE", z.transformation="inverse",
  include.study=T, include.education=T, Z.LEVELS=c(-25, -27, -29),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4b, file="../presaved_fits/fit_4B.rdata")

# 4C
fit.4c <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_ABSet_SPARE_AD",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(3.7, 4.0, 4.3),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4c, file="../presaved_fits/fit_4C.rdata")

# 4D
fit.4d <- fit.gam(
  data, x.column.name="AGE", y.column.name="sMRI_ABSet_SPARE_AD",
  z.column.name="MMSE", z.transformation="inverse",
  include.study=T, include.education=T, Z.LEVELS=c(-23, -25, -27, -29),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4d, file="../presaved_fits/fit_4D.rdata")

# 4E
fit.4e <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="TMT_COMBINED", z.transformation="cubicroot",
  include.study=T, include.education=T, Z.LEVELS=c(3.9, 4.2, 4.5),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4e, file="../presaved_fits/fit_4E.rdata")

# 4F
fit.4f <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="MMSE", z.transformation="inverse",
  include.study=F, include.education=T, Z.LEVELS=c(-24, -26),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4f, file="../presaved_fits/fit_4F.rdata")

# 4H
fit.4h <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="sMRI_FullSet_Pred_Age_Adj",
  include.study=T, Z.LEVELS=c(65, 75, 85),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4h, file="../presaved_fits/fit_4H.rdata")

# 4I
fit.4i <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="sMRI_ABSet_SPARE_AD",
  include.study=T, Z.LEVELS=c(-1, 0, 1),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4i, file="../presaved_fits/fit_4I.rdata")

# 4J
fit.4j <- fit.gam(
  data, x.column.name="AGE", y.column.name="L701",
  z.column.name="ABeta_STATUS",
  include.study=T, Z.LEVELS=c(0.6, 0.8), gam.family=binomial(),
  age.range=c(55, 90), dx.group=c("MCI", "AD"))
save(fit.4j, file="../presaved_fits/fit_4J.rdata")

### demonstrate usage
# image(fit.4j$x, fit.4j$y, fit.4j$Z, col=cm.colors(10))
# contour(fit.4j$x, fit.4j$y, fit.4j$Z, nlevels=10, add=T, levels=fit.4j$levels)

