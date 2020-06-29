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

### demonstrate usage
# fit.2a
# save(fit.2a, file="fit_2a.rdata")
# image(fit.2a$x, fit.2a$y, fit.2a$Z, col=cm.colors(10))
# contour(fit.2a$x, fit.2a$y, fit.2a$Z, nlevels=10, add=T, levels=fit.2a$levels)

