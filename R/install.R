# RUN THIS ONCE TO INSTALL NECESSARY PACKAGES

PACKAGES.TO.INSTALL <- c("shiny", "ggplot2", "mgcv")

for (package in PACKAGES.TO.INSTALL){
  if (package %in% rownames(installed.packages())){
    cat(paste("Found installed package:", package))
  } else {
    cat(paste("Attempting to install package:", package))
    install.packages(package)
  }
}