source("helpers.R", local=TRUE)

################
input <- list()
input$X.COL <- "AGE"
input$Y.COL <- "TOTALBRAIN_VOLUME"
input$Z.COL <- "CS1"
input$X.TRANSFORM <- input$Y.TRANSFORM <- input$Z.TRANSFORM <- "None"
input$STUDIES.SUBSET <- default.studies
input$DIAGNOSIS.GROUP <- c("CN", "PT")
input$SEX.GROUP <- default.sex
input$AGE.RANGE <- c(0, 100)
input$GAM.K <- 5
input$GAM.GAMMA <- 1
input$GAM.COVARIATES <- biological.covariates
input$GAM.STUDY <- FALSE
input$GAM.ADDITIVESMOOTHS <- FALSE
input$CONTOURS.POINTS <- TRUE
input$CONTOURS.RSQ <- FALSE
input$CONTOURS.EXCLUSION <- 0.1
input$CONTOURS.NLEVELS <- 10
input$CONTOURS.POINTSIZE <- 1.0
input$CONTOURS.POINTCOLORS <- FALSE
################

# SELECT DATA BASED ON UI INPUT
selectedData <- function(){
  selected.data <- data
  selected.data <- selected.data[!is.na(selected.data[, input$X.COL]), ]
  selected.data <- selected.data[!is.na(selected.data[, input$Y.COL]), ]
  selected.data <- selected.data[!is.na(selected.data[, input$Z.COL]), ]
  if (length(input$STUDIES.SUBSET)>0){
    selected.data <- subset(selected.data, STUDY%in%input$STUDIES.SUBSET)
  }
  selected.data <- subset(selected.data, DIAGNOSIS%in%input$DIAGNOSIS.GROUP)
  selected.data <- subset(selected.data, SEX%in%input$SEX.GROUP)
  selected.data <- subset(selected.data,
                          AGE>=input$AGE.RANGE[1] & AGE<=input$AGE.RANGE[2])
  cols.to.transform <- c("X.COL", "Y.COL", "Z.COL")
  transforms <- c("X.TRANSFORM", "Y.TRANSFORM", "Z.TRANSFORM")
  for (i in 1:3){
    col <- cols.to.transform[i]
    transform <- input[[transforms[i]]]
    original.values <- selected.data[, input[[col]]]
    if (transform=="Natural Log"){
      selected.data[, input[[col]]] <- log(original.values + 1)
    } else if (transform=="Cubic Root"){
      selected.data[, input[[col]]] <- (original.values) ^ (1/3)
    } else if (transform=="Categorical"){
      selected.data[, input[[col]]] <- as.factor(original.values)
    } else if (transform=="Binary"){
      cases.all <- unique(original.values)
      cases.sorted <- cases.all[order(cases.all)]
      case.positive <- cases.sorted[length(cases.sorted)]
      selected.data[, input[[col]]] <- as.numeric(original.values==case.positive)
    }
  }
  
  return(selected.data)
}

# OBTAIN DESCRIPTIVE SUMMARY OF SELECTED DATA
summaryTable <- function(){
  summary.table <- data.frame(matrix(NA, nrow=3, ncol=2))
  colnames(summary.table) <- c("ATTRIBUTE", "VALUE")
  summary.table$ATTRIBUTE <- c("Number of Studies:",
                               "Number of Subjects:",
                               " % female:")
  summary.table$VALUE[1] <- length(unique(selectedData()$STUDY))
  summary.table$VALUE[2] <- length(unique(selectedData()$ID))
  summary.table$VALUE[3] <- as.integer(100 * mean(selectedData()$SEX=="F"))
  
  summary.table
}

summaryTable()


# PLOT HISTOGRAM OF Y-VARIABLE

if (input$Y.TRANSFORM=="Categorical"){
  plt.title <- paste("Barplot of Selected Y-Variable")
  barplot(table(selectedData()[, input$Y.COL]),
          main=plt.title, xlab=input$Y.COL)
} else {
  plt.title <- paste("Histogram of Selected Y-Variable")
  hist(selectedData()[, input$Y.COL], col="gray", breaks=50,
       main=plt.title, xlab=input$Y.COL)
}

################################################################################
# FIT GAM MODEL AND PRODUCE SUMMARY
gamFit <- function(){
  y.categorical <- input$Y.TRANSFORM=="Categorical"
  covars <- input$GAM.COVARIATES
  if (input$GAM.STUDY){ covars <- c(covars, "STUDY") }
  gam.formula.string <- construct_gam_formula_string(input$X.COL,
                                                     input$Y.COL,
                                                     input$Z.COL,
                                                     COVARIATES=covars,
                                                     GAM.K=input$GAM.K,
                                                     Y.CATEGORICAL=y.categorical,
                                                     ADDITIVE.SMOOTHS=input$GAM.ADDITIVESMOOTHS)
  gam.formula <- as.formula(gam.formula.string)
  if (input$Z.TRANSFORM=="Binary"){
    gam.fit <- gam(gam.formula, data=selectedData(), method="REML",
                   gamma=input$GAM.GAMMA, family=binomial)
  } else {
    gam.fit <- gam(gam.formula, data=selectedData(), method="REML",
                   gamma=input$GAM.GAMMA)
  }
  
  gam.fit
}
summary(gamFit())

# VISUALIZE GAM CONTOURS
plt.title <- paste0(input$Z.COL, " (N=", nrow(selectedData()),")")

if (input$CONTOURS.RSQ){
  x.lab <- paste0(input$X.COL, " (R-Sq: ", RSqXY()[1], ")")
  y.lab <- paste0(input$Y.COL, " (R-Sq: ", RSqXY()[2], ")")
} else {
  x.lab <- input$X.COL
  y.lab <- input$Y.COL
}

vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL), type="response",
        plot.type="contour", too.far=input$CONTOURS.EXCLUSION,
        main=plt.title, color="cm", contour.col="black", xlab=x.lab, ylab=y.lab,
        labcex=1.5, method="edge", nlevels=input$CONTOURS.NLEVELS)

if (input$CONTOURS.POINTS){
  
  if (input$CONTOURS.POINTCOLORS){
    # Create a function to generate a continuous color palette
    rbPal <- colorRampPalette(c("blue", "red"))
    # This vector of color values based on the continuous variable
    point.colors <- rbPal(10)[as.numeric(cut(selectedData()[, input$Z.COL], breaks=10))]
  } else {
    point.colors <- "gray18"
  }
  
  points(selectedData()[, input$X.COL], selectedData()[, input$Y.COL],
         pch=16, cex=input$CONTOURS.POINTSIZE, col=point.colors)
}

################################################################################
# TEST CI BANDS

plot(gamFit(), select=1, rug=FALSE, se=0.1, seWithMean=T, cex.lab=2)


















