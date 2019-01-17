source("helpers.R", local=TRUE)

################
input <- list()
input$X.COL <- "AGE"
input$Y.COL <- "DIAGNOSIS"
input$Z.COL <- "TOTALBRAIN_VOLUME"
input$X.TRANSFORM <- input$Y.TRANSFORM <- input$Z.TRANSFORM <- "None"
input$STUDIES.SUBSET <- default.studies
input$DIAGNOSIS.GROUP <- c("CN", "PT")
input$SEX.GROUP <- default.sex
input$AGE.RANGE <- c(0, 100)
input$GAM.K <- 5
input$GAM.GAMMA <- 1
input$GAM.COVARIATES <- biological.covariates
input$GAM.STUDY <- FALSE

data[data$DIAGNOSIS=="CN", "TOTALBRAIN_VOLUME"] <- data[data$DIAGNOSIS=="CN", "TOTALBRAIN_VOLUME"] + 250000


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
  
  return(summary.table)
}

summaryTable()


# PLOT HISTOGRAM OF Y-VARIABLE
par(cex=1.5)
plt.title <- paste("Histogram of Selected Y-Variable")
barplot(table(selectedData()[, input$Y.COL]), main=plt.title, xlab=input$Y.COL)

################################################################################
# FIT GAM MODEL AND PRODUCE SUMMARY
gam.fit <- gam(TOTALBRAIN_VOLUME ~ s(AGE, k=5) + DIAGNOSIS, data=selectedData(), method="REML")
summary(gam.fit)

gam.persp <- vis.gam(gam.fit,
                     type="response", color="cm", se=0)

# TRY ANOTHER KIND OF PLOT
par(cex=1.5)
plt.title <- paste("Scatterplot of Selected Z-Variable vs. Selected X-Variable")
point.colors <- rainbow(2)[as.numeric(selectedData()[, input$Y.COL])]
plot(selectedData()[, input$X.COL], selectedData()[, input$Z.COL],
     pch=16, col=point.colors, cex=0.6,
     main=plt.title, xlab=input$X.COL, ylab=input$Z.COL)

x.seq <- seq(min(selectedData()[, input$X.COL]), max(selectedData()[, input$X.COL]), length.out=250)
y.seq <- unique(selectedData()[, input$Y.COL])
plot.data <- expand.grid(x.seq, y.seq)
names(plot.data) <- c(input$X.COL, input$Y.COL)
plot.data[, input$Z.COL] <- predict(gam.fit, newdata=plot.data)
lines(plot.data[1:250, input$X.COL], plot.data[1:250, input$Z.COL])
lines(plot.data[251:500, input$X.COL], plot.data[251:500, input$Z.COL])



visreg(gam.fit, xvar=input$X.COL, by=input$Y.COL, overlay=TRUE, ylab=input$Z.COL)



