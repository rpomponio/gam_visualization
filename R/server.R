options(warn=-1)
source("helpers.R", local=TRUE)

function(input, output, session) {
  
  # DEFINE SELECTABLE VARIABLES
  observe({
    updateSelectInput(session, "X.COL", choices=selectable.variables, selected=default.x.col)
    updateSelectInput(session, "Y.COL", choices=selectable.variables, selected=default.y.col)
    updateSelectInput(session, "Z.COL", choices=selectable.variables, selected=default.z.col)
    updateSelectInput(session, "STUDIES.SUBSET", choices=studies, selected=default.studies)
    updateSelectInput(session, "DIAGNOSIS.GROUP", choices=diagnosis.groups, selected=default.diagnosis)
    updateSelectInput(session, "SEX.GROUP", choices=sex.groups, selected=default.sex)
    updateSelectInput(session, "GAM.COVARIATES", choices=biological.covariates)
  })
  
  # SELECT DATA BASED ON UI INPUT
  selectedData <- reactive({
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
    
    selected.data
  })
  
  # OBTAIN DESCRIPTIVE SUMMARY OF SELECTED DATA
  summaryTable <- reactive({
    summary.table <- data.frame(matrix(NA, nrow=3, ncol=2))
    colnames(summary.table) <- c("ATTRIBUTE", "VALUE")
    summary.table$ATTRIBUTE <- c("Number of Studies:",
                                 "Number of Subjects:",
                                 " % female:")
    summary.table$VALUE[1] <- length(unique(selectedData()$STUDY))
    summary.table$VALUE[2] <- length(unique(selectedData()$ID))
    summary.table$VALUE[3] <- as.integer(100 * mean(selectedData()$SEX=="F"))
    
    summary.table
  })
  output$summary_table <- renderTable({
    summaryTable()
  })

  # PLOT BARS WITH SUBJECT COUNTS FOR EACH STUDY
  output$subject_counts_by_study <- renderPlot({
    par(cex=1.5)
    plt.title <- "Subject Counts by Study"
    tbl <- table(selectedData()$STUDY)
    plt <- barplot(tbl, ylim=c(0, max(tbl) + 500),
                   main=plt.title, las=3, cex.names=0.75)
    text(plt, tbl + 200, labels=tbl)
  })

  # PLOT HISTOGRAM OF X-VARIABLE
  output$x_histogram <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Histogram of Selected X-Variable")
    hist(selectedData()[, input$X.COL], col="gray", breaks=50,
         main=plt.title, xlab=input$X.COL)
  })

  # PLOT HISTOGRAM OF Y-VARIABLE
  output$y_histogram <- renderPlot({
    par(cex=1.5)
    if (input$Y.TRANSFORM=="Categorical"){
      plt.title <- paste("Barplot of Selected Y-Variable")
      barplot(table(selectedData()[, input$Y.COL]),
           main=plt.title, xlab=input$Y.COL)
    } else {
      plt.title <- paste("Histogram of Selected Y-Variable")
      hist(selectedData()[, input$Y.COL], col="gray", breaks=50,
           main=plt.title, xlab=input$Y.COL)
    }
  })

  # PLOT HISTOGRAM OF Z-VARIABLE
  output$z_histogram <- renderPlot({
    par(cex=1.5)
    if (input$Z.TRANSFORM=="Binary"){
      plt.title <- paste("Barplot of Selected Z-Variable")
      barplot(table(selectedData()[, input$Z.COL]),
              main=plt.title, xlab=input$Z.COL)
    } else {
      plt.title <- paste("Histogram of Selected Z-Variable")
      hist(selectedData()[, input$Z.COL], col="gray", breaks=50,
           main=plt.title, xlab=input$Z.COL)
    }
  })
  
  # PLOT SCATTERPLOT OF Y-VARIABLE VS X-VARIABLE
  output$xy_scatter <- renderPlot({
    par(cex=1.5)
    plt.title <- paste ("Scatterplot of Selected Y-Variable vs. Selected X-Variable")
    plot(selectedData()[, input$X.COL], selectedData()[, input$Y.COL],
         pch=16, col="gray18", cex=0.6,
         main=plt.title, xlab=input$X.COL, ylab=input$Y.COL)
  })
  
  # PLOT SCATTERPLOT OF Z-VARIABLE VS X-VARIABLE
  output$xz_scatter <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Scatterplot of Selected Z-Variable vs. Selected X-Variable")
    plot(selectedData()[, input$X.COL], selectedData()[, input$Z.COL],
         pch=16, col="gray18", cex=0.6,
         main=plt.title, xlab=input$X.COL, ylab=input$Z.COL)
  })
  
  # PLOT SCATTERPLOT OF Z-VARIABLE VS Y-VARIABLE
  output$yz_scatter <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Scatterplot of Selected Z-Variable vs. Selected Y-Variable")
    plot(selectedData()[, input$Y.COL], selectedData()[, input$Z.COL],
         pch=16, col="gray18", cex=0.6,
         main=plt.title, xlab=input$Y.COL, ylab=input$Z.COL)
  })
  
  # PLOT HEATMAP OF DATA USING KNN
  output$knn_heatmap <- renderPlot({
    # function for rescaling vectors
    range01 <- function(x){(x-min(x))/(max(x)-min(x))}
    x.rescaled <- range01(selectedData()[, input$X.COL])
    y.rescaled <- range01(selectedData()[, input$Y.COL])
    df.train <- data.frame(x=x.rescaled, y=y.rescaled)
    # new data frame for creating an image
    n.grid <- 100
    x.seq <- seq(0, 1, length.out=n.grid)
    y.seq <- seq(0, 1, length.out=n.grid)
    df.seq <- data.frame(x=x.seq, y=y.seq)
    xy.grid <- expand.grid(df.seq)
    # predict with knn
    knn <- knn.reg(df.train, y=selectedData()[, input$Z.COL],
                   k=input$HEATMAP.NEIGHBORS, test=xy.grid)
    z.pred <- knn$pred
    # exlude points that are too far from data
    exclude.points <- exclude.too.far(xy.grid[, 1], xy.grid[, 2],
                                      x.rescaled,
                                      y.rescaled,
                                      dist=input$HEATMAP.EXCLUSION)
    z.pred[exclude.points] <- NA
    # convert to matrix and plot image
    z.grid <- matrix(z.pred, nrow=n.grid, byrow=FALSE)
    image(x=x.seq, y=y.seq, z=z.grid, col=cm.colors(n.grid),
          xlab=input$X.COL, ylab=input$Y.COL, xaxt="n", yaxt="n",
          main=input$Z.COL)
    if (input$HEATMAP.POINTS){
      points(x.rescaled, y.rescaled,
             pch=16, cex=1, col="gray18")
    }
  })
  
  
  # FIT GAM MODEL AND PRODUCE SUMMARY
  gamFit <- reactive({
    y.categorical <- input$Y.TRANSFORM=="Categorical"
    covars <- input$GAM.COVARIATES
    if (input$GAM.STUDY){ covars <- c(covars, "STUDY") }
    gam.formula.string <- construct_gam_formula_string(input$X.COL,
                                                       input$Y.COL,
                                                       input$Z.COL,
                                                       COVARIATES=covars,
                                                       GAM.K=input$GAM.K,
                                                       Y.CATEGORICAL=y.categorical,
                                                       SMOOTH.CONSTRUCTION=input$GAM.SMOOTHCONSTRUCTION)
    gam.formula <- as.formula(gam.formula.string)
    if (input$Z.TRANSFORM=="Binary"){
      gam.fit <- gam(gam.formula, data=selectedData(), method=input$GAM.METHOD,
                     gamma=input$GAM.GAMMA, family=binomial)
    } else {
      gam.fit <- gam(gam.formula, data=selectedData(), method=input$GAM.METHOD,
                     gamma=input$GAM.GAMMA)
    }
    
    gam.fit
  })
  output$gam_summary <- renderPrint({
    summary(gamFit())
  })
  output$gam_performance <- renderPrint({
    mae <- mean(abs(predict(gamFit()) - selectedData()[, input$Z.COL]))
    print("Mean Absolute Error (MAE):")
    mae
  })
  
  # CHECK GAM WITH DIAGNOSTICS
  output$gam_check <- renderPrint({
    gam.check(gamFit())
  })
  
  # FIT INDIVIDUAL GAMS FOR X, Y VARIABLES
  RSqXY <- reactive({
    x.formula <- as.formula(paste0(input$Z.COL, " ~ s(", input$X.COL,")"))
    x.gam <- gam(x.formula, data=selectedData(), method="REML")
    x.rsq <- round(as.numeric(summary(x.gam)["r.sq"]), 3)
    y.formula <- as.formula(paste0(input$Z.COL, " ~ s(", input$Y.COL,")"))
    y.gam <- gam(y.formula, data=selectedData(), method="REML")
    y.rsq <- round(as.numeric(summary(y.gam)["r.sq"]), 3)
    
    list(x.rsq, y.rsq)
  })
  
  # VISUALIZE GAM PERSPECTIVE
  output$gam_persp <- renderPlot({
    persp.se <- ifelse(input$PERSP.SE, 2, 0)
    if (input$Y.TRANSFORM=="Categorical"){
      gam.persp <- vis.gam(gamFit(), type="response", color="cm", se=persp.se, too.far=input$PERSP.EXCLUSION,
                           zlab=input$Z.COL, theta=input$PERSP.THETA, phi=input$PERSP.PHI)
    } else {
      gam.persp <- vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL),
                           type="response", color="cm", se=persp.se, too.far=input$PERSP.EXCLUSION,
                           zlab=input$Z.COL, theta=input$PERSP.THETA, phi=input$PERSP.PHI)
      if (input$PERSP.POINTS){
        points.3d <- trans3d(selectedData()[, input$X.COL],
                             selectedData()[, input$Y.COL],
                             selectedData()[, input$Z.COL],
                             pmat=gam.persp)
        points(points.3d, pch=16, cex=0.6, col="gray18")
      }
    }
  })
  
  # VISUALIZE GAM CONTOURS
  output$gam_contours <- renderPlot({
    plt.title <- paste0(input$Z.COL, " (N=", nrow(selectedData()),")")

    if (input$CONTOURS.RSQ){
      x.lab <- paste0(input$X.COL, " (R-Sq: ", RSqXY()[1], ")")
      y.lab <- paste0(input$Y.COL, " (R-Sq: ", RSqXY()[2], ")")
    } else {
      x.lab <- input$X.COL
      y.lab <- input$Y.COL
    }
    
    vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL), type="response",
            plot.type="contour", too.far=input$CONTOURS.EXCLUSION, n.grid=100,
            main=plt.title, color="cm", contour.col="black", xlab=x.lab, ylab=y.lab,
            labcex=1.5, method="edge", nlevels=input$CONTOURS.NLEVELS)
    
    if (input$CONTOURS.SE > 0){
      # number of grid nodes in one dimension (same as vis.gam)
      n.grid <- 100
      # create dataframe of grid nodes
      x.seq <- seq(min(selectedData()[, input$X.COL]), max(selectedData()[, input$X.COL]), length.out=n.grid)
      y.seq <- seq(min(selectedData()[, input$Y.COL]), max(selectedData()[, input$Y.COL]), length.out=n.grid)
      df.seq <- data.frame(x.seq, y.seq)
      xy.grid <- expand.grid(df.seq)
      names(xy.grid) <- c(input$X.COL, input$Y.COL)
      # predict value of model at each node with SE
      pred <- predict(gamFit(), newdata=xy.grid, type="response", se=TRUE)
      z.pred <- pred$fit
      z.se <- pred$se.fit
      # exlude points that are too far from data
      exclude.points <- exclude.too.far(xy.grid[, 1], xy.grid[, 2],
                                        selectedData()[, input$X.COL],
                                        selectedData()[, input$Y.COL],
                                        dist=input$CONTOURS.EXCLUSION)
      z.pred[exclude.points] <- NA
      # create matrices for upper and lower surfaces
      z.grid.lwr <- matrix(z.pred - (input$CONTOURS.SE * z.se), nrow=n.grid, byrow=FALSE)
      z.grid.upr <- matrix(z.pred + (input$CONTOURS.SE * z.se), nrow=n.grid, byrow=FALSE)
      # add contours to figure
      contour(x=x.seq, y=y.seq, z=z.grid.lwr,
              labcex=1.0, method="edge", nlevels=input$CONTOURS.NLEVELS,
              lty=2, lwd=0.5, col="darkgreen", add=TRUE)
      contour(x=x.seq, y=y.seq, z=z.grid.upr,
              labcex=1.0, method="edge", nlevels=input$CONTOURS.NLEVELS,
              lty=2, lwd=0.5, col="red", add=TRUE)
      # add legend
      legend("topleft", legend = c(paste0("+", input$CONTOURS.SE, " SEs"),
                                   paste0("-", input$CONTOURS.SE, " SEs")), bty = "n",
             cex = 1.0, col=c("red", "darkgreen"), lty=c(2, 2), lwd=c(1, 1))
    }
    
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
    
    # Plot predicted values
    if (!is.na(input$CONTOURS.XVAL1)&!is.na(input$CONTOURS.YVAL1)){
      points(input$CONTOURS.XVAL1, input$CONTOURS.YVAL1,
             pch=4, cex=4.0, col="darkred")
      df.point <- data.frame(x=input$CONTOURS.XVAL1, y=input$CONTOURS.YVAL1)
      names(df.point) <- c(input$X.COL, input$Y.COL)
      point.pred <- predict(gamFit(), newdata=df.point, type="response")
      point.text <- paste0("PREDICTED VALUE: ", round(point.pred, 3))
      text(input$CONTOURS.XVAL1, input$CONTOURS.YVAL1, point.text, pos=4, offset=0.75, col="darkred")
    }
    if (!is.na(input$CONTOURS.XVAL2)&!is.na(input$CONTOURS.YVAL2)){
      points(input$CONTOURS.XVAL2, input$CONTOURS.YVAL2,
             pch=4, cex=4.0, col="darkblue")
      df.point <- data.frame(x=input$CONTOURS.XVAL2, y=input$CONTOURS.YVAL2)
      names(df.point) <- c(input$X.COL, input$Y.COL)
      point.pred <- predict(gamFit(), newdata=df.point, type="response")
      point.text <- paste0("PREDICTED VALUE: ", round(point.pred, 3))
      text(input$CONTOURS.XVAL2, input$CONTOURS.YVAL2, point.text, pos=4, offset=0.75, col="darkblue")
    }
    
  })
  
  # PLOT PARTIAL DEPENDENCY OF GAM MODEL
  output$gam_partial <- renderPlot({
    visreg(gamFit(), xvar=input$X.COL, by=input$Y.COL,
           overlay=TRUE, ylab=input$Z.COL, alpha=input$PARTIAL.ALPHA)
  })
  

}