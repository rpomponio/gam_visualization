library(mgcv)
options(warn=-1, stringsAsFactors=F)

load("../presaved_fits/fit_2a.rdata")
load("../presaved_fits/fit_2b.rdata")
load("../presaved_fits/fit_2c.rdata")
load("../presaved_fits/fit_4a.rdata")

function(input, output, session) {
  
  # upload csv file
  uploadedData <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    df
  })
  
  # update UI to accomodate input file
  observe({
    updateSelectInput(session, "X.COL", choices=colnames(uploadedData()), selected=colnames(uploadedData())[1])
    updateSelectInput(session, "Y.COL", choices=colnames(uploadedData()), selected=colnames(uploadedData())[2])
    updateSelectInput(session, "Z.COL", choices=colnames(uploadedData()), selected=colnames(uploadedData())[3])
    updateSelectInput(session, "GAM.COVARIATES", choices=colnames(uploadedData()))
  })
  fitSelected <- reactive({
    if (grepl("2A", input$PRESAVED.FIT)){ fit.2a }
    else if (grepl("2B", input$PRESAVED.FIT)){fit.2b}
    else if (grepl("2C", input$PRESAVED.FIT)){fit.2c}
    else if (grepl("4A", input$PRESAVED.FIT)){fit.4a}
  })
  
  # transform uploaded dataset
  transformedData <- reactive({
    if(is.null(input$file1)){
      uploadedData()
    }
    else{
      df <- uploadedData()
      # variable transformations
      if (input$X.TRANSFORM!="None"){
        original.values <- df[, input$X.COL]
        if (input$X.TRANSFORM=="Natural Log"){
          df[, input$X.COL] <- log(original.values + 1)
        } else if (input$X.TRANSFORM=="Cubic Root"){
          df[, input$X.COL] <- (original.values)^(1/3)
        }
      }
      if (input$Y.TRANSFORM!="None"){
        original.values <- df[, input$Y.COL]
        if (input$Y.TRANSFORM=="Natural Log"){
          df[, input$Y.COL] <- log(original.values + 1)
        } else if (input$Y.TRANSFORM=="Cubic Root"){
          df[, input$Y.COL] <- (original.values)^(1/3)
        }
      }
      if (input$Z.BINARIZE){
        original.values <- df[, input$Z.COL]
        cases.all <- unique(original.values)
        cases.sorted <- cases.all[order(cases.all)]
        case.positive <- cases.sorted[length(cases.sorted)]
        df[, input$Z.COL] <- as.numeric(original.values==case.positive)
      } else if (input$Z.INVERSE){
        original.values <- df[, input$Z.COL]
        df[, input$Z.COL] <- -original.values
      }
      df[rowSums(is.na(df[, c(input$X.COL, input$Y.COL, input$Z.COL)]))==0, ]
    }
  })

  # histograms and summaries of variables
  output$x_histogram <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Histogram of Selected X-Variable:", input$X.COL)
    hist(transformedData()[, input$X.COL], col="gray", breaks=50, main=plt.title, xlab="")
  })
  output$x_summary <- renderPrint({
    summary(transformedData()[, input$X.COL])
  })
  output$y_histogram <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Histogram of Selected Y-Variable:", input$Y.COL)
    hist(transformedData()[, input$Y.COL], col="gray", breaks=50, main=plt.title, xlab="")
  })
  output$y_summary <- renderPrint({
    summary(transformedData()[, input$Y.COL])
  })
  output$z_histogram <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Histogram of Selected Z-Variable:", input$Z.COL)
    hist(transformedData()[, input$Z.COL], col="gray", breaks=50, main=plt.title, xlab="")
  })
  output$z_summary <- renderPrint({
    summary(transformedData()[, input$Z.COL])
  })
  
  # fit gam to uploaded data
  gamFit <- reactive({
    gam.formula.string <- paste0(input$Z.COL, " ~ s(", input$X.COL, ", ", input$Y.COL, ")")
    for (covar in input$GAM.COVARIATES){
      gam.formula.string <- paste0(gam.formula.string, " + ", covar)
    }
    gam.formula <- as.formula(gam.formula.string)
    if (input$Z.BINARIZE){
      gam.fit <- gam.fit <- gam(gam.formula, data=transformedData(), method="REML", family="binomial")
    } else {
      gam.fit <- gam(gam.formula, data=transformedData(), method="REML")
    }
    gam.fit
  })
  
  # print gam diagnostics
  output$gam_summary <- renderPrint({
    if (is.null(input$file1)){
      cat("Upload csv first")
    }
    else{
      summary(gamFit())
    }
  })
  output$gam_performance <- renderPrint({
    if (is.null(input$file1)){
      cat("Upload csv first")
    }
    else{
      mae <- mean(abs(predict(gamFit()) - transformedData()[, input$Z.COL]))
      cat("Mean Absolute Error (MAE): ", mae)
    }
  })
  output$gam_check <- renderPrint({
    if (is.null(input$file1)){
      cat("Upload csv first")
    }
    else{
      gam.check(gamFit())
    }
  })
  
  # plot pre-saved isocontours or newly-fitted isocontours
  output$gam_contours <- renderPlot({
    if (input$PLOT.PRESAVED.FIT=="Pre-saved isocontours"){
      plt.title <- paste0(input$PRESAVED.FIT)
      image(fitSelected()$x, fitSelected()$y, fitSelected()$Z, col=cm.colors(10), main=plt.title, xlab="", ylab="")
      contour(fitSelected()$x, fitSelected()$y, fitSelected()$Z, nlevels=10, add=T, levels=fitSelected()$levels, labcex=1.25)
    } else{
      plt.title <- paste0(input$Z.COL, " (N=", nrow(transformedData()),")")
      vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL), type="response",
              plot.type="contour", too.far=0.05, n.grid=100,
              main=plt.title, contour.col="black",
              xlab=input$X.COL, ylab=input$Y.COL, labcex=1.5,
              method="edge", nlevels=input$CONTOURS.NLEVELS)
      
      # add points to figure
      if (input$CONTOURS.POINTS){
        points(transformedData()[, input$X.COL], transformedData()[, input$Y.COL], pch=16)
      }
      
      # add SE contours to figure
      if (input$CONTOURS.SE > 0){
        # create dataframe of grid nodes
        x.seq <- seq(min(transformedData()[, input$X.COL]), max(transformedData()[, input$X.COL]), length.out=100)
        y.seq <- seq(min(transformedData()[, input$Y.COL]), max(transformedData()[, input$Y.COL]), length.out=100)
        df.seq <- data.frame(x.seq, y.seq)
        xy.grid <- expand.grid(df.seq)
        names(xy.grid) <- c(input$X.COL, input$Y.COL)
        # predict value of model at each node with SE
        pred <- predict(gamFit(), newdata=xy.grid, type="response", se=TRUE)
        z.pred <- pred$fit
        z.se <- pred$se.fit
        # exlude points that are too far from data
        exclude.points <- exclude.too.far(xy.grid[, 1], xy.grid[, 2],
                                          transformedData()[, input$X.COL],
                                          transformedData()[, input$Y.COL],
                                          dist=0.05)
        z.pred[exclude.points] <- NA
        # create matrices for upper and lower surfaces
        z.grid.lwr <- matrix(z.pred - (input$CONTOURS.SE * z.se), nrow=100, byrow=FALSE)
        z.grid.upr <- matrix(z.pred + (input$CONTOURS.SE * z.se), nrow=100, byrow=FALSE)
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
    }
  })
}