options(warn=-1)
source("helpers.R", local=TRUE)

function(input, output, session) {
  
  # DEFINE SELECTABLE VARIABLES
  observe({
    updateSelectInput(session, "X.COL", choices=selectable.variables, selected=default.x.col)
    updateSelectInput(session, "Y.COL", choices=selectable.variables, selected=default.y.col)
    updateSelectInput(session, "Z.COL", choices=selectable.variables, selected=default.z.col)
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
    plt.title <- paste("Histogram of Selected Y-Variable")
    hist(selectedData()[, input$Y.COL], col="gray", breaks=50,
         main=plt.title, xlab=input$Y.COL)
  })

  # PLOT HISTOGRAM OF Y-VARIABLE
  output$z_histogram <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Histogram of Selected Z-Variable")
    hist(selectedData()[, input$Z.COL], col="gray", breaks=50,
         main=plt.title, xlab=input$Z.COL)
  })
  
  # PLOT SCATTERPLOT OF Y-VARIABLE VS X-VARIABLE
  output$xy_scatter <- renderPlot({
    par(cex=1.5)
    plt.title <- paste("Scatterplot of Selected Y-Variable vs. Selected X-Variable")
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
  
  # FIT GAM MODEL AND PRODUCE SUMMARY
  gamFit <- reactive({
    covars <- input$GAM.COVARIATES
    if (input$GAM.STUDY){ covars <- c(covars, "STUDY") }
    gam.formula.string <- construct_gam_formula_string(input$X.COL,
                                                       input$Y.COL,
                                                       input$Z.COL,
                                                       COVARIATES=covars,
                                                       GAM.K=input$GAM.K)
    gam.formula <- as.formula(gam.formula.string)
    gam.fit <- gam(gam.formula, data=selectedData(), method="REML",
                   gamma=input$GAM.GAMMA)
    
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
  
  # VISUALIZE GAM PERSPECTIVE
  output$gam_persp <- renderPlot({
    persp.se <- ifelse(input$PERSP.SE, 2, 0)
    gam.persp <- vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL),
                         type="response", color="cm", se=persp.se,
                         zlab=input$Z.COL, theta=input$PERSP.THETA)
    if (input$PERSP.POINTS){
      points.3d <- trans3d(selectedData()[, input$X.COL],
                           selectedData()[, input$Y.COL],
                           selectedData()[, input$Z.COL],
                           pmat=gam.persp)
      points(points.3d, pch=16, cex=0.6, col="gray18")
    }
  })
  
  # VISUALIZE GAM CONTOURS
  output$gam_contours <- renderPlot({
    vis.gam(gamFit(), view=c(input$X.COL, input$Y.COL), type="response",
            plot.type="contour", too.far=input$CONTOURS.EXCLUSION,
            main=input$Z.COL, color="cm", contour.col="black", labcex=1.5, method="edge")
    if (input$CONTOURS.POINTS){
      
      #Create a function to generate a continuous color palette
      rbPal <- colorRampPalette(c("blue", "red"))
      #This adds a column of color values based on the y values
      point.colors <- rbPal(10)[as.numeric(cut(selectedData()[, input$Z.COL], breaks=10))]
      
      points(selectedData()[, input$X.COL], selectedData()[, input$Y.COL],
             pch=16, cex=input$CONTOURS.POINTSIZE, col=point.colors)
    }
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
}