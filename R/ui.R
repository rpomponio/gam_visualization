
# HARDCODED PARAMETERS
variable.transforms <- c("None", "Natural Log", "Cubic Root", "Binary")
presaved.fits <- c("2A: TMT vs Age, Brain-Age")

fluidPage(
  headerPanel("Interactive GAM Visualization"),
  fluidRow(
    column(4,
           wellPanel(
             h4("Pre-saved Isocontours"),
             selectInput("PRESAVED.FIT", "Select Figure", presaved.fits),
             checkboxInput("PLOT.PRESAVED.FIT", "Plot pre-saved isocontours", TRUE)),
           # selectInput("DIAGNOSIS.GROUP", "Diagnosis Group", "", selected="", multiple=TRUE),
           # selectInput("SEX.GROUP", "Sex Group", "", selected="", multiple=TRUE),
           # sliderInput("AGE.RANGE", "Age Range", min=0, max=100, value=c(0, 100), step=1)),
          wellPanel(
             h4("Upload New Data"),
             fileInput(
               "file1", "Choose CSV File",multiple=F, accept=c("text/csv", "text/comma-separated-values,text/plain",".csv")),
             tags$hr(),
             selectInput("X.COL", "X-Variable (Predictor #1)", ""),
             selectInput("Y.COL", "Y-Variable (Predictor #2)", ""),
             selectInput("Z.COL", "Z-Variable (Response)", ""),
             tags$hr(),
             selectInput("X.TRANSFORM", "X-Variable Transformation", variable.transforms),
             selectInput("Y.TRANSFORM", "Y-Variable Transformation", variable.transforms),
             checkboxInput("Z.BINARIZE", "Binarize Z-Variable", FALSE),
             tags$hr(),
             selectInput("GAM.COVARIATES", "Control for Biocovariates:", "", multiple=TRUE))),
    column(8,
           tabsetPanel(
             type="tabs",
             tabPanel(
               "Summary",
               h4("Description of uploaded data."),
               plotOutput("x_histogram"),
               verbatimTextOutput("x_summary", placeholder=TRUE),
               plotOutput("y_histogram"),
               verbatimTextOutput("y_summary", placeholder=TRUE),
               plotOutput("z_histogram"),
               verbatimTextOutput("z_summary", placeholder=TRUE)),
             tabPanel(
               "GAM Diagnostic",
               h4("Summary of selected GAM fit."),
               verbatimTextOutput("gam_summary", placeholder=TRUE),
               h4("Performance of GAM fit."),
               verbatimTextOutput("gam_performance", placeholder=TRUE),
               h4("Output from gam.check()."),
               verbatimTextOutput("gam_check", placeholder=TRUE)),
             tabPanel(
               "Isocontours",
               plotOutput("gam_contours", height="600px"),
               wellPanel(
                 h4("Isocontours Settings (New Data Only)"),
                 checkboxInput("CONTOURS.POINTS", "Plot Data Points", value=TRUE),
                 sliderInput(
                   "CONTOURS.SE", "Number of Standard Errors",
                   min=0, max=5, value=0, step=0.5),
                 sliderInput(
                   "CONTOURS.NLEVELS", "Number of Contours", min=1, max=30, value=10, step=1)))))))
