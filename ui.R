# Note: RPostgreSQL requires libpq-dev on linux:
# sudo apt install libpq-dev
#
# Run install_deps.R to install missing packages:
#
# list.of.packages <- c("shiny", "ggplot2", "lubridate", "ggthemes", "devtools", "ggTimeSeries", "DT", "RPostgreSQL")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages[!new.packages=="ggTimeSeries"])
# if('ggTimeSeries' %in% new.packages) devtools::install_github('Ather-Energy/ggTimeSeries')
# devtools::install_github("hrbrmstr/ggalt")

library("shiny")
library("RPostgreSQL")
library("lubridate")
require("DT")

# not used for now
# library("ggplot2")
# library("ggthemes")
# library("ggTimeSeries")
# library("bazRtools") # using local source for convenience instead
# library("gridExtra")
# library("plotly")

# list of possible parameters from DB and calculated in baztools.R
paramList <- list(
  "Time" = "time_and_date",
  "Current" = "current",
  "Voltage" = "voltage",
  "Real Power" = "real_power",
  "Reactive Power" = "reac_power",
  "Apparent Power" = "app_power",
  "Displacement Power Factor" = "disp_power_factor",
  "True Power Factor" = "true_power_factor",
  "Phase ID" = "phase_id",
  "Current THD" = "current_thd",
  "Voltage THD" = "voltage_thd",
  "Temperature" = "temperature",
  "Frequency" = "frequency",
  "Apparent Power (True)" = "app_power_t",
  "Reactive Power (True)" = "reac_power_t",
  "Minute of Day" = "min_of_day"
  )

# transformer feeders grouped by bundle (L1,L2,L3), neutrals omitted
# tf1 <- c(5,1,6, 9,7,8, 11,10,12)  # tf1 dec-16 - sep17
# tf3 <- c(33,34,35, 41,42,43, 45,46,47)  # tf3 14-jun - 25-jul
# tf5 <- c(65,66,68, 70,69,71, 74,75,76)  # tf5 23-aug - now

# list of transformers
trafoSelectList<-list(
  'Please select a transformer'="",
  'Drogheda (5)'='tf5',
  'Limerick (1)'='tf1',
  'Oranmore (3)'='tf3'
  )

# build a shiny UI
shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-spacelab.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "overrides.css"),
    tags$style(HTML(".col-sm-4 {
      height: 90vh; overflow-y: auto;
      }")) # scroll sidebar
  ),
  # Application title
  titlePanel("Electrical Analytics"),

  sidebarLayout(
    # sidebar with controls
    sidebarPanel(
      selectInput("trafoNumber", "Select Transformer:",
        choices=trafoSelectList
        ),
      # this is populated from server.R once trafoNumber is chosen
      selectInput("feederNumber", "Select Feeder:",
        choices=list("First select a transformer"="")
        ),
      dateRangeInput("dateRange", "Select Date Range:", 
        format="dd/mm/yyyy"
        # start=today()-4, 
        # end=today(), 
        # min="2017-08-23",
        # max=today()
        ),
      actionButton("queryBtn", "Start Query"),
      br(),
      br(),
      selectInput("paramX", "X Parameter:",
                   paramList,
                   selected = paramList[1]),

      selectInput("paramY", "Y Parameter:",
                   paramList,
                   selected = paramList[2]),

      selectInput("paramCol", "Colour Parameter:",
                   paramList,
                   selected = paramList[10]),

      sliderInput("alpha", 
                  "Alpha:", 
                   value = 0.5,
                   min = 0, 
                   max = 1,
                   step = 0.01),

      sliderInput("n", 
                  "Subsampling percentage:", 
                   value = 75,
                   min = 1, 
                   max = 100),

      radioButtons("smoothOption", "Add trend line?",
                    list("Yes" = TRUE,
                      "No" = FALSE
                      )),

      selectInput("smoothType", "Fitting type",
        list("Linear" = "lm",
          # "glm",
          # "gam",
          "Best fit" = "loess"
          # "rlm"
          ),
        selected="loess"
        ),

      radioButtons("normOption", "Normalise?",
                    list("Yes" = TRUE,
                      "No" = FALSE
                      ),
                      selected = FALSE),

      radioButtons("plotType", "Plot type:",
                    list("Points" = "geom_point",
                      "Line" = "geom_line"
                      )),

      selectInput("yScaleType", "Y-scale transformation:",
                    list("No scaling" = "identity",
                      "Log base 10" = "log10",
                      "Natural Log" = "log",
                      "Reciprocal" = "reciprocal",
                      "Exponential" = "exp",
                      "Reverse" = "reverse",
                      "Square root" = "sqrt",
                      "Arc-sin square root" = "asn",
                      "Arc-tan" = "atanh",
                      # "Box-cox" = "boxcox",
                      "Log + 1" = "log1p",
                      "Log base 2" = "log2"
                      # "Inverse sigmoidal" = "logit"
                      # "Probability" = "probability",
                      # "Probit" = "probit",
                      )),

      selectInput("xScaleType", "X-scale transformation:",
                    list("No scaling" = "identity",
                      "Log base 10" = "log10",
                      "Natural Log" = "log",
                      "Reciprocal" = "reciprocal",
                      "Exponential" = "exp",
                      "Reverse" = "reverse",
                      "Square root" = "sqrt",
                      "Arc-sin square root" = "asn",
                      "Arc-tan" = "atanh",
                      # "Box-cox" = "boxcox",
                      "Log + 1" = "log1p",
                      "Log base 2" = "log2"
                      # "Inverse sigmoidal" = "logit"
                      # "Probability" = "probability",
                      # "Probit" = "probit",
                      )),
      br()
    ),

    # main tabbed panel with plots and data representation
    mainPanel(
      div(
        style = "position:relative",
        plotOutput("plot", 
                   hover = hoverOpts("plot_hover", delay = 0, delayType = "debounce")),
        uiOutput("hover_info")
      ),
      tabsetPanel(
        # tabPanel("Plot", plotOutput("plot", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))), 
        tabPanel("Stats", list(
            verbatimTextOutput("summary_Feederinfo"),
            verbatimTextOutput("summary_Xinfo"),
            verbatimTextOutput("summary_Yinfo"),
            verbatimTextOutput("summary_Colinfo")          
          )),
        tabPanel("Raw Data", DT::dataTableOutput("dataTable")),
        tabPanel("Feeders", DT::dataTableOutput("feederTable")),
        tabPanel("Hourly Plot", plotOutput("hourlyplot")),
        tabPanel("Calendar Plot", plotOutput("calendarplot"))
      )
    )
  )
))
