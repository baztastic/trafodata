# Note: RPostgreSQL requires libpq-dev on linux:
# sudo apt install libpq-dev
#
# Run install_deps.R to install missing packages

library(shiny)
library(shinyjs) # take a look at usability tweaks
library(RPostgreSQL)
library(lubridate)
library(shinyWidgets)
require(DT)

Sys.setenv(TZ="UTC")

# not used for now
# jscode <- "shinyjs.init = function() {
# var signaturePad = new SignaturePad(document.getElementById('signature-pad'), {
#   backgroundColor: 'rgba(255, 255, 255, 0)',
#   penColor: 'rgb(0, 0, 0)'
# });
# var saveButton = document.getElementById('save');
# var cancelButton = document.getElementById('clear');
# saveButton.addEventListener('click', function (event) {
#   var data = signaturePad.toDataURL('image/png');
# // Send data to server instead...
#   window.open(data);
# });
# cancelButton.addEventListener('click', function (event) {
#   signaturePad.clear();
# });
# }"
# useShinyjs()
# jsCode <- 'shinyjs.init = function(){document.getElementsByTagName("form")[0].setAttribute("data-lpignore", "true"); alert("fired");}'
# extendShinyjs(text = jsCode)
# js$disableLastPass

# list of possible parameters from DB and calculated in baztools.R
paramList <- list(
  "Time" = "time_and_date",
  "Current" = "current",
  "Voltage" = "voltage",
  "Voltage Imbalance" = "imbalance",
  "Real Power" = "real_power",
  "Reactive Power" = "reac_power",
  "Apparent Power" = "app_power",
  "Displacement Power Factor" = "disp_power_factor",
  "True Power Factor" = "true_power_factor",
  # "Phase ID" = "phase_id",
  "Current THD" = "current_thd",
  "Voltage THD" = "voltage_thd",
  "Temperature" = "temperature",
  "Frequency" = "frequency",
  "Apparent Power (True)" = "app_power_t",
  "Reactive Power (True)" = "reac_power_t",
  "Minute of Day" = "min_of_day",
  "Hour of Day" = "hour_of_day",
  "Day of Week" = "day_of_week",
  "L1 Voltage" = "v1",
  "L2 Voltage" = "v2",
  "L3 Voltage" = "v3",
  "Problem Phase" = "problem_phase"
  )

# transformer feeders grouped by bundle (L1,L2,L3), neutrals omitted
# tf1 <- c(5,1,6, 9,7,8, 11,10,12)  # tf1 dec-16 - sep17
# tf6 <- c(33,34,35, 41,42,43, 45,46,47)  # tf6 14-jun - 25-jul
# tf5 <- c(65,66,68, 70,69,71, 74,75,76)  # tf5 23-aug - now

# list of transformers
trafoSelectList<-list(
  'Please select a transformer'="",
  'Drogheda (5)'='tf5',
  'Limerick (1)'='tf1'
  # 'Oranmore (6)'='tf6'
  )

# build a shiny UI
shinyUI(fluidPage(
  tags$head(
    # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-spacelab.min.css"),
    # tags$script(src = "https://cdn.jsdelivr.net/npm/signature_pad@2.3.2/dist/signature_pad.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-solar.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "overrides.css"),
    tags$style(HTML(".col-sm-4 {
                    height: 90vh; overflow-y: auto;
                    }")), # scroll sidebar
    tags$style(HTML(".action-button{
                    display:inline-block;
                    width:10%;
                    padding-left:0px;
                    padding-right:0px;}"
                    )),
    tags$style(HTML('#queryBtn{
                    width:20%;
                    background:radial-gradient(#ecaf3f, #cc880c);
                    color:#002c37;
                    font-weight:bold;
                    border-color:#cc880c;
                    }
                    #queryBtn:hover{
                    background:radial-gradient(#cc880c, #ecaf3f);
                    }')) # call to action
  ),
  useShinyjs(),
  # Application title
  titlePanel("Electrical Analytics"),
  # shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = jscode),

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
      tags$div(style="position:relative;",
        tags$div(style="position:absolute; top:0; left:0; width:100%;",
          sliderInput("dateRangeExtents", 
            label = NULL, 
            min = as.Date("2018-02-01"), 
            max = Sys.Date(), 
            value = c(
              as.Date("2018-02-25"), 
              Sys.Date()
            ),
            ticks=FALSE,
            dragRange=FALSE
          )
        ),
        tags$div(style="",
          sliderInput("dateRangeSlider", 
            label = NULL, 
            min = as.Date("2018-02-01"), 
            max = Sys.Date(), 
            value = c(
              as.Date("2018-02-25"), 
              Sys.Date()
              )
          )
        )
      ),
      tags$div(style="width:100%;text-align: center;",
               actionButton("backWeek", "<️"),
               # actionButton("backWeek", "⬅️"),
               actionButton("addDayStart", "+"),
               actionButton("subDayStart", "–"),
               actionButton("queryBtn", "Go"),
               actionButton("subDayEnd", "–"),
               actionButton("addDayEnd", "+"),
               actionButton("fwdWeek", ">")
               # actionButton("fwdWeek", "➡️")
      ),
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
      
      switchInput("advanced", offLabel="Simple", onLabel="Advanced"),

      conditionalPanel
        (
        condition = "input.advanced == true",
        sliderInput("alpha", 
                    "Alpha:", 
                     value = 0.5,
                     min = 0, 
                     max = 1,
                     step = 0.01),

        sliderInput("n", 
                    "Subsampling percentage:", 
                     value = 100,
                     min = 1, 
                     max = 100),

        radioButtons("smoothOption", "Add trend line?",
                      list("Yes" = TRUE,
                        "No" = FALSE
                        ),
                      selected=TRUE
                      ),
        conditionalPanel(
          condition = "input.smoothOption == 'TRUE'",  # note this is javascript notation
          selectInput("smoothType", "Fitting type",
            list("Linear" = "lm",
              # "glm",
              # "gam",
              "Best fit" = "loess"
              # "rlm"
              ),
            selected="loess"
            )        
          ),
        # too complicated - need to guess starting values
        # textInput("arbFitting",
        #   label="Arbitrary Fitting Function:",
        #   value="y ~ a * I(log(x + b)) + c",
        #   width="100%",
        #   placeholder="e.g. y ~ x"
        #   ),

        radioButtons("normOption", "Normalise?",
                      list("Yes" = TRUE,
                        "No" = FALSE
                        ),
                        selected = FALSE),

        radioButtons("plotType", "Plot type:",
                      list("Points" = "geom_point",
                        "Line" = "geom_line"
                        ),
                      selected = "geom_point"),

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
                        ),
                      selected="identity"
                      ),

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
                        ),
                      selected="identity"
                      ),
        br()
      ),
      br()
    ),

    # main tabbed panel with plots and data representation
    mainPanel(
      tabsetPanel(
        tabPanel("Hourly Resolution",
            div(style = "position:relative;",
              plotOutput("hourlyScatter")
              )
            ),
        tabPanel("Full Resolution",
            div(
              # class="wrapper", 
              style = "position:relative;",
              plotOutput("plot", 
                         hover = hoverOpts("plot_hover", delay = 0, delayType = "debounce")),
              uiOutput("hover_info")
              # HTML("<canvas id='signature-pad' class='signature-pad' width=600 height=400></canvas>"),
              # HTML("<div>
              #      <button id='save'>Save</button>
              #      <button id='clear'>Clear</button>
              #      </div>")
              )
            ),
        tabPanel("Daily Resolution",
            div(style = "position:relative;",
              plotOutput("dailyScatter")
              )
            )
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
