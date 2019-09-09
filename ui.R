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
library(dygraphs)

Sys.setenv(TZ="UTC")


# list of transformers
trafoSelectList<-list(
  'Please select a transformer'="",
  'Transformer 1 : Drogheda'='tf5',
  'Transformer 2 : Limerick'='tf1'
  )

modalText <- HTML('<p>This tool explores time-series data streaming from custom hardware monitoring power distribution transformers out in the field.</p>
                  <p>I wrote it from scratch in <b>R</b> using <i>Shiny</i> for the web-app elements, and <i>RPostgreSQL</i> to handle database requests. Source code is available on <a href="https://github.com/baztastic/trafodata">github</a>.</p>
                  <p>The demo connects to a small database with a few weeks worth of data for two transformers.</p>
                  <p>To play with it, first select from the list of transformers, then hit GO.</p>')

# build a shiny UI
shinyUI(fluidPage(
  tags$head(
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
  titlePanel("Barry Murphy Time Series Demo"
             ),
  # Popup with demo info
  modalDialog(modalText, title='Time Series Demo'),
  actionButton("toggle", "Hide Menu", width="90px"),
  sidebarLayout(
    # sidebar with controls
    conditionalPanel(
      condition="input.toggle % 2 == 0",
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
          ),
        # This hack places one slider below another, showing the extents of 
        # the data already queried from the server and cached in memory
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
          # This is the "live" date range slider for range control
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
        # Buttons to jump around dates - particularly important for long ranges!
        tags$div(style="width:100%;text-align: center;",
                 actionButton("backWeek", "<️"),
                 actionButton("addDayStart", "+"),
                 actionButton("subDayStart", "–"),
                 actionButton("queryBtn", "Go"),
                 actionButton("subDayEnd", "–"),
                 actionButton("addDayEnd", "+"),
                 actionButton("fwdWeek", ">")
        ),
        br(),
        br(),
        selectInput("paramX", "X Parameter:",
                    choices=list("First select a transformer"="")
                    ),
  
        selectInput("paramY", "Y Parameter:",
                    choices=list("First select a transformer"="")
                    ),
  
        selectInput("paramCol", "Colour Parameter:",
                    choices=list("First select a transformer"="")
                    ),
        
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
      )
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
            ),
        tabPanel("Compare Timeseries",
                 div(style = "position:relative; background-color:white;",
                     dygraphOutput("dygraph")
                 )
            )
          ),
      tabsetPanel(
        # tabPanel("Plot", plotOutput("plot", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))), 
        tabPanel("Hourly Plot", plotOutput("hourlyplot")),
        tabPanel("Calendar Plot", plotOutput("calendarplot")),
        tabPanel("Stats", list(
            verbatimTextOutput("summary_Feederinfo"),
            verbatimTextOutput("summary_Xinfo"),
            verbatimTextOutput("summary_Yinfo"),
            verbatimTextOutput("summary_Colinfo")          
          )),
        tabPanel("Raw Data", DT::dataTableOutput("dataTable")),
        tabPanel("Feeders", DT::dataTableOutput("feederTable"))
      )
    )
  )
))
