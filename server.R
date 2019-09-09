# TODO:
# Clustering with ggalt
# Usability enhancements with shinyjs
# Cache previous queries
# Fix equation display on or below chart
# Do something about plot titles

library(shiny)
library(RPostgreSQL)
library(ggplot2)
library(lubridate)
source("baztools.R")
require(DT)
library(ggthemes)
library(ggTimeSeries)
library(gridExtra)
library(ggExtra)
library(dplyr) # for grouping by hour and day etc
library(shinyjs)
library(dygraphs)

require(tictoc, quietly=TRUE) # for timing code (not essential)
Sys.setenv(TZ="UTC")

# not used for now
# library("bazRtools") # using local source for convenience instead
# library("plotly") # way too slow
# library(ggalt) # for clustering
library(ggpmisc) # use stat_poly_eq to show fitted equation

# transformer feeders approximately grouped, neutrals omitted
# feeder_list <- c(33,34,35, 41,42,43, 45,46,47)  # tf6
# feeder_list <- c(65,66,68, 70,69,71, 74,75,76)  # tf5

colors <- matrix(
  c("#3E110C", "#F05D4B", #red
  "#082606", "#3EA13B",   #green
  "#0C2139", "#4F90DC"),  #blue
  nrow=3, ncol=2, byrow=TRUE)

# list of possible parameters either queried from DB or calculated in baztools.R
paramList <- list(
  "Time" = "time_and_date",
  "Current (A)" = "current",
  "Voltage (V)" = "voltage",
  "Voltage Imbalance (%)" = "imbalance",
  "Real Power (W)" = "real_power",
  "Reactive Power (VAr)" = "reac_power",
  "Apparent Power (VA)" = "app_power",
  "Displacement Power Factor" = "disp_power_factor",
  "True Power Factor" = "true_power_factor",
  # "Phase ID" = "phase_id",
  "Current THD (%)" = "current_thd",
  "Current THD Magnitude (A)" = "current_thd_magnitude",
  "Voltage THD (%)" = "voltage_thd",
  "Voltage THD Magnitude (V)" = "voltage_thd_magnitude",
  "Temperature (°C)" = "temperature",
  "Frequency (Hz)" = "frequency",
  "Reactive Power (True, VAr)" = "reac_power_t",
  "Apparent Power (True, VA)" = "app_power_t",
  "Minute of Day" = "min_of_day",
  "Hour of Day" = "hour_of_day",
  "Day of Week" = "day_of_week",
  "L1 Voltage (V)" = "v1",
  "L2 Voltage (V)" = "v2",
  "L3 Voltage (V)" = "v3",
  "Problem Phase" = "problem_phase",
  "Energy Units (kWh)" = "kwh_cs"
  )

# this is the inverse of paramList to speed up lookup
labelList = list()
for(i in 1:length(paramList)){
  labelList[paramList[[i]]] = names(paramList)[i]
  }

shinyServer(function(input, output, session) {
  # declare reactive data variables
  d <- reactiveValues(
    feeder_data=data.frame(), 
    stored_data=data.frame(),
    hourly_stats=data.frame(), 
    daily_stats=data.frame(), 
    feeders=data.frame()
    )

  # shinyjs functions
  # This is part of the hack to show the second slider with the previously queried dates
  disable(id="dateRangeExtents")
  
  # trying to disable lastpass fill
  addCssClass("trafoNumber", "search")
  runjs('document.getElementsByTagName("form")[0].setAttribute("data-lpignore", "");')
  runjs('document.getElementsByTagName("select")[0].setAttribute("data-lpignore", "");')
  
  # watch for changes to the date slider, and date buttons
  observeEvent({
    input$dateRangeSlider
    }, ignoreInit=TRUE, {
    updateDateRangeInput(session, "dateRange",
      start=ymd(input$dateRangeSlider[1]),
      end=ymd(input$dateRangeSlider[2])
      )
    }
  )
  observeEvent(input$backWeek,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1])-7,
      as.Date(input$dateRangeSlider[2])-7)
      )})
  observeEvent(input$fwdWeek,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1])+7,
      as.Date(input$dateRangeSlider[2])+7)
      )})
  observeEvent(input$subDayStart,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1])+1,
      as.Date(input$dateRangeSlider[2]))
      )})
  observeEvent(input$addDayStart,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1])-1,
      as.Date(input$dateRangeSlider[2]))
      )})
  observeEvent(input$subDayEnd,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1]),
      as.Date(input$dateRangeSlider[2])-1)
      )})
  observeEvent(input$addDayEnd,
    {updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRangeSlider[1]),
      as.Date(input$dateRangeSlider[2])+1)
      )})

  # wait for query button to be pressed
  observeEvent(input$queryBtn, {
    # browser()
    if(input$feederNumber=="") {
      showNotification("Please select a transformer", duration=3, type='message')
      return(NULL)
    }
    # dates from selector
    start_date <- ymd(input$dateRange[1])
    end_date <- ymd(input$dateRange[2])
    
    updateSliderInput(session, "dateRangeSlider",
      value=c(as.Date(input$dateRange[1]),
              as.Date(input$dateRange[2]))
      )
    updateSliderInput(session, "dateRangeExtents",
      value=c(as.Date(input$dateRange[1]),
              as.Date(input$dateRange[2]))
    )
    
    # format the dates for SQL
    start_time <- paste0("'", start_date, " 00:00:00", "'")
    end_time <- paste0("'", end_date, " 23:59:59", "'")

    # try to connect to the database
    withProgress(message="Please Wait", style="notification", {
        incProgress(detail="Connecting")
        tryCatch({
          # check if connection already exists
          dbGetQuery(con, '')
          print("Connection still alive")},
          # if not, start connection
          error = function(e) {
          tryCatch({
            print("Trying local connection")
            con <<- start_sql('local')
            },
            error=function(cond) {
              tryCatch({
                print("Trying remote connection")
                con <<- start_sql('remote')
                },
                error=function(cond2) {
                  print("Couldn't connect to database")
                  showNotification("Couldn't connect to database", type='error')
                  return()
                })
              })
            }
          )
      # try to make the query
        incProgress(detail="Getting data")

      tryCatch({
        if("tictoc" %in% (.packages())) {
                tic("get_data")
              }
        d$feeders <- get_feeders(con)
        
        # This logic block cuts down on queries to the server - especially useful for large queries
        # It checks to see if the data being requested already exists, or if a subset of it already exists
        if(length(d$feeder_data) > 0) {
          # decide if the query is new data or a subset of data already queried
          if(length(d$stored_data) == 0){
            d$stored_data <- d$feeder_data
          } else if(length(d$feeder_data$time_and_date) > length(d$stored_data$time_and_date)){
            d$stored_data <- d$feeder_data
          }
          sd1 <- min(d$stored_data$time_and_date)  # old start time
          ed1 <- max(d$stored_data$time_and_date)  # old end time
          sd2 <- ymd_hms(start_time)               # new start time
          ed2 <- ymd_hms(end_time)                 # new end time
          
          # timeClose(time1, time2, thresh=5) checks whether two timestamps are within thresh minutes
          if(timeClose(sd2, sd1) && timeClose(ed2, ed1)){
            # no need to do new query, just return previous results
            # print("identical")
            return_data <- d$stored_data
          } else if((sd2 > ed1 && !timeClose(sd2, ed1)) || (ed2 < sd1 && !timeClose(ed2, sd1))){
            # discard old data, perform a new query
            # gets way too complicated otherwise!!
            # print("noncontiguous")
            d$stored_data <- data.frame()
            return_data <- get_data(con, d$feeders, as.integer(input$feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
            d$stored_data <- return_data
          } else if(((sd2 >= sd1 || timeClose(sd2, sd1)) && (ed2 <= ed1 || timeClose(ed2, ed1)))){
            # return subset between sd2 and ed2
            # print("inside")
            return_data <- d$stored_data[which(
              ymd_hms(d$stored_data$time_and_date) >= sd2 & 
                ymd_hms(d$stored_data$time_and_date) <= ed2),]
          } else if(sd2 < sd1 && (ed2 <= ed1 || timeClose(ed1, ed2)) && (ed2 >= sd1 || timeClose(sd1, ed2)) ){
            # perform new query between sd2 and sd1
            # merge new query with previous results
            # return subset between sd2 and ed2
            # print("leftside")
            new_data <- get_data(con, d$feeders, as.integer(input$feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(sd1-seconds(1), "'%Y-%m-%d %H:%M:%S'"))
            d$stored_data <- rbind(new_data, d$stored_data)
            return_data <- d$stored_data[which(
              ymd_hms(d$stored_data$time_and_date) >= sd2 & 
                ymd_hms(d$stored_data$time_and_date) <= ed2),]
          } else if(ed2 > ed1 && (sd2 >= sd1 || timeClose(sd1, sd2)) && (sd2 <= ed1 || timeClose(ed1, sd2))){
            # perform new query between ed1 and ed2
            # merge new query with previous results
            # return subset between sd2 and ed2
            # print("rightside")
            new_data <- get_data(con, d$feeders, as.integer(input$feederNumber), format(ed1+seconds(1), "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
            d$stored_data <- rbind(d$stored_data, new_data)
            return_data <- d$stored_data[which(
              ymd_hms(d$stored_data$time_and_date) >= sd2 & 
                ymd_hms(d$stored_data$time_and_date) <= ed2),]
          } else if(sd2 < sd1 && ed2 > ed1 && !timeClose(sd1, sd2) && !timeClose(ed1, ed2)){
            # perform two new queries between sd2 and sd1 AND ed1 and ed2
            # merge new query with previous results
            # return subset between sd2 and ed2
            # print("outside")
            left_data <- get_data(con, d$feeders, as.integer(input$feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(sd1-seconds(1), "'%Y-%m-%d %H:%M:%S'"))
            right_data <- get_data(con, d$feeders, as.integer(input$feederNumber), format(ed1+seconds(1), "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
            d$stored_data <- rbind(left_data, d$stored_data, right_data)
            return_data <- d$stored_data[which(
              ymd_hms(d$stored_data$time_and_date) >= sd2 & 
                ymd_hms(d$stored_data$time_and_date) <= ed2),]
          } else {
            # It should not get to here, this will just catch weirdness, so just print the times and return the old data
            print("???")
            print("old times")
            print(c(sd1, ed1))
            print("new times")
            print(c(sd2, ed2))
            print("???")
            print("")
            return_data <- d$stored_data
          }
          d$feeder_data <- return_data
          updateSliderInput(session, "dateRangeExtents",
                            value=c(as.Date(ymd_hms(min(d$stored_data$time_and_date))),
                                    as.Date(ymd_hms(max(d$stored_data$time_and_date))))
          )
        }else{
          d$feeder_data <- get_data(con, d$feeders, as.integer(input$feederNumber), start_time, end_time)
        }

        # log how long queries are taking
        if("tictoc" %in% (.packages())) {
                timer <- toc()
                days_int <- round(interval(ymd_hms(start_time), ymd_hms(end_time)) / days(1))
                secsPerDay <- (timer$toc - timer$tic) / days_int
                print(paste(
                  round(secsPerDay, 3),
                  "seconds per day of data"
                  ))
        }

        # do some selection of data to remove outliers
        d$feeder_data <- d$feeder_data[which(d$feeder_data$temperature < 1000),]
        # d$feeder_data[which(d$feeder_data$current_thd == 64),]$current_thd <- NA
        
        # custom function to generate statistics summarised on a time increment e.g. 'hour' or 'day'
        d$hourly_stats <- calc_time_stats(d$feeder_data, 'hour')
        d$daily_stats <- calc_time_stats(d$feeder_data, 'day')
      },
        error=function(cond){
          print(cond)
          return()
        },
        warning=function(cond){
          print(cond)
          return()
        })
        incProgress(detail="Done!")
    })
  })

  # build the data frame to be plotted
  subSampling <- reactive({
    # for large datasets it may be nice to just take a random subsample to speed up plotting
    # this is now forced for > 10000 data points just for sake of demo speed
    subsample <- input$n/100
    # build data frame
    if(input$normOption) {
      if(input$paramX != "time_and_date") x <- normalise(xdata())
      else x <- xdata()
      if(input$paramY != "time_and_date") y <- normalise(ydata())
      else y <- ydata()
      df <- data.frame(x, y, coldata())
    }
    else {
      df <- data.frame(xdata(), ydata(), coldata())
    }
    if(input$paramCol == "time_and_date"){
      # to have a continuous colour range, convert datetime to integer
      df$coldata.. <- as.integer(df$coldata..)
    }
    # take a random subsample of the data
    if(nrow(df) > 10000){
      # limit number of points in full resolution to 10000
      df <- df[sample(nrow(df),10000*subsample),]
    }
    else {
      df <- df[sample(nrow(df),nrow(df)*subsample),]
    }
    })

  # populate feeder and date selectors based on the trafoNumber
  observe({
    selected_trafo <- input$trafoNumber
    if(selected_trafo=="") return(NULL)

    feederSelectList<-rbind(
      'tf1'=list(5, 1, 6,  9, 7, 8,  11,10,12), 
      'tf3'=list(33,34,35, 41,42,43, 45,46,47), 
      'tf6'=list(81,82,83, 89,90,91, 93,94,95), 
      'tf5'=list(65,66,68, 70,69,71, 74,75,76)
      )
    colnames(feederSelectList) <- c(
      'A1','A2','A3',
      'B1','B2','B3',
      'C1','C2','C3'
      )

    # these are the date ranges that we have demo data for on each trafo
    date_ranges <- data.frame(stringsAsFactors=FALSE,
      row.names=c("tf1", "tf6", "tf5"),
      "min" = c("2019-05-01", "2019-05-01", "2019-05-01"),
      "max" = c( "2019-05-31", "2019-05-31", "2019-05-31")
    )
    
    # Update the controls based on the chosen transformer - some may have different options
    updateSelectInput(session, "feederNumber",
      choices=feederSelectList[selected_trafo,],
      selected = feederSelectList[[selected_trafo,1]]
      )
    updateSliderInput(session, "dateRangeSlider",
      min=as.Date(date_ranges[selected_trafo, "min"]),
      max=as.Date(date_ranges[selected_trafo, "max"]),
      value=c(as.Date(ymd(date_ranges[selected_trafo, "max"])-4),
      as.Date(date_ranges[selected_trafo, "max"]))
      )
    updateSliderInput(session, "dateRangeExtents",
      min=as.Date(date_ranges[selected_trafo, "min"]),
      max=as.Date(date_ranges[selected_trafo, "max"]),
      value=c(as.Date(ymd(date_ranges[selected_trafo, "max"])-4),
      as.Date(date_ranges[selected_trafo, "max"]))
    )
    updateDateRangeInput(session, "dateRange",
      min=date_ranges[selected_trafo, "min"],
      max=date_ranges[selected_trafo, "max"],
      start=ymd(date_ranges[selected_trafo, "max"])-4,
      end=date_ranges[selected_trafo, "max"]
      )
    updateSelectInput(session, "paramX",
                      choices = paramList,
                      selected = paramList[1]
    )
    updateSelectInput(session, "paramY",
                      choices = paramList,
                      selected = paramList[2]
    )
    updateSelectInput(session, "paramCol",
                      choices = paramList,
                      selected = paramList[11]
    )
  })
  
  # discard stored data when changing transformer/feeder
  observeEvent({
    input$feederNumber
    input$trafoNumber
    },{
    d$stored_data <- data.frame()
    d$feeder_data <- data.frame()
  })
  
  # take the x parameter chosen and form a valid R variable name
  xdata <- reactive({
    # everything to be refreshed needs to be connected to queryBtn
    btnPress <- input$queryBtn
    eval(parse(text = paste0("d$feeder_data$", input$paramX)))
    })

  # take the y parameter chosen and form a valid R variable name
  ydata <- reactive({
    btnPress <- input$queryBtn
    eval(parse(text = paste0("d$feeder_data$", input$paramY)))
    })

  # take the colour parameter chosen and form a valid R variable name
  coldata <- reactive({
    btnPress <- input$queryBtn
    eval(parse(text = paste0("d$feeder_data$", input$paramCol)))
    })

  # show feeder info
  output$summary_Feederinfo <- renderPrint({
    # length(isolate(reactiveValuesToList(d$feeder_data)))
    if(input$queryBtn > 0 && length(d$feeder_data) != 0) {print("Feeder info:"); d$feeders[d$feeders$id==as.integer(input$feederNumber),]}
    })

  # show the summary of the X data if there's anything to show
  output$summary_Xinfo <- renderPrint({
    if(input$queryBtn > 0 && length(d$feeder_data) != 0) {print(input$paramX); summary(xdata())}
    })

  # show the summary of the Y data if there's anything to show
  output$summary_Yinfo <- renderPrint({
    if(input$queryBtn > 0 && length(d$feeder_data) != 0) {print(input$paramY); summary(ydata())}
    })

  # show the summary of the colour data if there's anything to show
  output$summary_Colinfo <- renderPrint({
    if(input$queryBtn > 0 && length(d$feeder_data) != 0) {print(input$paramCol); summary(coldata())}
    })

  # render the plot
  output$plot <- renderPlot({
    # everything to be refreshed needs to be connected to queryBtn
      btnPress <- input$queryBtn
      if(btnPress == 0) return(NULL)
    withProgress(message="Rendering Plot", detail="Please Wait", {
    # make data frame
      df <- subSampling()
    incProgress(1/6)
    # define plot area
      p <- ggplot(df, aes(df$x, df$y))
    incProgress(1/6)
    # check options and add lines or points
      if(input$smoothOption) {
        p <- p + geom_smooth(
          method=input$smoothType, 
          span=0.05, 
          level=0.99999, 
          na.rm=TRUE)
      }
      if(input$plotType == "geom_point") p <- p + geom_point(aes(colour=df$coldata), alpha = input$alpha)
      if(input$plotType == "geom_line") p <- p + geom_line(aes(colour=df$coldata), alpha = input$alpha)
      
      # TODO check whether arbitrary fitting is useful (or possible!)
      # if(input$arbFitting != "") {
      #   tryCatch({
      #       attach(d) # so I can refer directly to x and y
      #       myFormula <- eval(parse(text = input$arbFitting))
      #       print(myFormula)
      #       # p <- p + geom_smooth(method="loess", formula=myFormula)
      #       # p <- p + stat_poly_eq(formula = myFormula,
      #       #   aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
      #       #   parse = TRUE)
      #       regressor <- lm(formula = myFormula)
      #       print(summary(regressor))
      #       },
      #       warning = function(w){
      #         showNotification("Warning", duration=1, type='message')
      #         return(NULL)
      #       },
      #       error = function(e){
      #         showNotification("Invalid function. Must be of form y ~ x", duration=1, type='message')
      #         return(NULL)
      #       },
      #       finally={
      #         detach(d) # make sure d is detached so it won't mask future fits
      #       }
      #     )
      #   }

      # TODO implement grouping
      # encircle <- TRUE
      # d_select <- d$feeder_data[d$feeder_data$current_thd > 40,]
      # if(encircle == TRUE) p <- p + geom_encircle(aes(x=input$paramX, y=input$paramY), data=d_select, color="red", size=2, expand=0)
    incProgress(1/6)
    # do some general setup on the plot (from baztools.R)
      p <- setup_plot(p, id)
    incProgress(1/6)

      # TODO add a nice title
      # p <- p + ggtitle(paste(input$trafoNumber, input$feederNumber))
      # p <- p + ggtitle(paste(
      #   names(trafoSelectList[which(trafoSelectList == input$trafoNumber)]), 
      #   "Feeder",
      #   input$feederNumber
      #   ))
      # browser()
      
    incProgress(1/6)
      p <- p + theme(legend.position = "bottom")
      if(input$paramX != "time_and_date") p <- p + scale_x_continuous(trans=input$xScaleType)
      if(input$paramY != "time_and_date") p <- p + scale_y_continuous(trans=input$yScaleType)
      # labels and legends
      p <- p + xlab(labelList[[input$paramX]])
      p <- p + ylab(labelList[[input$paramY]])
      p <- p + scale_colour_continuous(low="blue", high="red",
                                       guide = guide_colorbar(direction = "horizontal", 
                                                              title=labelList[[input$paramCol]], 
                                                              title.position="top", 
                                                              title.hjust=0.5,
                                                              barwidth = 30)
      )
      # print(p)  # show plot (doesn't work with hover tooltips)
    # ggMarginal(p, type = "histogram", fill="transparent")

    
    p
      # incProgress(1/6)
    })
  })

  # render the hourly plot
  output$hourlyScatter <- renderPlot({
    # everything to be refreshed needs to be connected to queryBtn
      btnPress <- input$queryBtn
      if(btnPress == 0) return(NULL)
    withProgress(message="Rendering Plot", detail="Please Wait", {
    # make data frame
      # df <- subSampling()
      
      ## this isn't used yet, trying to figure out if it's necessary
      if (input$paramX != "time_and_date") {
        xText <- paste0("d$hourly_stats$", input$paramX, "_mean")
      } else{
        xText <- paste0("d$hourly_stats$", input$paramX)
      }
      if (input$paramCol != "time_and_date") {
        colText <- paste0("d$hourly_stats$", input$paramCol, "_mean")
      } else{
        colText <- paste0("d$hourly_stats$", input$paramCol)
      }
      ##
      
      xdata <- eval(parse(text = paste0("d$hourly_stats$", input$paramX, "_mean")))
      ydata <- eval(parse(text = paste0("d$hourly_stats$", input$paramY, "_mean")))
      coldata <- eval(parse(text = paste0("d$hourly_stats$", input$paramCol, "_mean")))
      df <- data.frame(xdata, ydata, coldata)
      # browser()
    incProgress(1/6)
    # define plot area
      p <- ggplot(df, aes(df$x, df$y))
    incProgress(1/6)
    # check options and add lines or points
      # if(input$smoothOption) {
      #   p <- p + geom_smooth(
      #     method=input$smoothType, 
      #     span=0.05, 
      #     level=0.99, 
      #     na.rm=TRUE)
      # }
      # if(input$plotType == "geom_point") p <- p + geom_point(aes(colour=df$coldata), alpha = input$alpha)
      # if(input$plotType == "geom_line") p <- p + geom_line(aes(colour=df$coldata), alpha = input$alpha)
      p <- p + geom_ribbon(aes(
        ymax = eval(parse(text = paste0("d$hourly_stats$", input$paramY, "_max"))),
        ymin = eval(parse(text = paste0("d$hourly_stats$", input$paramY, "_min")))
        ), alpha=0.5, fill="skyblue")
      p <- p + geom_point(aes(colour=df$coldata), alpha = input$alpha)
      p <- p + geom_line(aes(colour=df$coldata), alpha = input$alpha)
      
    incProgress(1/6)
    # do some general setup on the plot (from baztools.R)
      p <- setup_plot(p, id)
    incProgress(1/6)

      # TODO add a nice title
      # p <- p + ggtitle(paste(input$trafoNumber, input$feederNumber))
      # p <- p + ggtitle(paste(
      #   names(trafoSelectList[which(trafoSelectList == input$trafoNumber)]), 
      #   "Feeder",
      #   input$feederNumber
      #   ))
      # browser()
      
    incProgress(1/6)
      p <- p + theme(legend.position = "bottom")
      if(input$paramX != "time_and_date") p <- p + scale_x_continuous(trans=input$xScaleType)
      if(input$paramY != "time_and_date") p <- p + scale_y_continuous(trans=input$yScaleType)
      # labels and legends
      p <- p + xlab(labelList[[input$paramX]])
      p <- p + ylab(labelList[[input$paramY]])
      p <- p + scale_colour_continuous(low="blue", high="red",
                                       guide = guide_colorbar(direction = "horizontal", 
                                                              title=labelList[[input$paramCol]], 
                                                              title.position="top", 
                                                              title.hjust=0.5,
                                                              barwidth = 30)
      )
      # print(p)  # show plot (doesn't work with hover tooltips)
    # ggMarginal(p, type = "histogram", fill="transparent")
    
    p
      # incProgress(1/6)
    })
  })

  # render the daily plot
  output$dailyScatter <- renderPlot({
    # everything to be refreshed needs to be connected to queryBtn
      btnPress <- input$queryBtn
      if(btnPress == 0) return(NULL)
    withProgress(message="Rendering Plot", detail="Please Wait", {
    # make data frame
      # df <- subSampling()
      xdata <- eval(parse(text = paste0("d$daily_stats$", input$paramX, "_mean")))
      ydata <- eval(parse(text = paste0("d$daily_stats$", input$paramY, "_mean")))
      coldata <- eval(parse(text = paste0("d$daily_stats$", input$paramCol, "_mean")))
      df <- data.frame(xdata, ydata, coldata)
    incProgress(1/6)
    # define plot area
      p <- ggplot(df, aes(df$x, df$y))
    incProgress(1/6)
    # check options and add lines or points
      # if(input$smoothOption) {
      #   p <- p + geom_smooth(
      #     method=input$smoothType, 
      #     span=0.05, 
      #     level=0.99, 
      #     na.rm=TRUE)
      # }
      # if(input$plotType == "geom_point") p <- p + geom_point(aes(colour=df$coldata), alpha = input$alpha)
      # if(input$plotType == "geom_line") p <- p + geom_line(aes(colour=df$coldata), alpha = input$alpha)
      p <- p + geom_ribbon(aes(
        ymax = eval(parse(text = paste0("d$daily_stats$", input$paramY, "_mean + d$daily_stats$", input$paramY, "_sd"))),
        ymin = eval(parse(text = paste0("d$daily_stats$", input$paramY, "_mean - d$daily_stats$", input$paramY, "_sd")))
        ), alpha=0.5, fill="skyblue")
      p <- p + geom_point(aes(colour=df$coldata), alpha = input$alpha)
      p <- p + geom_line(aes(colour=df$coldata), alpha = input$alpha)
      
    incProgress(1/6)
    # do some general setup on the plot (from baztools.R)
      p <- setup_plot(p, id)
    incProgress(1/6)

      # TODO add a nice title
      # p <- p + ggtitle(paste(input$trafoNumber, input$feederNumber))
      # p <- p + ggtitle(paste(
      #   names(trafoSelectList[which(trafoSelectList == input$trafoNumber)]), 
      #   "Feeder",
      #   input$feederNumber
      #   ))
      # browser()
      
    incProgress(1/6)
      p <- p + theme(legend.position = "bottom")
      if(input$paramX != "time_and_date") p <- p + scale_x_continuous(trans=input$xScaleType)
      if(input$paramY != "time_and_date") p <- p + scale_y_continuous(trans=input$yScaleType)
      p <- p + xlab(labelList[[input$paramX]])
      p <- p + ylab(labelList[[input$paramY]])
      p <- p + scale_colour_continuous(low="blue", high="red",
                                       guide = guide_colorbar(direction = "horizontal", 
                                                              title=labelList[[input$paramCol]], 
                                                              title.position="top", 
                                                              title.hjust=0.5,
                                                              barwidth = 30)
      )
      # print(p)  # show plot (doesn't work with hover tooltips)
    # ggMarginal(p, type = "histogram", fill="transparent")

    p
      # incProgress(1/6)
    })
  })
  
  output$dygraph <- renderDygraph({
    btnPress <- input$queryBtn
    if(btnPress == 0) return(NULL)
    # if(input$paramX != "time_and_date") return(NULL)

    # make data frame
    df <- d$feeder_data
    df <- unique(df)
    q <- data.frame(eval(parse(text = paste0("df$", input$paramY))), eval(parse(text = paste0("df$", input$paramCol))))
    # browser()
    rownames(q) <- df$time_and_date
    colnames(q) <- c(input$paramY, input$paramCol)
    dygraph(q) %>% 
      dyRangeSelector() %>%
      dyShading(
        from = min(df$time_and_date), 
        to = max(df$time_and_date)
        # color="white"
        ) %>%
      dyAxis("y", label=labelList[[input$paramY]]) %>%
      dyAxis("y2", label=labelList[[input$paramCol]], independentTicks = TRUE, drawGrid=FALSE) %>%
      dySeries(
        input$paramY, 
        axis='y',
        color=paste0("rgba(253,0,15,",input$alpha,")") #red
        ) %>%
      dySeries(
        input$paramCol, 
        axis='y2',
        color=paste0("rgba(25,0,255,",input$alpha,")") #blue
        ) %>%
      dyHighlight() %>%
      dyLegend(
        show="follow",
        width=200,
        labelsSeparateLines = T
        ) %>%
      dyRoller(rollPeriod = 1) %>%
      dyUnzoom() %>%
      dyOptions(
        # axisLabelColor = "white",
        drawPoints = FALSE,
        strokeBorderWidth = 0.1
        )
    })

  # Generate an HTML table view of the data
  output$dataTable <- renderDataTable({
    btnPress <- input$queryBtn
    DT::datatable(d$feeder_data, extensions=c('Buttons','Scroller'),
      rownames=FALSE,
      escape=FALSE,
      options=list(dom='Bfrtip',
        buttons=
          list('colvis', list(
            extend = 'collection',
            buttons = list(
              list(extend='csv',
                filename = 'd$feeder_data'),
              list(extend='excel',
                filename = 'd$feeder_data'),
              list(extend='pdf',
                filename= 'd$feeder_data')),
                   text = 'Download'
                 )),
          scrollX=TRUE,
          pageLength=nrow(d$feeder_data),
          deferRender=TRUE,
          scrollY=400,
          scroller=TRUE
        )
      )
  })

  # Generate an HTML table view of the feeder list
  output$feederTable <- renderDataTable({
    btnPress <- input$queryBtn
    DT::datatable(d$feeders, extensions=c('Buttons','Scroller'),
      rownames=FALSE,
      escape=FALSE,
      options=list(dom='Bfrtip',
        buttons=
          list('colvis', list(
            extend = 'collection',
            buttons = list(
              list(extend='csv',
                filename = 'd$feeders'),
              list(extend='excel',
                filename = 'd$feeders'),
              list(extend='pdf',
                filename= 'd$feeders')),
                   text = 'Download'
                 )),
          scrollX=TRUE,
          pageLength=nrow(d$feeders),
          deferRender=TRUE,
          scrollY=400,
          scroller=TRUE
        )
      )
  })

  output$hourlyplot <- renderPlot({
    if(input$queryBtn == 0) return(NULL)
    hourly_fill <- eval(parse(text = paste0("d$hourly_stats$", input$paramY, "_mean")))

    plot1 <- ggplot(d$hourly_stats, 
      aes(as_date(d$hourly_stats$time_and_date), d$hourly_stats$hour_fac)
      # aes(as_date(d$hourly_stats$hour), d$hourly_stats$hour_fac)
      ) + 
      geom_tile(aes(fill=hourly_fill)) + 
      ylim(rev(levels(d$hourly_stats$hour_fac))) + 
      scale_fill_continuous(low=colors[3,1], high=colors[1,2]) + 
      scale_x_date(date_breaks = "day", date_labels = "%a %d %b") +
      labs(
        fill=paste0("Hourly\n", labelList[[input$paramY]]), 
        x="Date", 
        y="Time") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    # plist <- list(plot1, plot2)

    # g <- do.call("grid.arrange", c(plist, ncol=1))
    print(plot1)
  })

  output$calendarplot <- renderPlot({
    if(input$queryBtn == 0) return(NULL)
    # options for data to plot are min, max, mean, sd
    if(input$paramY == "kwh_cs") {
      calendarTitle = paste("Daily", labelList[[input$paramY]], "\n", round(sum(d$daily_stats$kwh_cs_mean), digits=1), "kWh (Total for period)")
    } else {
      calendarTitle = paste0("Daily ", labelList[[input$paramY]])
    }
    calPlot <- ggplot_calendar_heatmap(d$daily_stats, 'time_and_date', paste0(input$paramY, '_mean')) + 
      xlab(NULL) + 
      ylab(NULL) + 
      labs(title=calendarTitle) +
      scale_fill_continuous(low=colors[3,1], high=colors[1,2])

    print(calPlot)
  })

  output$hover_info <- renderUI({
    if(input$queryBtn == 0 || nrow(d$feeder_data)==0) return(NULL)
    hover <- input$plot_hover
    point <- nearPoints(d$feeder_data, 
      hover, 
      xvar=input$paramX, 
      yvar=input$paramY, 
      threshold = 5, 
      maxpoints = 1, 
      addDist = TRUE
      )
    if (nrow(point) == 0) return(NULL)

    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px - 150, "px; top:", top_px + 2, "px;")

    # do some formatting on the point data
    days_of_week <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    if(input$paramX != "time_and_date") pointX <- eval(parse(text = paste0("round(as.numeric(point$", input$paramX, "), 2)")))
    else pointX <- eval(parse(text = paste0("point$", input$paramX)))
    pointY <- eval(parse(text = paste0("round(as.numeric(point$", input$paramY, "), 2)")))
    pointZ <- eval(parse(text = paste0("round(as.numeric(point$", input$paramCol, "), 2)")))
    if(input$paramX == "min_of_day") pointX <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointX/1440)))),"%H:%M")
    if(input$paramY == "min_of_day") pointY <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointY/1440)))),"%H:%M")
    if(input$paramCol == "min_of_day") pointZ <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointZ/1440)))),"%H:%M")
    if(input$paramX == "day_of_week") pointX <-   days_of_week[as.integer(pointX)]
    if(input$paramY == "day_of_week") pointY <-   days_of_week[as.integer(pointY)]
    if(input$paramCol == "day_of_week") pointZ <- days_of_week[as.integer(pointZ)]

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(
        "<b>X: ", labelList[[input$paramX]], " = ", pointX, "<br/>",
        "<b>Y: ", labelList[[input$paramY]], " = ", pointY, "<br/>",
        "<b>Z: ", labelList[[input$paramCol]], " = ", pointZ, "<br/>",
        ""
        )))
    )
  })
  session$onSessionEnded(function() {
    if(exists("con")){
      dbDisconnect(con)
    }
    })
})
