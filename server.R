# for local TCD access, open tunnel to database with this command:
# ssh -L 9000:localhost:5432 murphyb8@transglobal.cloud.tilaa.com

library("shiny")
library("RPostgreSQL")
library("ggplot2")
library("lubridate")
source("baztools.R")
require("DT")
library("ggthemes")
library("ggTimeSeries")
library("gridExtra")

# not used for now
# library("bazRtools") # using local source for convenience instead
# library("plotly")

# transformer feeders approximately grouped, neutrals omitted
# feeder_list <- c(5,1,6, 9,7,8, 11,10,12)  # tf1
# feeder_list <- c(33,34,35, 41,42,43, 45,46,47)  # tf3
# feeder_list <- c(65,66,68, 70,69,71, 74,75,76)  # tf5

# declare global variables
# TODO: using reactive variables might be better
feeder_data <- data.frame()
hourly_stats <- data.frame()
date_stats <- data.frame()
feeders <- data.frame()
colors <- matrix(
  c("#3E110C", "#F05D4B", #red
  "#082606", "#3EA13B",   #green
  "#0C2139", "#4F90DC"),  #blue
  nrow=3, ncol=2, byrow=TRUE)

shinyServer(function(input, output, session) {

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

    # format the dates for SQL
    start_time <- paste0("'", start_date, " 00:00:00", "'")
    end_time <- paste0("'", end_date, " 23:59:59", "'")

    # try to connect to the database
    withProgress(message="Please Wait", style="notification", {
        incProgress(detail="Connecting")
      tryCatch({
        print("Trying local connection")
        con <- start_sql('local')
        },
        error=function(cond) {
          tryCatch({
            print("Trying remote connection")
            con <- start_sql('remote')
            },
            error=function(cond2) {
              print("Couldn't connect to database")
              showNotification("Couldn't connect to database", type='error')
              return()
            })
          }
        )
      # try to make the query
        incProgress(detail="Getting data")
      tryCatch({
        # double arrows (<<-) for global variable assignment
        feeder_data <<- get_data(con, as.integer(input$feederNumber), start_time, end_time)
        
        hourly_stats <<- get_hourly_power_stats(con, "real_power", as.integer(input$feederNumber), start_date, end_date)
        # hourly_stats <<- get_hourly_stats(con, input$paramY, as.integer(input$feederNumber), start_date, end_date)
        date_stats <<- get_date_stats(con, input$paramY, as.integer(input$feederNumber), start_date, end_date)
        hourly_stats$hour_fac <<- as.factor(hourly_stats$hour)
        feeder_data <<- feeder_data[which(feeder_data$temperature < 1000),]
        feeders <<- get_feeders(con)
        # feeder_data$min_of_day <<- format((as_datetime(hms("00:00:00") + as.integer(ddays(feeder_data$min_of_day/1440)))),"%H:%M")
        },
        error=function(cond){
          return()
          })
        incProgress(detail="Closing connection")
      print(paste("Connection closed?", dbDisconnect(con)))
        incProgress(detail="Done!")
    })
  })

  # build the data frame to be plotted
  subSampling <- reactive({
    subsample <- input$n/100
    # build data frame
    d <- data.frame(xdata(), ydata(), coldata())
    if(input$paramCol == "time_and_date"){
      # to have a continuous colour range, convert dt to integer
      d$coldata.. <- as.integer(d$coldata..)
    }
    # take a random subsample of the data
    d <- d[sample(nrow(d),nrow(d)*subsample),]
    })

  # populate feeder and date selectors based on the trafoNumber
  observe({
    selected_trafo <- input$trafoNumber
    if(selected_trafo=="") return(NULL)

    feederSelectList<-rbind(
      'tf1'=list(5, 1, 6,  9, 7, 8,  11,10,12), 
      'tf3'=list(33,34,35, 41,42,43, 45,46,47), 
      'tf5'=list(65,66,68, 70,69,71, 74,75,76)
      )
    colnames(feederSelectList) <- c(
      'A1','A2','A3',
      'B1','B2','B3',
      'C1','C2','C3'
      )

    # these are the date ranges that we have data for on each trafo
    # in future should be populated by database query?
    date_ranges <- data.frame(stringsAsFactors=FALSE,
      row.names=c("tf1", "tf3", "tf5"),
      "min" = c("2017-01-01", "2017-06-14", "2017-08-23"),
      "max" = c("2017-09-16", "2017-06-25", format.Date(today()))
      )

    updateSelectInput(session, "feederNumber",
      choices=feederSelectList[selected_trafo,],
      selected = feederSelectList[[selected_trafo,1]],
      )

    updateDateRangeInput(session, "dateRange",
      min=date_ranges[selected_trafo, "min"],
      max=date_ranges[selected_trafo, "max"],
      start=ymd(date_ranges[selected_trafo, "max"])-4,
      end=date_ranges[selected_trafo, "max"]
      )
    })

  # take the x parameter chosen and form a valid R variable name
  xdata <- reactive({
    # everything to be refreshed needs to be connected to queryBtn
    btnPress <- input$queryBtn
    eval(parse(text = paste0("feeder_data$", input$paramX)))
    })

  # take the y parameter chosen and form a valid R variable name
  ydata <- reactive({
    btnPress <- input$queryBtn
    eval(parse(text = paste0("feeder_data$", input$paramY)))
    })

  # take the colour parameter chosen and form a valid R variable name
  coldata <- reactive({
    btnPress <- input$queryBtn
    eval(parse(text = paste0("feeder_data$", input$paramCol)))
    })

  # show feeder info
  output$summary_Feederinfo <- renderPrint({
    if(input$queryBtn > 0 && length(feeder_data) != 0) {print("Feeder info:");feeders[feeders$id==as.integer(input$feederNumber),]}
    })

  # show the summary of the X data if there's anything to show
  output$summary_Xinfo <- renderPrint({
    if(input$queryBtn > 0 && length(feeder_data) != 0) {print(input$paramX); summary(xdata())}
    })

  # show the summary of the Y data if there's anything to show
  output$summary_Yinfo <- renderPrint({
    if(input$queryBtn > 0 && length(feeder_data) != 0) {print(input$paramY); summary(ydata())}
    })

  # show the summary of the colour data if there's anything to show
  output$summary_Colinfo <- renderPrint({
    if(input$queryBtn > 0 && length(feeder_data) != 0) {print(input$paramCol); summary(coldata())}
    })

  # render the plot
  output$plot <- renderPlot({
    # everything to be refreshed needs to be connected to queryBtn
      btnPress <- input$queryBtn
    withProgress(message="Rendering Plot", detail="Please Wait", {
    # make data frame
      d <- subSampling()
    incProgress(1/6)
    # define plot area
      p <- ggplot(d, aes(d$x, d$y))
    incProgress(1/6)
    # check options and add lines or points
      if(input$smoothOption) p <- p + geom_smooth(method='loess', span=0.05, level=0.99999)
      if(input$plotType == "geom_point") p <- p + geom_point(aes(colour=d$coldata), alpha = input$alpha)
      if(input$plotType == "geom_line") p <- p + geom_line(aes(colour=d$coldata), alpha = input$alpha)
    incProgress(1/6)
    # do some general setup on the plot (from baztools.R)
      p <- setup_plot(p, id)
    incProgress(1/6)
    # labels and legends
      p <- p + xlab(input$paramX)
      p <- p + ylab(input$paramY)
    incProgress(1/6)
      p <- p + theme(legend.position = "bottom")
      p <- p + scale_colour_continuous(low="blue", high="red",
    guide = guide_colorbar(direction = "horizontal", 
      title=input$paramCol, 
      title.position="top", 
      title.hjust=0.5,
      barwidth = 30)
    )
    # print(p)  # show plot (doesn't work with tooltips)
    p
      # incProgress(1/6)
    })
  })

  # Generate an HTML table view of the data
  output$dataTable <- renderDataTable({
    btnPress <- input$queryBtn
    DT::datatable(feeder_data, extensions=c('Buttons','Scroller'),
      rownames=FALSE,
      escape=FALSE,
      options=list(dom='Bfrtip',
        buttons=
          list('colvis', list(
            extend = 'collection',
            buttons = list(
              list(extend='csv',
                filename = 'feeder_data'),
              list(extend='excel',
                filename = 'feeder_data'),
              list(extend='pdf',
                filename= 'feeder_data')),
                   text = 'Download'
                 )),
          scrollX=TRUE,
          pageLength=nrow(feeder_data),
          deferRender=TRUE,
          scrollY=400,
          scroller=TRUE
        )
      )
  })

  # Generate an HTML table view of the feeder list
  output$feederTable <- renderDataTable({
    btnPress <- input$queryBtn
    DT::datatable(feeders, extensions=c('Buttons','Scroller'),
      rownames=FALSE,
      escape=FALSE,
      options=list(dom='Bfrtip',
        buttons=
          list('colvis', list(
            extend = 'collection',
            buttons = list(
              list(extend='csv',
                filename = 'feeders'),
              list(extend='excel',
                filename = 'feeders'),
              list(extend='pdf',
                filename= 'feeders')),
                   text = 'Download'
                 )),
          scrollX=TRUE,
          pageLength=nrow(feeders),
          deferRender=TRUE,
          scrollY=400,
          scroller=TRUE
        )
      )
  })

  output$hourlyplot <- renderPlot({
    if(input$queryBtn == 0) return(NULL)
    # hourly_fill <- eval(parse(text = paste0("hourly_stats$", input$paramY)))
    hourly_fill <- hourly_stats$avgreal
    # hourly_fill <- hourly_stats$avg

    plot1 <- ggplot(hourly_stats, 
      aes(hourly_stats$dt, hourly_stats$hour_fac)
      ) + 
      geom_tile(aes(fill=hourly_fill)) + 
      ylim(rev(levels(hourly_stats$hour_fac))) + 
      scale_fill_continuous(low=colors[3,1], high=colors[1,2]) + 
      scale_x_date(date_breaks = "day", date_labels = "%a %d %b") +
    #   scale_x_date(date_breaks = "month", date_labels = "%B") +
      labs(
        fill=paste0("Average\n", "Real\nPower"), 
        # fill=paste0("Average\n", input$paramY), 
        x="Date", 
        y="Time") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    # plist <- list(plot1, plot2)

    # g <- do.call("grid.arrange", c(plist, ncol=1))
    print(plot1)
  })

  output$calendarplot <- renderPlot({
    if(input$queryBtn == 0) return(NULL)
    # options for data to plot are min, max, avg, std
    calPlot <- ggplot_calendar_heatmap(date_stats, 'dt', 'avg') + 
      xlab(NULL) + 
      ylab(NULL) + 
      # labs(title=input$paramY) +
      scale_fill_continuous(low=colors[3,1], high=colors[1,2])

    print(calPlot)
  })

  output$hover_info <- renderUI({
    if(input$queryBtn == 0 || nrow(feeder_data)==0) return(NULL)
    hover <- input$plot_hover
    point <- nearPoints(feeder_data, 
      hover, 
      xvar=input$paramX, 
      yvar=input$paramY, 
      threshold = 5, 
      maxpoints = 1, 
      addDist = TRUE
      )
    if (nrow(point) == 0) return(NULL)
          
    # browser()
    # if (nrow(point) == 0) return(NULL)

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
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # do some formatting on the point data    
    pointX <- eval(parse(text = paste0("point$", input$paramX)))
    pointY <- eval(parse(text = paste0("point$", input$paramY)))
    pointZ <- eval(parse(text = paste0("point$", input$paramCol)))
    if(input$paramX == "min_of_day") pointX <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointX/1440)))),"%H:%M")
    if(input$paramY == "min_of_day") pointY <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointY/1440)))),"%H:%M")
    if(input$paramCol == "min_of_day") pointZ <- format((as_datetime(hms("00:00:00") + as.integer(ddays(pointZ/1440)))),"%H:%M")

    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0(
        "<b>X: ", input$paramX, " = ", pointX, "<br/>",
        "<b>Y: ", input$paramY, " = ", pointY, "<br/>",
        "<b>Z: ", input$paramCol, " = ", pointZ, "<br/>",
        ""
        )))
    )
  })
})
