require("RPostgreSQL")
require("ggplot2")
require("ggthemes")
require("lubridate")
require("dplyr")

#' Multiple plot function
#' 
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param ggplot plots or plotlist
#' @param int cols=Number of columns in layout
#' @param matrix layout=A matrix specifying the layout. If present, 'cols' is ignored

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	# Multiple plot function
	#
	# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
	# - cols:   Number of columns in layout
	# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
	#
	# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
	# then plot 1 will go in the upper left, 2 will go in the upper right, and
	# 3 will go all the way across the bottom.
	#
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Normalise a list
#' 
#' Remap a list of values from 0 - 1
#' @param list x
#' @return list x (normalised)

normalise <- function(x) {
  return( (x-min(x))/(max(x)-min(x)) )
}

#' Get pretty label for graphs
#' 
#' Find the correct label for a given parameter in paramList
#' @param string param
#' @param list paramList
#' @return string label

get_label <- function(param, paramList) {
  # note "\\b" are word boundary anchors to only match the full param
  # "\\<" and "\\>" could also be used for start and end
  label <- names(unlist(paramList[grep(paste0("\\b", param, "\\b"), paramList)]))
  return( label )
}

#' Time close
#' 
#' Check if two times are "close" together, within thresh minutes
#' @param datetime time1
#' @param datetime time2
#' @param float thresh (optional)
#' @return boolean

timeClose <- function(time1, time2, thresh=5) {
  if (abs(as.double(difftime(time1, time2, units="mins"))) <= thresh) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Start SQL
#' 
#' Open a connection to the TRANSGLOBAL transformer DB
#' @param character db='local' for tunneled connection, anything else for direct connection
#' @return connection object

start_sql <- function(db='local') {
  if("RcppTOML" %in% (installed.packages())){
    require("RcppTOML")
    config <- parseTOML("./config.toml")
  }
  else {
    # fallback to yaml
    require("yaml")
    config <- read_yaml("./config.yaml")
  }

	# open connection
	drv <- dbDriver("PostgreSQL")
	if(db == 'local') {
		database <- config$main$database
		login <- config$main$login
		connection <- dbConnect(drv,
					dbname = database$name,
					host = database$host,
					port = database$port,
					user = login$user,
					password = login$password)
	}
	else {
		database <- config$alternative$database
		login <- config$alternative$login
		connection <- dbConnect(drv,
					dbname = database$name,
					host = database$host,
					port = database$port,
					user = login$user,
					password = login$password)
	}
	rm(config) # removes the config info
	print(paste("Connection successful?", dbExistsTable(connection, "feeder_stats"), sep=" "))
	return(connection)
}

#' Get Feeders
#' 
#' Get a list of feeders from the database, with phase and transformer information
#' @param connection sql connection object
#' @return data.frame of feeders

get_feeders <- function(connection) {
	feeders_query <- paste("SELECT", 
				"feeders.id AS id,", 
				"feeders.phase_id AS phase,", 
				"feeders.num AS num,", 
				"feeders.polarity AS polarity,", 
				"phases.phase_type AS phase_type,",
				"phases.transformer_id AS transformer", 
				"FROM feeders",
				"JOIN phases ON phases.id = feeders.phase_id",
				"ORDER BY feeders.id;", sep=" ")

	return(dbGetQuery(connection, feeders_query))
}

#' Get Data
#' 
#' Get data from database on specific feeder. Joins feeder_stats, phase_stats, and transformer_stats on date_and_time
#' @param connection SQL connection object
#' @param data_frame List of feeders from get_feeders()
#' @param int Feeder id
#' @param character Start time for query. Make sure to include single quotes around string
#' @param character End time for query. Make sure to include single quotes around string
#' @return data.frame containing query data

get_data <- function(connection, feeders_info, feeder_id=1, start_time="'2017-06-16 11:00:00'", end_time="'2017-06-24 13:00:00'") {
	# if(feeders$phase_type[feeder_id] == 0){
	# 	dbDisconnect(con)
	# 	print("Neutral feeders are boring")
	# 	return(NULL)
	# }
	# feeders_info <- get_feeders(connection)
	
	trafo_id <- feeders_info$transformer[feeder_id]
	trafo_feeders <- feeders_info[which(feeders_info$transformer == trafo_id),]
	# think about grabbing all feeders for a particular trafo?
	# "WHERE feeder_stats.feeder_id IN ", paste0("(", paste0(trafo_feeders$id, collapse=", "), ")"),
	# throw out phase_type=0 since there are only 3 phases
	phase_ids <- unique(trafo_feeders$phase[which(trafo_feeders$phase_type != 0)])
	phase_ids <- sort(phase_ids)

	queryStr <- paste("SELECT",
					"feeder_stats.feeder_id AS feeder_id,", 
					"feeder_stats.time_and_date AS time_and_date,", 
					"feeder_stats.current AS current,", 
					"feeder_stats.real_power AS real_power,", 
					"feeder_stats.thd AS current_thd,", 
					"feeder_stats.power_factor AS power_factor,", 
					"feeder_stats.power_factor AS disp_power_factor,",  
					"feeder_stats.power_factor/SQRT(1+feeder_stats.thd*feeder_stats.thd/10000) AS true_power_factor,",
					"phase_stats.phase_id AS phase_id,", 
					"phase_stats.voltage AS voltage,", 
					"phase_stats.thd AS voltage_thd,",
					"transformer_stats.temperature AS temperature,",
					"transformer_stats.frequency AS frequency,",
					"p1.phase_id AS id1,", 
					"p1.voltage AS v1,", 
					"p2.phase_id AS id2,", 
					"p2.voltage AS v2,", 
					"p3.phase_id AS id3,", 
					"p3.voltage AS v3",
					"FROM feeder_stats",
					"JOIN phase_stats ON phase_stats.time_and_date = feeder_stats.time_and_date",
					"JOIN transformer_stats ON transformer_stats.time_and_date = feeder_stats.time_and_date",
					"JOIN phase_stats AS p1 ON p1.time_and_date = feeder_stats.time_and_date",
					"JOIN phase_stats AS p2 ON p2.time_and_date = feeder_stats.time_and_date",
					"JOIN phase_stats AS p3 ON p3.time_and_date = feeder_stats.time_and_date",
					"WHERE feeder_stats.feeder_id =", feeders_info$id[feeder_id],
					"AND phase_stats.phase_id =", feeders_info$phase[feeder_id],
					"AND transformer_stats.transformer_id = ", feeders_info$transformer[feeder_id],
					"AND p1.phase_id =", phase_ids[1],
					"AND p2.phase_id =", phase_ids[2],
					"AND p3.phase_id =", phase_ids[3],
					"AND feeder_stats.time_and_date BETWEEN",
					start_time,
					"AND",
					end_time,
					"ORDER BY feeder_stats.time_and_date ASC",
					# "LIMIT 10",
					";", sep=" ")
	# print(queryStr)
	print(paste("Starting query for feeder ", feeder_id, sep=''))
	queryRtn <- dbGetQuery(connection, queryStr)
	if(feeders_info$phase_type[feeder_id]!=0) queryRtn <- calc_power(queryRtn)
	
	queryRtn['min_of_day'] <- as.integer(format(queryRtn$time_and_date, "%H"))*60 + as.integer(format(queryRtn$time_and_date, "%M"))
	queryRtn['hour_of_day'] <- as.integer(format(queryRtn$time_and_date, "%H"))
	queryRtn['day_of_week'] <- as.integer(format(queryRtn$time_and_date, "%u"))
	queryRtn['day_of_month'] <- as.integer(format(queryRtn$time_and_date, "%e"))
	queryRtn['day_of_year'] <- as.integer(format(queryRtn$time_and_date, "%j"))
	
	queryRtn['current_thd_magnitude'] <- queryRtn$current * queryRtn$current_thd/100
	queryRtn['voltage_thd_magnitude'] <- queryRtn$voltage * queryRtn$voltage_thd/100
	
	voltages <- list(queryRtn$v1, queryRtn$v2, queryRtn$v3)
	V_mean <- unlist(lapply(data.table::transpose(voltages), mean))
	V_delta <- list(abs(voltages[[1]]-V_mean), abs(voltages[[2]]-V_mean), abs(voltages[[3]]-V_mean))
	V_max <- unlist(lapply(data.table::transpose(V_delta), max))
	imbalance <- 100 * (V_max) / V_mean
	problem_phase <- cbind(V_delta[[1]] == V_max, V_delta[[2]] == V_max, V_delta[[3]] == V_max)
	problem_phase <- (problem_phase %*% phase_ids)
	problem_phase[!(problem_phase %in% phase_ids)] <- NA # if more than one phase is a problem, discard that point	
	queryRtn <- cbind(queryRtn, imbalance, problem_phase)
  
	queryRtn <- calc_kwh(queryRtn)
	
	print("Finished query!")
	return(queryRtn)
}

#' Calculate daily statistics
#' 
#' Instead of a separate query, calculate the statistics inside R using dplyr and lubridate
#' @param data.frame data_df object
#' @return data.frame containing daily statistics data

calc_daily_stats <- function(data_df) {
	daily_stats <- data_df %>% 
		mutate(DateTime = time_and_date) %>% 
		group_by(daily = as.Date(as_date(time_and_date))) %>% 
	  summarise_all(.funs = list(
	    ~ max(., na.rm=TRUE), 
	    ~ mean(., na.rm=TRUE), 
	    ~ min(., na.rm=TRUE), 
	    ~ sd(., na.rm=TRUE)
	    )
	  )
	daily_stats$time_and_date <- floor_date(daily_stats$time_and_date_min, unit="day")
	# daily_stats <- clean_stats(daily_stats)
	
	return(daily_stats)
}

#' Calculate hourly statistics
#' 
#' Instead of a separate query, calculate the statistics inside R using dplyr and lubridate
#' @param data.frame data_df object
#' @return data.frame containing hourly statistics data

calc_hourly_stats <- function(data_df) {
	hourly_stats <- data_df %>% 
		mutate(DateTime = time_and_date) %>% 
		group_by(hour = floor_date(time_and_date, unit="hour")) %>% 
	  summarise_all(.funs = list(
	    ~ max(., na.rm=TRUE), 
	    ~ mean(., na.rm=TRUE), 
	    ~ min(., na.rm=TRUE), 
	    ~ sd(., na.rm=TRUE)
	  )
	)
	hourly_stats$time_and_date <- floor_date(hourly_stats$time_and_date_min, unit="hour")
	hourly_stats$hour_fac <- as.factor(hour(hourly_stats$hour))
	
	# hourly_stats <- clean_stats(hourly_stats)
	return(hourly_stats)
}

#' Clean hourly_stats names and columns
#' 
#' Throw out junk variables and give more sensible names
#' @param data.frame stats_df object
#' @return data.frame cleaned stats

clean_stats <- function(stats_df) {
  # drop_h <- c("time_and_date_max", "min_of_day_max", "hour_of_day_max", "problem_phase_max", "DateTime_max", "feeder_id_mean", "time_and_date_mean", "phase_id_mean", "id1_mean", "id2_mean", "id3_mean", "min_of_day_mean", "hour_of_day_mean", "day_of_week_mean", "DateTime_mean", "feeder_id_min", "phase_id_min", "id1_min", "id2_min", "id3_min", "min_of_day_min", "day_of_week_min", "problem_phase_min", "feeder_id_sd", "time_and_date_sd", "phase_id_sd", "id1_sd", "id2_sd", "id3_sd", "hour_of_day_sd", "day_of_week_sd", "problem_phase_sd", "DateTime_sd")
  keep_h <- c("hour", "time_and_date", "current_max", "real_power_max", "current_thd_max", "power_factor_max", "disp_power_factor_max", "true_power_factor_max", "voltage_max", "voltage_thd_max", "temperature_max", "frequency_max", "v1_max", "v2_max", "v3_max", "app_power_max", "app_power_t_max", "reac_power_max", "reac_power_t_max", "imbalance_max", "current_mean", "real_power_mean", "current_thd_mean", "power_factor_mean", "disp_power_factor_mean", "true_power_factor_mean", "voltage_mean", "voltage_thd_mean", "temperature_mean", "frequency_mean", "v1_mean", "v2_mean", "v3_mean", "app_power_mean", "app_power_t_mean", "reac_power_mean", "reac_power_t_mean", "imbalance_mean", "problem_phase_mean", "current_min", "real_power_min", "current_thd_min", "power_factor_min", "disp_power_factor_min", "true_power_factor_min", "voltage_min", "voltage_thd_min", "temperature_min", "frequency_min", "v1_min", "v2_min", "v3_min", "app_power_min", "app_power_t_min", "reac_power_min", "reac_power_t_min", "imbalance_min", "current_sd", "real_power_sd", "current_thd_sd", "power_factor_sd", "disp_power_factor_sd", "true_power_factor_sd", "voltage_sd", "voltage_thd_sd", "temperature_sd", "frequency_sd", "v1_sd", "v2_sd", "v3_sd", "app_power_sd", "app_power_t_sd", "reac_power_sd", "reac_power_t_sd", "min_of_day_sd", "imbalance_sd", "hour_fac")
  old_h <- c("feeder_id_max", "phase_id_max", "id1_max", "id2_max", "id3_max", "day_of_week_max", "hour_of_day_min", "DateTime_min")
  new_h <- c("feeder_id", "phase_id", "id1", "id2", "id3", "day_of_week", "hour_of_day", "DateTime")
  # drop_d <- c("min_of_day_max", "hour_of_day_max", "problem_phase_max", "feeder_id_mean", "time_and_date_mean", "phase_id_mean", "id1_mean", "id2_mean", "id3_mean", "min_of_day_mean", "hour_of_day_mean", "day_of_week_mean", "DateTime_mean", "feeder_id_min", "time_and_date_min", "phase_id_min", "id1_min", "id2_min", "id3_min", "min_of_day_min", "hour_of_day_min", "day_of_week_min", "problem_phase_min", "DateTime_min", "feeder_id_sd", "time_and_date_sd", "phase_id_sd", "id1_sd", "id2_sd", "id3_sd", "day_of_week_sd", "DateTime_sd")
  keep_d <- c("daily", "time_and_date", "current_max", "real_power_max", "current_thd_max", "power_factor_max", "disp_power_factor_max", "true_power_factor_max", "voltage_max", "voltage_thd_max", "temperature_max", "frequency_max", "v1_max", "v2_max", "v3_max", "app_power_max", "app_power_t_max", "reac_power_max", "reac_power_t_max", "imbalance_max", "current_mean", "real_power_mean", "current_thd_mean", "power_factor_mean", "disp_power_factor_mean", "true_power_factor_mean", "voltage_mean", "voltage_thd_mean", "temperature_mean", "frequency_mean", "v1_mean", "v2_mean", "v3_mean", "app_power_mean", "app_power_t_mean", "reac_power_mean", "reac_power_t_mean", "imbalance_mean", "problem_phase_mean", "current_min", "real_power_min", "current_thd_min", "power_factor_min", "disp_power_factor_min", "true_power_factor_min", "voltage_min", "voltage_thd_min", "temperature_min", "frequency_min", "v1_min", "v2_min", "v3_min", "app_power_min", "app_power_t_min", "reac_power_min", "reac_power_t_min", "imbalance_min", "current_sd", "real_power_sd", "current_thd_sd", "power_factor_sd", "disp_power_factor_sd", "true_power_factor_sd", "voltage_sd", "voltage_thd_sd", "temperature_sd", "frequency_sd", "v1_sd", "v2_sd", "v3_sd", "app_power_sd", "app_power_t_sd", "reac_power_sd", "reac_power_t_sd", "min_of_day_sd", "hour_of_day_sd", "imbalance_sd", "problem_phase_sd")
  old_d <- c("feeder_id_max", "phase_id_max", "id1_max", "id2_max", "id3_max", "day_of_week_max", "DateTime_max")
  new_d <- c("feeder_id", "phase_id", "id1", "id2", "id3", "day_of_week", "DateTime")
  
  if (colnames(stats_df)[1] == "hour") {
    keep <- keep_h
    old <- old_h
    new <- new_h
  } else if(colnames(stats_df)[1] == "daily") {
    keep <- keep_d
    old <- old_d
    new <- new_d
  } else {
    print("Error, colnames(stats_df)[1] not recognised")
    print("usage: hourly_stats <- clean_stats(hourly_stats)")
    print("stats not cleaned")
    return(stats_df)
  }
  
  # more sensible naming
  for (i in 1:length(old)) {
    colnames(stats_df)[which(names(stats_df) == old[i])] <- new[i]
  }
  # throw out junk variables
  stats_df <- stats_df[c(keep, new)]
}

#' Calculate time statistics for an arbitrary time unit
#' 
#' Instead of a separate query, calculate the statistics inside R using dplyr and lubridate
#' @param data.frame feeder_data object
#' @param time string time unit e.g. "hour", "15 min"
#' @return data.frame containing time statistics data

calc_time_stats <- function(data_df, time="hour") {
	time_stats <- data_df %>% 
		mutate(DateTime = time_and_date) %>% 
		group_by(dt = floor_date(time_and_date, unit=time)) %>% 
	  summarise_all(.funs = list(
	    ~ max(., na.rm=TRUE), 
	    ~ mean(., na.rm=TRUE), 
	    ~ min(., na.rm=TRUE), 
	    ~ sd(., na.rm=TRUE)
	  )
	)

	if (time == "hour") {
		time_stats$hour_fac <- as.factor(hour(time_stats$dt))
	}
	# throw out min and mean cumulative sum kWh, as these don't matter for cumsum - only max
	if (length(data_df$kwh_cs) != 0) {
	  time_stats$kwh_cs_mean <- time_stats$kwh_cs_max
  	time_stats$kwh_cs_min <- time_stats$kwh_cs_max
  }
	time_stats$time_and_date <- time_stats$dt
	return(time_stats)
}

#' Statistics by date
#' 
#' Query the database for summary data ordered by date. Min, average, max, standard deviation calculated by SQL
#' @param connection SQL connection object
#' @param character SQL parameter to summarize
#' @param int Feeder/phase/transformer id
#' @param character Start date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @param character End date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @return data.frame containing query data

get_date_stats <- function(connection, param="real_power", id=1, start_date=ymd("2017-07-09"), end_date=ymd("2017-07-10")) {
	sd <- paste0("'", start_date, "'")
	ed <- paste0("'", end_date, "'")
	params <- list(
		"current" = "feeder", 
		"real_power" = "feeder", 
		"feeder_stats.thd" = "feeder", 
		"power_factor" = "feeder", 
		"voltage" = "phase", 
		"phase_stats.thd" = "phase",
		"temperature" = "transformer",
		"frequency" = "transformer"
		)
	if (param %in% names(params) == FALSE) {
		stop("Parameter not recognised. Options: current, real_power, feeder_stats.thd, power_factor, voltage, phase_stats.thd, temperature, frequency")
	}
	id_str <- params[[param]]
	db_type <- paste0(id_str, "_stats")
	id_type <- paste0(id_str, "_id")

	queryStr <- paste("SELECT",
		"CAST(time_and_date as date) AS dt, ", 
		"MIN(", param, ") AS min,",
		"AVG(", param, ") AS avg,",
		"MAX(", param, ") AS max,",
		"STDDEV(", param, ") AS std",
		"FROM", db_type,
		"WHERE", id_type, "=", id,
		"AND time_and_date BETWEEN",
		"DATE(", sd, ")",
		"AND",
		"DATE(", ed, ")",
		"GROUP BY CAST(time_and_date as date)",
		"ORDER BY CAST(time_and_date as date) ASC",
		";",
		sep=" ")
	queryRtn <- dbGetQuery(connection, queryStr)
	queryRtn['date'] <- start_date
	return(queryRtn)
}

#' Hourly statistics
#' 
#' Query the database for summary data ordered by hour. Min, average, max, standard deviation calculated by SQL
#' @param connection SQL connection object
#' @param character SQL parameter to summarize
#' @param int Feeder/phase/transformer id
#' @param character Start date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @param character End date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @return data.frame containing query data

get_hourly_stats <- function(connection, param="real_power", id=1, start_date=ymd("2017-07-09"), end_date=ymd("2017-07-10")) {
	sd <- paste0("'", start_date, " 00:00:00'")
	ed <- paste0("'", end_date, " 00:00:00'")
	params <- list(
		"current" = "feeder", 
		"real_power" = "feeder", 
		"feeder_stats.thd" = "feeder", 
		"power_factor" = "feeder", 
		"voltage" = "phase", 
		"phase_stats.thd" = "phase",
		"temperature" = "transformer",
		"frequency" = "transformer"
		)
	if (param %in% names(params) == FALSE) {
		stop("Parameter not recognised. Options: current, real_power, feeder_stats.thd, power_factor, voltage, phase_stats.thd, temperature, frequency")
	}
	id_str <- params[[param]]
	if (param == "feeder_stats.thd" || param == "phase_stats.thd") {param <- "thd"}
	db_type <- paste0(id_str, "_stats")
	id_type <- paste0(id_str, "_id")

	queryStr <- paste(
		"SELECT t2.dt, t2.hour, ",
		"MIN(", paste0("t2.", param), ") AS min,",
		"AVG(", paste0("t2.", param), ") AS avg,",
		"MAX(", paste0("t2.", param), ") AS max",
		"FROM",
		"(",
		  "SELECT CAST(CAST(t1.time_and_date as time) as varchar(2)) || ':00:00' as hour, ", paste0("t1.", param), " as ", param, ", CAST(t1.time_and_date as date) as dt",
		  "FROM ",
		  db_type, "AS t1",
		  "WHERE t1.time_and_date BETWEEN ", sd, "AND", ed,
		  "AND t1.", id_type," = ", id,
		") t2",
		"GROUP BY t2.dt, t2.hour",
		"ORDER BY t2.dt, t2.hour ASC;",
		sep=" ")

	queryRtn <- dbGetQuery(connection, queryStr)
	# queryRtn['date'] <- start_date
	return(queryRtn)
}

#' Hourly power statistics
#' 
#' Query the database for summary data ordered by hour. Min, average, max, standard deviation calculated by SQL
#' @param connection SQL connection object
#' @param character SQL parameter to summarize
#' @param int Feeder/phase/transformer id
#' @param character Start date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @param character End date for query. Use lubridate's ymd("YYYY-MM-DD") function for dates
#' @return data.frame containing query data

get_hourly_power_stats <- function(connection, param="real_power", id=1, start_date=ymd("2017-07-09"), end_date=ymd("2017-07-10"), discard_negatives=TRUE) {
	sd <- paste0("'", start_date, " 00:00:00'")
	ed <- paste0("'", end_date, " 00:00:00'")
	params <- list(
		"real_power" = "feeder"
		)
	if (param %in% names(params) == FALSE) {
		stop("Parameter not recognised. Options: real_power")
	}
	id_str <- params[[param]]
	if (param == "feeder_stats.thd" || param == "phase_stats.thd") {param <- "thd"}
	db_type <- paste0(id_str, "_stats")
	id_type <- paste0(id_str, "_id")

	queryStr <- paste(
		"SELECT t2.dt, t2.hour, ",
		"MIN(", paste0("t2.", "real_power"), ") AS minReal,",
		"AVG(", paste0("t2.", "real_power"), ") AS avgReal,",
		"MAX(", paste0("t2.", "real_power"), ") AS maxReal,",
		"MIN(", paste0("t2.", "power_factor"), ") AS minPF,",
		"AVG(", paste0("t2.", "power_factor"), ") AS avgPF,",
		"MAX(", paste0("t2.", "power_factor"), ") AS maxPF",
		"FROM",
		"(",
		  "SELECT CAST(CAST(t1.time_and_date as time) as varchar(2)) || ':00:00' as hour, t1.real_power as real_power, t1.power_factor as power_factor, CAST(t1.time_and_date as date) as dt",
		  "FROM ",
		  db_type, "AS t1",
		  "WHERE t1.time_and_date BETWEEN ", sd, "AND", ed,
		  "AND t1.", id_type," = ", id,
		") t2",
		"GROUP BY t2.dt, t2.hour",
		"ORDER BY t2.dt, t2.hour ASC;",
		sep=" ")

	queryRtn <- dbGetQuery(connection, queryStr)
	if(discard_negatives) {
		queryRtn$minreal <- abs(queryRtn$minreal)
		queryRtn$avgreal <- abs(queryRtn$avgreal)
		queryRtn$maxreal <- abs(queryRtn$maxreal)	
	}
	queryRtn$minapp <- queryRtn$minreal/queryRtn$minpf
	queryRtn$avgapp <- queryRtn$avgreal/queryRtn$avgpf
	queryRtn$maxapp <- queryRtn$maxreal/queryRtn$maxpf

	queryRtn$minreac <- sqrt(queryRtn$minapp^2 - queryRtn$minreal^2)
	queryRtn$avgreac <- sqrt(queryRtn$avgapp^2 - queryRtn$avgreal^2)
	queryRtn$maxreac <- sqrt(queryRtn$maxapp^2 - queryRtn$maxreal^2)
	return(queryRtn)
}

#' Get voltage imbalance
#' 
#' Get voltage imbalance for a specific trafo - needs input from all phases on that trafo
#' @param connection SQL connection object
#' @param int Feeder id - get the trafo from feeders_info
#' @param character Start time for query. Make sure to include single quotes around string
#' @param character End time for query. Make sure to include single quotes around string
#' @return data.frame containing query data

get_voltage_imbalance <- function(connection, feeder_id=1, start_time="'2017-06-16 11:00:00'", end_time="'2017-06-24 13:00:00'") {
	# if(feeders$phase_type[feeder_id] == 0){
	# 	dbDisconnect(con)
	# 	print("Neutral feeders are boring")
	# 	return(NULL)
	# }
	feeders_info <- get_feeders(connection)
	trafo_id <- feeders_info$transformer[feeder_id]
	trafo_feeders <- feeders_info[which(feeders_info$transformer == trafo_id),]
	# throw out phase_type=0 since there are only 3 phases
	phase_ids <- unique(trafo_feeders$phase[which(trafo_feeders$phase_type != 0)])
	phase_ids <- sort(phase_ids)
	queryStr <- paste("SELECT",
					"p1.time_and_date AS time_and_date,", 
					"p1.phase_id AS id1,", 
					"p1.voltage AS v1,", 
					"p2.phase_id AS id2,", 
					"p2.voltage AS v2,", 
					"p3.phase_id AS id3,", 
					"p3.voltage AS v3",
					"FROM phase_stats AS p1",
					"JOIN phase_stats AS p2 ON p2.time_and_date = p1.time_and_date",
					"JOIN phase_stats AS p3 ON p3.time_and_date = p1.time_and_date",
					"WHERE p1.phase_id =", phase_ids[1],
					"AND p2.phase_id =", phase_ids[2],
					"AND p3.phase_id =", phase_ids[3],
					"AND p1.time_and_date BETWEEN",
					start_time,
					"AND",
					end_time,
					"ORDER BY p1.time_and_date ASC",
					";", sep=" ")
	print(paste("Starting imbalance query for transformer ", trafo_id, sep=''))
	queryRtn <- dbGetQuery(connection, queryStr)
	voltages <- list(queryRtn$v1, queryRtn$v2, queryRtn$v3)
	V_mean <- unlist(lapply(data.table::transpose(voltages), mean))
	V_delta <- list(abs(voltages[[1]]-V_mean), abs(voltages[[2]]-V_mean), abs(voltages[[3]]-V_mean))
	# V_max <- unlist(lapply(data.table::transpose(voltages), max))  # wrong!
	V_max <- unlist(lapply(data.table::transpose(V_delta), max))
	imbalance <- 100 * (V_max) / V_mean
	problem_phase <- cbind(V_delta[[1]] == V_max, V_delta[[2]] == V_max, V_delta[[3]] == V_max)
	problem_phase <- (problem_phase %*% phase_ids)
	problem_phase[!(problem_phase %in% phase_ids)] <- NA # if more than one phase is a problem, discard that point
	
	queryRtn <- cbind(queryRtn, imbalance, problem_phase)
	print("Finished imbalance query!")
	return(queryRtn)
}

#' Calculate power
#' 
#' Calculate reactive and apparent power from real power and power factor
#' @param data.frame Feeder data with real_power and power_factor cols
#' @param bool discard_negatives If TRUE, take absolute value of real_power
#' @return data.frame Feeder data with new cols reac_power and app_power
calc_power <- function(df, discard_negatives=TRUE) {
  if(length(df$real_power) == 0) return(df)
	if(discard_negatives) df$real_power <- abs(df$real_power)

	df$app_power <- df$real_power/df$power_factor
	df$app_power_t <- df$real_power/df$true_power_factor
	# screen out cases where Real=0 results in App=Inf or NaN
	# df$app_power[!is.finite(df$app_power)] <- 0.1

	df$reac_power <- sqrt(df$app_power^2 - df$real_power^2)
	df$reac_power_t <- sqrt(df$app_power_t^2 - df$real_power^2)
	# probably unnecessary, but just in case, do same for Reac
	# df$reac_power[!is.finite(df$reac_power)] <- 0
	return(df)
}

#' Calculate kilowatt-hours
#' 
#' Calculate energy used in the form of kilowatt-hours kWh
#' @param data.frame Feeder data with real_power and power_factor cols
#' @param bool discard_negatives If TRUE, take absolute value of real_power
#' @return data.frame Feeder data with new cols reac_power and app_power
calc_kwh <- function(df, discard_negatives=TRUE) {
  # if(discard_negatives) df$real_power <- abs(df$real_power)
  
  td <- data.frame(df$time_and_date[1:(length(df$time_and_date)-1)])
  colnames(td) <- 'shifted_time'
  td0 <- data.frame(floor_date(td$shifted_time[1], 'day'))
  colnames(td0) <- 'shifted_time'
  td <- rbind(td0, td)
  df <- cbind(df, td)
  df$kwh <- df$real_power/1000 * as.numeric(seconds(df$time_and_date - df$shifted_time))/3600
  df <- data.frame(df %>% group_by(day_of_year) %>% arrange(time_and_date) %>% mutate(kwh_cs = cumsum(kwh)))
  return(df)
}

#' Check if new requests include cached data
#' 
#' First check to see if there is any previous data, then compare its shape to the new request, and optimise the request
#' @param session session Shiny UI session object
#' @param data.frame feeder_data New feeder data d$feeder_data
#' @param data.frame stored_data Old feeder data d$stored_data
#' @param data.frame feeders Feeder info d$feeders
#' @param int feederNumber Feeder number input$feederNumber
#' @return data.frame Feeder data
cache_data <- function(session, feeder_data, stored_data, feeders, feederNumber) {
  if(length(feeder_data) > 0) {
    # decide if the query is new data or a subset of data already queried
    if(length(stored_data) == 0){
      stored_data <- feeder_data
    } else if(length(feeder_data$time_and_date) > length(stored_data$time_and_date)){
      stored_data <- feeder_data
    }
    sd1 <- min(stored_data$time_and_date)
    ed1 <- max(stored_data$time_and_date)
    sd2 <- ymd_hms(start_time)
    ed2 <- ymd_hms(end_time)
    
    if(timeClose(sd2, sd1) && timeClose(ed2, ed1)){
      # no need to do new query, just return previous results
      # print("identical")
      return_data <- stored_data
    } else if((sd2 > ed1 && !timeClose(sd2, ed1)) || (ed2 < sd1 && !timeClose(ed2, sd1))){
      # discard old data, perform a new query
      # gets way too complicated otherwise!!
      # print("noncontiguous")
      stored_data <- data.frame()
      return_data <- get_data(con, feeders, as.integer(feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
      stored_data <- return_data
    } else if(((sd2 >= sd1 || timeClose(sd2, sd1)) && (ed2 <= ed1 || timeClose(ed2, ed1)))){
      # return subset between sd2 and ed2
      # print("inside")
      return_data <- stored_data[which(
        ymd_hms(stored_data$time_and_date) >= sd2 & 
          ymd_hms(stored_data$time_and_date) <= ed2),]
    } else if(sd2 < sd1 && (ed2 <= ed1 || timeClose(ed1, ed2)) && (ed2 >= sd1 || timeClose(sd1, ed2)) ){
      # perform new query between sd2 and sd1
      # merge new query with previous results
      # return subset between sd2 and ed2
      # print("leftside")
      new_data <- get_data(con, feeders, as.integer(feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(sd1, "'%Y-%m-%d %H:%M:%S'"))
      stored_data <- rbind(new_data, stored_data)
      return_data <- stored_data[which(
        ymd_hms(stored_data$time_and_date) >= sd2 & 
          ymd_hms(stored_data$time_and_date) <= ed2),]
    } else if(ed2 > ed1 && (sd2 >= sd1 || timeClose(sd1, sd2)) && (sd2 <= ed1 || timeClose(ed1, sd2))){
      # perform new query between ed1 and ed2
      # merge new query with previous results
      # return subset between sd2 and ed2
      # print("rightside")
      new_data <- get_data(con, feeders, as.integer(feederNumber), format(ed1, "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
      stored_data <- rbind(stored_data, new_data)
      return_data <- stored_data[which(
        ymd_hms(stored_data$time_and_date) >= sd2 & 
          ymd_hms(stored_data$time_and_date) <= ed2),]
    } else if(sd2 < sd1 && ed2 > ed1 && !timeClose(sd1, sd2) && !timeClose(ed1, ed2)){
      # perform new query between sd2 and sd1 AND ed1 and ed2
      # merge new query with previous results
      # return subset between sd2 and ed2
      # print("outside")
      left_data <- get_data(con, feeders, as.integer(feederNumber), format(sd2, "'%Y-%m-%d %H:%M:%S'"), format(sd1, "'%Y-%m-%d %H:%M:%S'"))
      right_data <- get_data(con, feeders, as.integer(feederNumber), format(ed1, "'%Y-%m-%d %H:%M:%S'"), format(ed2, "'%Y-%m-%d %H:%M:%S'"))
      stored_data <- rbind(left_data, stored_data, right_data)
      return_data <- stored_data[which(
        ymd_hms(stored_data$time_and_date) >= sd2 & 
          ymd_hms(stored_data$time_and_date) <= ed2),]
    } else {
      # Not sure how it would get to here, so just print the times and return the old data
      print("???")
      print("old times")
      print(c(sd1, ed1))
      print("new times")
      print(c(sd2, ed2))
      print("???")
      print("")
      return_data <- stored_data
    }
    feeder_data <- return_data
    updateSliderInput(session, "dateRangeExtents",
                      value=c(as.Date(ymd_hms(min(stored_data$time_and_date))),
                              as.Date(ymd_hms(max(stored_data$time_and_date))))
    )
  }else{
    feeder_data <- get_data(con, feeders, as.integer(feederNumber), start_time, end_time)
  }
}

#' Default origin for POSIXlt dates
#' 
#' hack for correct colour date labels
#' @param x POSIXlt date as integer

as.POSIXlt_origin <- function(x){
	# hack for correct colour date labels
	as.POSIXlt(x, origin = '1970-01-01')
}

#' Plot 1
#' 
#' power factor vs real power
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_1 <- function(feeder_id, df) {
	# power factor vs real power
	p <- ggplot(df,
		aes(
			df$real_power/1000.0, 
			df$power_factor,
			colour=df$min_of_day
			)
		) + 
		geom_point(alpha = 0.5) +
		scale_colour_gradientn(colours=c('red', 'green', 'blue'))
	p <- setup_plot(p, feeder_id)
	return(p)
}

#' Plot 2
#' 
#' current vs time
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_2 <- function(feeder_id, df) {
	# current vs time
	p <- ggplot(df,
		aes(
			df$time_and_date, 
			df$current,
			colour=df$power_factor
			)
		) + 
		geom_point(alpha = 0.5) +
		scale_colour_gradientn(colours=c('red', 'green', 'blue'))
	p <- setup_plot(p, feeder_id, smooth=TRUE)
	return(p)
}

#' Plot 3
#' 
#' voltage vs time
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_3 <- function(feeder_id, df) {
	# voltage vs time
	p <- ggplot(df,
		aes(
			df$time_and_date, 
			df$voltage,
			colour=df$power_factor
			)
		) + 
		geom_point(alpha = 0.5) +
		scale_colour_gradientn(colours=c('red', 'green', 'blue'))
	p <- setup_plot(p, feeder_id, smooth=TRUE)
	return(p)
}

#' Plot 4
#' 
#' voltage vs current
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_4 <- function(feeder_id, df) {
	# voltage vs current
	p <- ggplot(df,
		aes(
			df$current, 
			df$voltage,
			colour=df$power_factor
			)
		) + 
		geom_point(alpha = 0.5) +
		scale_colour_gradientn(colours=c('red', 'green', 'blue'))
	p <- setup_plot(p, feeder_id, smooth=FALSE)
	return(p)
}

#' Plot 5
#' 
#' voltage_thd vs current_thd
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_5 <- function(feeder_id, df) {
	# voltage_thd vs current_thd
	p <- ggplot(df,
		aes(
			df$current_thd, 
			df$voltage_thd,
			colour=df$power_factor
			)
		) + 
		geom_point(alpha = 0.5) +
		scale_colour_gradientn(colours=c('red', 'green', 'blue'))
	if (max(df$current_thd) == 64) p <- p + xlim(0, 63.9)
	if (max(df$voltage_thd) == 64) p <- p + ylim(0, 63.9)
	p <- setup_plot(p, feeder_id, smooth=FALSE)
	return(p)
}

#' Plot 6
#' 
#' current histogram
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_6 <- function(feeder_id, df) {
	# current histogram
	p <- ggplot(df,
		aes(
			df$current,
			y=..ncount..*100 -> ncount,
			fill=..density..
			)
		) + 
		stat_bin(bins=20) +
		scale_x_continuous(breaks = scales::pretty_breaks(n=10))
	p <- setup_plot(p, feeder_id, smooth=FALSE)
	return(p)
}

#' Plot 7
#' 
#' real & reactive power vs time
#' @param int Feeder id
#' @param data.frame Containing feeder data
#' @return ggplot object

plot_7 <- function(feeder_id, df) {
	# real & reactive power vs time
	p <- ggplot(df, aes(df$time_and_date))
	p <- p + geom_point(aes(y = df$real_power), colour = "pink", alpha = 0.1)
	p <- p + geom_point(aes(y = df$reac_power), colour = "lightgreen", alpha = 0.1)
	p <- p + geom_point(aes(y = df$app_power), colour = "lightblue", alpha = 0.1)
	p <- p + geom_smooth(method='loess', span=0.05, aes(y = df$app_power, colour = "Apparent"))
	p <- p + geom_smooth(method='loess', span=0.05, aes(y = df$real_power, colour = "Real"))
	p <- p + geom_smooth(method='loess', span=0.05, aes(y = df$reac_power, colour = "Reactive"))
	p <- p + scale_colour_manual(name="", values=c("blue", "green", "red"))
	p <- p + theme_gdocs(base_size=12)
	return(p)
}

#' Setup plot
#' 
#' Common formatting applied to all plots
#' @param ggplot plot object
#' @param int feeder id
#' @param bool apply smoothing =FALSE
#' @return ggplot object

setup_plot <- function(p, feeder_id, smooth=FALSE){
	# applied to all plots!
	if(smooth) p <- p + geom_smooth(method='loess', span=0.05, level=0.99999)
	p <- p + theme_gdocs(base_size=12)
	p <- p + labs(colour="")
	p <- p + theme(legend.position = "none")
	# p <- p + theme(plot.title = element_text(hjust=0.5, size=12))

	# ggsave(filename=paste(feeder_id, ".pdf", sep=""), plot=p)
	return(p)
}

#' Plot choose
#' 
#' Choose which plot to output. Applies specific labels, limits, etc. for each plot type. Also does some common setup.
#' @param int feeder id
#' @param int plot type
#' @param data.frame feeder data
#' @return ggplot plot object

plot_choose <- function(feeder_id, type=1, df) {
	# df <- get_data(con, feeder_id)
	load_factor <- mean(df$current)/max(df$current)*100
	d_start <- df$time_and_date[1]
	d_end <- df$time_and_date[length(df$time_and_date)]

	if (type == 1) {
		# power_factor vs real_power
		p <- plot_1(feeder_id, df)
		p <- p + ylim(0.5,1.0)
		p <- p + xlab("Real Power (kW)") + ylab("Power Factor")
	}
	else if (type == 2) {
		# current vs time
		p <- plot_2(feeder_id, df)
		p <- p + xlab("") + ylab("Current (A)")
	}
	else if (type == 3) {
		# voltage vs time
		p <- plot_3(feeder_id, df)
		p <- p + xlab("") + ylab("Voltage (V)")
	}
	else if (type == 4) {
		# voltage vs current
		p <- plot_4(feeder_id, df)
		p <- p + xlab("Current (A)") + ylab("Voltage (V)")
	}
	else if (type == 5) {
		# voltage vs current
		p <- plot_5(feeder_id, df)
		p <- p + xlab("Current THD (%)") + ylab("Voltage THD (%)")
	}
	else if (type == 6) {
		# current histogram
		p <- plot_6(feeder_id, df)
		p <- p + xlab("Current (A)") + ylab("Count (%)")
	}
	else if (type == 7) {
		# real, apparent and reactive power
		p <- plot_7(feeder_id, df)
		p <- p + xlab("") + ylab("Power (W, VA, VAr)")
	}
	else {
		stop("Choose plot type!")
	}
	p <- p + ggtitle(paste(
		"Feeder ", feeders$id[feeder_id], " ",
		"Phase ", feeders$phase_type[feeder_id], " ",
		"Transformer ", feeders$transformer[feeder_id], "\n",
		"Load factor = ", round(load_factor, 1),'%', sep=""), 
		subtitle=paste(format(d_start, "%d/%m/%y %H:%M"), "\u2013", format(d_end, "%d/%m/%y %H:%M"), sep=" ")
		)

	return(p)
}
