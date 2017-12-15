# devtools::use_package("RPostgreSQL")
# devtools::use_package("ggplot2")
# devtools::use_package("ggthemes")
# devtools::use_package("lubridate")
# devtools::use_package("dplyr")
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

#' Start SQL
#' 
#' Open a connection to the TRANSGLOBAL transformer DB
#' @param character db='local' for tunneled connection, anything else for direct connection
#' @return connection object

start_sql <- function(db='local') {
	# open connection
	drv <- dbDriver("PostgreSQL")
	pw <- "O$V*&Iw7dfp03x"
	if(db == 'local') {
		connection <- dbConnect(drv, dbname = "transformer_db", host = "localhost", port = 9000, user = "barry_read", password = pw)
	}
	else {
		connection <- dbConnect(drv, dbname = "transformer_db", host = "transglobal.cloud.tilaa.com", port = 5432, user = "barry_read", password = pw)
	}
	rm(pw) # removes the password
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
#' @param int Feeder id
#' @param character Start time for query. Make sure to include single quotes around string
#' @param character End time for query. Make sure to include single quotes around string
#' @return data.frame containing query data

get_data <- function(connection, feeder_id=1, start_time="'2017-06-16 11:00:00'", end_time="'2017-06-24 13:00:00'", imbal=TRUE) {
	# if(feeders$phase_type[feeder_id] == 0){
	# 	dbDisconnect(con)
	# 	print("Neutral feeders are boring")
	# 	return(NULL)
	# }
	feeders_info <- get_feeders(connection)
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
					"transformer_stats.frequency AS frequency",
					"FROM feeder_stats",
					"JOIN phase_stats ON phase_stats.time_and_date = feeder_stats.time_and_date",
					"JOIN transformer_stats ON transformer_stats.time_and_date = feeder_stats.time_and_date",
					"WHERE feeder_stats.feeder_id =", feeders_info$id[feeder_id],
					"AND phase_stats.phase_id =", feeders_info$phase[feeder_id],
					"AND transformer_stats.transformer_id = ", feeders_info$transformer[feeder_id],
					"AND feeder_stats.time_and_date BETWEEN",
					start_time,
					"AND",
					end_time,
					"ORDER BY feeder_stats.time_and_date ASC",
					# "LIMIT 10",
					";", sep=" ")
	print(paste("Starting query for feeder ", feeder_id, sep=''))
	queryRtn <- dbGetQuery(connection, queryStr)
	if(feeders_info$phase_type[feeder_id]!=0) queryRtn <- calc_power(queryRtn)
	queryRtn['min_of_day'] <- as.integer(format(queryRtn$time_and_date, "%H"))*60 + as.integer(format(queryRtn$time_and_date, "%M"))
	queryRtn['hour_of_day'] <- as.integer(format(queryRtn$time_and_date, "%H"))
	queryRtn['day_of_week'] <- as.integer(format(queryRtn$time_and_date, "%u"))
	if(imbal) {
		imbalance <- get_voltage_imbalance(connection, feeder_id, start_time, end_time)
		queryRtn <- merge(queryRtn, imbalance)  # join data frames on time_and_date
	}
	print("Finished query!")
	return(queryRtn)
}

#' Calculate daily statistics
#' 
#' Instead of a separate query, calculate the statistics inside R using dplyr and lubridate
#' @param data.frame feeder_data object
#' @return data.frame containing daily statistics data

calc_daily_stats <- function(data_df) {
	daily_stats <- data_df %>% 
		mutate(DateTime = time_and_date) %>% 
		group_by(daily = as.Date(as_date(time_and_date))) %>% 
		summarise_all(funs(
			max=max(., na.rm=TRUE),
			mean=mean(., na.rm=TRUE),
			min=min(., na.rm=TRUE),
			sd=sd(., na.rm=TRUE)
			)
		)
	return(daily_stats)
}

#' Calculate hourly statistics
#' 
#' Instead of a separate query, calculate the statistics inside R using dplyr and lubridate
#' @param data.frame feeder_data object
#' @return data.frame containing hourly statistics data

calc_hourly_stats <- function(data_df) {
	hourly_stats <- data_df %>% 
		mutate(DateTime = time_and_date) %>% 
		group_by(hour = floor_date(time_and_date, unit="hour")) %>% 
		summarise_all(funs(
			max=max(., na.rm=TRUE),
			mean=mean(., na.rm=TRUE),
			min=min(., na.rm=TRUE),
			sd=sd(., na.rm=TRUE)
			)
		)
	hourly_stats$hour_fac <- as.factor(hour(hourly_stats$hour))
	return(hourly_stats)
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
