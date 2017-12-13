list.of.packages <- c(
	"shiny", 
	"ggplot2", 
	"lubridate", 
	"ggthemes", 
	"devtools", 
	"ggTimeSeries", 
	"DT", 
	"RPostgreSQL", 
	"ggpmisc",
	"dplyr",
	"ggalt"
	)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages[!new.packages=="ggTimeSeries"])
if('ggTimeSeries' %in% new.packages) devtools::install_github('Ather-Energy/ggTimeSeries')
if('ggalt' %in% new.packages) devtools::install_github("hrbrmstr/ggalt")

print("Done! Feel free to use shiny::runApp('.') now.")
