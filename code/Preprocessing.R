options(java.parameters = "-Xmx31g")


source("https://raw.githubusercontent.com/leonardosminfo/presentation/master/code/myFeature.R")

source("https://raw.githubusercontent.com/leonardosminfo/presentation/master/code/myPreprocessing.R")

source("https://raw.githubusercontent.com/leonardosminfo/presentation/master/code/myPrediction.R")



loadlibrary("parallelMap")
loadlibrary("parallel")
loadlibrary("dplyr")
loadlibrary("caret")
loadlibrary("dplyr")
loadlibrary("tidyr")
loadlibrary("stringr")
loadlibrary("ggplot2")
loadlibrary("corrplot")
loadlibrary("VIM")
loadlibrary("lubridate")

load_vra <- function() {
  if(!file.exists("vra-wu-a.RData")){
    if(!file.exists("vra-wu.RData")){
      res <- tryCatch(download.file("http://eic.cefet-rj.br/~eogasawara/data/flight/vra-wu.RData",
                                    destfile="vra-wu.RData", mode="wb",
                                    method="auto"),
                      error=function(e) 1)
    }

    load("vra-wu.RData") 
    
    vra_wu <- adjust_fields(vra_wu)    
    
    save(vra_wu, file="vra-wu-a.RData")
  }
  load("vra-wu-a.RData") 
  return(list(vra_wu=vra_wu))
}

adjust_fields <- function(vra_wu) {
  vra_wu$departure_expect <- as.POSIXct(vra_wu$depart_expect)
  vra_wu$departure_time <- as.POSIXct(vra_wu$depart)
  vra_wu$departure_year <- year(vra_wu$departure_expect)
  vra_wu$departure_month <- month(vra_wu$departure_expect)
  vra_wu$departure_hour <- hour(vra_wu$departure_expect)
  vra_wu$departure_day <- day(vra_wu$departure_expect)
  vra_wu$departure_weekday = wday(vra_wu$departure_expect)
  vra_wu$departure_delay <- ifelse(vra_wu$departure_delay < 0, 0, vra_wu$departure_delay)
  vra_wu$departure <- vra_wu$origin

  vra_wu$arrival_expect <- as.POSIXct(vra_wu$arrival_expect)
  vra_wu$arrival_time <- as.POSIXct(vra_wu$arrival)
  vra_wu$arrival_delay <- ifelse(vra_wu$arrival_delay < 0, 0, vra_wu$arrival_delay)
  vra_wu$delayed = ifelse(vra_wu$arrival_delay>=15, 1, 0)
  vra_wu$arrival <- vra_wu$destiny

  vra_wu <- vra_wu %>%
    select(airline, flight, line_type, departure, departure_state = origin.state, departure_expect, departure_time, departure_delay,
           departure_year, departure_month, departure_hour, departure_day, departure_weekday,
           departure_temperature = depart_temperature, departure_dew_point = depart_dew_point, departure_humidity = depart_humidity, departure_pressure = depart_pressure, departure_visibility = depart_visibility, departure_events = depart_events, departure_conditions = depart_conditions,
           arrival, arrival_state = destiny.state, arrival_expect, arrival_time, arrival_delay, delayed,
           arrival_temperature, arrival_dew_point, arrival_humidity, arrival_pressure, arrival_visibility, arrival_events, arrival_conditions,
           duration_expect, duration, duration_delta, status, observation)    
  
  #toupper
  vra_wu$departure=trimws(toupper(as.character(vra_wu$departure)))
  vra_wu$arrival=trimws(toupper(as.character(vra_wu$arrival)))
  vra_wu$airline=trimws(toupper(as.character(vra_wu$airline)))
  vra_wu$flight=trimws(toupper(as.character(vra_wu$flight)))
  vra_wu$observation=trimws(toupper(as.character(vra_wu$observation)))
  vra_wu$departure_conditions=trimws(toupper(as.character(vra_wu$departure_conditions)))
  vra_wu$arrival_conditions=trimws(toupper(as.character(vra_wu$arrival_conditions)))
  vra_wu$departure_events=trimws(toupper(as.character(vra_wu$departure_events)))
  vra_wu$arrival_events=trimws(toupper(as.character(vra_wu$arrival_events)))
  vra_wu$status=trimws(toupper(as.character(vra_wu$status)))

  #EVENTS
  vra_wu$departure_events[is.na(vra_wu$departure_events)] <- ''
  vra_wu <- vra_wu %>% mutate(departure_events = ifelse(departure_events =="", "NONE", departure_events))
  vra_wu$arrival_events[is.na(vra_wu$arrival_events)] <- ''
  vra_wu <- vra_wu %>% mutate(arrival_events = ifelse(arrival_events =="", "NONE", arrival_events))
  
  #CONDITIONS
  vra_wu$departure_conditions[is.na(vra_wu$departure_conditions)] <- ''
  vra_wu <- vra_wu %>% mutate(departure_conditions = ifelse(departure_conditions =="", "NONE", departure_conditions))
  vra_wu$arrival_conditions[is.na(vra_wu$arrival_conditions)] <- ''
  vra_wu <- vra_wu %>% mutate(arrival_conditions = ifelse(arrival_conditions =="", "NONE", arrival_conditions))
  
  #OBSERVATIONS
  vra_wu <- vra_wu %>% mutate(observation = ifelse(observation =="", "NO", observation))

  #remove , \ space
  rspc=c("arrival_conditions","departure_conditions","arrival_events","departure_events")
  vra_wu[,rspc]=sapply(vra_wu[,rspc], function(x) {gsub("\\s*|\\,", "",  x)})
  vra_wu[,rspc]=sapply(vra_wu[,rspc], function(x) {gsub("\\s*|\\,", "",  x)})
  
  
  vra_wu$airline <- as.factor(vra_wu$airline)
  vra_wu$flight <- as.factor(vra_wu$flight)
  vra_wu$departure <- as.factor(vra_wu$departure)
  vra_wu$arrival <- as.factor(vra_wu$arrival)
  vra_wu$observation <- as.factor(vra_wu$observation)
  vra_wu$arrival <- as.factor(vra_wu$arrival)
  vra_wu$observation <- as.factor(vra_wu$observation)
  vra_wu$departure_events <- as.factor(vra_wu$departure_events)
  vra_wu$departure_conditions <- as.factor(vra_wu$departure_conditions)
  vra_wu$arrival_events <- as.factor(vra_wu$arrival_events)
  vra_wu$arrival_conditions <- as.factor(vra_wu$arrival_conditions)
  vra_wu$status <- as.factor(vra_wu$status)
  
  return(vra_wu)  
}

data_clean_by_domain <- function(vra_wu) {
  nrows <- nrow(vra_wu)
  clean <- rep(FALSE, nrows)
  report <- NULL
  
  cond <- vra_wu$status == "CANCELADO"
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="canceled flights", qtd = sum(cond, na.rm = TRUE)))
  
  
  cond <- vra_wu$departure_time > vra_wu$arrival_time
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="depature time occuring after arrival time", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$departure == vra_wu$arrival
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="depature equals arrival", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$duration < 0
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="negative flight duration", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$duration > 1440
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="flight duration greather than one day", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$departure_temperature > 57 & vra_wu$arrival_temperature > 57 & vra_wu$departure_temperature < -68 & vra_wu$arrival_temperature < -68
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="too low or too high temperature values", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$departure_dew_point>84 | vra_wu$arrival_dew_point>84
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="too high dew point", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$arrival_humidity>100 | vra_wu$departure_humidity>100 | vra_wu$arrival_humidity<0 | vra_wu$departure_humidity<0
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="invalid humidity range", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$arrival_pressure<860 | vra_wu$departure_pressure<860 | vra_wu$arrival_pressure>1084 | vra_wu$departure_pressure>1084
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="invalid preasure range", qtd = sum(cond, na.rm = TRUE)))
  
  cond <- vra_wu$arrival_visibility>100 | vra_wu$departure_visibility>100 | vra_wu$arrival_visibility<0 | vra_wu$departure_visibility<0
  cond[is.na(cond)] <- FALSE
  clean <- clean | cond
  report <- rbind(report, data.frame(condition="invalid visibility range", qtd = sum(cond, na.rm = TRUE)))
  
  vra_wu <- vra_wu[!clean, ]
  
  return (list(vra_wu=vra_wu, report=report))
}

filter_vra <- function(vra_wu) {
  vra_wu <- vra_wu %>%
    filter(departure_year %in% 2015:2017) %>%
    filter(!is.na(departure_state) & !is.na(arrival_state)) %>%
    filter(line_type == 'N') %>%
    filter((status == "REALIZADO"))  %>%
    select(airline, flight, departure, arrival, 
           departure_expect, departure_time, 
           departure_year, departure_month, departure_hour, departure_day, departure_weekday,
           departure_temperature, departure_dew_point, departure_humidity, departure_pressure, departure_visibility, departure_events, departure_conditions,
           delayed #, departure_delay, arrival_delay, 
           #arrival_expect, arrival_time, 
           #arrival_temperature, arrival_dew_point, arrival_humidity, arrival_pressure, arrival_visibility, arrival_events, arrival_conditions,
           #duration_expect, duration, duration_delta, 
           #observation
           )    
  
  vra_wu$departure=as.factor(as.character(vra_wu$departure))
  vra_wu$arrival=as.factor(as.character(vra_wu$arrival))
  vra_wu$airline=as.factor(as.character(vra_wu$airline))
  vra_wu$flight=as.factor(as.character(vra_wu$flight))
  vra_wu$departure_events=as.factor(as.character(vra_wu$departure_events))
  vra_wu$departure_conditions=as.factor(as.character(vra_wu$departure_conditions))
  
  return(list(vra_wu=vra_wu))
}


filter_vra2 <- function(vra_wu) {
  vra_wu <- vra_wu %>%
    filter(departure_year %in% 2015:2017) %>%
    filter(!is.na(departure_state) & !is.na(arrival_state)) %>%
    filter(line_type == 'N') %>%
    filter((status == "REALIZADO"))  %>%
    select(airline, flight, departure, arrival, 
           departure_expect, departure_time, 
           departure_year, departure_month, departure_hour, departure_day, departure_weekday,
           departure_temperature, departure_dew_point, departure_humidity, departure_pressure, departure_visibility, departure_events, departure_conditions,
           delayed, arrival_expect, arrival_time, 
           arrival_temperature, arrival_dew_point, arrival_humidity, arrival_pressure, arrival_visibility, arrival_events, arrival_conditions
           #duration_expect, duration, duration_delta, 
           #observation
    )    
  
  
  vra_wu$departure=as.factor(as.character(vra_wu$departure))
  vra_wu$arrival=as.factor(as.character(vra_wu$arrival))
  vra_wu$airline=as.factor(as.character(vra_wu$airline))
  vra_wu$flight=as.factor(as.character(vra_wu$flight))
  vra_wu$departure_events=as.factor(as.character(vra_wu$departure_events))
  vra_wu$departure_conditions=as.factor(as.character(vra_wu$departure_conditions))
  vra_wu$arrival_events=as.factor(as.character(vra_wu$arrival_events))
  vra_wu$arrival_conditions=as.factor(as.character(vra_wu$arrival_conditions))
  
  return(list(vra_wu=vra_wu))
}


data_missing <- function(vra_wu)
{
  glimpse(vra_wu)
  
  summary(vra_wu)
  
  vmissing_data <- as.data.frame(sort(sapply(vra_wu, function(x) sum(is.na(x))),decreasing = T))
  
  vmissing_data <- (vmissing_data/nrow(vra_wu))*100
  
  colnames(vmissing_data)[1] <- "missingvaluesPercentage"
  vmissing_data$features <- rownames(vmissing_data)
  
  grf <- ggplot(vmissing_data[vmissing_data$missingvaluesPercentage >0,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage, fill = features)) +
    geom_bar(stat="identity") +theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missing values") +
    xlab("Feature") + ggtitle("Understanding Missing Data")
  
  return(grf)
}


data_inputation <- function(vra_wu, remove_flags=TRUE) {
  grf <- data_missing(vra_wu)

  #HOT-DECK IMPUTING
  vra_wu=hotdeck(vra_wu,variable="departure_temperature",domain_var="departure",ord_var=c("departure_visibility", "departure_pressure", "departure_dew_point", "departure_humidity", "departure_conditions", "departure_events", "departure_month"))
  vra_wu=hotdeck(vra_wu,variable="departure_humidity",domain_var="departure",ord_var=c("departure_visibility", "departure_pressure", "departure_dew_point", "departure_temperature", "departure_conditions", "departure_events", "departure_month"))
  vra_wu=hotdeck(vra_wu,variable="departure_dew_point",domain_var="departure",ord_var=c("departure_visibility", "departure_pressure", "departure_humidity", "departure_temperature", "departure_conditions", "departure_events", "departure_month"))
  vra_wu=hotdeck(vra_wu,variable="departure_pressure",domain_var="departure",ord_var=c("departure_visibility", "departure_dew_point", "departure_humidity", "departure_temperature", "departure_conditions", "departure_events", "departure_month"))
  vra_wu=hotdeck(vra_wu,variable="departure_visibility",domain_var="departure",ord_var=c("departure_pressure", "departure_dew_point", "departure_humidity", "departure_temperature", "departure_conditions", "departure_events", "departure_month"))
  
  if (FALSE) {
    vra_wu=hotdeck(vra_wu,variable="arrival_temperature",domain_var="arrival",ord_var=c("arrival_visibility", "arrival_pressure", "arrival_dew_point", "arrival_humidity", "arrival_conditions", "arrival_events", "departure_month"))
    vra_wu=hotdeck(vra_wu,variable="arrival_humidity",domain_var="arrival",ord_var=c("arrival_visibility", "arrival_pressure", "arrival_dew_point", "arrival_temperature", "arrival_conditions", "arrival_events", "departure_month"))
    vra_wu=hotdeck(vra_wu,variable="arrival_dew_point",domain_var="arrival",ord_var=c("arrival_visibility", "arrival_pressure", "arrival_humidity", "arrival_temperature", "arrival_conditions", "arrival_events", "departure_month"))
    vra_wu=hotdeck(vra_wu,variable="arrival_pressure",domain_var="arrival",ord_var=c("arrival_visibility", "arrival_dew_point", "arrival_humidity", "arrival_temperature", "arrival_conditions", "arrival_events", "departure_month"))
    vra_wu=hotdeck(vra_wu,variable="arrival_visibility",domain_var="arrival",ord_var=c("arrival_pressure", "arrival_dew_point", "arrival_humidity", "arrival_temperature", "arrival_conditions", "arrival_events", "departure_month"))
  }  
  
  #GRAPHIC REPRESENTATION OF IMPUTING DATA PROCESS
  histMiss(vra_wu[,c("departure","departure_visibility","departure_visibility_imp")], delimiter="_imp", selection="all",only.miss=FALSE)
  if (FALSE) {
    histMiss(vra_wu[,c("arrival","arrival_visibility","arrival_visibility_imp")], delimiter="_imp", selection="all",only.miss=FALSE)
  }
  
  if (remove_flags) {
    vra_wu$departure_temperature_imp <- NULL
    vra_wu$departure_humidity_imp <- NULL
    vra_wu$departure_dew_point_imp <- NULL
    vra_wu$departure_pressure_imp <- NULL
    vra_wu$departure_visibility_imp <- NULL
    
    if (FALSE) {
      vra_wu$arrival_temperature_imp <- NULL
      vra_wu$arrival_humidity_imp <- NULL
      vra_wu$arrival_dew_point_imp <- NULL
      vra_wu$arrival_pressure_imp <- NULL
      vra_wu$arrival_visibility_imp <- NULL
    }    
  }
  
  #vra_wu = na.omit(vra_wu)
  return(list(vra_wu=vra_wu, missing_values=grf))
}

data_transform <- function(vra_wu){
  transf <- list()
  
  na_prior <- sum(is.na(vra_wu)) 
  
  ###############################################
  
  #BINNING
  
    #Depart Expected Hour/Date:
  transf$deh <- binning.opt(vra_wu$departure_hour, binning=binning.interval)
  vra_wu$departure_hour_bin <- transf$deh$bins_factor
  
  #Departure Temperature
  transf$dt <- binning.opt(vra_wu$departure_temperature, binning=binning.interval)
  vra_wu$departure_temperature_bin <- transf$dt$bins_factor
  
  #Departure Dew Point
  transf$ddp <- binning.opt(vra_wu$departure_dew_point, binning=binning.interval)
  vra_wu$departure_dew_point_bin <- transf$ddp$bins_factor
  
  #Departure Humidity
  transf$ddh <- binning.opt(vra_wu$departure_humidity, binning=binning.interval)
  vra_wu$departure_humidity_bin <- transf$ddh$bins_factor
  
  #Departure Pressure
  transf$dp <- binning.opt(vra_wu$departure_pressure, binning=binning.interval)
  vra_wu$arrival_humidity_bin <- transf$dp$bins_factor
  
  #Departure Visibility
  transf$dv <- binning.opt(vra_wu$departure_visibility, binning=binning.interval)
  vra_wu$departure_visibility_bin <- transf$dv$bins_factor
  
  if (FALSE) {
    #Arrival Temperature
    transf$at <- binning.opt(vra_wu$arrival_temperature, binning=binning.interval)
    vra_wu$arrival_temperature_bin <- transf$at$bins_factor
    
    #Arrival Dew Point
    transf$adp <- binning.opt(vra_wu$arrival_dew_point, binning=binning.interval)
    vra_wu$arrival_dew_point_bin <- transf$adp$bins_factor

    #Arrival Humidity
    transf$adh <- binning.opt(vra_wu$arrival_humidity, binning=binning.interval)
    vra_wu$arrival_humidity_bin <- transf$adh$bins_factor
    
    #Arrival Pressure
    transf$ap <- binning.opt(vra_wu$arrival_pressure, binning=binning.interval)
    vra_wu$arrival_pressure_bin <- transf$ap$bins_factor
    
    #Arrival Visibility
    transf$av <- binning.opt(vra_wu$arrival_visibility, binning=binning.interval)
    vra_wu$arrival_visibility_bin <- transf$av$bins_factor
  }
  
  
  ###############################################
  
  #CATEGORICAL MAPPING
  
  #Events:
  vra_wu=dt.categ_mapping(vra_wu,"departure_events")
  if (FALSE) {
    vra_wu=dt.categ_mapping(vra_wu,"arrival_events")
  }
  
  #Conditions:
  vra_wu=dt.categ_mapping(vra_wu,"departure_conditions")
  if (FALSE) {
    vra_wu=dt.categ_mapping(vra_wu,"arrival_conditions")
  }

  ###############EXTRA###############
  if(FALSE)
  {
  #BINNING
  #departure_month
  transf$deh <- binning.opt(vra_wu$departure_month, binning=binning.interval)
  vra_wu$departure_month_bin <- transf$deh$bins_factor
  
  #departure_day
  transf$deh <- binning.opt(vra_wu$departure_day, binning=binning.interval)
  vra_wu$departure_day_bin <- transf$deh$bins_factor
  
  #departure_weekday
  transf$deh <- binning.opt(vra_wu$departure_weekday, binning=binning.interval)
  vra_wu$departure_weekday_bin <- transf$deh$bins_factor
  }
  #CATEGORICAL MAPPING
  #Airline
  vra_wu=dt.categ_mapping(vra_wu,"airline")
  
  #Departure
  vra_wu=dt.categ_mapping(vra_wu,"departure")
  
  #Arrival
  vra_wu=dt.categ_mapping(vra_wu,"arrival")
  
  ###############################################
  
  na_after <- sum(is.na(vra_wu)) 
  
  return(list(vra_wu=vra_wu, transf=transf, na_prior = na_prior, na_after = na_after))
}

data_normalization <- function(vra_wu, method) {
  if (method=="minmax")
    normalization = normalize.minmax(vra_wu)
  else if (method=="zscore")
    normalization = normalize.zscore(vra_wu)
  return (list(vra_wu=normalization$data, norm.set=normalization$norm.set))  
}  


data_transform2 <- function(vra_wu){
  transf <- list()
  
  na_prior <- sum(is.na(vra_wu)) 
  
  ###############################################
  
  #BINNING
  
  #Depart Expected Hour/Date:
  transf$deh <- binning.opt(vra_wu$departure_hour, binning=binning.interval)
  vra_wu$departure_hour_bin <- transf$deh$bins_factor
  
  #Departure Temperature
  transf$dt <- binning.opt(vra_wu$departure_temperature, binning=binning.interval)
  vra_wu$departure_temperature_bin <- transf$dt$bins_factor
  
  #Departure Dew Point
  transf$ddp <- binning.opt(vra_wu$departure_dew_point, binning=binning.interval)
  vra_wu$departure_dew_point_bin <- transf$ddp$bins_factor
  
  #Departure Humidity
  transf$ddh <- binning.opt(vra_wu$departure_humidity, binning=binning.interval)
  vra_wu$departure_humidity_bin <- transf$ddh$bins_factor
  
  #Departure Pressure
  transf$dp <- binning.opt(vra_wu$departure_pressure, binning=binning.interval)
  vra_wu$arrival_humidity_bin <- transf$dp$bins_factor
  
  #Departure Visibility
  transf$dv <- binning.opt(vra_wu$departure_visibility, binning=binning.interval)
  vra_wu$departure_visibility_bin <- transf$dv$bins_factor
  
  if (TRUE) {
    
    #Arrival Expected Hour/Date:
    transf$aeh <- binning.opt(vra_wu$arrival_hour, binning=binning.interval)
    vra_wu$arrival_hour_bin <- transf$aeh$bins_factor
    
    #Arrival Temperature
    transf$at <- binning.opt(vra_wu$arrival_temperature, binning=binning.interval)
    vra_wu$arrival_temperature_bin <- transf$at$bins_factor
    
    #Arrival Dew Point
    transf$adp <- binning.opt(vra_wu$arrival_dew_point, binning=binning.interval)
    vra_wu$arrival_dew_point_bin <- transf$adp$bins_factor
    
    #Arrival Humidity
    transf$adh <- binning.opt(vra_wu$arrival_humidity, binning=binning.interval)
    vra_wu$arrival_humidity_bin <- transf$adh$bins_factor
    
    #Arrival Pressure
    transf$ap <- binning.opt(vra_wu$arrival_pressure, binning=binning.interval)
    vra_wu$arrival_pressure_bin <- transf$ap$bins_factor
    
    #Arrival Visibility
    transf$av <- binning.opt(vra_wu$arrival_visibility, binning=binning.interval)
    vra_wu$arrival_visibility_bin <- transf$av$bins_factor
  }
  
  
  ###############################################
  
  #CATEGORICAL MAPPING
  
  #Events:
  vra_wu=dt.categ_mapping(vra_wu,"departure_events")
  if (TRUE) {
    vra_wu=dt.categ_mapping(vra_wu,"arrival_events")
  }
  
  #Conditions:
  vra_wu=dt.categ_mapping(vra_wu,"departure_conditions")
  if (TRUE) {
    vra_wu=dt.categ_mapping(vra_wu,"arrival_conditions")
  }
  
  ###############EXTRA###############
  if(FALSE)
  {
    #BINNING
    #departure_month
    transf$deh <- binning.opt(vra_wu$departure_month, binning=binning.interval)
    vra_wu$departure_month_bin <- transf$deh$bins_factor
    
    #departure_day
    transf$deh <- binning.opt(vra_wu$departure_day, binning=binning.interval)
    vra_wu$departure_day_bin <- transf$deh$bins_factor
    
    #departure_weekday
    transf$deh <- binning.opt(vra_wu$departure_weekday, binning=binning.interval)
    vra_wu$departure_weekday_bin <- transf$deh$bins_factor
  }
  #CATEGORICAL MAPPING
  #Airline
  vra_wu=dt.categ_mapping(vra_wu,"airline")
  
  #Departure
  vra_wu=dt.categ_mapping(vra_wu,"departure")
  
  #Arrival
  vra_wu=dt.categ_mapping(vra_wu,"arrival")
  
  ###############################################
  
  na_after <- sum(is.na(vra_wu)) 
  
  return(list(vra_wu=vra_wu, transf=transf, na_prior = na_prior, na_after = na_after))
}

data_normalization <- function(vra_wu, method) {
  if (method=="minmax")
    normalization = normalize.minmax(vra_wu)
  else if (method=="zscore")
    normalization = normalize.zscore(vra_wu)
  return (list(vra_wu=normalization$data, norm.set=normalization$norm.set))  
}  


# DATA SAMPLE

data_sample <- function(vra_wu) {
  sample <- sample.stratified(data=vra_wu, clabel="delayed")  
  return(list(vra_wu_train=sample$sample, vra_wu_test=sample$residual))
}

# FEATURE SELECTION

data_feature <- function(vra_wu, method) {
  if (method == "lasso") 
    feature <- fs.lasso(vra_wu, "delayed")
  else if (method == "fs.fss") 
    feature <- fs.fss(vra_wu, "delayed")
  else if (method == "cfs")  
    feature <- fs.cfs(vra_wu, "delayed")
  else if (method == "ig")  
    feature <- fs.ig(vra_wu, "delayed")
  return(list(vra_wu=feature$data, features=feature$features))  
}

# DIMENSIONALITY REDUCTION

data_reduce_dim <- function(vra_wu, method="pca", transf = NULL) {
  if (method=="pca")
    reduce_dim <- dt.pca(vra_wu, "delayed", transf)
  return(list(vra_wu = reduce_dim$pca, transf=reduce_dim$transf))
}

# BALANCING DATASET

data_balancing <- function(vra_wu, method) {
  balancing <- NA
  if (method == "smote")
    balancing <- balance.oversampling(vra_wu, "delayed")  
  else if (method == "subsampling")
    balancing <- balance.subsampling(vra_wu, "delayed")  
  return(list(vra_wu = balancing))
}

dictionary <- function() {
  x <- NULL
  x <- rbind(x, data.frame(name="airline", type=1, description="basic"))
  x <- rbind(x, data.frame(name="flight", type=0, description="basic"))
  x <- rbind(x, data.frame(name="departure", type=1, description="basic"))
  x <- rbind(x, data.frame(name="arrival", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_expect", type=0, description="remove"))
  x <- rbind(x, data.frame(name="departure_time", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_year", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_month", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_day", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_weekday", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_hour", type=2, description="original"))
  x <- rbind(x, data.frame(name="departure_hour_bin", type=3, description="bin"))
  x <- rbind(x, data.frame(name="departure_temperature", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_dew_point", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_humidity", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_pressure", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_visibility", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_temperature_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_dew_point_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_humidity_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="arrival_humidity_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_visibility_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_events", type=6, description="original"))
  x <- rbind(x, data.frame(name="departure_eventsFRESHBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGENTLEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsHURRICANE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTAIR", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsMODERATEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNEARGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNONE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsVIOLENTSTORM", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditions", type=8, description="original"))
  x <- rbind(x, data.frame(name="departure_conditionsBLOWINGSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsCLEAR", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDUSTWHIRLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFUNNELCLOUD", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAZE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMIST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMOSTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsNONE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsOVERCAST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTIALFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPATCHESOFFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSANDSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSCATTEREDCLOUDS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSHALLOWFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSMOKE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOWTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSQUALLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHSMALLHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTORNADO", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsWIDESPREADDUST", type=9, description="cm"))
  
  x <- rbind(x, data.frame(name="delayed", type=10, description="target"))
  return(x)
  }

dictionary2 <- function() {
  x <- NULL
  x <- rbind(x, data.frame(name="airline", type=1, description="basic"))
  x <- rbind(x, data.frame(name="flight", type=0, description="basic"))
  x <- rbind(x, data.frame(name="departure", type=1, description="basic"))
  x <- rbind(x, data.frame(name="arrival", type=1, description="basic"))
  x <- rbind(x, data.frame(name="departure_expect", type=0, description="remove"))
  x <- rbind(x, data.frame(name="departure_time", type=12, description="basic"))
  x <- rbind(x, data.frame(name="departure_year", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_month", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_day", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_weekday", type=12, description="basic"))
  x <- rbind(x, data.frame(name="departure_weekday", type=15, description="basic"))
  x <- rbind(x, data.frame(name="departure_hour", type=2, description="original"))
  x <- rbind(x, data.frame(name="departure_hour_bin", type=3, description="bin"))
  x <- rbind(x, data.frame(name="departure_temperature", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_dew_point", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_humidity", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_pressure", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_visibility", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_temperature_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_dew_point_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_humidity_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="arrival_humidity_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_visibility_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_events", type=6, description="original"))
  x <- rbind(x, data.frame(name="departure_eventsFRESHBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGENTLEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsHURRICANE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTAIR", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsMODERATEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNEARGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNONE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsVIOLENTSTORM", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditions", type=8, description="original"))
  x <- rbind(x, data.frame(name="departure_conditionsBLOWINGSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsCLEAR", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDUSTWHIRLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFUNNELCLOUD", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAZE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMIST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMOSTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsNONE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsOVERCAST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTIALFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPATCHESOFFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSANDSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSCATTEREDCLOUDS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSHALLOWFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSMOKE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOWTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSQUALLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHSMALLHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTORNADO", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsWIDESPREADDUST", type=9, description="cm"))
  
  x <- rbind(x, data.frame(name="delayed", type=10, description="target"))
  ########
  
  
  #description="bin"
  #type=13
  #x <- rbind(x, data.frame(name="delayed", type=10, description="target"))
  
  column_name=colnames(sample_wn$vra_wu_train)
    for (i in 70:78) 
      {
      x <- rbind(x, data.frame(name=column_name[i], type=11, description="cm"))  
    }
  
  for (i in 79:130) 
  {
    x <- rbind(x, data.frame(name=column_name[i], type=13, description="cm"))  
  }
  
    for (i in 131:182) {
    x <- rbind(x, data.frame(name=column_name[i], type=14, description="cm"))  
    }
return(x)  
}

par_start <- function() {
  start_time <- Sys.time()
  
  set.seed(220)
  parallelStartSocket(cpus = (detectCores()-1))
  #parallelStop()
  
  return(start_time)
}

par_end <- function(start_time) {
  end_time <- Sys.time()
  #parallelStop()
  a=end_time - start_time
  print(a)
  return(as.double(a))
  }


measure_metrics_list<-function(predictions,time_elapsed,thresholdTest)
{
 
  #X0 <- ifelse(predictions[1] >= predictions[2], 1.0, 0.0)
  #X1 <- ifelse(predictions[2] > predictions[1], 1.0, 0.0)
  X0 <- ifelse(predictions[1] >= 0.5, 1.0, 0.0)
  X1 <- ifelse(predictions[2] >= 0.5, 1.0, 0.0)
  TP <- sum((X1 == 1) & (X1 == predictions[4]))
  TN <- sum((X0 == 0) & (X1 == predictions[3]))
  FP <- sum((X1 == 1) & (X1 != predictions[4]))
  FN <- sum((X0 == 0) & (X0 != predictions[3]))
  
  #Precision
  #P<-(TP)/(TP+FP)
  ifelse((TP+FP)!=0 && (TP!=0),P<-(TP)/(TP+FP),P<-0)
  
  #Recall
  #R<-(TP)/(TP+FN)
  ifelse((TP+FN)!=0 && (TP!=0),R<-(TP)/(TP+FN),R<-0)
  
  #F1
  #F1<-(2*P*R)/(P+R)
  ifelse((P+R)!=0 && ((2*P*R)!=0),F1<-(2*P*R)/(P+R),F1<-0)
  
  specificity <- (TN)/(TN+FP)  
  sensitivity <- (TP)/(TP+FN)
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  #print(c("clÃ¡ssica", label, train_accuracy, train_sensitivity,train_specificity))
  #print(c("clÃ¡ssica - Precision/Recall/F1", label, P, R,F1))
  results=list(accuracy=accuracy,sensitivity=sensitivity,specificity=specificity,P=P,R=R,F1=F1,time_elapsed=time_elapsed)
  
  X0 <- ifelse(predictions[1]>=0.8566669, 1.0, 0.0)
  X1 <- ifelse(predictions[2]>=0.1433331, 1.0, 0.0)
  TP <- sum((X1 == 1) & (X1 == predictions[4]))
  TN <- sum((X0 == 0) & (X1 == predictions[3]))
  FP <- sum((X1 == 1) & (X1 != predictions[4]))
  FN <- sum((X0 == 0) & (X0 != predictions[3]))
  
  
  #Precision
  #P<-(TP)/(TP+FP)
  ifelse((TP+FP)!=0 && (TP!=0),P<-(TP)/(TP+FP),P<-0)
  
  #Recall
  #R<-(TP)/(TP+FN)
  ifelse((TP+FN)!=0 && (TP!=0),R<-(TP)/(TP+FN),R<-0)
  
  #F1
  #F1<-(2*P*R)/(P+R)
  ifelse((P+R)!=0 && ((2*P*R)!=0),F1<-(2*P*R)/(P+R),F1<-0)
  
  specificity <- (TN)/(TN+FP)  
  sensitivity <- (TP)/(TP+FN)
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  #print(c("classe majoritÃ¡ria", label, train_accuracy, train_sensitivity,train_specificity <- (TN)/(TN+FP)  ))
  #print(c("classe majoritÃ¡ria- Precision/Recall/F1", label, P, R,F1))
  
  results_majority=list(accuracy=accuracy,sensitivity=sensitivity,specificity=specificity,P=P,R=R,F1=F1,time_elapsed=time_elapsed)
  
  
  
  X0 <- ifelse(predictions[1]>=thresholdTest, 1.0, 0.0)
  X1 <- ifelse(predictions[2]>=(thresholdTest), 1.0, 0.0)
  TP <- sum((X1 == 1) & (X1 == predictions[4]))
  TN <- sum((X0 == 0) & (X1 == predictions[3]))
  FP <- sum((X1 == 1) & (X1 != predictions[4]))
  FN <- sum((X0 == 0) & (X0 != predictions[3]))
  sensitivity <- (TP)/(TP+FN)
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  specificity <- (TN)/(TN+FP)  
  
  #Precision
  #P<-(TP)/(TP+FP)
  ifelse((TP+FP)!=0 && (TP!=0),P<-(TP)/(TP+FP),P<-0)
  
  #Recall
  #R<-(TP)/(TP+FN)
  ifelse((TP+FN)!=0 && (TP!=0),R<-(TP)/(TP+FN),R<-0)
  
  #F1
  #F1<-(2*P*R)/(P+R)
  ifelse((P+R)!=0 && ((2*P*R)!=0),F1<-(2*P*R)/(P+R),F1<-0)
  
  #print(c("classe majoritÃ¡ria - limiar selecionado", "teste limiar","accuracy:", test_limiar_accuracy,"precision:",test_limiar_precision,"recall:",test_limiar_sensitivity,"f1-score",test_limiar_f1))
  
  
  results_threshold=list(accuracy=accuracy,sensitivity=sensitivity,specificity=specificity,P=P,R=R,F1=F1,time_elapsed=time_elapsed,threshold=thresholdTest)
  return(measure=list(results=results,results_majority=results_majority,results_threshold=results_threshold))

  
}
  
#  myrf_test$predictions
# measure_metrics(myrf_test$predictions,"test",0,0)
measure_metrics <- function(predictions, label, x0, x1) {
  #X0 <- ifelse(predictions$X0 >= predictions$X1, 1.0, 0.0)
  X0 <- ifelse(predictions[1] >= predictions[2], 1.0, 0.0)
  X1 <- ifelse(predictions[2] > predictions[1], 1.0, 0.0)
  #TP <- sum((X1 == 1) & (X1 == predictions[4]))
  #TN <- sum((X1 == 0) & (X1 == predictions[4]))
  #FP <- sum((X1 == 1) & (X1 != predictions[4]))
  #FN <- sum((X1 == 0) & (X1 != predictions[4]))
  TP <- sum((X1 == 1) & (X1 == predictions[4]))
  TN <- sum((X0 == 0) & (X1 == predictions[3]))
  FP <- sum((X1 == 1) & (X1 != predictions[4]))
  FN <- sum((X0 == 0) & (X0 != predictions[3]))
  #Precision
  #P<-(TP)/(TP+FP)
  ifelse((TP+FP)!=0 && (TP!=0),P<-(TP)/(TP+FP),P<-0)
  
  #Recall
  #R<-(TP)/(TP+FN)
  ifelse((TP+FN)!=0 && (TP!=0),R<-(TP)/(TP+FN),R<-0)
  
  #F1
  #F1<-(2*P*R)/(P+R)
  ifelse((P+R)!=0 && ((2*P*R)!=0),F1<-(2*P*R)/(P+R),F1<-0)
  
  train_specificity <- (TN)/(TN+FP)  
  train_sensitivity <- (TP)/(TP+FN)
  train_accuracy <- (TP+TN)/(TP+TN+FP+FN)
  print(c("clÃ¡ssica", label, train_accuracy, train_sensitivity,train_specificity))
  print(c("clÃ¡ssica - Precision/Recall/F1", label, P, R,F1))
  

  
  X0 <- ifelse(predictions[1]>=0.8566669, 1.0, 0.0)
  X1 <- ifelse(predictions[2]>=0.1433331, 1.0, 0.0)
  #X0 <- ifelse(predictions[1]>=0.472, 1.0, 0.0)
  #X1 <- ifelse(predictions[2]>=0.289, 1.0, 0.0)
  TP <- sum((X1 == 1) & (X1 == predictions[4]))
  TN <- sum((X1 == 0) & (X1 == predictions[4]))
  FP <- sum((X1 == 1) & (X1 != predictions[4]))
  FN <- sum((X1 == 0) & (X1 != predictions[4]))
  
  
  #Precision
  #P<-(TP)/(TP+FP)
  ifelse((TP+FP)!=0 && (TP!=0),P<-(TP)/(TP+FP),P<-0)
  
  #Recall
  #R<-(TP)/(TP+FN)
  ifelse((TP+FN)!=0 && (TP!=0),R<-(TP)/(TP+FN),R<-0)
  
  #F1
  #F1<-(2*P*R)/(P+R)
  ifelse((P+R)!=0 && ((2*P*R)!=0),F1<-(2*P*R)/(P+R),F1<-0)
  
  train_specificity <- (TN)/(TN+FP)  
  train_sensitivity <- (TP)/(TP+FN)
  train_accuracy <- (TP+TN)/(TP+TN+FP+FN)
  print(c("classe majoritÃ¡ria", label, train_accuracy, train_sensitivity,train_specificity <- (TN)/(TN+FP)  ))
  print(c("classe majoritÃ¡ria- Precision/Recall/F1", label, P, R,F1))
}



measure_metrics_ensamble <- function(predictions_n,predictions_p, label, x0, x1) {
  
  #predictions_n, model specialized to predict negative (0)
  #predictions_p, model specialized to predict positive (1)
  
  
  X0_n <- ifelse(predictions_n[1] >= predictions_n[2], 1.0, 0.0)
  X1_n <- ifelse(predictions_n[2] > predictions_n[1], 1.0, 0.0)
  
  X0_p <- ifelse(predictions_p[2] > predictions_p[1], 1.0, 0.0)
  X1_p <- ifelse(predictions_p[2] > predictions_p[1], 1.0, 0.0)
  
  
  X0 <- ifelse(X0_n==0 && X0_p==1, 0.0, 1.0)
  X1 <- ifelse(X1_n==0 && X1_p==1, 0.0, 1.0)
  
  
  TP <- sum((X1 == 1) & (X1 == predictions_n[4]))
  TN <- sum((X1 == 0) & (X1 == predictions_n[4]))
  FP <- sum((X1 == 1) & (X1 != predictions_n[4]))
  FN <- sum((X1 == 0) & (X1 != predictions_n[4]))
  
  #Precision
  P<-(TP)/(TP+FP)
  #Recall
  R<-(TP)/(TP+FN)
  #F1
  F1<-(2*P*R)/(P+R)
  
  train_specificity <- (TN)/(TN+FP)  
  train_sensitivity <- (TP)/(TP+FN)
  train_accuracy <- (TP+TN)/(TP+TN+FP+FN)
  print(c("clÃ¡ssica", label, train_accuracy, train_sensitivity,train_specificity))
  #print(c("clÃ¡ssica - Precision/Recall/F1", label, P, R,F1))
  
  
}



dictionary3 <- function() {
  x <- NULL
  x <- rbind(x, data.frame(name="airline", type=1, description="basic"))
  x <- rbind(x, data.frame(name="flight", type=0, description="basic"))
  x <- rbind(x, data.frame(name="departure", type=1, description="basic"))
  x <- rbind(x, data.frame(name="arrival", type=1, description="basic"))
    x <- rbind(x, data.frame(name="departure_expect", type=0, description="remove"))
    x <- rbind(x, data.frame(name="arrival_expect", type=0, description="remove"))
    x <- rbind(x, data.frame(name="departure_time", type=0, description="basic"))
    x <- rbind(x, data.frame(name="arrival_time", type=0, description="basic"))
  x <- rbind(x, data.frame(name="departure_year", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_month", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_day", type=2, description="basic"))
  x <- rbind(x, data.frame(name="departure_hour", type=2, description="original"))
  x <- rbind(x, data.frame(name="departure_weekday", type=3, description="basic"))
  x <- rbind(x, data.frame(name="departure_hour_bin", type=3, description="bin"))
  x <- rbind(x, data.frame(name="departure_temperature", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_dew_point", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_humidity", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_pressure", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_visibility", type=4, description="original"))
  x <- rbind(x, data.frame(name="departure_temperature_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_dew_point_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_humidity_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_visibility_bin", type=5, description="bin"))
  x <- rbind(x, data.frame(name="departure_events", type=6, description="original"))
  x <- rbind(x, data.frame(name="departure_eventsFRESHBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsGENTLEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsHURRICANE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTAIR", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsLIGHTBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsMODERATEBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNEARGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsNONE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGBREEZE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsSTRONGGALE", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_eventsVIOLENTSTORM", type=7, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditions", type=8, description="original"))
  x <- rbind(x, data.frame(name="departure_conditionsBLOWINGSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsCLEAR", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsDUSTWHIRLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFOGTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsFUNNELCLOUD", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHAZE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsHEAVYTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTDRIZZLE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsLIGHTVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMIST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsMOSTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsNONE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsOVERCAST", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTIALFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPARTLYCLOUDY", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsPATCHESOFFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINHAILTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSHOWERS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsRAINTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSAND", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSANDSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSCATTEREDCLOUDS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSHALLOWFOG", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSMOKE", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOW", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSNOWTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsSQUALLS", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORM", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSANDRAIN", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTHUNDERSTORMSWITHSMALLHAIL", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsTORNADO", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsVOLCANICASH", type=9, description="cm"))
  x <- rbind(x, data.frame(name="departure_conditionsWIDESPREADDUST", type=9, description="cm"))
  
  
  
  x <- rbind(x, data.frame(name="arrival_year", type=21, description="basic"))
  x <- rbind(x, data.frame(name="arrival_month", type=21, description="basic"))
  x <- rbind(x, data.frame(name="arrival_day", type=21, description="basic"))
  x <- rbind(x, data.frame(name="arrival_hour", type=21, description="original"))
  x <- rbind(x, data.frame(name="arrival_weekday", type=31, description="basic"))
  x <- rbind(x, data.frame(name="arrival_hour_bin", type=31, description="bin"))
  x <- rbind(x, data.frame(name="arrival_temperature", type=41, description="original"))
  x <- rbind(x, data.frame(name="arrival_dew_point", type=41, description="original"))
  x <- rbind(x, data.frame(name="arrival_humidity", type=41, description="original"))
  x <- rbind(x, data.frame(name="arrival_pressure", type=41, description="original"))
  x <- rbind(x, data.frame(name="arrival_visibility", type=41, description="original"))
  x <- rbind(x, data.frame(name="arrival_temperature_bin", type=51, description="bin"))
  x <- rbind(x, data.frame(name="arrival_dew_point_bin", type=51, description="bin"))
  x <- rbind(x, data.frame(name="arrival_humidity_bin", type=51, description="bin"))
  x <- rbind(x, data.frame(name="arrival_visibility_bin", type=51, description="bin"))
  x <- rbind(x, data.frame(name="arrival_events", type=61, description="original"))
  x <- rbind(x, data.frame(name="arrival_eventsFRESHBREEZE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsGALE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsGENTLEBREEZE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsHURRICANE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsLIGHTAIR", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsLIGHTBREEZE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsMODERATEBREEZE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsNEARGALE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsNONE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsSTRONGBREEZE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsSTRONGGALE", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_eventsVIOLENTSTORM", type=71, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditions", type=81, description="original"))
  x <- rbind(x, data.frame(name="arrival_conditionsBLOWINGSAND", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsCLEAR", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsDRIZZLE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsDUSTWHIRLS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFOGRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFOGRAINTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFOGSNOW", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFOGTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsFUNNELCLOUD", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHAIL", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHAILTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHAZE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYDRIZZLE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYRAINSHOWERS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYTHUNDERSTORMSANDRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsHEAVYTHUNDERSTORMSWITHHAIL", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTDRIZZLE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTRAINSHOWERS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTTHUNDERSTORMSANDRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsLIGHTVOLCANICASH", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsMIST", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsMOSTLYCLOUDY", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsNONE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsOVERCAST", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsPARTIALFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsPARTLYCLOUDY", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsPATCHESOFFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsRAINHAILTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsRAINSHOWERS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsRAINSNOW", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsRAINTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSAND", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSANDSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSCATTEREDCLOUDS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSHALLOWFOG", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSMOKE", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSNOW", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSNOWTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsSQUALLS", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsTHUNDERSTORM", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsTHUNDERSTORMSANDRAIN", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsTHUNDERSTORMSWITHHAIL", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsTHUNDERSTORMSWITHSMALLHAIL", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsTORNADO", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsVOLCANICASH", type=91, description="cm"))
  x <- rbind(x, data.frame(name="arrival_conditionsWIDESPREADDUST", type=91, description="cm"))
  
  
  x <- rbind(x, data.frame(name="delayed", type=10, description="target"))
  ########
  
  
  #description="bin"
  #type=13
  #x <- rbind(x, data.frame(name="delayed", type=10, description="target"))
  
  column_name=colnames(sample_wn$vra_wu_train)
  
  #airlines
  for (i in 84:92) 
  {
    x <- rbind(x, data.frame(name=column_name[i], type=11, description="cm - airlines"))  
  }
  
  #departure_airport
  for (i in 93:144) 
  {
    x <- rbind(x, data.frame(name=column_name[i], type=12, description="cm - departure_airport"))  
  }
  
  #arrival_airport
  for (i in 145:196) {
    x <- rbind(x, data.frame(name=column_name[i], type=13, description="cm - arrival_airport"))  
  }
  return(x)  
}


threshold_selection <- function(data,betha,numberSamplesPositive){
  p_i <- 1
  n_i <- 1
  k <- 0
  i <- 1
  cp_module <- numberSamplesPositive
  
  value_condition <- data[i,'true_value'] 
  
  while(value_condition == 0){
    i <- i +1 
    n_i <- n_i +1
    value_condition <- data[i,'true_value'] 
  }
  
  threshold_current <- data[i,'prob_pos'] 
  F_score_current <- (betha^2 + 1)*(p_i / n_i)*(p_i / cp_module)/((betha^2)*(p_i / n_i) + p_i / cp_module)
  
  i <- i + 1
  
  for(x in seq(from = i, to = nrow(data), by = 1)){
    if(data[i,'true_value'] == 0){
      k <- k + 1
    }else if(k >= ((cp_module - p_i)/p_i)*(betha^2*cp_module + n_i - p_i)) {
      break            
    }else{
      n_i <- n_i + k 
      k <- 0
      p_i <- p_i + 1
      F_score_temp <- (betha^2 + 1)*(p_i / n_i)*(p_i / cp_module)/((betha^2)*(p_i / n_i) + p_i / cp_module)
      
      if(F_score_temp > F_score_current){
        F_score_current <- F_score_temp
        threshold_current <- data[x,'prob_pos']
      }
    }
  }
  return(threshold_current)
}
