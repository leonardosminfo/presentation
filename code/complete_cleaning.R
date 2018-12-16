setwd("~/Flight-Delay/Cleaning")
options(java.parameters = "-Xmx31g")
load("~/Flight-Delay/Cleaning/dataset/vra-wu.RData")
#load("E:/leonardosm/Documents/Mestrado/Monografia/novo_servidor/Brazilian-Flight-Delay/demonstracao/dataset/vra-wu.RData")
path="~/Flight-Delay/Cleaning/"
#path="E:/leonardosm/Documents/Mestrado/Monografia/novo_servidor/Brazilian-Flight-Delay/demonstracao/"
#######################

loadlibrary <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x, repos='http://cran.fiocruz.br', dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}


#######################
loadlibrary("dplyr")
loadlibrary("caret")
loadlibrary("dplyr")
loadlibrary("tidyr")
loadlibrary("stringr")
loadlibrary("ggplot2")
loadlibrary("corrplot")


glimpse(vra_wu)

#features with caracteres
clevels=c("destiny","origin","airline","flight","autho_code","line_type","status","observation","depart_conditions","arrival_conditions","origin.state","destiny.state")


#treating data
mutate_if(vra_wu[,clevels], is.factor, funs(as.character(.)))
mutate_if(vra_wu[,clevels], is.character, funs(as.character(.)))


#toupper
vra_wu[,"destiny"]=trimws(toupper(as.character(vra_wu[,"destiny"])))
vra_wu[,"origin"]=trimws(toupper(as.character(vra_wu[,"origin"])))
vra_wu[,"airline"]=trimws(toupper(as.character(vra_wu[,"airline"])))
vra_wu[,"status"]=trimws(toupper(as.character(vra_wu[,"status"])))
vra_wu[,"observation"]=trimws(toupper(as.character(vra_wu[,"observation"])))
vra_wu[,"origin.state"]=trimws(toupper(as.character(vra_wu[,"origin.state"])))
vra_wu[,"destiny.state"]=trimws(toupper(as.character(vra_wu[,"destiny.state"])))
vra_wu[,"depart_events"]=trimws(toupper(as.character(vra_wu[,"depart_events"])))
vra_wu[,"arrival_events"]=trimws(toupper(as.character(vra_wu[,"arrival_events"])))



#treating  utf8 problems
vra_wu$depart_conditions=iconv(vra_wu$depart_conditions, "ASCII", "UTF-8", sub="")
vra_wu[,"depart_conditions"]=trimws(toupper(as.character(vra_wu[,"depart_conditions"])))

#treating  utf8 problems
vra_wu$arrival_conditions=iconv(vra_wu$arrival_conditions, "ASCII", "UTF-8", sub="")
vra_wu[,"arrival_conditions"]=trimws(toupper(as.character(vra_wu[,"arrival_conditions"])))

#showing status, events, condition and observation levels
levels(as.factor(vra_wu[["depart_conditions"]]))
levels(as.factor(vra_wu[["arrival_conditions"]]))
levels(as.factor(vra_wu[["status"]]))
levels(as.factor(vra_wu$depart_events))
levels(as.factor(vra_wu$arrival_events))
levels(as.factor(vra_wu$observation))


#convert  POSIXlt para POSIXct (dplyr)
vra_wu$depart_expect=as.POSIXct(vra_wu$depart_expect)
vra_wu$arrival_expect=as.POSIXct(vra_wu$arrival_expect)

vra_wu$depart=as.POSIXct(vra_wu$depart)
vra_wu$arrival=as.POSIXct(vra_wu$arrival)


##treating observation, events and condition levels 

#EVENTS
vra_wu <- vra_wu %>% mutate(depart_events = ifelse(depart_events =="", "NONE", depart_events))
vra_wu <- vra_wu %>% mutate(arrival_events = ifelse(arrival_events =="", "NONE", arrival_events))

#CONDITIONS
vra_wu <- vra_wu %>% mutate(depart_conditions = ifelse(depart_conditions =="", "NONE", depart_conditions))
vra_wu <- vra_wu %>% mutate(arrival_conditions = ifelse(arrival_conditions =="", "NONE", arrival_conditions))

#OBSERVATIONS
vra_wu <- vra_wu %>% mutate(observation = ifelse(observation =="", "NO", observation))


#remove , \ space
rspc=c("arrival_conditions","depart_conditions","arrival_events","depart_events")
vra_wu[,rspc]=sapply(vra_wu[,rspc], function(x) {gsub("\\s*|\\,", "",  x)})
vra_wu[,rspc]=sapply(vra_wu[,rspc], function(x) {gsub("\\s*|\\,", "",  x)})


##################################################
#summaries of data
summary(vra_wu)

#counting na data  
sum(is.na(vra_wu)) #21296331

#More NA From Visibility 
#analyzing na (feature view)
missing_data <- as.data.frame(sort(sapply(vra_wu, function(x) sum(is.na(x))),decreasing = T))
View(missing_data)


##############################################
#filtering data

#departure_before_arrival #1819
flight_departure_before_arrival=filter(vra_wu, difftime (vra_wu$arrival,vra_wu$depart)<=0)

#origin_same_destiny #15775
flight_origin_same_destiny=filter(vra_wu,vra_wu$origin==vra_wu$destiny)

#flight_duration_negative #1758
flight_duration_negative=filter(vra_wu,vra_wu$duration<0)

#flight_duration_sup24h  #2349
flight_duration_sup24=filter(vra_wu,vra_wu$duration>1440)

#example of duration above 340 days, between 2008-03-11 and 2009-03-12
difftime(flight_duration_sup24[5,"arrival"],flight_duration_sup24[5,"depart"])

#flight_nrealized #830436
flight_nrealized=filter(vra_wu,vra_wu$status!="REALIZADO")

##############################################
##removing filtered data

#original #8683195

# vra_wu_cl will recept filtered data 

#departure_before_arrival
vra_wu_cl=anti_join(vra_wu, flight_departure_before_arrival, by=colnames(vra_wu))
#8681376

#origin_same_destiny
vra_wu_cl=anti_join(vra_wu_cl, flight_origin_same_destiny, by=colnames(vra_wu_cl))
#8665606

#flight_duration_negative
vra_wu_cl=anti_join(vra_wu_cl, flight_duration_negative, by=colnames(vra_wu_cl))
#8665606


#flight_duration_sup24h
vra_wu_cl=anti_join(vra_wu_cl, flight_duration_sup24, by=colnames(vra_wu_cl))
#8663265

#flight_nrealized
vra_wu_cl=anti_join(vra_wu_cl, flight_nrealized, by=colnames(vra_wu_cl))
#7832834

##############################################

#FILTERING TEMPERATURE, DEW POINT, HUMIDITY, PRESSURE ( WITH INCONSISTENT VALUES FOUNDED)

#TEMPERATURE
#HIGHER  56.7 DEGREE(S) CELSIUS
temp_filter=filter(vra_wu, depart_temperature>57 | arrival_temperature>57 | depart_temperature < -68 | arrival_temperature < -68)
#17

#DEWPOINT
#HIGHER 84 degrees
dewpoint_filter=filter(vra_wu, depart_dew_point>84 | arrival_dew_point>84)
#0

#HUMIDITY  #BETWEEN 0 and 100
humidity_filter=filter(vra_wu, arrival_humidity>100 | depart_humidity>100 | arrival_humidity<0 | depart_humidity<0)
#21

#PRESSURE
#HIGHER  1083.8mb
#LOWER 860mb
pressure_filter=filter(vra_wu, arrival_pressure<860 | depart_pressure<860 | arrival_pressure>1084 |depart_pressure>1084)
#136


#visibility
visibility_filter=filter(vra_wu, arrival_visibility>100 | depart_visibility>100 | arrival_visibility<0 | depart_visibility<0)
#2

###############
##removing filtered data

#original #7832834

#HUMIDITY 
vra_wu_cl=anti_join(vra_wu_cl, humidity_filter, by=colnames(vra_wu_cl))
#7832816

#TEMPERATURE
vra_wu_cl=anti_join(vra_wu_cl, temp_filter, by=colnames(vra_wu_cl))
#7832802

#DEWPOINT
vra_wu_cl=anti_join(vra_wu_cl, dewpoint_filter, by=colnames(vra_wu_cl))
#7832802

#PRESSURE
vra_wu_cl=anti_join(vra_wu_cl, pressure_filter, by=colnames(vra_wu_cl))
#7832677

#VISIBILITY
vra_wu_cl=anti_join(vra_wu_cl, visibility_filter, by=colnames(vra_wu_cl))
#7832675
#########################################

#creating predictive feature [delay(arrival or departure) >=15 min]
#1 yes  0 no
vra_wu_cl <- vra_wu_cl %>% mutate(delayed = ifelse(arrival_delay>=15, 1, ifelse(departure_delay>=15, 1, 0)))
vra_wu_cl[,"delayed"]=as.factor(vra_wu_cl[,"delayed"])


#saving cleaned data
saveRDS(vra_wu_cl, paste(path, "dataset/vra_wu_cl.RData",sep="")) 


