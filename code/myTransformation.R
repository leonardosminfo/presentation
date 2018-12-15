transformData<- function(df){

  ###############################  
#Temperature:
  jpeg(filename=paste(path,"images/arrival_temperature_bin.jpeg",sep=""))
arrival_temperature.bin <- binning.opt(df[,"arrival_temperature"], binning=binning.interval)
arrival_temperature.bin$binning
arrival_temperature.bin$bins
dev.off()
saveRDS(arrival_temperature.bin,paste(path,"dataset/arrival_temperature_bin.RData",sep=""))
df <- df %>% mutate(arrival_temperature_bin = factor(arrival_temperature.bin$bins_factor))


  jpeg(filename=paste(path,"images/depart_temperature_bin.jpeg",sep=""))
depart_temperature.bin <- binning.opt(df[,"depart_temperature"], binning=binning.interval)
depart_temperature.bin$binning
depart_temperature.bin$bins
dev.off()
saveRDS(depart_temperature.bin,paste(path,"dataset/depart_temperature_bin.RData",sep=""))
df <- df %>% mutate(depart_temperature_bin = factor(depart_temperature.bin$bins_factor))
#################################

#Dew Point:
jpeg(filename=,paste(path,"images/arrival_dew_point_bin.jpeg",sep=""))
arrival_dew_point.bin <- binning.opt(df[,"arrival_dew_point"], binning=binning.interval)
arrival_dew_point.bin$binning
arrival_dew_point.bin$bins
dev.off()
saveRDS(arrival_dew_point.bin, paste(path,"dataset/arrival_dew_point_bin.RData",sep=""))
df <- df %>% mutate(arrival_dew_point_bin = factor(arrival_dew_point.bin$bins_factor))

jpeg(filename=paste(path,"images/depart_dew_point_bin.jpeg",sep=""))
depart_dew_point.bin <- binning.opt(df[,"depart_dew_point"], binning=binning.interval)
depart_dew_point.bin$binning
depart_dew_point.bin$bins
dev.off()
saveRDS(depart_dew_point.bin,paste(path,"dataset/depart_dew_point_bin.RData",sep=""))
df <- df %>% mutate(depart_dew_point_bin = factor(depart_dew_point.bin$bins_factor))
#######################################

#Humidity:
  jpeg(filename=paste(path,"images/arrival_humidity_bin.jpeg",sep=""))
arrival_humidity.bin <- binning.opt(df[,"arrival_humidity"], binning=binning.interval)
arrival_humidity.bin$binning
arrival_humidity.bin$bins
dev.off()
saveRDS(arrival_humidity.bin,paste(path, "dataset/arrival_humidity_bin.RData",sep=""))
df <- df %>% mutate(arrival_humidity_bin = factor(arrival_humidity.bin$bins_factor))

  jpeg(filename=paste(path,"images/depart_humidity_bin.jpeg",sep=""))
depart_humidity.bin <- binning.opt(df[,"depart_humidity"], binning=binning.interval)
depart_humidity.bin$binning
depart_humidity.bin$bins
dev.off()
saveRDS(depart_humidity.bin, paste(path,"dataset/depart_humidity_bin.RData",sep=""))
df <- df %>% mutate(depart_humidity_bin = factor(depart_humidity.bin$bins_factor))
###################################

#Pressure:

  jpeg(filename=paste(path,"images/arrival_pressure_bin.jpeg",sep=""))
arrival_pressure.bin <- binning.opt(df[,"arrival_pressure"], binning=binning.interval)
arrival_pressure.bin$binning
arrival_pressure.bin$bins
dev.off()
saveRDS(arrival_pressure.bin,paste(path,"dataset/arrival_pressure_bin.RData",sep=""))
df <- df %>% mutate(arrival_pressure_bin = factor(arrival_pressure.bin$bins_factor))

  jpeg(filename=paste(path,"images/depart_pressure_bin.jpeg",sep=""))
depart_pressure.bin <- binning.opt(df[,"depart_pressure"], binning=binning.interval)
depart_pressure.bin$binning
depart_pressure.bin$bins
dev.off()
saveRDS(depart_pressure.bin,paste(path,"dataset/depart_pressure_bin.RData",sep=""))
df <- df %>% mutate(depart_pressure_bin = factor(depart_pressure.bin$bins_factor))
######################################

#Visibility:

  jpeg(filename=paste(path,"images/arrival_visibility_bin.jpeg",sep=""))
arrival_visibility.bin <- binning.opt(df[,"arrival_visibility"], binning=binning.cluster)
arrival_visibility.bin$binning
arrival_visibility.bin$bins
dev.off()
saveRDS(arrival_visibility.bin,paste(path,"dataset/arrival_visibility_bin.RData",sep=""))
df <- df %>% mutate(arrival_visibility_bin = factor(arrival_visibility.bin$bins_factor))

  jpeg(filename=paste(path,"images/depart_visibility_bin.jpeg",sep=""))
depart_visibility.bin <- binning.opt(df[,"depart_visibility"], binning=binning.cluster)
depart_visibility.bin$binning
depart_visibility.bin$bins
dev.off()
saveRDS(depart_visibility.bin,paste(path,"dataset/depart_visibility_bin.RData",sep=""))
df <- df %>% mutate(depart_visibility_bin = factor(depart_visibility.bin$bins_factor))

##############################################
saveRDS(df, paste(path,"dataset/df.RData",sep=""))
#View(head(df))
##############################################

#CATEGORICAL MAPPING

#df=vra_wu_cleaned_imp

#Events:
df=dt.categ_mapping(df,"depart_events")
df=dt.categ_mapping(df,"arrival_events")

#Conditions:
#conditions=levels(as.factor(df[,"arrival_conditions"]))
#saveRDS(conditions, paste(path,"dataset/conditions.RData",sep=""))
df=dt.categ_mapping(df,"depart_conditions")
df=dt.categ_mapping(df,"arrival_conditions")
###############################################

#CONCEPTUAL HIERARCHY

#Depart/Arrival Expected Hour/Date:
loadlibrary("lubridate")

#depart
df <- df %>% mutate(depart_expect_year = year(df[,"depart_expect"]))
df <- df %>% mutate(depart_expect_month = month(df[,"depart_expect"]))
df <- df %>% mutate(depart_expect_day = day(df[,"depart_expect"]))
df <- df %>% mutate(depart_expect_weekday = wday(df[,"depart_expect"]))
df <- df %>% mutate(depart_expect_hr = hour(df[,"depart_expect"]))

#arrival
df <- df %>% mutate(arrival_expect_year = year(df[,"arrival_expect"]))
df <- df %>% mutate(arrival_expect_month = month(df[,"arrival_expect"]))
df <- df %>% mutate(arrival_expect_day = day(df[,"arrival_expect"]))
df <- df %>% mutate(arrival_expect_weekday = wday(df[,"arrival_expect"]))
df <- df %>% mutate(arrival_expect_hr = hour(df[,"arrival_expect"]))


###############################################
#BINNING
jpeg(filename=paste(path,"images/depart_expect_hr.jpeg",sep=""))
depart_expect_hr.bin=binning.opt(df[,"depart_expect_hr"], binning=binning.interval)
depart_expect_hr.bin$binning
depart_expect_hr.bin$bins
dev.off()
saveRDS(depart_expect_hr.bin,paste(path,"dataset/depart_expect_hr_bin.RData",sep=""))
df <- df %>% mutate(depart_expect_hr_bin = factor(depart_expect_hr.bin$bins_factor))

#ERROR =>adjusting zero hour??
#df <- df %>% mutate(depart_expect_hr_bin = ifelse(is.na(depart_expect_hr_bin),1,depart_expect_hr_bin))


  jpeg(filename=paste(path,"images/arrival_expect_hr.jpeg",sep=""))
arrival_expect_hr.bin=binning.opt(df[,"arrival_expect_hr"], binning=binning.interval)
arrival_expect_hr.bin$binning
arrival_expect_hr.bin$bins
dev.off()
saveRDS(arrival_expect_hr.bin,paste(path,"dataset/arrival_expect_hr_bin.RData",sep=""))
df <- df %>% mutate(arrival_expect_hr_bin = factor(arrival_expect_hr.bin$bins_factor))

#ERROR adjusting zero hour??
#df <- df %>% mutate(arrival_expect_hr_bin = ifelse(is.na(arrival_expect_hr_bin),1,arrival_expect_hr_bin))

return(df)
}




transformDataimp<- function(df){
  
    #Temperature:
    jpeg(filename=paste(path,"images/imp/arrival_temperature_bin.jpeg",sep=""))
  arrival_temperature.bin <- binning.opt(df[,"arrival_temperature"], binning=binning.interval)
  arrival_temperature.bin$binning
  arrival_temperature.bin$bins
  dev.off()
    saveRDS(arrival_temperature.bin,paste(path,"dataset/imp/arrival_temperature_bin.RData",sep=""))
  df <- df %>% mutate(arrival_temperature_bin = factor(arrival_temperature.bin$bins_factor))
  
  
    jpeg(filename=paste(path,"images/imp/depart_temperature_bin.jpeg",sep=""))
  depart_temperature.bin <- binning.opt(df[,"depart_temperature"], binning=binning.interval)
  depart_temperature.bin$binning
  depart_temperature.bin$bins
  dev.off()
  saveRDS(depart_temperature.bin,paste(path,"dataset/imp/depart_temperature_bin.RData",sep=""))
  df <- df %>% mutate(depart_temperature_bin = factor(depart_temperature.bin$bins_factor))
  #################################
  
  #Dew Point:
    jpeg(filename=,paste(path,"images/imp/arrival_dew_point_bin.jpeg",sep=""))
  arrival_dew_point.bin <- binning.opt(df[,"arrival_dew_point"], binning=binning.interval)
  arrival_dew_point.bin$binning
  arrival_dew_point.bin$bins
  dev.off()
  saveRDS(arrival_dew_point.bin, paste(path,"dataset/imp/arrival_dew_point_bin.RData",sep=""))
  df <- df %>% mutate(arrival_dew_point_bin = factor(arrival_dew_point.bin$bins_factor))
  
    jpeg(filename=paste(path,"images/imp/depart_dew_point_bin.jpeg",sep=""))
  depart_dew_point.bin <- binning.opt(df[,"depart_dew_point"], binning=binning.interval)
  depart_dew_point.bin$binning
  depart_dew_point.bin$bins
  dev.off()
  saveRDS(depart_dew_point.bin,paste(path,"dataset/imp/depart_dew_point_bin.RData",sep=""))
  df <- df %>% mutate(depart_dew_point_bin = factor(depart_dew_point.bin$bins_factor))
  #######################################
  
  #Humidity:
  jpeg(filename=paste(path,"images/imp/arrival_humidity_bin.jpeg",sep=""))
  arrival_humidity.bin <- binning.opt(df[,"arrival_humidity"], binning=binning.interval)
  arrival_humidity.bin$binning
  arrival_humidity.bin$bins
  dev.off()
  saveRDS(arrival_humidity.bin,paste(path, "dataset/imp/arrival_humidity_bin.RData",sep=""))
  df <- df %>% mutate(arrival_humidity_bin = factor(arrival_humidity.bin$bins_factor))
  
  jpeg(filename=paste(path,"images/imp/depart_humidity_bin.jpeg",sep=""))
  depart_humidity.bin <- binning.opt(df[,"depart_humidity"], binning=binning.interval)
  depart_humidity.bin$binning
  depart_humidity.bin$bins
  dev.off()
  saveRDS(depart_humidity.bin, paste(path,"dataset/imp/depart_humidity_bin.RData",sep=""))
  df <- df %>% mutate(depart_humidity_bin = factor(depart_humidity.bin$bins_factor))
  ###################################
  
  #Pressure:
    jpeg(filename=paste(path,"images/imp/arrival_pressure_bin.jpeg",sep=""))
  arrival_pressure.bin <- binning.opt(df[,"arrival_pressure"], binning=binning.interval)
  arrival_pressure.bin$binning
  arrival_pressure.bin$bins
  dev.off()
  saveRDS(arrival_pressure.bin,paste(path,"dataset/imp/arrival_pressure_bin.RData",sep=""))
  df <- df %>% mutate(arrival_pressure_bin = factor(arrival_pressure.bin$bins_factor))
  
    jpeg(filename=paste(path,"images/imp/depart_pressure_bin.jpeg",sep=""))
  depart_pressure.bin <- binning.opt(df[,"depart_pressure"], binning=binning.interval)
  depart_pressure.bin$binning
  depart_pressure.bin$bins
  dev.off()
  saveRDS(depart_pressure.bin,paste(path,"dataset/imp/depart_pressure_bin.RData",sep=""))
  df <- df %>% mutate(depart_pressure_bin = factor(depart_pressure.bin$bins_factor))
  ######################################
  
  #Visibility:
    jpeg(filename=paste(path,"images/imp/arrival_visibility_bin.jpeg",sep=""))
  arrival_visibility.bin <- binning.opt(df[,"arrival_visibility"], binning=binning.cluster)
  arrival_visibility.bin$binning
  arrival_visibility.bin$bins
  dev.off()
  saveRDS(arrival_visibility.bin,paste(path,"dataset/imp/arrival_visibility_bin.RData",sep=""))
  df <- df %>% mutate(arrival_visibility_bin = factor(arrival_visibility.bin$bins_factor))
  
    jpeg(filename=paste(path,"images/imp/depart_visibility_bin.jpeg",sep=""))
  depart_visibility.bin <- binning.opt(df[,"depart_visibility"], binning=binning.cluster)
  depart_visibility.bin$binning
  depart_visibility.bin$bins
  dev.off()
  saveRDS(depart_visibility.bin,paste(path,"dataset/imp/depart_visibility_bin.RData",sep=""))
  df <- df %>% mutate(depart_visibility_bin = factor(depart_visibility.bin$bins_factor))
  
  saveRDS(df, paste(path,"dataset/imp/df.RData",sep=""))
  ##############################################
  
  #CATEGORICAL MAPPING
  
  #Events:
    df=dt.categ_mapping(df,"depart_events")
    df=dt.categ_mapping(df,"arrival_events")
  
  #Conditions:
    #conditions=levels(as.factor(df[,"arrival_conditions"]))
    #saveRDS(conditions, paste(path,"dataset/imp/conditions.RData",sep=""))
  
    df=dt.categ_mapping(df,"depart_conditions")
    df=dt.categ_mapping(df,"arrival_conditions")
  ###############################################
  
  #CONCEPTUAL HIERARCHY
  
  #Depart/Arrival Expected Hour/Date:
  
  loadlibrary("lubridate")
  
  #depart
  df <- df %>% mutate(depart_expect_year = year(df[,"depart_expect"]))
  df <- df %>% mutate(depart_expect_month = month(df[,"depart_expect"]))
  df <- df %>% mutate(depart_expect_day = day(df[,"depart_expect"]))
  df <- df %>% mutate(depart_expect_weekday = wday(df[,"depart_expect"]))
  df <- df %>% mutate(depart_expect_hr = hour(df[,"depart_expect"]))
  
  #arrival
  df <- df %>% mutate(arrival_expect_year = year(df[,"arrival_expect"]))
  df <- df %>% mutate(arrival_expect_month = month(df[,"arrival_expect"]))
  df <- df %>% mutate(arrival_expect_day = day(df[,"arrival_expect"]))
  df <- df %>% mutate(arrival_expect_weekday = wday(df[,"arrival_expect"]))
  df <- df %>% mutate(arrival_expect_hr = hour(df[,"arrival_expect"]))
  
  
  ###############################################
  
  #BINNING
     jpeg(filename=paste(path,"images/imp/depart_expect_hr.jpeg",sep=""))
  depart_expect_hr.bin=binning.opt(df[,"depart_expect_hr"], binning=binning.interval)
  depart_expect_hr.bin$binning
  depart_expect_hr.bin$bins
  dev.off()
  saveRDS(depart_expect_hr.bin,paste(path,"dataset/imp/depart_expect_hr_bin.RData",sep=""))
  df <- df %>% mutate(depart_expect_hr_bin = factor(depart_expect_hr.bin$bins_factor))
  
  #ERROR IN BINNING => adjusting zero hour?
  #df <- df %>% mutate(depart_expect_hr_bin = ifelse(is.na(depart_expect_hr_bin),1,depart_expect_hr_bin))
  
    jpeg(filename=paste(path,"images/imp/arrival_expect_hr.jpeg",sep=""))
  arrival_expect_hr.bin=binning.opt(df[,"arrival_expect_hr"], binning=binning.interval)
  arrival_expect_hr.bin$binning
  arrival_expect_hr.bin$bins
  dev.off()
  saveRDS(arrival_expect_hr.bin,paste(path,"dataset/imp/arrival_expect_hr_bin.RData",sep=""))
  df <- df %>% mutate(arrival_expect_hr_bin = factor(arrival_expect_hr.bin$bins_factor))
  
  #ERROR IN BINNING => adjusting zero hour?
  #df <- df %>% mutate(arrival_expect_hr_bin = ifelse(is.na(arrival_expect_hr_bin),1,arrival_expect_hr_bin))
  
  return(df)
}

data_missing<-function(df)
{
  
  missing_data <- as.data.frame(sort(sapply(df, function(x) sum(is.na(x))),decreasing = T))
  
  missing_data <- (missing_data/nrow(df))*100
  
  colnames(missing_data)[1] <- "missingvaluesPercentage"
  missing_data$features <- rownames(missing_data)
  
  ggplot(missing_data[missing_data$missingvaluesPercentage >0,],aes(reorder(features,-missingvaluesPercentage),missingvaluesPercentage,fill= features)) +
    geom_bar(stat="identity") +theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + ylab("Percentage of missingvalues") +
    xlab("Feature") + ggtitle("Understanding Missing Data")
}

