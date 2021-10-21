library(readxl)
library(XML)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(ggthemes)
library(stringr)
#unzip file
file <- "C:/Users/gabe7/OneDrive/Documents/projectApolloV2.0/exports/"
cd <- getwd()
pd <- paste0(cd, '/projectApolloV2.0/')
list.files(paste0(pd,'apple_health_export'))
xml <- xmlParse(paste0(pd, 'apple_health_export/export.xml'))
csv <- paste0(pd,'tempData.txt', sep ='')

#create df from iphone health folder
expDate <- str_sub(XML:::xmlAttrsToDataFrame(xml["//ExportDate"])[1,1], 0, -7)

#import nutritional and bodyweight data from excel file
df_bw <- read_excel(paste0(cd,"/Nutritional_Data.xlsx", sep = ''), sheet = 1) # body weight and others
df_nutrition <- read_excel(paste0(cd,"/Nutritional_Data.xlsx", sep = ''), sheet = 2) #nutritional data

#import training data from .txt file

#end of importation

#start data cleaning
#clean workout df
df_workout <- XML:::xmlAttrsToDataFrame(xml["//Workout"]) %>%
  select(1 , 2, 6, 12, 13) %>% #keep selected cols
  rename(type = workoutActivityType) %>% #simplify col name
  mutate(date = as.Date(startDate),
         type = substr(type, 22, nchar(type)),
         startDate = str_sub(startDate, 1, nchar(startDate) - 5),
         endDate = str_sub(endDate, 1, nchar(endDate) - 5),
         duration.inMin = round(as.numeric(duration), digits = 1),
         net.eat.inCal = round(as.numeric(totalEnergyBurned), digits = 1),
         interval = interval(startDate, endDate)) %>%
  select(date, type, interval, duration.inMin, net.eat.inCal)
#clean and summarize entire record
df_record <- XML:::xmlAttrsToDataFrame(xml["//Record"]) %>% #create df from health record
  select(-3,-4,-5,-9) %>% # delete unneeded cols
  mutate(type = substr(type , 25, nchar(.)), #simplify naming for type of data
         startDate = str_sub(startDate, 1, nchar(startDate) - 5), #format startDate
         endDate = str_sub(endDate, 1, nchar(endDate) - 5), #format endDate
         sourceName = case_when( #simplify naming using a case_when fxn
           sourceName == unique(sourceName)[2] ~ "watch",
           sourceName == unique(sourceName)[4] ~ "phone",
           TRUE ~ sourceName)) #keep sourceName if it doesn't match above criteria

#create sleep data frame using steps
sleep.summary <- df_record[df_record$type == "SleepAnalysis",] %>%
  select(-1) %>% #remove col 3
  filter(sourceName != 'phone' & str_detect(value,'Asleep')) %>%  #filter out sources recorded by 'phone' or 'InBed' vals
  mutate(status = substr(value, 29,nchar(value)), #edit value col into useable string values
         sleep.inHrs = round(as.numeric(difftime(endDate, startDate, units = "hours")), digits = 3),
         interval = interval(startDate, endDate)) %>% # calculate new sleep.in.hrs col
  select(-sourceName, -startDate, -endDate, -value)
  #create nightIndex col using lag time between observations
sleep.summary$index <- sleep.summary %>%
  {difftime(int_end(.$interval), lag(int_end(.$interval), default = int_end(.$interval[1])), units = "hours")} %>%
  {cumsum(ifelse(. > 6,1,0))}
#summarize data to find quantity of sleep
sleep.summary <- sleep.summary %>%
  mutate(date = as.Date(int_end(interval))) %>%
  group_by(index) %>%
  summarise(date = max(date),
            sleep.inHrs = round(sum(sleep.inHrs), digits = 1),
            interval = interval(min(int_start(interval)), max(int_end(interval)))) %>%
  {. ->> df_sleep} %>%
  group_by(date) %>%
  summarise(sleep.inHrs = sum(sleep.inHrs))

#extract step count data and merge with sleep df
df.ofApollo <- df_record[df_record$type == "StepCount",] %>% #extract stepCount data from df
  select(-4) %>% 
  filter(sourceName == 'watch') %>%
  group_by(date = as.Date(startDate)) %>%
  summarise(numSteps = sum(as.numeric(value))) %>%
  {merge(x = sleep.summary, y = ., by = 'date', all = TRUE)}

#save heart rate data type to new dataframe
df_hr <- df_record[df_record$type == "HeartRate",] %>%
  select(-1,-2,-4) %>% #delete col 2
  mutate(value = round(as.numeric(value), digits = 1), # format value col
         startDate = as.POSIXct(startDate, format="%Y-%m-%d %H:%M:%S")) # format endDate col
#use intervals to create new col didExer & wasAsleep
# create new columns and store NA value in them
df_hr$didExer <- NA
df_hr$wasAsleep <- NA
#initialize index wasAsleep and didExer Cols
df_hr$didExer.index <- NA 
df_hr$wasAsleep.index <- NA 
i <- 1 #initialize index var for intervals
iPrime <- FALSE # initialize bool var for conditional i increment
for(j in seq_len(nrow(df_hr))){
  ifelse((df_hr$startDate[j]-5*3600) %within% df_workout$interval[i],
         {iPrime <- TRUE
         df_hr$didExer[j] <- TRUE
         df_hr$didExer.index[j] <- i},
         {if(iPrime){
           i <- i+1
           iPrime <- FALSE
         }
           df_hr$didExer[j] <- FALSE
           df_hr$didExer.index[j] <- -1}) 
} # check if they are within workout interval and assign value if yes
i <- 1
iPrime <- FALSE
for(j in seq_len(nrow(df_hr))){
  ifelse((df_hr$startDate[j]-5*3600) %within% df_sleep$interval[i],
         {iPrime <- TRUE
         df_hr$wasAsleep[j] <- TRUE
         df_hr$wasAsleep.index[j] <- i},
         {if(iPrime){
           i <- i+1
           iPrime <- FALSE
         }
           df_hr$wasAsleep[j] <- FALSE
           df_hr$wasAsleep.index[j] <- -1}) 
} # repeat process from above for sleep col
# repeat process from above
df_hr <- df_hr %>%
  mutate(., didExer.index = ifelse(is.na(didExer.index), -1, didExer.index)) # replace NAs with FALSE
df_hr <- df_hr %>%
  mutate(., wasAsleep.index = ifelse(is.na(wasAsleep.index), -1, wasAsleep.index)) #replace NAs with FALSE
# create hr summary A where no sleep or exercise occurred
df.ofApollo <- df_hr %>%
  filter(!wasAsleep & !didExer) %>%
  group_by(date = as.Date(startDate)) %>%
  summarise(avg.hr.bpm = round(mean(value), digits = 1),
            var.hr.bpm = round(var(value), digits = 1),
            sd.hr.bpm = round(sd(value), digits = 1),
            min.hr.bpm = min(value),
            max.hr.bpm = max(value)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}
#create hr summary B where sleep was occurring
df.ofApollo <- df_hr %>%
  filter(wasAsleep) %>%
  group_by(date = as.Date(startDate)) %>%
  summarise(avg.sleep.hr.bpm = round(mean(value), digits = 1),
            var.sleep.hr.bpm = round(var(value), digits = 1),
            sd.sleep.hr.bpm = round(sd(value), digits = 1),
            min.sleep.hr.bpm = min(value),
            max.sleep.hr.bpm = max(value)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}
#create hr summary C where exercise was occurring
df.ofApollo <- df_hr %>%
  filter(didExer) %>%
  group_by(date = as.Date(startDate)) %>%
  summarise(avg.exer.hr.bpm = round(mean(value), digits = 1),
            var.exer.hr.bpm = round(var(value), digits = 1),
            sd.exer.hr.bpm = round(sd(value), digits = 1),
            min.exer.hr.bpm = min(value),
            max.exer.hr.bpm = max(value)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}

#create hrv summary df
df.ofApollo <- df_record[df_record$type == "HeartRateVariabilitySDNN",] %>%
  select(-sourceName, -type,) %>%
  transmute(interval = interval(startDate, endDate),
         HRVinSDNNvalue.inMs = round(as.numeric(value), digits = 1)) %>%
  group_by(date = as.Date(int_start(interval))) %>%
  summarise(avg.hrv.inMs = round(mean(HRVinSDNNvalue.inMs), digits = 1)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}

#create df oxygensaturation data frame
df.ofApollo <- df_record[df_record$type == "OxygenSaturation",] %>%
  select(-type, -sourceName) %>% # delete second col
  transmute(value = as.numeric(value), date = as.Date(startDate)) %>%
  group_by(date) %>%
  summarise(avg.o2sat.inPerc = round(mean(value), 2) * 100,
            min.o2sat.inPerc = min(value) * 100,
            max.o2sat.inPerc = max(value) * 100) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}

#create basal energy burned df
df.ofApollo <- df_record[df_record$type == "BasalEnergyBurned",] %>%
  select(-type, -sourceName) %>% #delete second col
  transmute(interval = interval(startDate, endDate), value = as.numeric(value)) %>%
  {. ->> df_beb} %>%
  group_by(date = as.Date(int_start(interval))) %>%
  summarise(basalEnergy.inCal = round(sum(value), 1)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}

#active energy burned df
df.ofApollo <- df_record[df_record$type == "ActiveEnergyBurned",] %>%
  select(-type, -sourceName) %>% #delete second col 
  transmute(interval = interval(startDate, endDate), value = as.numeric(value)) %>%
  {. ->> df_aeb} %>%
  group_by(date = as.Date(int_start(interval))) %>%
  summarise(activeEnergy.inCal = round(sum(value), 1)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)}
rm(df_record) #df_record is no longer needed

#calculate change in bw in df.bw dataframe
df_bw <- df_bw %>%
  mutate(change.in.bwInLbs = bw.in.lbs - lag(bw.in.lbs), .after = bw.in.lbs)

#merge all data from excel import with df.ofApollo
df.ofApollo <-  merge(x = df_bw, y = df_nutrition, by = "date" , all = TRUE) %>%
  mutate(across(2:28, round, 1),
         date = as.Date(date)) %>% # round all numbers to one decimal place
  merge(., y = df.ofApollo, by = 'date', all =TRUE)
rm(df_bw) #delete unneeded data frames
rm(df_nutrition)

#calculate work volume
setwd(pd)
df.ofApollo <- read_csv(csv) %>%
  mutate(date = as.Date(date, format = '%m/%d/%Y'),
         numSets = select(., s1:s5) %>% apply(1, function (x) sum(!is.na(x))),
         numReps = select(., s1:s5) %>% apply(1, sum, na.rm = TRUE),
         workVol = weight * numReps) %>%
  {. ->> df_exer} %>%
  group_by(date) %>%
  select(-exercise:-s5) %>%
  summarise(workVol.inLbs = sum(workVol),
            numReps = sum(numReps),
            numSets = sum(numSets)) %>%
  {merge(x = df.ofApollo, y = ., by = 'date', all = TRUE)} %>%
  mutate(workVol.inLbs = ifelse(is.na(workVol.inLbs), 0, workVol.inLbs),
         numReps = ifelse(is.na(numReps), 0, numReps),
         numSets = ifelse(is.na(numSets), 0, numSets))
setwd(cd)

#delete row where it was the 23rd of Aug
df.ofApollo <- df.ofApollo[-3,]

#save cleaned data frames to designated folder
setwd(paste(file, 'data',sep = ''))
write.csv(sleep.summary, 'df_sleepSummary.csv')
write.csv(df.ofApollo, "df.ofApollo.csv")
write.csv(df_aeb, "df_activeEnergyBurned.csv")
write.csv(df_beb, "df_basalEnergyBurned.csv")
write.csv(df_exer, "exercise_df.csv")
write.csv(df_hr, "heartRate_df.csv")
#save .RData to separate folder
setwd(paste(file, 'RData', sep = ''))
save(df.ofApollo, file = "df.ofApollo.RData")
save(df_hr, file = "df.hr.RData")
save(sleep.summary, file = "df.sleep.summary.RData")
save(df_aeb, file = 'aeb.RData')
save(df_beb, file = 'beb.RData')
save(df_exer, file = 'exer.RData')
setwd(cd)
#delete rest of environment
rm(list = ls())
