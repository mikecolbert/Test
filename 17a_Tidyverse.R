rm(list=ls())
### Tidyverse ###

install.packages("tidyverse")
library(tidyverse)
install.packages("janitor")
library(janitor)
library(dplyr)

#Import data
#readr
ufo <- read_csv("ufo_tidy.csv")
str(ufo) #data is read in as a tibble (https://r4ds.had.co.nz/tibbles.html)

ufo2<-read.csv("ufo_tidy.csv", na.strings=c("-",""," ","NA","N/A"), encoding="UTF-8")
str(ufo2)

print(ufo)
print(ufo2)


ufo <-ufo2


#Clean column names
#janitor
ufo2 <- ufo %>% clean_names()
ufo3 <- clean_names(ufo, "snake")
ufo4 <- clean_names(ufo, "lower_camel")


#Rename columns
#tidyr
ufo5 <- rename(ufo3, witnesses = number_of_witnesses) 

ufo6 <- rename_with(ufo, tolower)

ufo <- ufo5

#remove data frames
rm(ufo2)
rm(ufo3, ufo4, ufo5, ufo6)


#Date
#lubridate
str(ufo)
library(lubridate)
ufo$sighting_date <- mdy(ufo$sighting_date)
ufo$posted_date <- dmy(ufo$posted_date)
str(ufo)


#Remove duplicate rows in a data frame
#dplyr

duplicated(ufo)
sum(duplicated(ufo))
unique(ufo)
distinct(ufo)

ufo2 <- distinct(ufo)

ufo <- ufo2
sort(table(ufo$city), decreasing=TRUE)


str(ufo)

unique(ufo2$duration)

#Mutate
#dplyr
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "1-2 minutes", "1.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "2-3 minutes", "2.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "3-4 minutes", "3.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "5-7 minutes", "6 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "7-8 minutes", "7.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "8-10 minutes", "9 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "7 & 6 minutes", "6.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "8-10 minute", "9 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "2-3 seconds", "2.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "13-20 seconds", "16.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "20-30 seconds", "25 seconds"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "3-5 minutes", "4 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "5:00", "5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "15-20 minutes", "17.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "45-60 seconds", "52.5 seconds"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "10-15 minutes", "17.5 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "12:30 a.m. 3:30 a.m.", "3 hours"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "0:00:17 seconds", "17 seconds"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "3 minutes each", "3 minutes"))
ufo2$duration[(ufo2$duration =="A few seconds")]<-NA
ufo2$duration[(ufo2$duration =="until sunrise")]<-NA
ufo2$duration[(ufo2$duration =="Ongoing")]<-NA
ufo2$duration[(ufo2$duration =="0.05")]<-NA
ufo2$duration[(ufo2$duration =="1")]<-NA
ufo2$duration[(ufo2$duration =="UNKNOWN")]<-NA
ufo2$duration[(ufo2$duration =="Unknown")]<-NA
ufo2$duration[(ufo2$duration =="seconds")]<-NA
ufo2$duration[(ufo2$duration =="I don't know")]<-NA
ufo2$duration[(ufo2$duration =="30- NA")]<-NA
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "~1 hour", "1 hour"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "~2 hours", "2 hours"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "~3 minutes", "3 minutes"))
ufo2 <- ufo2 %>% mutate(duration = replace(duration, duration == "~5 seconds", "5 seconds"))

#Split a column into 2 columns
#dplyr
ufo2 <- separate(ufo2, duration, into = c("duration2", "time_frame"), sep = " (?=[^ ]+$)")

#Mutate multiplier to get duration in seconds
#dplyr
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "minutes" | time_frame == "minute", 60, time_frame))
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "hours" | time_frame == "hour", 3600, time_frame))
ufo2 <- mutate(ufo2, time_frame = ifelse(time_frame == "seconds" | time_frame == "second", 1, time_frame))

ufo2 <- mutate(ufo2, duration_seconds = ifelse(duration2 > 0, as.numeric(duration2) * as.numeric(time_frame), duration2))

#Remove a column
#dplyr
ufo2 <- select(ufo2, -duration2)
ufo2 <- select(ufo2, -time_frame)

#Reorder columns
#dplyr

ufo2 <- select(ufo2, sighting_date, city, state, duration_seconds, shape, intensity, summary, witnesses, posted_date)


ufo <- ufo2



#Filter and sort data
#dplyr
IL_ufo <- filter(ufo, state=="IL")

IL_ufo <- arrange(IL_ufo, sighting_date)
IL_ufo <- arrange(IL_ufo, desc(sighting_date)) #desc() = descending order

IL_ufo %>% group_by(city) %>% summarize(mean_sightings = mean(witnesses, na.rm=T))

IL_ufo %>% count(city)


#Mutate
#dplyr
IL_ufo <- mutate(IL_ufo, witness_2 = witnesses * 200)
IL_ufo <- mutate(IL_ufo, posting_delay = as.duration(interval(IL_ufo$sighting_date, IL_ufo$posted_date))/as.duration(days(1)) )



#Remove a column
#dplyr
IL_ufo <- select(IL_ufo, -witness_2) #can't modify tibble




write.table(ufo, "ufo_tidy_edited.csv", sep=",", fileEncoding="UTF-8", row.names=FALSE)








