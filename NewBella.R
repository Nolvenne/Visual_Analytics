#install all package
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("janitor")
install.packages("plotrix")

#Load all required package 
library("tidyverse") #for data manipulation
library("lubridate") #for data and time
library("ggplot2") #for plot
library("dplyr") #for data cleaning
library("here") #to load csv file
library("skimr") #for skimming data set
library("janitor") # for cleaning data
library("plotrix") #to plot in 3D


#load Dataset
dailyActivity<- read_csv("/Users/meme/Desktop/GOOGLE COURS/archive (1)/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
hourlyIntensity<-read_csv("/Users/meme/Desktop/GOOGLE COURS/archive (1)/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyIntensities_merged.csv")
minuteSleep<-read.csv("/Users/meme/Desktop/GOOGLE COURS/archive (1)/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv")
weigthLogInfo<-read_csv("/Users/meme/Desktop/GOOGLE COURS/archive (1)/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")

#Explore dataset
head(dailyActivity)
head(hourlyIntensity)
head(minuteSleep)
head(weigthLogInfo)
glimpse(dailyActivity)
glimpse(hourlyIntensity)
glimpse(minuteSleep)

#Data Cleanning

clean_names(dailyActivity)
clean_names(hourlyIntensity)
clean_names(minuteSleep)
clean_names(weigthLogInfo)

#Change activity date in date format
ReportActivity<- dailyActivity %>%
  mutate(ActivityDate=as.Date(ActivityDate, format="%m/%d/%Y"),
         TotalMinutes=VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes,
         Weekday=weekdays(as.Date(ReportActivity$ActivityDate, "%m/%d/%Y")), 
         ID=as.character(Id))
View(ReportActivity)

#Change Activity hour of HourlyIntensity and add weekdays
Intensity<-hourlyIntensity %>% 
  mutate(ActivityHour=as.POSIXct(ActivityHour, format="%m/%d/%Y %I:%M:%S %p"),
         Weekday=weekdays(as.Date(hourlyIntensity$ActivityHour, "%m/%d/%Y %I:%M:%S %p")),
         ID=as.character(Id))
View(Intensity)
#Change Activity hour in minute sleep
SleepTime<-minuteSleep %>% 
  mutate(date=as.POSIXct(date, format="%m/%d/%Y %I:%M:%S %p"),
         Weekday=weekdays(as.Date(minuteSleep$date, "%m/%d/%Y %I:%M:%S %p")),
         ID=as.character(Id))
View(SleepTime)

#Change Activity hour in weightLogInfo
WeightInfo<-weigthLogInfo %>% 
  mutate(Date=as.POSIXct(Date, format="%m/%d/%Y %I:%M:%S %p"),
         Weekday=weekdays(as.Date(weigthLogInfo$Date, "%m/%d/%Y %I:%M:%S %p")),
         ID=as.character(Id))
View(WeightInfo)


#Pie Chart
pie(TimeSlices, labels=lbls,labelcex = 0.90, main = "Pie chart of weekly activity from March to April 2016")

#split the column date into two; date and time
Intensity<-separate(Intensity,ActivityHour, into = c('Date','Hours'), sep =' ')
View(Intensity)
#split the column date into two; date and time
SleepTime<-separate(SleepTime, date, into=c('Date', 'Hours'), sep=' ')
View(SleepTime)
#split the column date into two; date and time
WeightInfo<-separate(WeightInfo,Date, into=c('Date', 'Hours'), sep = ' ')
View(WeightInfo)


#Visualization

#let see how the time is slipt for workout
ActivityTime<-ReportActivity %>% 
  summarise(TotalActiveTime=sum(VeryActiveMinutes),
            TotalFairlyActiveTime=sum(FairlyActiveMinutes),
            TotalLightlyActiveTime=sum(LightlyActiveMinutes),
            TotalSedentaryTime=sum(SedentaryMinutes))
SlideTime<-c(ActivityTime$TotalActiveTime,
             ActivityTime$TotalFairlyActiveTime,
             ActivityTime$TotalLightlyActiveTime,
             ActivityTime$TotalSedentaryTime)
lbls<-c('Very Active Time','Fairly Active Time','Lightly Active Time','Sedentary Time')
pct<-round(SlideTime/sum(SlideTime)*100)
lbls<-paste(lbls, pct)
#add percentage to the Pie Label
lbls<-paste(lbls,"%", sep="")
pie(SlideTime, labels=lbls,radius=1,col=rainbow(length(lbls)), cex=0.7, main = "Different time during workout")
#the can see that the sedentary time is biger than other activity time



#1: let check the correlation within the activity time and the lost of calories
CorActivity<-cor(ReportActivity$TotalSteps, ReportActivity$Calories)
CorActivity<-paste("A possitive correlation of", round(CorActivity*100))
lbls<-paste(CorActivity,"%", sep="")
ggplot(ReportActivity, aes(x=TotalSteps, y=Calories))+geom_point()+geom_smooth()+
  labs(title = "Correlation between Total Steps and Calories lost", subtitle = "for 35 User", caption = "Data Source:FitBit Fitness Tracker Data")+
  annotate("text", x=20000, y=5000, label=lbls, color="dark violet")


#2: Let determine on wich Weekday most user excerce

WeeklyAct<-ReportActivity%>% 
  group_by(Weekday) %>% 
  summarise(Aveg_Totalstep=mean(TotalSteps))
slices<-WeeklyAct$Aveg_Totalstep
lbls<-WeeklyAct$Weekday
pct<-round(slices/sum(slices)*100)
lbls<-paste(lbls, pct)
#add percentage to the Pie Label
lbls<-paste(lbls,"%", sep="")
pie3D(slices, labels=lbls,explode=0.05,labelcex = 0.90, main = "Pie chart of weekly activity from March to April 2016")
#this pie revealed that most user excersice on wednesday base on the average number of steps

#3: let identify which hours most user workout
Workout<-Intensity %>% 
  group_by(Hours) %>% 
  summarise(Avegintensity= mean(TotalIntensity))
  
ggplot(data=Workout, aes(x=Hours, y=Avegintensity))+
  geom_col(fill="dark violet")+
  theme(axis.text.x = element_text(size=7, angle = 50))
#we can see that the most user workout mostly for 6 pm to 7pm and 11am to 12 pm

#4:let see the correlation between workout time and sleeping time
CorTime<-cor()