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

#Load the data set
dailyReport1<- read_csv("archive (1)/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
dailyReport2<- read_csv("archive (1)/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

#Data exploration
glimpse(dailyReport1) #to summarize the data set
glimpse(dailyReport2)
skim_without_charts(dailyReport1) #for skimming the data
skim_without_charts(dailyReport2)

#DATA CLEANING
#check the column name
clean_names(dailyReport1)
clean_names(dailyReport2)

#change the date data type
dailyReport1 <- dailyReport1 %>%
  mutate(ActivityDate=as.Date(ActivityDate, format="%m/%d/%Y"))
View(dailyReport1)
glimpse(dailyReport1)
dailyReport2 <- dailyReport2 %>%
  mutate(ActivityDate=as.Date(ActivityDate, "%m/%d/%Y"))
glimpse(dailyReport2)

#filter by total Steps
Report1<-dailyReport1 %>% 
  filter(TotalSteps>0)

Report2<-dailyReport2 %>% 
  filter(TotalSteps>0)

#arrange in descending order
Report1<-arrange(Report1, -Calories)
Report2<-arrange(Report2, -Calories)
#add a new colomn

#let determine which day of the week user are more active
Plot2<-Report1 %>% 
  mutate(TotalMinutes=VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes,
         Weekday=weekdays(as.Date(Report1$ActivityDate, "%m/%d/%Y")),
         ID=as.character(Id))
Plot2

WeeklyAct<-Plot2 %>% 
  group_by(Weekday) %>% 
  summarise(Aveg_Totalstep=mean(TotalSteps))

ggplot(data = WeeklyAct, aes(x=Weekday, y=Aveg_Totalstep))+geom_col(fill="dark blue")

#summary to determine the statistic all selected column
PlotActivity<-Plot2 %>% 
  select(TotalSteps,TotalDistance, Calories, TotalMinutes) %>% 
  summary()
PlotActivity

#plot a pie
slices<-WeeklyAct$Aveg_Totalstep
lbls<-WeeklyAct$Weekday
pct<-round(slices/sum(slices)*100)
lbls<-paste(lbls, pct)
#add percentage to the Pie Label
lbls<-paste(lbls,"%", sep="")
pie3D(slices, labels=lbls,explode=0.05,labelcex = 0.90, main = "Pie chart of weekly activity from March to April 2016")
 


Plot<-Report1 %>% 
  group_by(ActivityDate) %>% 
  summarise(MeanStep=mean(TotalSteps))

#plot the average step per day
  ggplot(data=Plot, aes(x=ActivityDate, y=MeanStep))+
  geom_col(fill="dark violet")

#let see which weekly date each user perform the best
Plot3<-Plot2 %>% 
    group_by(ID, Weekday) %>% 
  summarise(Steps=sum(TotalSteps),Distance=sum(TotalDistance), Minutes=sum(TotalMinutes))
  
ggplot(data=Plot3,aes(x=ID, fill=Weekday))+geom_bar()+
theme(axis.text.x = element_text(size=7, angle = 50)) #we can see that Friday and monday are the day user exerce the most.
  

#create a new data set
Calory<-Plot2 %>% 
  group_by(ID) %>% 
  summarise(Mean_Calory=mean(Calories),MeanStep=mean(TotalSteps), Mean_TotalMinutes=mean(TotalMinutes))

summarize(Plot2) %>% 
  select(Calo)
Calory %>% 
  arrange(Mean_Calory)

#plot each user calories lost
ggplot(data=Calory, aes(x=ID, y=Mean_Calory))+ geom_col(fill="dark blue")+ 
  theme(axis.text.x = element_text(size=7, angle = 50))+
  labs(title = "User vs Calories", subtitle = "Average calories lost per month" , caption = " FitBit dataset") #labels on plot

#Plot activity time vs calories
ggplot(data=Plot2, aes(x=TotalMinutes, y=Calories))+geom_point()+geom_smooth()+geom_jitter()+
  labs(title = "Total Minute vs Calories")


       
#group by ID
Report1 %>% 
  group_by(Id) %>% 
  summarise(Mean_Calories=mean(Calories), mean_TSteps=mean(TotalSteps))
Report2 %>% 
  group_by(Id) %>% 
  summarise(Mean_Calories=mean(Calories), mean_TSteps=mean(TotalSteps))

#VISUALIZATION
ggplot(data=Report1)+
  geom_point(mapping=aes(x=TotalSteps, y=Calories, color=Id))+
annotate("text",x=250, y=3000, label="the peak")
ggplot(data=Report2)+
  geom_point(mapping=aes(x=TotalSteps, y=Calories))

#Bar
ggplot(data=Report1)+
  geom_col(mapping = aes(x=Id, y= Calories, fill=" dark violet"))+
  labs(title ="Number of BitFit Users", subtitle = "03/12/2016 to 04/12/2016", caption ="Source:Data from Kaggel")

ggplot(data=Report2)+
  geom_bin_2d(mapping = aes(x=Calories, y=ActivityDate))+
  labs(title ="Calories vs Daily Report", subtitle = "04/12/2016 to 05/12/2016", caption ="Source:Data from Kaggel")








