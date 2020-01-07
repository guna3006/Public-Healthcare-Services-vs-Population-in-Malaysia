# QUESTION 1
# install.packages("dplyr")
# install.packages("magrittr")
library(dplyr)
library(magrittr)

# 4.	a. Read the CSV dataset available for this assignment.
setwd("/Users/gunasegarran/Documents/Assignment-1-20191013/")
getwd()

Q1_data <- read.csv("/Users/gunasegarran/Documents/Assignment-1-20191013/Q1-dataset.csv")
Q1_data

# 5.	b. The dataset include "na", "NA" and "--" value, you need<— to clean that dataset, as mentioned above.
is.na(Q1_data)

is.numeric(Q1_data$MonthlyCharges)
Q1_data$MonthlyCharges <- ifelse(is.na(Q1_data$MonthlyCharges), mean(Q1_data$MonthlyCharges, na.rm=TRUE), Q1_data$MonthlyCharges)
Q1_data$MonthlyCharges

is.numeric(Q1_data$TotalCharges)
Q1_data$TotalCharges <- as.numeric(as.character(Q1_data$TotalCharges))
Q1_data$TotalCharges <- ifelse(is.na(Q1_data$TotalCharges ), mean(Q1_data$TotalCharges , na.rm=TRUE), Q1_data$TotalCharges)
Q1_data$TotalCharges

is.na(Q1_data$PaymentMethod)
Q1_data <- Q1_data %>% mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod ==  "--", NA))
Q1_data <- Q1_data %>% mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod ==  "", NA))
Q1_data$PaymentMethod <- as.character(Q1_data$PaymentMethod)
Q1_data <- Q1_data %>% mutate(PaymentMethod = replace(PaymentMethod, is.na(PaymentMethod), "not-known"))
Q1_data$PaymentMethod

Q1_data

# 6.	c. Store the new dataset into CSV format.
write.csv(Q1_data,'/Users/gunasegarran/Documents/Assignment-1-20191013/Q1-dataset_new.csv', row.names = FALSE)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------


survey <- read.table("http://www.andrew.cmu.edu/user/achoulde/94842/data/survey_data.csv", header=TRUE, sep =",")
survey

program <- table(survey$Program)

# (a)	How many survey respondents are from MISM or Other?
MISM_Other <- program["MISM"] + program["Other"]
names(MISM_Other)<-paste("MISM or Other")
MISM_Other

# 5. (b)	What % of survey respondents are from PPM?
PMM <- program["PPM"] / (program["MISM"] + program["Other"] + program["PPM"]) * 100
names(PMM)<-paste("PMM %")
PMM

# 6. (c)	Use $ notation to pull the OperatingSystem column from the survey data
survey$OperatingSystem

# 7  (d) Do the same thing with [,] notation, referring to OperatingSystem by name
survey[,"OperatingSystem"]


# 8. (e) Repeat part (d), this time referring to Operatingsystem by column number
survey[,4]

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Question 1

# a
library(tidyverse)
library(dplyr)
library(reshape)
answerA <- seq(from=50,to=200,by=20)
answerA


# b
answerB <- seq(from=5,to=20,length.out = 8)
answerB

# c
contentC <-c(1,0,1,0)
answerC <- rep(contentC,times=3)
answerC

# d
a <- sum(answerA>150)
b<- sum(answerB<6)
answerD <- sum(a&b)
# Question 2

# a
getwd()
setwd("/Users/gunasegarran/Desktop/R Programming/PastYear/WQD7004_2_2019/")
myDF <- read.csv(file = '2008.csv')
head(myDF)


# b
answerB <- sum(myDF$Origin=="IND" & myDF$Dest=="ORD")
answerB

# c
myTUPOrigin=subset(myDF,myDF$Origin=="TUP")
answerC <- mean(myTUPOrigin$DepDelay)
answerC

# d
answerD <- max(myTUPOrigin$FlightNum)
answerD

# e
ca=as.data.frame(table(data$CancellationCode))
ca1=ca[order(ca$Freq, decreasing = TRUE),]

answerE <- ggplot(data=ca1, aes(x = reorder(Var1, -Freq), y = Freq, fill=Var1)) +
  geom_bar(stat="identity")+geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  scale_x_discrete(name ="Cancellation Code")

answerE


# f
answerF<- min(myTUPOrigin$CarrierDelay)
answerF
# Question 3

# a
data(mtcars)
answerA <- tapply(mtcars$hp, mtcars$mpg, median)

# b
names(mtcars)[names(mtcars)=="hp"] <- "Ghp"
answerB <- names(mtcars)

# c
ave_function <- function(dataset) {
  average <- apply(dataset, 2, median)
  return(average)
} 
ave_function(mtcars)
# Question 4

# a
# create the dataframe
q4<-data.frame(StudentName=c("Andrey","John","Ronald","Jacky","Fred"), History=c("A+","A-","B+","A+","A"), Math=c("C+","B-","A-","C","D"), Science=c("B","B+","B","A","B"))
library(tidyr)
answerA <- arrange(gather(q4,Class,Grade,History:Math:Science),StudentName)
answerA

# b
answerB <- arrange(spread(answerA, Class, Grade),StudentName)
answerB
# Question 5

# a
library(MASS)
answerA <- hist(Cars93$Horsepower,breaks=5,xlab="cars93$horsepower",main=("hist()plot"))

# b
answerB <- truehist(Cars93$Horsepower,main=("treuhist()plot"))
# Question 6

# a
library(dplyr)
answerA <-table(comics$align,comics$gender)

# b
answerB <- comics %>% filter (align!="Reformed Criminals") %>% droplevels()

# c
ggplot(comics,aes(x=gender,fill=align))+geom_bar()
ggplot(comics,aes(x=gender))+geom_bar(aes(fill=align))
ggplot(comics,aes(x=gender,y=align))+geom_bar(stat="identity")

# d
answerD <- prop.table(answerA)
# Question 7

# a
answerA <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1946)

# b
answerB <- answerA %>%
  summarise(
    total = n(),
    percent_yes = mean(vote == 1)
  )

# c
answerC <- ggplot(by_year, aes(year, percent_yes)) +
  geom_point() +
  geom_smooth()
# Question 8

# a
getwd()
setwd("/Users/gunasegarran/Desktop/R Programming/PastYear/WQD7004_2_2019/")
data<-read.csv("50_Startups.csv")
data<-data[,3:5]
library(caTools)
set.seed(123)
split=sample.split(data$Profit,SplitRatio = 0.75)
training_set=subset(data,split==TRUE)
test_set=subset(data,split==FALSE)


# b
answerB <- scatter.smooth(x=data$Marketing.Spend, y=data$Profit, main="Marketing.Spend ~ Profit")
# Question 1
# a
f<-function(x) {
  g<-function(y){
    y+z
  }
  z<-4
  x+g(x)
}
z<-10
f(4)

# b
x<-0
if(x<0){
  print("MNM")
} else if(x>0){
  print("NMN")
} else
  print("MMM")

# c
i<-1
while(i<6) {
  print(i)
  i=i+2
}

# d
i<-1:5
for (val in x) {
  if (val==3) {
    break
  }
  print(val)
}
# Question 2

# create the dataframe
q2<-data.frame(storm=c("Alberto","Alex","Alison","Ana","Arlene","Arthur"),
               wind=c(110,45,65,40,50,45),
               pressure=c(1007,1009,1005,1013,1010,1010),
               Date=c("2000-8-12","1998-07-30","1995-06-04","1997-07-01",
                      "1999-06-13","1996-06-21"))


# a
answerA <- dplyr::select(q2,-storm)
answerA


# b
answerB <- filter(q2,wind>50) 
answerB

# c
answerC <- mutate(q2,ratio=pressure/wind) 
answerC <- mutate(answerC,inverse=ratio^-1) 
answerC

#d
answerD <- arrange(q2,desc(wind))
answerD
# Question 3

# create the dataframe
q3<-data.frame(Grade=c("A","B","C"),Male=c(10,20,30),Female=c(15,15,35))
# a
library(tidyr)
answerA <- arrange(gather(q3,Sex,Count,Male:Female),Grade)
answerA

# b
library(readr)
contentB <- c("20180624", "06-24-2018", "24/06/2018", "24 June 2018")
answerB <- parse_date(contentB, "%Y-%b-%d")

# c
contentC <- c(" Flip", "Nick ","Jonathan")
answerC <- trimws(contentC)
answerC

# d
library(stringr)
contentD <- c("234585W","8823453Q","994Z")
answerD <- str_pad(contentD, 10, side = "left", pad = "0")
answerD
# Question 4

# a
#1 Standard plots - standard statistical plots, including scatterplots, boxplots, histograms, barplots, piecharts, and basic 3D plots.
#2 Trellis plots - traditional statistical plots, using lattice  library
#3 Special-purpose plots - provides a set of functions for producing graphical output primitives, such as lines, text, rectangles, and polygons
#4 General graphical scenes - ability to embed tabular arrangements of text as graphical elements within a plot

# b
library(ggplot2)
Animals2 <- data_frame(brains=c(0,1000,2000,3000,4000,5000),body=c(0,20000,40000,60000,80000,10000))
answerB <- ggplot(Animals2, aes(x=Animals2$body, y=Animals2$brains)) + geom_point()

# c
attach(mtcars)
answerC <- plot(wt, mpg)

# d
answerD <- par(mfrow=c(3,4))
# Question 5

# a
answerAi <- class(comics$align)
answerAii <- class(comics$gender)

# b
answerB <- table(comics$align,comics$gender)
answerB

# c
answerC <- ggplot(comics,aes(x=gender,fill=align))+geom_bar()

# d
answerD <- prop.table(answerB)
answerD
# Question 6

# a
answerA <- ncbirths %>% ggplot(aes(x = weeks, y = weight)) + geom_point() 

# b
answerB <- ncbirths %>% summarize(N = n(), r = cor(weight, mage))

# c
answerC <- ncbirths %>% ggplot(aes(x = weeks, y = weight)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
