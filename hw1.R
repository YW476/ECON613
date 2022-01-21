####HW1 Yifan Wang
library("REAT", lib.loc="~/Library/R/4.0/library")
library(ggplot2)
library(tidyverse)
library(dplyr)
##EX1
#1)
dathh2007 <- read.csv("~/Desktop/Data/dathh2007.csv")
dathh2007=data.frame(dathh2007)
summary(dathh2007$X)#10498
#2)
dathh2005 <- read.csv("~/Desktop/Data/dathh2005.csv")
table(dathh2005$mstatus) #3374
#3)
datind2008 <- read.csv("~/Desktop/Data/datind2008.csv")
datind2008=data.frame(datind2008)
summary(datind2008)
#4)
datind2016 <- data.frame(read.csv("~/Desktop/Data/datind2016.csv"))
age1=datind2016[datind2016$age>=25,]
age2=age1[age1$age<=35,]
nrow(age2)#2765
#5)
datind2009 <- read.csv("~/Desktop/Data/datind2009.csv")
datind2000=data.frame(datind2009)
table(data.frame(datind2009$gender,datind2009$profession))
#6)
datind2005 <- read.csv("~/Desktop/Data/datind2005.csv")
wage<-na.omit(datind2005$wage)
mean(wage)
sd(wage)
quantile(wage, probs = seq(.1, .9, by = .1))
gini(datind2005$wage)#0.6671654

wage1<-na.omit(datind2009$wage)
mean(wage1)
sd(wage1)
quantile(wage1, probs = seq(.1, .9, by = .1))
gini(datind2009$wage)#0.6504572
#7)
datind2010 <- read.csv("~/Desktop/Data/datind2010.csv")
hist(datind2010$age)
data1<-data.frame(datind2010$gender,datind2010$age)
ggplot(data1,aes(x=datind2010.age))+geom_histogram()+facet_grid(.~datind2010.gender)+theme_bw()
#8)
datind2011 <- read.csv("~/Desktop/Data/datind2011.csv")
dathh2011 <- read.csv("~/Desktop/Data/dathh2011.csv")
dathh2011=dathh2011[dathh2011$location=="Paris", c("idmen", "location")]
data2011=merge(datind2011, dathh2011,by ="idmen")
nrow(data2011)

##EX2
#1)
datind2004 <- data.frame(read.csv("~/Desktop/Data/datind2004.csv", colClasses = c(idmen="character", idind="character")))
datind2005 <- data.frame(read.csv("~/Desktop/Data/datind2005.csv", colClasses = c(idmen="character", idind="character")))
datind2006 <- data.frame(read.csv("~/Desktop/Data/datind2006.csv", colClasses = c(idmen="character", idind="character")))
datind2007 <- data.frame(read.csv("~/Desktop/Data/datind2007.csv", colClasses = c(idmen="character", idind="character")))
datind2008 <- data.frame(read.csv("~/Desktop/Data/datind2008.csv", colClasses = c(idmen="character", idind="character")))
datind2009 <- data.frame(read.csv("~/Desktop/Data/datind2009.csv", colClasses = c(idmen="character", idind="character")))
datind2010 <- data.frame(read.csv("~/Desktop/Data/datind2010.csv", colClasses = c(idmen="character", idind="character")))
datind2011 <- data.frame(read.csv("~/Desktop/Data/datind2011.csv", colClasses = c(idmen="character", idind="character")))
datind2012 <- data.frame(read.csv("~/Desktop/Data/datind2012.csv", colClasses = c(idmen="character", idind="character")))
datind2013 <- data.frame(read.csv("~/Desktop/Data/datind2013.csv", colClasses = c(idmen="character", idind="character")))
datind2014 <- data.frame(read.csv("~/Desktop/Data/datind2014.csv", colClasses = c(idmen="character", idind="character")))
datind2015 <- data.frame(read.csv("~/Desktop/Data/datind2015.csv", colClasses = c(idmen="character", idind="character")))
datind2016 <- data.frame(read.csv("~/Desktop/Data/datind2016.csv", colClasses = c(idmen="character", idind="character")))
datind2017 <- data.frame(read.csv("~/Desktop/Data/datind2017.csv", colClasses = c(idmen="character", idind="character")))
datind2018 <- data.frame(read.csv("~/Desktop/Data/datind2018.csv", colClasses = c(idmen="character", idind="character")))
datind2019 <- data.frame(read.csv("~/Desktop/Data/datind2019.csv", colClasses = c(idmen="character", idind="character")))

datind=rbind(datind2004,datind2005,datind2006,datind2007,datind2008,
                      datind2009,datind2010,datind2011,datind2012,datind2013,
                      datind2014,datind2015,datind2016,datind2017,datind2018,datind2019)

#2)
dathh2004 <- data.frame(read.csv("~/Desktop/Data/dathh2004.csv", colClasses = c(idmen="character")))
dathh2005 <- data.frame(read.csv("~/Desktop/Data/dathh2005.csv", colClasses = c(idmen="character")))
dathh2006 <- data.frame(read.csv("~/Desktop/Data/dathh2006.csv", colClasses = c(idmen="character")))
dathh2007 <- data.frame(read.csv("~/Desktop/Data/dathh2007.csv", colClasses = c(idmen="character")))
dathh2008 <- data.frame(read.csv("~/Desktop/Data/dathh2008.csv", colClasses = c(idmen="character")))
dathh2009 <- data.frame(read.csv("~/Desktop/Data/dathh2009.csv", colClasses = c(idmen="character")))
dathh2010 <- data.frame(read.csv("~/Desktop/Data/dathh2010.csv", colClasses = c(idmen="character")))
dathh2011 <- data.frame(read.csv("~/Desktop/Data/dathh2011.csv", colClasses = c(idmen="character")))
dathh2012 <- data.frame(read.csv("~/Desktop/Data/dathh2012.csv", colClasses = c(idmen="character")))
dathh2013 <- data.frame(read.csv("~/Desktop/Data/dathh2013.csv", colClasses = c(idmen="character")))
dathh2014 <- data.frame(read.csv("~/Desktop/Data/dathh2014.csv", colClasses = c(idmen="character")))
dathh2015 <- data.frame(read.csv("~/Desktop/Data/dathh2015.csv", colClasses = c(idmen="character")))
dathh2016 <- data.frame(read.csv("~/Desktop/Data/dathh2016.csv", colClasses = c(idmen="character")))
dathh2017 <- data.frame(read.csv("~/Desktop/Data/dathh2017.csv", colClasses = c(idmen="character")))
dathh2018 <- data.frame(read.csv("~/Desktop/Data/dathh2018.csv", colClasses = c(idmen="character")))
dathh2019 <- data.frame(read.csv("~/Desktop/Data/dathh2019.csv", colClasses = c(idmen="character")))
dathh=rbind(dathh2004,dathh2005,dathh2006,dathh2007,dathh2008,
                     dathh2009,dathh2010,dathh2011,dathh2012,dathh2013,
                     dathh2014,dathh2015,dathh2016,dathh2017,dathh2018,dathh2019)
#3)
names(dathh)
names(datind) #idmen and year
#4)
# merge two data frames by idmen and year
total <- merge(datind,dathh,by=c("idmen","year"))

#5) 
hhmember=total %>% group_by(idmen,year) %>%tally()
fourmem=hhmember[hhmember$n>4,]
nrow(distinct(fourmem,idmen, .keep_all = TRUE))#3622
#6)
unemp=total[total$empstat=='Unemployed',]
hhunemp=unemp %>% group_by(idmen,year)%>%tally()
nrow(distinct(hhunemp,idmen, .keep_all = TRUE))#8161
#7)
prof=total %>% group_by(profession,idmen,year)%>%tally()
twomem=prof[prof$n>=2,]
nrow(distinct(twomem,idmen, .keep_all = TRUE))#28024
#8)
couple=total[total$mstatus == "Couple, with Kids",]
hhcouple=couple %>% group_by(idmen,year)%>%tally()
nrow(distinct(hhcouple,idmen, .keep_all = TRUE))#13931
#9)
paris=total[total$location == "Paris",]
indparis=paris %>% group_by(idind,year)%>%tally()
nrow(distinct(indparis,idind))#6178
#10)
total$idmen=as.character(total$idmen)
total %>% group_by(idmen,year) %>%tally(sort = TRUE) #14
#11)
present=total[total$year == 2010 &2011,]
present1=present %>% group_by(idmen) %>%tally()
nrow(distinct(present1,idmen))#11048

##EX3
#1)
hhpanel= total %>% group_by(idmen) %>% summarize(time = max(year)-min(year))
plot(density(hhpanel$time))
#2)
sameyear= total %>% group_by(idmen,year) %>%mutate(same=case_when(year==datent~1,
                                                                  TRUE~0))
totalyear=sameyear %>% group_by(year) %>%tally()
trueyear=sameyear %>% group_by(year) %>%summarise(true=sum(same))
ind=merge(totalyear,trueyear, by="year")
ind$prop=ind$true/ind$n
ind
ggplot(ind,aes(x=year,y=prop))+geom_line()+ylab("proportion migrated")

#3
totalbefore=total[total$year<=2014,]
totalafter=total[total$year>2014,]
before= totalbefore %>% group_by(idmen,year) %>% mutate(same=case_when(year==myear~1,
                                                                    TRUE~0))
beforetrueyear=before %>% group_by(year) %>%summarise(true=sum(same))

totalafter[is.na(totalafter)] <- 0 #remove NA in move
moveafter=totalafter[totalafter$move==2,]
aftertureyear=moveafter %>% group_by(year) %>% summarise(true=sum(move))

trueyear2=rbind(beforetrueyear,aftertureyear)
ind2=merge(totalyear,trueyear2, by="year")
ind2$prop=ind2$true/ind2$n
ind2
ggplot(ind2,aes(x=year,y=prop))+geom_line()+ylab("proportion migrated")
#4
ggplot(NULL,aes(x=year,y=prop))+geom_line(data=ind)+geom_line(data=ind2,col="blue")
#5)
survey=total %>% group_by(idind,year)
lag_survey=lag(survey)
lag_survey$lag_emp=survey$empstat
lag_survey$lag_pro=survey$profession
lag_survey$lag_id=survey$idind
lag_survey$match=(lag_survey$lag_id == lag_survey$idind)
lag_survey = lag_survey[-1,]
miglag=merge(sameyear,lag_survey, by="idind")
miglag1=miglag[miglag$same == 1 & miglag$match == "TRUE",]
miglag1$matchemp=(miglag1$empstat.x == miglag1$lag_emp)
miglag1$matchpro=(miglag1$profession.x == miglag1$lag_pro)
migfinal=miglag1[miglag1$matchemp == "FALSE" | miglag1$matchpro == "FALSE",]
nrow(migfinal)

#ex4
indpanel= total %>% group_by(idind) %>% mutate(exit=max(year))
individual=distinct(indpanel,idind, .keep_all = TRUE)
attrition=individual %>% group_by(exit) %>%tally
print(attrition)
prop.table(attrition$n)

