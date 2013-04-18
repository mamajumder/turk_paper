# Complete R code for turk paper

library(ggplot2)
library(plyr)

# Turk experiment data summary

turk.dat <- read.csv("../data/turk_summary.csv",header=T)
turk.dat$percent_rejected <- with(turk.dat, rejected*100/submitted)

ggplot(turk.dat, aes(x = factor(plot, levels=plot[order(percent_rejected)]), y=percent_rejected))+
  geom_bar(stat="identity") + xlab("Experimental plot type") +
  coord_flip() + ylab("Percentage of rejected task")


# plotting the ip locations in the world map

ip.details <- read.csv("../data/ip_detais.csv",header=T)

ip.details$x <- as.numeric(ip.details$longitude)
ip.details$y <- as.numeric(ip.details$latitude)

library(maps)
map.dat <- as.data.frame(map("world",ylim=c(-45,70), plot=FALSE)[c("x","y")])
map.dat <- map_data("world")

qplot( longitude,latitude,  geom="point", data=ip.details)

ggplot() +
  geom_polygon(aes(long,lat, group=group), fill="grey65", data=map.dat) +
  geom_point(aes(x,y, colour=factor("A")), data=ip.details, alpha=.6) +
  theme(legend.position="none") + xlab("Longitude") +ylab("Latitude")
ggsave("../images/turker_location.pdf", width=8, height=4)

country.cnt <- ddply(ip.details,.(country_name), nrow)
country.cnt <- country.cnt[order(country.cnt$V1, decreasing=T),]
cnt <- rbind(country.cnt[1:2,],data.frame(country_name="rest of the world", V1=sum(country.cnt[-c(1:2),2])))
qplot(country_name,V1, geom="bar", data=cnt, stat="identity") + 
coord_flip() +ylab("Number of turk worker")


ggplot() +
  geom_bar(aes(country_name,V1, fill=factor("A")), data=cnt, stat="identity") +
  theme(legend.position="none") + ylab("Number of turk worker") +
  xlab("Country") + coord_flip()

ggsave("../images/turker_country.pdf", width=7, height=2)  

qplot(country_name, V1, geom="bar",data=country.cnt, stat="identity") + coord_flip()


dat1 <- read.csv("../data/raw_data_turk1.csv",header=T)[,c(1:11,15,18:20)]
dat1$uid <- with(dat1,paste(experiment,"_",id,sep=""))
dat2 <- read.csv("../data/raw_data_turk2.csv",header=T)[,c(1:11,15,18:20)]
dat2$uid <- with(dat2,paste(experiment,"_",id,sep=""))
dat3 <- read.csv("../data/raw_data_turk3.csv",header=T)[,c(1:11,15,18:20)]
dat3$uid <- with(dat3,paste(experiment,"_",id,sep=""))
dat4 <- read.csv("../data/raw_data_turk4.csv",header=T)[,c(1:11,16,19:21)]
dat4$uid <- with(dat4,paste(experiment,"_",id,sep=""))
dat5 <- read.csv("../data/raw_data_turk5.csv",header=T)[,c(1:11,17,20:22)]
dat5$uid <- with(dat5,paste(experiment,"_",id,sep=""))
dat6 <- read.csv("../data/raw_data_turk6.csv",header=T)[,c(1:11,17,20:22)]
dat6$uid <- with(dat6,paste(tolower(experiment),"_",id,sep=""))
dat7 <- read.csv("../data/raw_data_turk7.csv",header=T)[,c(1:11,17,20:22)]
dat7$uid <- with(dat7,paste(tolower(experiment),"_",id,sep=""))
dat8 <- read.csv("../data/raw_data_turk8.csv",header=T)[,c(1:11,17,20:22)]
dat8$uid <- with(dat8,paste(tolower(experiment),"_",id,sep=""))


dat <- rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8)

head(dat); tail(dat)


sg <- ddply(dat,.(gender), summarize,
   avg_time = mean(time_taken),
   response = length(response),
   corrected = sum(response),
   percent_correct = mean(response)*100,
   number_responders = length(unique(uid))
   )
sg

se <- ddply(dat,.(academic_study), summarize,
   avg_time = mean(time_taken),
   response = length(response),
   corrected = sum(response),
   percent_correct = mean(response)*100,
   number_responders = length(unique(uid))
   )    

sa <- ddply(dat,.(age), summarize,
   avg_time = mean(time_taken),
   response = length(response),
   corrected = sum(response),
   percent_correct = mean(response)*100,
   number_responders = length(unique(uid))
   )   

users <- read.csv("../data/turk_users.csv", header=T)


# ------------ description of coded variables in the data -----------

## gender 1 = male 
#         2 = female
#
## age 1 = below 18 
#      2 = 18-25 
#      3 = 26-30 
#      4 = 31-35 
#      5 = 36-40
#      6 = 41-45
#      7 = 46-50
#      8 = 51-55
#      9 = 56-60
#     10 = above 60
#
## academic_study 1 = High school or less
#                 2 = Some under graduate courses 
#                 3 = Under graduate degree
#                 4 = Some graduate courses
#                 5 = Graduate degree
#
## conf_level 1 = most, 5 = least
#
## choice_reason 1 = Big vertical difference 
#                2 = Medians are different
#                3 = Outliers
#                4 = others
#
## unit(time_taken) = second 


nrow(unique(dat[,13:16]))

tark_worker <- dat[!duplicated(dat$uid),13:16]
tworker <- with(tark_worker,table(academic_study, age)) [-1,][,-1]

library(reshape)
age_study <- melt(tworker)
age_study$degree <- c("High school or less", "Some under graduate courses",
          "Under graduate degree","Some graduate courses",
          "Graduate degree")[age_study$academic_study]
age_study$age_level <- c("below 18","18-25","26-30","31-35","36-40",
         "41-45","46-50","51-55","56-60","above 60")[age_study$age]
         
qplot(age, academic_study, geom="point", size=value, data=subset(age_study,value>0))+
     scale_x_discrete(labels=c("below 18","18-25","26-30","31-35","36-40",
         "41-45","46-50","51-55","56-60","above 60")) +
     scale_y_discrete(labels=c("High school or less", "Some under graduate courses",
          "Under graduate degree","Some graduate courses",
          "Graduate degree")) + scale_size(range=c(1,15))

ggfluctuation(xtabs(value~age+academic_study, age_study))
age_study$age <- factor(age_study$age)
levels(age_study$age)[8:10] <- 8
levels(age_study$age)[1:8] <- c("below 18","18-25","26-30","31-35","36-40",
         "41-45", "46-50", "above 50")
age_study$academic_study <- factor(age_study$academic_study)
levels(age_study$academic_study) <- c("High school or less", "Some under graduate courses",
          "Under graduate degree","Some graduate courses",
          "Graduate degree")

qplot(age, weight=value, facets=academic_study~., data=age_study) 
qplot(factor(academic_study), weight=value, facets=age~., data=age_study) +
   coord_flip() + xlab("Academic Study")
ggsave("../images/age_study_bar.pdf", width=6, height=7) 


ggplot(age_study, aes(age, academic_study, size=value)) +
    geom_point(subset=.(value>0))+
     scale_x_discrete(labels=c("below 18","18-25","26-30","31-35","36-40",
         "41-45","46-50","51-55","56-60","above 60")) +
     scale_y_discrete(labels=c("High school or less", "Some under graduate courses",
          "Under graduate degree","Some graduate courses",
          "Graduate degree")) + scale_size(range=c(1,15)) + ylab("Academic Study")
ggsave("../images/age_study.pdf", width=9, height=5.2)  


# country and education

educ_country <- merge(dat,ip.details,all.x=TRUE, by="ip_address")
worker_ec <- educ_country[!duplicated(educ_country$uid),c("uid","age","gender","academic_study","country_code","country_name")]
worker_ec$country <- as.character(worker_ec$country_name)
worker_ec$country[(worker_ec$country_code != "IN") & (worker_ec$country_code != "US")] <- "Rest of the world"
worker_ec$country[worker_ec$country=="Namibia"] <- "Rest of the world"
wec <- subset(worker_ec,academic_study>0 )



wec$academic_study <- factor(wec$academic_study)
levels(wec$academic_study) <- c("High school or less", "Some under graduate courses",
          "Under graduate degree","Some graduate courses",
          "Graduate degree")
          
qplot(factor(academic_study), geom="bar", facets=country~., data=wec) +
   coord_flip() + xlab("Academic Study")
ggsave("../images/country_study_bar.pdf", width=6, height=6) 

