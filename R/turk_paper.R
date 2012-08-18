# Codes to obtain data from mysql database for turk experiments
# Last updated by Mahbub on Mar 21, 2012

library(ggplot2)
library(plyr)

ip.details <- read.csv("../data/ip_detais.csv",header=T)

# plotting the ip locations in the world map

ip.details$x <- as.numeric(ip.details$longitude)
ip.details$y <- as.numeric(ip.details$latitude)

library(maps)
map.dat <- as.data.frame(map("world",ylim=c(-45,70), plot=FALSE)[c("x","y")])

qplot( longitude,latitude,  geom="point", data=ip.details)

ggplot() +
  geom_path(aes(x,y, colour=factor("B")), data=map.dat) +
  geom_point(aes(x,y ,  colour=factor("A")), data=ip.details, alpha=.3) +
  opts(legend.position="none")

ggsave("../images/turker_location.pdf", width=8, height=3)

country.cnt <- ddply(ip.details,.(country_name), nrow)
country.cnt <- country.cnt[order(country.cnt$V1),]

qplot(country_name, V1, geom="bar",data=country.cnt) + coord_flip()


#============ end getting data ============

