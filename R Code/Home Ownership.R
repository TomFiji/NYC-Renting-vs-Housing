library("readxl")
library("magrittr")
library("tidyverse")
library("data.table")
library("ggplot2")
ownership.df <- read_excel("NYC-housing-data.xlsx", sheet = "home ownership rate")
ownership.df <- ownership.df[-c(1,2,20:23)]
sapply(ownership.df, function(x) sum(is.na(x)))
ownership.df<-na.omit(ownership.df)

ownership.2005.df <-aggregate(ownership.df$"2005", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.2005.df %>% mutate('2005' =ownership.2005.df$x)
ownership.2006.df <-aggregate(ownership.df$"2006", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2006' =ownership.2006.df$x)
ownership.2007.df <-aggregate(ownership.df$"2007", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2007' =ownership.2007.df$x)
ownership.2008.df <-aggregate(ownership.df$"2008", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2008' =ownership.2008.df$x)
ownership.2009.df <-aggregate(ownership.df$"2009", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2009' =ownership.2009.df$x)
ownership.2010.df <-aggregate(ownership.df$"2010", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2010' =ownership.2010.df$x)
ownership.2011.df <-aggregate(ownership.df$"2011", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2011' =ownership.2011.df$x)

ownership.2012.df <-aggregate(ownership.df$"2012", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2012' =ownership.2012.df$x)
ownership.2013.df <-aggregate(ownership.df$"2013", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2013' =ownership.2013.df$x)
ownership.2014.df <-aggregate(ownership.df$"2014", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2014' =ownership.2014.df$x)
ownership.2015.df <-aggregate(ownership.df$"2015", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2015' =ownership.2015.df$x)
ownership.2016.df <-aggregate(ownership.df$"2016", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2016' =ownership.2016.df$x)
ownership.2017.df <-aggregate(ownership.df$"2017", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2017' =ownership.2017.df$x)
ownership.2018.df <-aggregate(ownership.df$"2018", by=list(ownership.df$Borough),FUN=mean) 
ownership.final.df<-ownership.final.df %>% mutate('2018' =ownership.2018.df$x)
ownership.final.df<-ownership.final.df[,-c(2)]
colnames(ownership.final.df)<-c('Borough','2005',
                               '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014',
                               '2015', '2016', '2017', '2018')

homeOwner<- melt(as.data.table(ownership.final.df), id = 1, variable.name ="year", value.name ="ownership")
homeOwner[,year := as.integer(gsub("[^0-9]", "", year)),]
homeOwner
colnames(homeOwner)<-c('Borough', 'Year', 'ownership')

ggplot(homeOwner, aes(x=Year, y=ownership, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Home Ownership Time Series", 
                                         subtitle="by Borough")


