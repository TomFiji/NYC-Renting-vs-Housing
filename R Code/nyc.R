# Let's create the first data frame
nyc.covid.df<-read.csv("NYC-COVID-data_01.csv") # 177 observation and not everything is a variable

# let's clean this data frame
nyc.covid.19.df<-nyc.covid.df[, - c(1,2,11,12,13)] 

# any missing data? Let's find out
data.frame(miss.val=sapply(nyc.covid.19.df, function(x)sum(length(which(is.na(x)))))) # no missing data

# let's review a summary
summary(nyc.covid.19.df) # BOROUGH_GROUP is categorical

# dplyr package, we can use summarise_all, summarise_at or summarise_if functions to aggregate multiple variables simultaneously
library(dplyr)

# creating a data frame by areas
borough.nyc.df <- nyc.covid.19.df %>% group_by(BOROUGH_GROUP) %>% summarise_all(sum)
# focusing on absolute not rates
borough.nyc.final.df<- borough.nyc.df[, - c(3,6,7)] 

# Conclusion: Queens and Brooklyn areas are the highest populated district of NYC and the most affected districts in ncy too. 
# let's plot this. We use ggplot
library(ggplot2)
ggplot(borough.nyc.final.df, aes(x=POP_DENOMINATOR, y=COVID_CASE_COUNT, color=BOROUGH_GROUP))+ 
  ggtitle("Relationship between Covid cases and Population")+ 
  xlab("Population") + ylab("Covid Cases") + geom_point() +  
  scale_color_manual(values = c("Brooklyn" = "red", "Queens"= "black",
                                "Bronx" = "orange", "Manhattan"="purple", "Staten Island"="blue")) 

# how about the incidence. Let's create a new data frame adding incidence
# we need to add new variables to a data frame in R. Required packages. Load the tidyverse packages, which include dplyr:
# mutate(): compute and add new variables into a data table. It preserves existing variables.
# transmute(): compute new columns but drop existing variables. We don't need this one
library(tidyverse)
borough.nyc.new.variable.df<-borough.nyc.final.df %>% 
  mutate(covid_incidence = (COVID_CASE_COUNT/POP_DENOMINATOR)*100)

# it's time to plot...Wow... we have to...It's gonna be alright 
barplot(borough.nyc.new.variable.df$covid_incidence, 
        main = "Codiv-19 incidence",  ylab = "%  Incidence", col = "orange", 
        names=borough.nyc.new.variable.df$BOROUGH_GROUP)
# the barplot shows that staten Island shown the highest incidence. 

# Conclusion: Staten Island, Bronx and Queens shown the highest codiv-19 percentuage incidence in NYC 
# But absolute values show that Brooklyn and Queens are the most affected districts. 
# The higher population density in this two states can give a better understanding 
# of what happened and will happen to house pricing.
# Recommendation: we should focus our analysis in Brooklyn and Queens in order to  analysis house pricing

#install.packages("readxl") #install packages to run xlsx
#install.packages("Rcpp")
library("readxl")
#read our data, make sure file is closed before opening 
HousePrice.df <- read_excel("NYC-housing-data.xlsx", sheet = "price single family home")
HousePrice.df <- HousePrice.df[, - c(1,2,3,23,24,25)] 
#missing values present, removed
data.frame(miss.val=sapply(HousePrice.df, function(x)sum(length(which(is.na(x))))))
HousePriceNM.df <- na.omit(HousePrice.df)

CondoPrice.df <- read_excel("NYC-housing-data.xlsx", sheet = "price condominium")
CondoPrice.df <- CondoPrice.df[, - c(1,2,3,23,24,25)] 
#missing values present, removed
data.frame(miss.val=sapply(CondoPrice.df, function(x)sum(length(which(is.na(x))))))
CondoPriceNM.df <- na.omit(CondoPrice.df)

# could do box plot of prices over years
boxplot(HousePriceNM.df,
        main = "House Price over Years",
        xlab = "Years",
        ylab = "Price",
        las = 1,
        ylim = c(0,15000000),
        col = "blue"
)
boxplot(CondoPriceNM.df ,
        main = "Condo Price over Years",
        xlab = "Years",
        ylab = "Price",
        las = 1,
        ylim = c(0,4000000),
        col = "red"
)





# we have 5 variables to study
# Price condo  ---> d1
# Price single unit --> d2
# Home owner income --> d3
# Renter income --> d4
# rent burden  -->d5

# ---------------- Price Condo --------------------------- d1 -----------------------------------------
# let's turn attention to price of condo
# now price condo
price.condo.df<-read.csv("PriceCondominium.csv")
price.condo.df<-price.condo.df[-c(1,2)]
colnames(price.condo.df) <- c('Borough','Sub.Borough.Area','2000', '2001', '2002', '2003', '2004', '2005',
                              '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014',
                              '2015', '2016', '2017', '2018')

# let's verify if there are missing data
sapply(price.condo.df, function(x) sum(is.na(x)))

# we create a data frame with no row with missing data
# removing records with missing values
priceCondo.df<-na.omit(price.condo.df)

# we create data frame  to store the average price by area per year
AVGPriceCbyBorough.2000.df <-aggregate(priceCondo.df$"2000", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.2000.df %>% mutate('2000' =AVGPriceCbyBorough.2000.df$x)
AVGPriceCbyBorough.2001.df <-aggregate(priceCondo.df$"2001", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2001' =AVGPriceCbyBorough.2001.df$x)
AVGPriceCbyBorough.2002.df <-aggregate(priceCondo.df$"2002", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2002' =AVGPriceCbyBorough.2002.df$x)
AVGPriceCbyBorough.2003.df <-aggregate(priceCondo.df$"2003", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2003' =AVGPriceCbyBorough.2003.df$x)
AVGPriceCbyBorough.2004.df <-aggregate(priceCondo.df$"2004", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2004' =AVGPriceCbyBorough.2004.df$x)
AVGPriceCbyBorough.2005.df <-aggregate(priceCondo.df$"2005", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2005' =AVGPriceCbyBorough.2005.df$x)
AVGPriceCbyBorough.2006.df <-aggregate(priceCondo.df$"2006", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2006' =AVGPriceCbyBorough.2006.df$x)
AVGPriceCbyBorough.2007.df <-aggregate(priceCondo.df$"2007", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2007' =AVGPriceCbyBorough.2007.df$x)
AVGPriceCbyBorough.2008.df <-aggregate(priceCondo.df$"2008", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2008' =AVGPriceCbyBorough.2008.df$x)
AVGPriceCbyBorough.2009.df <-aggregate(priceCondo.df$"2009", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2009' =AVGPriceCbyBorough.2009.df$x)

AVGPriceCbyBorough.2010.df <-aggregate(priceCondo.df$"2010", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2010' =AVGPriceCbyBorough.2010.df$x)
AVGPriceCbyBorough.2011.df <-aggregate(priceCondo.df$"2011", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2011' =AVGPriceCbyBorough.2011.df$x)
AVGPriceCbyBorough.2012.df <-aggregate(priceCondo.df$"2012", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2012' =AVGPriceCbyBorough.2012.df$x)
AVGPriceCbyBorough.2013.df <-aggregate(priceCondo.df$"2013", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2013' =AVGPriceCbyBorough.2013.df$x)
AVGPriceCbyBorough.2014.df <-aggregate(priceCondo.df$"2014", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2014' =AVGPriceCbyBorough.2014.df$x)
AVGPriceCbyBorough.2015.df <-aggregate(priceCondo.df$"2015", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2015' =AVGPriceCbyBorough.2015.df$x)
AVGPriceCbyBorough.2016.df <-aggregate(priceCondo.df$"2016", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2016' =AVGPriceCbyBorough.2016.df$x)
AVGPriceCbyBorough.2017.df <-aggregate(priceCondo.df$"2017", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2017' =AVGPriceCbyBorough.2017.df$x)
AVGPriceCbyBorough.2018.df <-aggregate(priceCondo.df$"2018", by=list(priceCondo.df$Borough),FUN=mean) 
AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df %>% mutate('2018' =AVGPriceCbyBorough.2018.df$x)

AVGPriceCbyBorough.final.df<-AVGPriceCbyBorough.final.df[-c(2)]
colnames(AVGPriceCbyBorough.final.df) <- c('Borough','2000', '2001', '2002', '2003', '2004', '2005',
                                           '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014',
                                           '2015', '2016', '2017', '2018')
f1.df<-AVGPriceCbyBorough.final.df[c(1,20)]
colnames(f1.df) <- c('Borough', 'PriceCondo')
f1.df

# we need to convert data frame into data table 
# quick tutorial 
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# use data.table(df) or as.data.table(df)
library(data.table)
d1<- melt(as.data.table(AVGPriceCbyBorough.final.df), id = 1, variable.name ="year", value.name ="priceCondo")
d1[,year := as.integer(gsub("[^0-9]", "", year)),]
d1
# we use ggplot to plot data by Area
library(ggplot2)
ggplot(d1, aes(year, priceCondo, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Condominium Price Time Series", 
                                         subtitle="by Borough")

#ANALYSIS of RESULTS

# the plot shows three groups of areas: Manhattan shows a time series of high prices and an overall increase trend
# the second group shows a time series of mid prices and a stable increasing price trend
# the third group shows price more or less stable 
# CONCLUSION: Manhattan should not include in any cluster for home buyer voucher
# this statement meaning that area qualified for the voucher to buy a house 
# should not include Manhattan 

#---------------------- Price single ---------------------- d2 --------------------------------------

# let's work on price single home 
price.single.df<-read.csv("PriceSingleFamilyHome.csv")
price.single.df<-price.single.df[-c(1,2,24,25,26)]
colnames(price.single.df) <- c('Borough','Sub.Borough.Area','2000', '2001', '2002', '2003', '2004', '2005',
                               '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014',
                               '2015', '2016', '2017', '2018')

# let's verify if there are missing data
sapply(price.single.df, function(x) sum(is.na(x)))

# we create a data frame with no row with missing data
# removing records with missing values
priceSingle.df<-na.omit(price.single.df)

# we create data frame  to store the average price by area per year
AVGPricebyBorough.2000.df <-aggregate(priceSingle.df$"2000", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.2000.df %>% mutate('2000' =AVGPricebyBorough.2000.df$x)
AVGPricebyBorough.2001.df <-aggregate(priceSingle.df$"2001", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2001' =AVGPricebyBorough.2001.df$x)
AVGPricebyBorough.2002.df <-aggregate(priceSingle.df$"2002", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2002' =AVGPricebyBorough.2002.df$x)
AVGPricebyBorough.2003.df <-aggregate(priceSingle.df$"2003", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2003' =AVGPricebyBorough.2003.df$x)
AVGPricebyBorough.2004.df <-aggregate(priceSingle.df$"2004", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2004' =AVGPricebyBorough.2004.df$x)
AVGPricebyBorough.2005.df <-aggregate(priceSingle.df$"2005", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2005' =AVGPricebyBorough.2005.df$x)
AVGPricebyBorough.2006.df <-aggregate(priceSingle.df$"2006", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2006' =AVGPricebyBorough.2006.df$x)
AVGPricebyBorough.2007.df <-aggregate(priceSingle.df$"2007", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2007' =AVGPricebyBorough.2007.df$x)
AVGPricebyBorough.2008.df <-aggregate(priceSingle.df$"2008", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2008' =AVGPricebyBorough.2008.df$x)
AVGPricebyBorough.2009.df <-aggregate(priceSingle.df$"2009", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2009' =AVGPricebyBorough.2009.df$x)

AVGPricebyBorough.2010.df <-aggregate(priceSingle.df$"2010", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2010' =AVGPricebyBorough.2010.df$x)
AVGPricebyBorough.2011.df <-aggregate(priceSingle.df$"2011", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2011' =AVGPricebyBorough.2011.df$x)
AVGPricebyBorough.2012.df <-aggregate(priceSingle.df$"2012", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2012' =AVGPricebyBorough.2012.df$x)
AVGPricebyBorough.2013.df <-aggregate(priceSingle.df$"2013", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2013' =AVGPricebyBorough.2013.df$x)
AVGPricebyBorough.2014.df <-aggregate(priceSingle.df$"2014", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2014' =AVGPricebyBorough.2014.df$x)
AVGPricebyBorough.2015.df <-aggregate(priceSingle.df$"2015", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2015' =AVGPricebyBorough.2015.df$x)
AVGPricebyBorough.2016.df <-aggregate(priceSingle.df$"2016", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2016' =AVGPricebyBorough.2016.df$x)
AVGPricebyBorough.2017.df <-aggregate(priceSingle.df$"2017", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2017' =AVGPricebyBorough.2017.df$x)
AVGPricebyBorough.2018.df <-aggregate(priceSingle.df$"2018", by=list(priceSingle.df$Borough),FUN=mean) 
AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df %>% mutate('2018' =AVGPricebyBorough.2018.df$x)

AVGPricebyBorough.final.df<-AVGPricebyBorough.final.df[-c(2)]
colnames(AVGPricebyBorough.final.df) <- c('Borough','2000', '2001', '2002', '2003', '2004', '2005',
                                          '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014',
                                          '2015', '2016', '2017', '2018')
f2.df<-AVGPricebyBorough.final.df[c(1,20)]
colnames(f2.df) <- c('Borough', 'PriceSingle')
f2.df

# we need to convert data frame into data table 
# quick tutorial 
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# use data.table(df) or as.data.table(df)
library(data.table)
d2<- melt(as.data.table(AVGPricebyBorough.final.df), id = 1, variable.name ="year", value.name ="priceSingle")
d2[,year := as.integer(gsub("[^0-9]", "", year)),]
d2
# we use ggplot to plot data by Area ""
library(ggplot2)
ggplot(d2, aes(year, priceSingle, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Single Family Price Time Series", 
                                         subtitle="by Borough")

#ANALYSIS of RESULTS

# the plot shows two group of areas: Manhattan show  of time series of high prices and an increase trend
# the second group shows a time series of lower prices and an stable price trend
# CONCLUSION: Manhattan should not include in any cluster for home buyer voucher

# -------------------- Homeowner Income --------------- d3 ------------------------

# let's turn attention to homeowner income
# 
homeowner.income.df<-read.csv("HomeownerIncome.csv")
homeowner.income.df<-homeowner.income.df[-c(1,2)]
colnames(homeowner.income.df) <- c('Borough','Sub.Borough.Area','2005', '2006', '2007', '2008', '2009', 
                                   '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

# let's verify if there are missing data
sapply(homeowner.income.df, function(x) sum(is.na(x)))

# we create a data frame with no row with missing data
# removing records with missing values
homeownerIncome.df<-na.omit(homeowner.income.df)

# we create data frame  to store the average price by area per year
homeownerIncome.2005.df <-aggregate(homeownerIncome.df$"2005", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.2005.df %>% mutate('2005' =homeownerIncome.2005.df$x)
homeownerIncome.2006.df <-aggregate(homeownerIncome.df$"2006", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2006' =homeownerIncome.2006.df$x)
homeownerIncome.2007.df <-aggregate(homeownerIncome.df$"2007", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2007' =homeownerIncome.2007.df$x)
homeownerIncome.2008.df <-aggregate(homeownerIncome.df$"2008", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2008' =homeownerIncome.2008.df$x)
homeownerIncome.2009.df <-aggregate(homeownerIncome.df$"2009", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2009' =homeownerIncome.2009.df$x)
homeownerIncome.2010.df <-aggregate(homeownerIncome.df$"2010", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2010' =homeownerIncome.2010.df$x)
homeownerIncome.2011.df <-aggregate(homeownerIncome.df$"2011", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2011' =homeownerIncome.2011.df$x)

homeownerIncome.2012.df <-aggregate(homeownerIncome.df$"2012", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2012' =homeownerIncome.2012.df$x)
homeownerIncome.2013.df <-aggregate(homeownerIncome.df$"2013", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2013' =homeownerIncome.2013.df$x)
homeownerIncome.2014.df <-aggregate(homeownerIncome.df$"2014", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2014' =homeownerIncome.2014.df$x)
homeownerIncome.2015.df <-aggregate(homeownerIncome.df$"2015", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2015' =homeownerIncome.2015.df$x)
homeownerIncome.2016.df <-aggregate(homeownerIncome.df$"2016", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2016' =homeownerIncome.2016.df$x)
homeownerIncome.2017.df <-aggregate(homeownerIncome.df$"2017", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2017' =homeownerIncome.2017.df$x)
homeownerIncome.2018.df <-aggregate(homeownerIncome.df$"2018", by=list(homeownerIncome.df$Borough),FUN=mean) 
homeownerIncome.final.df<-homeownerIncome.final.df %>% mutate('2018' =homeownerIncome.2018.df$x)

homeownerIncome.final.df<-homeownerIncome.final.df[-c(2)]
colnames(homeownerIncome.final.df) <- c('Borough','2005', '2006', '2007', '2008', '2009', 
                                        '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

f3.df<-homeownerIncome.final.df[c(1,15)]
colnames(f3.df) <- c('Borough', 'HomeOwnerIncome')
f3.df

# we need to convert data frame into data table 
# quick tutorial 
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# use data.table(df) or as.data.table(df)
library(data.table)
d3<- melt(as.data.table(homeownerIncome.final.df), id = 1, variable.name ="year", value.name ="HomeOwnerIncome")
d3[,year := as.integer(gsub("[^0-9]", "", year)),]
d3
# we use ggplot to plot data by Area
library(ggplot2)
ggplot(d3, aes(year, HomeOwnerIncome, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Homeowner Income Time Series", 
                                         subtitle="by Borough")

#ANALYSIS of RESULTS

# the plot shows three groups of areas: Manhattan show  of time series of high income and an overall increase trend
# the second group shows a time series of mid income and a stable increasing trend
# the third group shows income more or less stable 
# CONCLUSION: Once again Manhattan should not include in any cluster for home buyer voucher. Income in this area seems to be extremely high income area
# this statement meaning that in clustering areas qualified for the voucher to buy a house 
# should not include Manhattan 

# ------------------ Renter Income ------------------------ d4 ------------------------------------
# let's turn attention to renter  income
# 
renter.income.df<-read.csv("RenterIncome.csv")
renter.income.df<-renter.income.df[-c(1,2)]
colnames(renter.income.df) <- c('Borough','Sub.Borough.Area','2005', '2006', '2007', '2008', '2009', 
                                '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

# let's verify if there are missing data
sapply(renter.income.df, function(x) sum(is.na(x)))

# we create a data frame with no row with missing data
# removing records with missing values
renter.df<-na.omit(renter.income.df)

# we create data frame  to store the average income by area per year

renter.2005.df <-aggregate(renter.df$"2005", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.2005.df %>% mutate('2005' =renter.2005.df$x)
renter.2006.df <-aggregate(renter.df$"2006", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2006' =renter.2006.df$x)
renter.2007.df <-aggregate(renter.df$"2007", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2007' =renter.2007.df$x)
renter.2008.df <-aggregate(renter.df$"2008", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2008' =renter.2008.df$x)
renter.2009.df <-aggregate(renter.df$"2009", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2009' =renter.2009.df$x)
renter.2010.df <-aggregate(renter.df$"2010", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2010' =renter.2010.df$x)
renter.2011.df <-aggregate(renter.df$"2011", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2011' =renter.2011.df$x)

renter.2012.df <-aggregate(renter.df$"2012", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2012' =renter.2012.df$x)
renter.2013.df <-aggregate(renter.df$"2013", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2013' =renter.2013.df$x)
renter.2014.df <-aggregate(renter.df$"2014", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2014' =renter.2014.df$x)
renter.2015.df <-aggregate(renter.df$"2015", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2015' =renter.2015.df$x)
renter.2016.df <-aggregate(renter.df$"2016", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2016' =renter.2016.df$x)
renter.2017.df <-aggregate(renter.df$"2017", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2017' =renter.2017.df$x)
renter.2018.df <-aggregate(renter.df$"2018", by=list(renter.df$Borough),FUN=mean) 
renter.final.df<-renter.final.df %>% mutate('2018' =renter.2018.df$x)

renter.final.df<-renter.final.df[-c(2)]
colnames(renter.final.df) <- c('Borough','2005', '2006', '2007', '2008', '2009', 
                               '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

f4.df<-renter.final.df[c(1,15)]
colnames(f4.df) <- c('Borough', 'RenterIncome')
f4.df

# we need to convert data frame into data table 
# quick tutorial 
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# use data.table(df) or as.data.table(df)
library(data.table)
d4<- melt(as.data.table(renter.final.df), id = 1, variable.name ="year", value.name ="RenterIncome")
d4[,year := as.integer(gsub("[^0-9]", "", year)),]
d4
# we use ggplot to plot data by Area
library(ggplot2)
ggplot(d4, aes(year, RenterIncome, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Renter Income Time Series", 
                                         subtitle="by Borough")

#ANALYSIS of RESULTS

# Once again there are three groups: Manhattan with high renter income 
# second group with mid renter income and finally the last group with one borough and low renter income 

# -----------  Rent Burden ------------------------ d5 -----------------------------------
# let's turn attention to rent burden
# 
rent.burden.df<-read.csv("RentBurden.csv")
rent.burden.df<-rent.burden.df[-c(1,2,5)]
colnames(rent.burden.df) <- c('Borough','Sub.Borough.Area','2005', '2006', '2007', '2008', '2009', 
                              '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

# let's verify if there are missing data
sapply(rent.burden.df, function(x) sum(is.na(x)))

# we create a data frame with no row with missing data
# removing records with missing values
burden.df<-na.omit(rent.burden.df)

# we create data frame  to store the average income by area per year

burden.2005.df <-aggregate(burden.df$"2005", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.2005.df %>% mutate('2005' =burden.2005.df$x)
burden.2006.df <-aggregate(burden.df$"2006", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2006' =burden.2006.df$x)
burden.2007.df <-aggregate(burden.df$"2007", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2007' =burden.2007.df$x)
burden.2008.df <-aggregate(burden.df$"2008", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2008' =burden.2008.df$x)
burden.2009.df <-aggregate(burden.df$"2009", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2009' =burden.2009.df$x)
burden.2010.df <-aggregate(burden.df$"2010", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2010' =burden.2010.df$x)
burden.2011.df <-aggregate(burden.df$"2011", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2011' =burden.2011.df$x)

burden.2012.df <-aggregate(burden.df$"2012", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2012' =burden.2012.df$x)
burden.2013.df <-aggregate(burden.df$"2013", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2013' =burden.2013.df$x)
burden.2014.df <-aggregate(burden.df$"2014", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2014' =burden.2014.df$x)
burden.2015.df <-aggregate(burden.df$"2015", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2015' =burden.2015.df$x)
burden.2016.df <-aggregate(burden.df$"2016", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2016' =burden.2016.df$x)
burden.2017.df <-aggregate(burden.df$"2017", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2017' =burden.2017.df$x)
burden.2018.df <-aggregate(burden.df$"2018", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.final.df %>% mutate('2018' =burden.2018.df$x)

burden.final.df<-burden.final.df[-c(2)]
colnames(burden.final.df) <- c('Borough','2005', '2006', '2007', '2008', '2009', 
                               '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018')

f5.df<-burden.final.df[c(1,15)]
colnames(f5.df) <- c('Borough', 'RentBurden')
f5.df

# we need to convert data frame into data table 
# quick tutorial 
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# use data.table(df) or as.data.table(df)
library(data.table)
d5<- melt(as.data.table(burden.final.df), id = 1, variable.name ="year", value.name ="RentBurden")
d5[,year := as.integer(gsub("[^0-9]", "", year)),]
d5
# we use ggplot to plot data by Area
library(ggplot2)
ggplot(d5, aes(year, RentBurden, group=Borough)) +
  geom_line(aes(color = Borough)) + labs(title= "Rent Burden Time Series", 
                                         subtitle="by Borough")

#ANALYSIS of RESULTS

# the graph shows four  groups: a group of high rent burden (Bronx)
# a group with moderate rent burden 
# a third group with low rent burden (Staten Island)
# and finally the last group with very low rent burden 

# CLUSTER ANALYSIS

# creating data frame
ff.df<-f1.df %>% mutate('PriceCondo' =f1.df$PriceCondo)
ff.df<-ff.df  %>% mutate ('PriceSingle' =f2.df$PriceSingle)
ff.df<-ff.df %>% mutate('HomeOwnerIncome'=f3.df$HomeOwnerIncome)
ff.df<-ff.df %>% mutate('RenterIncome'=f4.df$RenterIncome)
ff.df<-ff.df %>% mutate('RentBurden'=f5.df$RentBurden)
ff.df

# ==========================
# Hierarchical - Agglomerative
# =============================
# All analysis preformed above gave us the idea we should cluster 
# with three clusters
# 

# exploration and data visualization
# first plot REnter Income vs Rent Burden
plot(ff.df$RenterIncome, ff.df$RentBurden, 
     xlim=c(30000,80000), ylim=c(0.2,0.4),
     main = "Relationship Renter Income vs Rent Burden", 
     xlab="Renter Income",
     ylab = "Rent Burden")
# the plot show three groups: low,  2 middle and high renter income
# let's use ggplot. 
ggplot(data=ff.df, aes(x=RenterIncome,y=RentBurden)) + geom_point() +
  labs(y= "Rent Burden", x = "Renter Income") + ggtitle("Scatter plot Renter Income vs Rent Burden")  

# second plot: Renter Income  vs Price condo. I feel like these three variable can drive me in choosing k
plot(ff.df$RenterIncome, ff.df$PriceCondo, 
     xlim=c(30000,80000), ylim=c(300000,1600000),
     main="Relationship Between Renter Income and Price Condominium",
     xlab="RenterIncome",
     ylab="PriceCondo")

# 
# once again it seems that we have 3 groups: 1 low, 1 middle and high level
# it seems to me that we can pick  k= 3

# Distances are highly influenced by the scale of each variable. We need to normalize
# set row names to the borough column. Let's retain as much info as possible
ff.01.df<-ff.df
row.names(ff.01.df) <- ff.01.df[,1]
# remove the name column name
ff.01.df <- ff.01.df[,-1]
# let's normalize........remember mean = 0, sd = 1 from this point of the analysis
ff.norm.df <- sapply(ff.01.df, scale)

# add row names: name are an important info
row.names(ff.norm.df) <- row.names(ff.01.df)
# computing Euclidean distance. 
# to compute other distance measures, change the value in method ="what you want"
# I believe working on Renter Income and Rent Burden makes more sense 
d.norm <- dist(ff.norm.df[,c(4,5)], method = "euclidian")
# we can print all distances
d.norm
# in hclust() set argument method to "ward.D", "single", "complete", "average", "median", or "centroid"
# once again we can pick the distance we want. Several analysts embrace Ward D but let's stay on simple stuff
hc1 <- hclust(d.norm, method = "single")
# let's plot a dendrogram 
plot(hc1, hang = -1, ann = FALSE)

# Single Linkage. let's cut the dendrogram to get 3 clusters
memb <- cutree(hc1, k = 3)
#find number of observations in each cluster
table(memb)
memb
# memb
# 1  2  3 
# 1  3  1
# append cluster labels to original data
final.data.single.df <- cbind(ff.norm.df, cluster = memb)
#display first six rows of final data
head(final.data.single.df)

#finding mean values for each cluster
# we have a vector. We want to use the $ operator, so we need to simply convert it to a data.frame
final.df<-data.frame(final.data.single.df)
aggregate(final.df, by=list(cluster=final.df$cluster), mean)

# setting labels as cluster membership and utility name
row.names(ff.norm.df) <- paste(memb, ": ", row.names(ff.01.df), sep = "")
# plotting heat map
# https://www.r-graph-gallery.com/215-the-heatmap-function.html
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(ff.norm.df), Colv = NA, hclustfun = hclust, cexRow=1, cexCol=0.7,
        col=rev(paste("gray",1:70,sep="")))

# let's create three frame from which we can pick cereals 
ff.cluster1.list.df <- final.df[final.df$cluster ==1,] 
ff.cluster2.list.df <- final.df[final.df$cluster ==2,] 
ff.cluster3.list.df <- final.df[final.df$cluster ==3,] 

# CONCLUSION
# Cluster 1 Bronx: this cluster should receive voucher to buy a house in place of rent vouchers
# Cluster 2: Brooklyn, Queens and Staten Island should receive the same financial help? 
# Cluster 3: Manhattan: with the lowest rent burden (light gray > heat map). I shpuld not be part of this financial aid

