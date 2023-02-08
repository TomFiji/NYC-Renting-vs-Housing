library("readxl")
library("magrittr")
library("tidyverse")
income.df <- read_excel("NYC-housing-data.xlsx", sheet = "renter income")
income.df <- income.df[,-c(1,2,19,20)]
income.2018.df <-aggregate(income.df$"2018", by=list(income.df$Borough),FUN=mean) 
income.final.df<-income.2018.df %>% mutate('2018' =income.2018.df$x)
income.final.df<-income.final.df[,-c(2)]
colnames(income.final.df)<- c("Borough", "Average_Income_in_2018")

burden.df <- read_excel("NYC-housing-data.xlsx", sheet = "rent burden")
burden.df <- burden.df[,-c(1,2,20,21)]
burden.2018.df <-aggregate(burden.df$"2018", by=list(burden.df$Borough),FUN=mean) 
burden.final.df<-burden.2018.df %>% mutate('2018' =burden.2018.df$x)
burden.final.df<-burden.final.df[,-c(2)]
colnames(burden.final.df)<- c("Borough", "Average_Renter_Burden_in_2018")

complete <-data.frame(income.final.df, burden.final.df$Average_Renter_Burden_in_2018)
complete['Average_Rent_per_Month'] = (complete$Average_Income_in_2018*complete$burden.final.df.Average_Renter_Burden_in_2018)/12
