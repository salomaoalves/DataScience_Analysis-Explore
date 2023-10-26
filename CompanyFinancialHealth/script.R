#Packages
library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)

#Load Data
df <- read_excel("Netflix_Data.xlsx", sheet='Dados')
colnames(df) <- c('Time','TotalSubscriptions','PaidSubscriptions','FreeTrails','Revenue',
                  'CostRevenues','Marketing','ContributionProfit','ContributionMargin',
                  'CostPerCustomer','RevenuePerCustomer','EarningPerCustomer','Segment')
View(df)
dim(df)
str(df)
getwd()


#Data Clean

##format field Time
fGetYear <- function(sTime) {
  return(strsplit(sTime,',')[[1]][2])
}
df$Year <- unlist(lapply(df$Time, fGetYear))
df$Year <- as.factor(df$Year)

fGetMonth <- function(sTime) {
  return(strsplit(sTime,' ')[[1]][1])
}
df$Month <- unlist(lapply(df$Time, fGetMonth))
df$Month <- as.factor(df$Month)

fGetDay <- function(sTime) {
  return(strsplit(strsplit(sTime,' ')[[1]][2],',')[[1]][1])
}
df$Day <- unlist(lapply(df$Time, fGetDay))
df$Day <- as.factor(df$Day)

fGetDate <- function(sTime) {
  sYear <- fGetYear(sTime)
  sMonth <- fGetMonth(sTime)
  if(sMonth=='December'){ 
    sMonth <- '12' 
  } else if(sMonth=='March'){ 
    sMonth <- '03' 
  } else if(sMonth=='June'){ 
    sMonth <- '06'  
  } else { 
    sMonth <- '09' 
  }
  sDay <- fGetDay(sTime)
  return(paste(sDay,sMonth,sYear, sep=''))
}
df$Date <- unlist(lapply(df$Time, fGetDate))
df$Date <- as.Date(df$Date, "%d%m%Y")

df$Time <- NULL

##remove fields with cardinality = 1
unique(df$Segment)
df$Segment <- NULL

##date normalized
df_normalized <- data.frame(df)
?tracemem(df_normalized)==tracemem(df)
df_normalized[,1:11] <- scale(df[,1:11])
View(df_normalized)
str(df_normalized)


#Explore

##Statistical Summary
### Date fields
summary(df[,c('Year','Month','Day')])

### Fields about the subscriptions
summary(df[,c('TotalSubscriptions','PaidSubscriptions','FreeTrails')])
data.frame(TotalSubscriptions=c(sd(df$TotalSubscriptions),var(df$TotalSubscriptions)),
           PaidSubscriptions=c(sd(df$PaidSubscriptions),var(df$PaidSubscriptions)),
           FreeTrails=c(sd(df$FreeTrails),var(df$FreeTrails)),
           row.names = c('Std Deviation','Variance'))

### Fields about the revenue and costs
summary(df[,c('Revenue','CostRevenues','Marketing')])
data.frame(Revenue=c(sd(df$Revenue),var(df$Revenue)),
           CostRevenues=c(sd(df$CostRevenues),var(df$CostRevenues)),
           Marketing=c(sd(df$Marketing),var(df$Marketing)),
           row.names = c('Std Deviation','Variance'))

### Fields about the contribution
summary(df[,c('ContributionProfit','ContributionMargin')])
data.frame(ContributionProfit=c(sd(df$ContributionProfit),var(df$ContributionProfit)),
           ContributionMargin=c(sd(df$ContributionMargin),var(df$ContributionMargin)),
           row.names = c('Std Deviation','Variance'))

### Fields 'per Customer'
summary(df[,c('CostPerCustomer','RevenuePerCustomer','EarningPerCustomer')])
data.frame(CostPerCustomer=c(sd(df$CostPerCustomer),var(df$CostPerCustomer)),
           RevenuePerCustomer=c(sd(df$RevenuePerCustomer),var(df$RevenuePerCustomer)),
           EarningPerCustomer=c(sd(df$EarningPerCustomer),var(df$EarningPerCustomer)),
           row.names = c('Std Deviation','Variance'))


##Individual Analysis
### For Total Subscriptions
gTotalSubs <- 
  ggplot(df,aes(x=Month,y=TotalSubscriptions,group=Year)) + 
  geom_line(aes(linetype=Year)) + 
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Paid Subscriptions
gPaidSubs <- 
  ggplot(df,aes(x=Month,y=PaidSubscriptions,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Contribution Profit
gContribProfit <- 
  ggplot(df,aes(x=Month,y=ContributionProfit,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Contribution Margin
gContribMargin <- 
  ggplot(df,aes(x=Month,y=ContributionMargin,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Cost Per Customer
gCostCustomer <- 
  ggplot(df,aes(x=Month,y=CostPerCustomer,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Revenue Per Customer
gRevenueCustomer <- 
  ggplot(df,aes(x=Month,y=RevenuePerCustomer,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

### For Earning Per Customer
gEarningCustomer <- 
  ggplot(df,aes(x=Month,y=EarningPerCustomer,group=Year)) + 
  geom_line(aes(linetype=Year)) +
  geom_point(aes(shape=Year)) + 
  scale_x_discrete(limits=c('March','June','September','December')) +
  theme_minimal()

plot_grid(gTotalSubs, gPaidSubs, gContribProfit, gContribMargin,
          gCostCustomer, gRevenueCustomer, gEarningCustomer, 
          labels=c("TSubs", "PSubs", "Profit", "Margin", 
                   "CostCusto", "RevCusto", "EarnCusto"), 
          ncol=3, nrow=3)

##Double Analysis
gSubscriptions <- 
  ggplot(df_normalized,aes(x=Date)) +
  geom_line(aes(y=TotalSubscriptions, colour='TotalSubscriptions'),linetype=2) + 
  geom_point(aes(y=TotalSubscriptions),shape=11) + 
  
  geom_line(aes(y=PaidSubscriptions, colour='PaidSubscriptions'),linetype=6) + 
  geom_point(aes(y=PaidSubscriptions),shape=3) +
  
  scale_colour_manual("", breaks = c("TotalSubscriptions", "PaidSubscriptions"),
                      values = c("pink4", "pink1")) +
  labs(y='') + theme_minimal()

gContribution <-
  ggplot(df_normalized,aes(x=Date)) +
    geom_line(aes(y=ContributionProfit,colour='ContributionProfit'),linetype=3) + 
    geom_point(aes(y=ContributionProfit),shape=15) + 
    
    geom_line(aes(y=ContributionMargin,colour='ContributionMargin'),linetype=2) +
    geom_point(aes(y=ContributionMargin),shape=17) +
  
    scale_colour_manual("",breaks=c("ContributionProfit","ContributionMargin"),
                        values=c("rosybrown4","cyan4")) +    
    labs(y='') + theme_minimal()
  
gPerCustomer <- 
  ggplot(df_normalized,aes(x=Date)) +
    geom_line(aes(y=CostPerCustomer,colour='CostPerCustomer'),linetype=3) + 
    geom_point(aes(y=CostPerCustomer),shape=17) + 
    
    geom_line(aes(y=RevenuePerCustomer,colour='RevenuePerCustomer'),linetype=2) + 
    geom_point(aes(y=RevenuePerCustomer),shape=15) +
    
    geom_line(aes(y=EarningPerCustomer,colour='EarningPerCustomer'),linetype=6) + 
    geom_point(aes(y=EarningPerCustomer),shape=19) +
  
    scale_colour_manual("",breaks=c("CostPerCustomer","RevenuePerCustomer","EarningPerCustomer"),
                        values=c("green4","cyan4","purple2")) +

    labs(y='') + theme_minimal()

plot_grid(gSubscriptions,gContribution, gPerCustomer, 
          labels=c("Subscriptions","Contibution", "Per Customer"), 
          ncol=1, nrow=3)



#Hypothesis Test


# Mann-Whitney test

## Null Hypothesis: median of the total subscription is equal to the median of the subscription paid
## Alter. Hypothesis: median of total subscription is different from the median of paid subscription
## p-val=0.6 --> do not reject the null hypothesis
wilcox.test(df$TotalSubscriptions, df$PaidSubscriptions, conf.level=0.95) 


# Test t

## Null Hypothesis: mean of the total subscription is equal to the mean of the subscription paid
## Alter. Hypothesis: mean of total subscription is different from the mean of paid subscription
## p-val=0.6 --> do not reject the null hypothesis
t.test(df$TotalSubscriptions, df$PaidSubscriptions, conf.level=0.95)

## Null Hypothesis: mean of the total subscription is equal to 70000
## Alter. Hypothesis: mean of total subscription is greater then 70000
## p-val=1.089e-12 --> reject the null hypothesis
t.test(df$Revenue, conf.level=0.95, mu=70000, alternative=c('greater'))

## Null Hypothesis: mean of the total subscription is equal to 70000
## Alter. Hypothesis: mean of total subscription is less then 70000
## p-val=1 --> do not reject the null hypothesis
t.test(df$Revenue, conf.level=0.95, mu=70000, alternative=c('less'))



#ANOVA
## Null Hypothesis: variance of the total subscription is equal between the factors
## Alter. Hypothesis: variance of total subscription is different between the factors
## p-val=2e-16 --> reject the null hypothesis
boxplot(df$TotalSubscriptions~factor(df$Year),xlab='Year',ylab='TotalSubscriptions')
year_aov <- aov(df$TotalSubscriptions~factor(df$Year))
summary(year_aov)
