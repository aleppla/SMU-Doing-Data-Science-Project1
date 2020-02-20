#summarize of data data

install.packages("vctrs")
library(ggplot2)
library(dplyr)
library(ggplot2)
summary(beers)
summary(Breweries)

str(beers)
str(Breweries)

state <-is.numeric(Breweries$State)

head(subset(Breweries, select = 'State'))
stcount <- table(Breweries$State)

#number of N/A values = 1067 how to treat?
sum(is.na(beers))

#omit beers
beers=na.omit(beers)

#Breweries with missing values
sum(is.na(Breweries))


#EDA Merge the beer and breweries data, the common identifier is brew ID
#change column name so they are they same to brew id
colnames(beers)[5]="Brew_ID"

#merge by Brew_ID
final=merge(beers,Breweries,by=c("Brew_ID"))

#Median ABV and IBU
final%>%group_by(State)%>%summarise(median(ABV),median(IBU))

#store values
median=as.data.frame(final%>%group_by(State)%>%summarise(median(ABV),median(IBU)))
#label columns
colnames(median)=c("State","ABV","IBU")

#create barplots one below another
par(mfrow=c(2,1))
barplot(median$ABV,xlab="State Name",names.arg=median$State,las=2,ylab="Alcohol by volume of the beer",main="Comparision of median ABV and IBU for each state")
barplot(median$IBU,xlab="State Name",names.arg=median$State,las=2,ylab="International Bitterness Units of the beer")

#which state has the highest IBU

ggplot(data=final) +
  geom_histogram(aes(x=ABV),bins=20)

#right skewed, mean is greater than median
summary(final$ABV)


#bitterness as dependend and abv as independent, bitterness increases as alc increases
ggplot(data=final) +
  geom_point(aes(x=ABV,y=IBU))