# Charts with ggplot ####

# Install packages

install.packages("ggforce")
install.packages("ggalt")
install.packages("reshape2")
install.packages("GGally")
install.packages("treemap")

# libraries

library(tidyverse) # metapackage of all tidyverse packages
library(ggplot2)
library(dplyr)
library(reshape2) # Melt
library(plyr)
library(scales) # visualisation
library(corrplot) # visualisation
library(GGally) # visualisation
library(ggthemes) # visualisation
library(ggalt) # encircle
library(maps) #maps
library(treemap)
library(dplyr)
library(ggdendro) # Dendogram
# Interactivity
library(crosstalk)
library(plotly)
#Date
library(scales)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggforce)
library(ggalt)

fig<-function(x,y){options(repr.plot.width = x, repr.plot.height = y)}

setwd("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R")

# Scatter ####
# Chart 1

titanicdata <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//test.csv")

fig(12,8)

titanicdataset <- ggplot(titanicdata, aes(x=Age, y=Fare)) + 
  geom_point() + 
  labs(y="Fare", 
       x="Age", 
       title="Titanic - Age vs Fare")+
  theme_gray()+
  theme(plot.title = element_text(size=15),axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7), axis.title=element_text(size=15))

ggsave("Titanic - Age vs Fare.png", 
       plot = titanicdataset, 
       width = 6 , height = 4 , units = "in")

# Chart 2

fig(12,8)

titanicdataset2 <- ggplot(titanicdata, aes(x=Age, y=Fare)) + 
  geom_point(aes(color=Sex)) + 
  labs(y="Fare", 
       x="Age", 
       title="Titanic - Age vs Fare against Gender ")+ 
  theme_gray()+ # Default theme 
  theme(plot.title = element_text(size=15),axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7), axis.title=element_text(size=15))
titanicdataset2

ggsave("Titanic - Age vs Fare against Gender.png", 
       plot = titanicdataset2, 
       width = 6, height = 4, units = "in")

# Chart 3 

universityeducation <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//cwurData.csv")

fig(12,8)

Qualityacademic <- ggplot(universityeducation, aes(x=quality_of_education, y=score)) + 
  geom_point(aes(color=country,size=citations)) + 
  labs(x="Quality Of Education", 
       y="Score",
       title="Quality of Education Vs Score against Country & Citations")+ 
  theme_linedraw()+
  theme(plot.title = element_text(size=10),axis.text.x= element_text(size=5),
        axis.text.y= element_text(size=5), axis.title=element_text(size=10))

Qualityacademic

ggsave("Quality of Education Vs Score against Country & Citations.png", 
       plot = Qualityacademic, 
       width = 12, height = 8, units = "in")

# Chart 4

datajobs <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//test insurance.csv")

fig(12,8)

grafico4 <- ggplot(sample_n(datajobs, 500), aes(x=Vintage,y=Annual_Premium,color=Age)) +
  geom_point()+
  scale_color_gradient(low = 'yellow', high = 'red')+
  labs(y="Insurance Amount", 
       x="Days associated with Company", 
       title="Days associated with Company vs Amount to be paid against Age of Customer ")+
  geom_jitter(width = 1, size = 1)+
  theme_dark()+ # Dark Theme
  theme(plot.title = element_text(size=10),axis.text.x= element_text(size=3),
        axis.text.y= element_text(size=3), axis.title=element_text(size=7))

grafico4

ggsave("Days associated with Company vs Amount to be paid against Age of Customer.png", 
       plot = grafico4, 
       width = 12, height = 8, units = "in")

# Chart 5

houseprice <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R/Datasets/train sale price.csv")

fig(12,8)

grafico5 <- ggplot(houseprice, aes(x = LotArea, y = SalePrice, color=LotShape)) +
  geom_jitter(width = 1, size = 1)+  # Adjusting size and width of points
  labs(x="Area",
       y="Sales Price", 
       title="Lot Area vs House Price against LotShape")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10),axis.text.x= element_text(size=3),
        axis.text.y= element_text(size=3), axis.title=element_text(size=7))

grafico5

str(houseprice)

ggsave("Lot Area vs House Price against LotShape.png", 
       plot = grafico5, 
       width = 12, height = 8, units = "in")

# Chart 6

costly_price_with_less_area<-houseprice[houseprice$SalePrice>600000 & houseprice$LotArea<25000,]

fig(12,8)

grafico6 <- ggplot(houseprice, aes(x=LotArea,y=SalePrice,color=LotShape)) +
  geom_jitter(width = 1, size = 1)+ # Adjusting size and width of points
  geom_encircle(aes(x = LotArea, y = SalePrice), 
                data = costly_price_with_less_area, 
                color = "red", 
                size = 2, 
                expand = 0.08)+
  labs(x = "Area",
       y = "Sales Price", 
       title = "Lot Area vs House Price (Circle Costlier House with Less Area) ")+ 
  theme_bw()+
  theme(plot.title = element_text(size = 10),axis.text.x= element_text(size = 3),
        axis.text.y = element_text(size = 3), axis.title=element_text(size = 7))

grafico6

ggsave("Lot Area vs House Price (Circle Costlier House with Less Area).png", 
       plot = grafico6, 
       width = 12, height = 8, units = "in")
# Bubble ####
# Chart 7

file.choose()

healthbuble <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets/train_data stay and age.csv")

healthbuble$Deposit <- healthbuble$Admission_Deposit

colnames(healthbuble)

fig(12,8)

grafico7 <- ggplot(sample_n(healthbuble,100), aes(x=Age,y=Stay)) +
  geom_jitter(aes(size = Admission_Deposit),color="blue")+
  labs(x="Age",
       y="Stay", 
       title=" Age vs Stay against Deposits")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10),axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7), axis.title=element_text(size=10))

grafico7

ggsave("Age vs Stay against Deposits.png", 
       plot = grafico7, 
       width = 12, height = 8, units = "in")

# Chart 8

fig(12,8)

grafico8 <- ggplot(sample_n(healthbuble,200), aes(x=Age,y=Stay)) + 
  geom_jitter(aes(size=Admission_Deposit,color=Stay))+ 
  labs(x="Age", 
       y="Stay", 
       title="Age vs Stay against Deposits")+ 
  theme_bw()+ 
  theme(plot.title = element_text(size=22),
        axis.text.x= element_text(size=15), 
        axis.text.y= element_text(size=15), 
        axis.title=element_text(size=18))+ scale_color_gradient(low = "blue", high = "red")

grafico8

str(healthbuble)

# Chart 9

fig(12,8)

grafico9 <- ggplot(sample_n(healthbuble,200), aes(x=Age,y=Stay)) +
  geom_jitter(aes(size=Admission_Deposit,color=Severity.of.Illness))+
  labs(x="Age",
       y="Stay", 
       title="Age vs Stay against Severity")+ 
  theme_bw()+
  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), axis.title=element_text(size=18))

grafico9 

ggsave("Age vs Stay against Severity.png", 
       plot = grafico9, 
       width = 12, height = 8, units = "in")

# Bar ####

# Chart 10
file.choose()

university <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets//cwurData.csv")
rm(University)
fig(12,8)

grafico10 <- ggplot(university[university$score>60,], aes(country))+
  geom_bar(stat="count", width = 0.5, fill="darkblue")+
  labs(x="Country",
       y="Score", 
       title="Country vs Score above 60 ")+ 
  theme_bw()+
  theme(plot.title = element_text(size=18),
        axis.text.x= element_text(size=6),
        axis.text.y= element_text(size=10), 
        axis.title=element_text(size=18))

grafico10

ggsave("Country vs Score above 60.png", 
       plot = grafico10, 
       width = 12, height = 8, units = "in")

# Chart 11 - Bar chart - Gradient & Text

file.choose()

playstore_data <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets//googleplaystore.csv")

names(playstore_data)

fig(12,8)

grafico11 <- ggplot(playstore_data, aes(Genres,Count,fill=Count))+
  geom_bar(stat="identity", width = 0.5)+
  geom_text(aes(label=Count), vjust=0) +
  scale_fill_gradient(low = "green", high = "red")+
  labs(x="Genre",
       y="Count", 
       title="Distribution of Playstore Genres ")+ 
  theme_bw()+
  theme(plot.title = element_text(size=22),
        axis.text.x= element_text(size=15,angle=90),
        axis.text.y= element_text(size=15), 
        axis.title=element_text(size=18))

grafico11  

# Chart 12 - Bar chart - Stacked & Group

netflixdata <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/netflix_titles.csv")

# Chart 12.1

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

animated_graph <- c("gganimate","ggplot2","dplyr","gapminder","ggthemes","gifski","readr","tidyr")

ipak(animated_graph) # activar todos los paquetes

ind_us_shows<-netflixdata%>%filter( (country == "United States" | country == "India" )& release_year>2015)

grafico12.1 <- ggplot(ind_us_shows, aes(release_year, fill = country)) + 
  geom_bar(stat = "count", position = 'stack', width = 0.5) +  # Stack for stacked chart
  labs(x = "Year",
       y = "Count", 
       title = "Distribution of Netflix Shows in India & US") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 10))

grafico12.1

ggsave("Distribution of Netflix Shows in India & US.png", 
       plot = grafico12.1, 
       width = 12, height = 8, units = "in")

# Chart 12.2

grafico12.2 <- ggplot(ind_us_shows, aes(release_year, fill = country)) + 
  geom_bar(stat = "count", position = 'dodge', width = 0.5) +  # Dodge for group
  labs(x = "Year",
       y = "Count", 
       title = "Distribution of Netflix Shows in India & US (Group) ") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 10))

grafico12.2

ggsave("Distribution of Netflix Shows in India & US (Group).png", 
       plot = grafico12.2, 
       width = 12, height = 8, units = "in")

# chart 13 - Facet Bar

Ind_US_UK_Aus_shows<-netflixdata%>%filter( (country == "United States" | country == "India" | country == "United Kingdom"| country == "Australia" )& release_year>2015)

grafico13 <- ggplot(Ind_US_UK_Aus_shows, aes(release_year))+
  geom_bar(stat="count", width = 0.5,aes(fill=country))+
  labs(x="Genre",
       y="Count", 
       title="Distribution of Netflix Shows in India,US,UK & Australia")+ 
  facet_wrap(~country)+
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.text.x= element_text(size=7,angle=90),
        axis.text.y= element_text(size=7), 
        axis.title=element_text(size=10))

grafico13

ggsave("Distribution of Netflix Shows in India,US,UK & Australia.png", 
       plot = grafico13, 
       width = 12, height = 8, units = "in")

# Chart 14 - Horizontal Bar

hospitaldata <- read.csv ("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/Hospital code.csv") 

hospitaldata <- as.data.frame(table(hospitaldata$Hospital_type_code))
colnames(hospitaldata) <- c('hospital_code', 'count')

grafico14 <- ggplot(hospitaldata, aes(x=hospital_code,y=count))+
  geom_bar(stat="identity",width = 0.5,aes(fill=count))+ 
  scale_fill_gradient(low = "red", high = "blue")+
  coord_flip()+
  labs(x="Hospital Code",
       y="Count", 
       title="Distribution of Hospital Type Code")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.text.x= element_text(size=7,angle=90),
        axis.text.y= element_text(size=7), 
        axis.title=element_text(size=10))

grafico14

ggsave("Distribution of Hospital Type Code.png", 
       plot = grafico14, 
       width = 12, height = 8, units = "in")

# Ranking ####

# Chart 15 - Dot Plot

coviddeath <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/us_counties_covid19_daily.csv")

rm(Coviddeath)

coviddeath<-as.data.frame(aggregate(coviddeath, by=list('Country/Region'),FUN=sum)) colnames(total_death)<-c('country','deaths') total_death<-total_death[order(total_death$deaths),]%>%top_n(10)

fig(12,8)

grafico15 <- ggplot(coviddeath, aes(x=county, y=deaths)) + 
  geom_point(col="blue", size=5) +   # Draw points
  geom_segment(aes(x=county, 
                   xend=county, 
                   y=min(deaths), 
                   yend=max(deaths)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(x="Country",
       y="Number of COVID Deaths", 
       title="Distribution of Deaths across countries ")+ 
  theme_bw()+
  theme(plot.title = element_text(size=22),
        axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), 
        axis.title=element_text(size=18))+
  coord_flip()

grafico15

# Chart 16 - Lollipop Plot

racevictims <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/shootings.csv")

racevictims<-as.data.frame(table(racevictims$race))

racevictims

colnames(racevictims)<-c('race','count')

fig(12,8)

grafico16 <- ggplot(racevictims, aes(x=race, y=count)) + 
  geom_point(size=5) + 
  geom_segment(aes(x=race, 
                   xend=race, 
                   y=0, 
                   yend=count)) + 
  labs(x="Race",
       y="Number of Victims", 
       title="Distribution of US Victims of each Race")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10),
        axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7), 
        axis.title=element_text(size=10))

grafico16

ggsave("Distribution of US Victims of each Race - #16.png", 
       plot = grafico16, 
       width = 12, height = 8, units = "in")

# Chart 17 - Ordered Bar Plot

racevictims<-as.data.frame(table(racevictims$race))

colnames(racevictims)<-c('race','count')

racevictims <- racevictims[order(racevictims$count), ]  # sort

racevictims$race <- factor(racevictims$race, levels = racevictims$race)

fig(12,8)

grafico17 <- ggplot(racevictims, aes(x=race, y=count)) + 
  geom_bar(stat="identity", width=.5, fill="darkblue") + 
  labs(x="Race",
       y="Number of Victims", 
       title="Distribution of US Victims of each race")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico17

ggsave("Distribution of US Victims of each race - #17.png", 
       plot = grafico17, 
       width = 12, height = 8, units = "in")

# Chart 18 - Slope Plot

bank_stock <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/AXISBANK.csv")

axis$year<-format(as.Date(axis$Date, format="%Y-%m-%d"),"%Y")
hdfc$year<-format(as.Date(hdfc$Date, format="%Y-%m-%d"),"%Y")
indus$year<-format(as.Date(indus$Date, format="%Y-%m-%d"),"%Y")
kotak$year<-format(as.Date(kotak$Date, format="%Y-%m-%d"),"%Y")
icici$year<-format(as.Date(icici$Date, format="%Y-%m-%d"),"%Y")

ici<-aggregate(list(icici[icici$year==2019,]$Open,icici[icici$year==2019,]$Close), by=list(year=icici[icici$year==2019,]$year), FUN=mean)
ici$name='icici'
colnames(ici)<-c('year','open','close','name')

axi<-aggregate(list(axis[axis$year==2019,]$Open,axis[axis$year==2019,]$Close), by=list(year=axis[axis$year==2019,]$year), FUN=mean)
axi$name='axis'
colnames(axi)<-c('year','open','close','name')

hdf<-aggregate(list(hdfc[hdfc$year==2019,]$Open,hdfc[hdfc$year==2019,]$Close), by=list(year=hdfc[hdfc$year==2019,]$year), FUN=mean)
hdf$name='hdfc'
colnames(hdf)<-c('year','open','close','name')

kot<-aggregate(list(kotak[kotak$year==2019,]$Open,kotak[kotak$year==2019,]$Close), by=list(year=kotak[kotak$year==2019,]$year), FUN=mean)
kot$name='kotak'
colnames(kot)<-c('year','open','close','name')

ind<-aggregate(list(indus[indus$year==2019,]$Open,indus[indus$year==2019,]$Close), by=list(year=indus[indus$year==2019,]$year), FUN=mean)
ind$name='indus'
colnames(ind)<-c('year','open','close','name')

bank_stock<-rbind(ici,axi,hdf,kot,ind)

left_label <- paste(bank_stock$name, round(bank_stock$open,2),sep=", ")
right_label <- paste(bank_stock$name, round(bank_stock$close,2),sep=", ")
bank_stock$class <- ifelse((bank_stock$open - bank_stock$close) < 0, "red", "green")

fig(12,8)

p<-ggplot(bank_stock) + geom_segment(aes(x=1, xend=2, y=open, yend=close, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x=" ",
       y="Average Price", 
       title="Average Open & Close price of Bank Stocks in 2019")+    # Axis labels
  xlim(.5, 2.5) + ylim(0,(1.1*(max(bank_stock$open, bank_stock$close))))  # X and Y axis limits

# Add texts
p <- p + geom_text(label=left_label, y=bank_stock$open, x=rep(1, NROW(df)), hjust=1.1, size=5)
p <- p + geom_text(label=right_label, y=bank_stock$close, x=rep(2, NROW(df)), hjust=-0.1, size=5)
p <- p + geom_text(label="Open Price", x=1, y=1.1*(max(bank_stock$open, bank_stock$close)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Close Price", x=2, y=1.1*(max(bank_stock$open, bank_stock$close)), hjust=-0.1, size=5)  # title

# Minify theme
p + theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

str(bank_stock)

# Chart 19 - Dumbbell Chart

covid_death <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/covid_19_data.csv")

covid_death<-aggregate(list(covid_death$Deaths,covid_death$Recovered), by=list(country=covid_death$'Country/Region'), FUN=sum)
covid_death=covid_death[-covid_death$recovered,]%>%top_n(10)
covid_death$country <- factor(covid_death$country, levels=as.character(covid_death$country))

fig(12,8)

grafico19 <- ggplot(covid_death, aes(x=death, xend=recovered, y=country, group=country)) + 
  geom_dumbbell(color="#a3c4dc", 
                size=0.75,
                colour_x ="red",
                colour_xend="darkblue",
                size_x = 2.5,
                size_xend = 2.5) + 
  labs(x="",
       y="", 
       title="Deaths and Recovered Cases over countries")+
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))
str(covid_death)
grafico19

# Pie ####
# Chart 20 - Pie Plot

netflix <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets/netflix_titles.csv")

fig(12,8)

grafico20 <- ggplot(netflix, aes(x = "", fill = factor(type))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5,size=10)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Netflix shows")

grafico20 + coord_polar(theta = "y", start=0)

ggsave("Pie Chart of Netflix shows - #20.png", 
       plot = grafico20 + coord_polar(theta = "y", start=0), 
       width = 12, height = 8, units = "in")

# Chart 21 - Color Pie

severity_df <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets/severity_df2.csv")

custom_col <- c("red", "green", "orange") 

severity_df<-as.data.frame(table(severity_df$Severity.of.Illness))
colnames(severity_df)<-c('severity','patientid')
str(severity_df)
fig(12,8)                   

grafico21 <- ggplot(severity_df, aes(x = "", y=patientid, fill = factor(severity))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.3,size=10)) + 
  labs(fill="severity", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of patient severeness") + coord_polar(theta = "y", start=0)+
  scale_fill_manual(values=custom_col)

grafico21

ggsave("Pie Chart of patient severeness - #21.png", 
       plot = grafico21, 
       width = 12, height = 8, units = "in")

# Chart 22 - Customized Pie- Gradient & Text

fig(12,8)                   

grafico22 <- ggplot(severity_df, aes(x = "", y=patientid, fill = severity)) + 
  geom_bar(width = 1, stat = "identity") +coord_polar(theta = "y", start=0)+
  scale_fill_brewer(palette="Blues")+
  labs(fill="Severity", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of patient severeness ") + 
  geom_text(aes(label = patientid),size=2, position = position_stack(vjust = 0.5))+
  theme_void()+theme(plot.title = element_text(hjust=0.5,size=10))

grafico22

ggsave("Pie Chart of patient severeness - #22.png", 
       plot = grafico22, 
       width = 12, height = 8, units = "in")

# Chart 23 - Donut Plot

content_df <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//Datasets/googleplaystore.csv")

content_df<-as.data.frame(table(content_df$Content.Rating))
colnames(content_df)<-c('content','count')
str(content_df)
# Compute percentages
content_df$fraction <- content_df$count / sum(content_df$count)

# Compute the cumulative percentages (top of each rectangle)
content_df$ymax <- cumsum(content_df$fraction)

# Compute the bottom of each rectangle
content_df$ymin <- c(0, head(content_df$ymax, n=-1))

# Compute label position
content_df$labelPosition <- (content_df$ymax + content_df$ymin) / 2

# Compute a good label
content_df$label <- paste0(content_df$content, "\n value: ", content_df$count)

# Make the plot
fig(12,8)  

grafico23 <- ggplot(content_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=content)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=2) +
  scale_fill_brewer(palette=7) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill='content', 
       x=NULL, 
       y=NULL, 
       title="Distribution of Playstore Content Ratings ")+
  theme_void() +
  theme(legend.position = "none")+theme(plot.title = element_text(hjust=0.5,size=10)) 

grafico23

ggsave("Distribution of Playstore Content Ratings - #23.png", 
       plot = grafico23, 
       width = 12, height = 8, units = "in")

# Chart 24 - Ring Plot

fig(12,8)  

grafico24 <- ggplot(content_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=content)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label), size=2) + # x here controls label position (inner / outer)
  #scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  labs( x=NULL, 
        y=NULL, 
        title="Distribution of Playstore Content Ratings")+
  theme_void() +
  theme(legend.position = "none")+theme(plot.title = element_text(hjust=0.5,size=10))

grafico24

ggsave("Distribution of Playstore Content Ratings - #24.png", 
       plot = grafico24, 
       width = 12, height = 8, units = "in")

# Time Series ####
# Chart 25 - Basic Time Series
covid_death <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/covid_19_data.csv")
covid_death$country=covid_death$'Country/Region'
covid_death$ObservationDate<-as.Date(covid_death$ObservationDate, format="%m/%d/%Y")

aus_deaths<-covid_death%>%filter(Country.Region == "Australia")%>%
  group_by(ObservationDate) %>%
  summarise(total = sum(Deaths, na.rm = TRUE))

fig(12,8)

grafico25 <- ggplot(aus_deaths, aes(x=ObservationDate)) + 
  geom_line(aes(y=total)) + 
  labs(x="Date",
       y="Number of Deaths", 
       title="Distribution of COVID Deaths in Australia ")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico25

ggsave("Distribution of COVID Deaths in Australia - #25.png", 
       plot = grafico25, 
       width = 12, height = 8, units = "in")

# Chart 26 - Time Series Color

fig(12,8)
grafico26 <- ggplot(aus_deaths, aes(x=ObservationDate)) + 
  geom_line(aes(y=total),color="red") + 
  labs(x="Date",
       y="Number of Deaths", 
       title="Distribution of COVID Deaths in Australia")+ 
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico26

ggsave("Distribution of COVID Deaths in Australia - #26.png", 
       plot = grafico26, 
       width = 12, height = 8, units = "in")

# Chart 27 - Multiple Time Series

covid_death$Country <- covid_death$CountrY

us_china_deaths<-covid_death%>%filter(Country=='US'|Country=='UK'|Country=='India')%>%
  group_by(Country,ObservationDate) %>%
  summarise(total = sum(Deaths, na.rm = TRUE))

fig(12,8)

grafico27 <- ggplot(us_china_deaths, aes(x=ObservationDate,color=Country)) +
  geom_line(aes(y=total)) +
  labs(x="Date",
       y="Number of Deaths",
       title="Distribution of COVID Deaths in United Kingdom,United States,India")+
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico27

ggsave("Distribution of COVID Deaths in United Kingdom,United States,India - #27.png", 
       plot = grafico27, 
       width = 12, height = 8, units = "in")

# Chart 28 - Format axis - Time Series

fig(12,8)

grafico28 <- ggplot(us_china_deaths, aes(x=ObservationDate,color=Country)) + 
  geom_line(aes(y=total)) + 
  labs(x="Date",
       y="Number of Deaths", 
       title="Distribution of COVID Deaths in UK,US,India")+ 
  scale_x_date(date_labels="%Y-%b",date_breaks="1 month")+  # Formating of x axis - Labels & Date Breaks
  
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico28

ggsave("Distribution of COVID Deaths in UK,US,India - #28.png", 
       plot = grafico28, 
       width = 12, height = 8, units = "in")


# Box ####
# Chart 29 - Basic Box Plot

university <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/cwurData.csv")

german_university<-university%>%filter(country=='Germany')

fig(12,8)

grafico29 <- ggplot(german_university, aes(y=score))+ 
  geom_boxplot(varwidth=T, fill="lightblue")  +
  labs(x="",
       y="Score", 
       title="Distribution of German Universities Score")+  
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico29

ggsave("Distribution of German Universities Score - #29.png", 
       plot = grafico29, 
       width = 12, height = 8, units = "in")


# Chart 30 - Multiple Box Plot

three_university<-university%>%filter(country=='Germany'|country=='United Kingdom'|country=='Canada')

fig(12,8)

grafico30 <- ggplot(three_university, aes(y=score,fill=country))+ 
  geom_boxplot(varwidth=T)  +
  labs(x="",
       y="Score", 
       title="Distribution of German ,United Kingdom & Canada Universities Score")+  
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico30

ggsave("Distribution of German ,United Kingdom & Canada Universities Score - #30.png", 
       plot = grafico30, 
       width = 12, height = 8, units = "in")

# Chart 31 - Grouped Box Plot

titanic <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/Titanicdata.csv")


fig(12,8)

grafico31 <- ggplot(titanic, aes(factor(Pclass), Age)) + 
  geom_boxplot(aes(fill=factor(Sex))) +
  labs(x="Pclass",
       y="Age", 
       fill="Gender",
       title="Distribution of Titanic Passengers Age from different class")+  
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico31

ggsave("Distribution of Titanic Passengers Age from different class - #31.png", 
       plot = grafico31, 
       width = 12, height = 8, units = "in")

# Chart 32 - Customized - Box Plot

fig(12,8)

grafico32 <- ggplot(titanic, aes(factor(Pclass), Age)) + 
  geom_boxplot(aes(fill=factor(Sex))) +
  labs(x="Pclass",
       y="Age", 
       fill="Gender",
       title="Distribution of Titanic Passengers Age from different Class")+ 
  scale_fill_brewer(palette="Dark2")+  # Custom theme
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10),
        legend.position="bottom") +   # change legend position
  scale_x_discrete(limits=c("2", "3", "1"))

grafico32

ggsave("istribution of Titanic Passengers Age from different Class - #32.png", 
       plot = grafico32, 
       width = 12, height = 8, units = "in")

# Chart 33 - Box & Dot Plot

fig(12,8)
grafico33 <- ggplot(titanic, aes(factor(Sex), Age)) + 
  geom_boxplot(aes(fill=factor(Sex))) +
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  labs(x="Gender",
       y="Age", 
       fill="Gender",
       title="Distribution of Titanic Passengers Age from different Gender")+  
  theme_bw()+
  theme(plot.title = element_text(size=10)
        ,axis.text.x= element_text(size=7),
        axis.text.y= element_text(size=7),
        axis.title=element_text(size=10))

grafico33

ggsave("Distribution of Titanic Passengers Age from different Gender - #33.png", 
       plot = grafico33, 
       width = 12, height = 8, units = "in")

# Chart 34 - Tufte Boxplot

fig(12,8)
ggplot(house, aes(SaleCondition, SalePrice))+
  geom_tufteboxplot() +
  labs(x="Sales Condition",
       y="Price", 
       title="Distribution of House Prices for different Sales Condition")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 35 - Basic Histogram

fig(12,8)
ggplot(com_df, aes(x=salary)) + 
  geom_histogram(binwidth=10000,fill="magenta")+
  labs(x="Salary",
       y="Count", 
       title="Distribution of Salaries for Comm&Mgmt")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 36 - Histogram Stacked

fig(12,8)
ggplot(campus, aes(x=salary, color=specialisation,fill=specialisation)) +
  geom_histogram(alpha=0.2,position="identity",binwidth=10000)+
  labs(x="Salary",
       y="Count", 
       title="Distribution of Salaries for different Specialisation")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 37 - Histogram - Mean & Line type

fig(12,8)
ggplot(cipla, aes(x=Open)) + 
  geom_histogram(binwidth=50,fill="lightblue",linetype="dashed",color="black",size=2)+
  geom_vline(aes(xintercept=mean(Open)),
             color="blue", linetype="dashed", size=2)+
  labs(x="Open Price",
       y="Count", 
       title="Distribution of CIPLA Open Price")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 38 - Facet Histogram

fig(12,8)
ggplot(med_stock, aes(x=Open)) + 
  geom_histogram(binwidth=50,aes(fill=Symbol))+
  facet_grid(Symbol ~ .)+
  labs(x="Open Price",
       y="Count", 
       title="Distribution of Pharma Stocks Open Price")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 39 - Basic Density Plot

fig(12,8)
ggplot(death_by_state, aes(x=freq))+
  geom_density(color="darkblue", fill="lightblue",alpha=0.7)+
  labs(x="Deaths Per State",
       y="Count", 
       title="Distribution of Victims in United States")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Chart 40 - Multiple Density

fig(12,8)
ggplot(health_df, aes(x=age))+
  geom_density(aes(fill=Severity),alpha=0.7)+
  labs(x="Age",
       y="Count", 
       title="Distribution of Patiens age for different Severity")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))
