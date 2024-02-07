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

houseprice <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//train sale price.csv")

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
       plot = grafico4, 
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
  geom_jitter(aes(size = Admission_Deposit, color = Stay))+
  labs(x="Age",
       y="Stay", 
       title="Age vs Stay against Deposits")+ 
  theme_bw()+
  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), axis.title=element_text(size=18))+
        scale_color_gradient(low = "blue", high = "red")
grafico8

str(healthbuble$Age)

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

# by Xavier

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

animated_graph <- c("gganimate","ggplot2","dplyr","gapminder","ggthemes","gifski","readr","tidyr")

ipak(animated_graph) # activar todos los paquetes

ind_us_shows<-netflixdata%>%filter( (country == "United States" | country == "India" )& release_year>2015)

ggplot(ind_us_shows, aes(release_year, fill = country)) + 
  geom_bar(stat = "count", position = 'stack', width = 0.5) +  # Stack for stacked chart
  labs(x = "Year",
       y = "Count", 
       title = "Distribution of Netflix Shows in India & US") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 7, angle = 90),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 10))

