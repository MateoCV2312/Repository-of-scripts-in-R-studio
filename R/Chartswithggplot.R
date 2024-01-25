# Charts with ggplot

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

# Chart 1

titanicdata <- read.csv("C://Users//MATEO//OneDrive//Documentos//Getting staRted Into R//test.csv")

fig<-function(x,y){options(repr.plot.width = x, repr.plot.height = y)}

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

grafico4 <- ggplot(datajobs, aes(x=Vintage,y=Annual_Premium,color=Age)) +
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