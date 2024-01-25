# R primera clase ######
print("Hello Mateo")
#Lectura archivo TXT
Mateo1 <- read.table("Data de trabajo/data.txt.txt", header=T, sep=",")
data
#Lectura achivo CSV
Mateo2 <- read.csv("Data de trabajo/data_cvs.csv", header=T, sep=",", dec=",")
#Exploratorio
head(Mateo2)
names(Mateo2)
#intalación
install.packages("readxl")
install.packages("tidyverse")
# Abrir paquetes instalados
library(readxl)
library(tidyverse)
#leer un archivo excel
Mateo3 <- readxl::read_xlsx("Data de trabajo/data_excel.xlsx")
head(Mateo3)


# R segunda clase ######
# Script para analisis de datos
# ricardo.joshua.rojas@gmail.com 
# Escuela de medicina humana, UPeU

# Instalar paquetes necesarios ####
install.packages("readxl") #leer excel
install.packages("tidyverse") #Tidydata
install.packages("janitor") #limpieza de datos
install.packages("compareGroups") #cuadros descriptivos y comparativos
install.packages("naniar") #limpieza de datos
# Abrir paquetes instalados ####
library(readxl)
library(tidyverse)
library(janitor)
library(compareGroups)
library(naniar)
# Ingresar data set 1 #######
MCV1 <- read.csv("Data de trabajo/data_cvs.csv", header=T, sep=",", dec=",")

# Ingresar data set 2 #######
MCV1 <- read_xlsx("")

# Analisis exploratorio #######

head(MCV1) # Observar las primeras 6 observaciones 
names(MCV1) # Visualizar las variables disponibles
MCV1 <- clean_names(MCV1) # Unificar nombres variables 
str(MCV1) # Identificar tipos de variables

# Conversion de variables #######

MCV1$pad_mm_hg <- as.numeric(MCV1$pad_mm_hg)
MCV1$pas_mm_hg <- as.numeric(MCV1$pas_mm_hg)
# Limpieza de datos #######

view(miss_var_summary(MCV1)) #data missing
MCV1 <- drop_na(MCV1$edad) # borrar Na´s
# Analisis descriptivo #######
# Tablas descriptivas con tablas 

tabla <- compareGroups(data = MCV1, method = NA) # method: 1=normal 2=no normal 3=categorical NA=automatico

tabla1 <- createTable(tabla) #crear tabla

tabla1 #ver tabla

export2xls(tabla1 ,"tabla1.xlsx") # exporta el excel




# R tercera clase ######
# Histogramas

## Edad trabajadores
hist(MCV1$edad, main="Trabajadores de salud Huaycan",
     xlab = "Edad (Años)",
     ylab ="Frecuencia",
     col= "black",
     border = "white")

## Colesterol 
hist(MCV1$colesterol_total_mg_dl, main="Trabajadores de salud Huaycan",
     xlab = "Colesterol (mg/dL)",
     ylab ="Frecuencia",
     col= "black",
     border = "white")

## Sexo
hist(MCV1$percent_grasa, main="Trabajadores de salud Huaycan",
     xlab = "Porcentaje de grasa corporal",
     ylab ="Frecuencia",
     col= "black",
     border = "white")



# R cuarta clase
# Establece ruta de trabajo #######

setwd("C:/Users/MATEO/OneDrive/Documentos/Mateo Pre examen/Data de trabajo")

## Para comprobar que se estableció la ruta con éxito ejecute el siguiente comando:

getwd()

# Importar datasets ######
## Importar archivos.csv:

MCV1 <- read.csv("data_cvs.csv", header=T, sep=",", dec=",")

## Importar archivos.xlsx:

data1 <- read_xlsx("data_MetS.xlsx")

# Paquetes para la practica en R ########

install.packages("foreign")
install.packages("readxl")
install.packages("tidyverse")
install.packages("janitor")
install.packages("compareGroups")
install.packages("nortest")
install.packages ("MASS")
install.packages ("FSA")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("ggsci")
install.packages("magrittr")
install.packages("sjPlot")
install.packages("survival")
install.packages("survminer")
install.packages("flexsurv")
install.packages("mice")

library(foreign)
library(readxl)
library(tidyverse)
library(janitor)
library(compareGroups)
library(nortest)
library(MASS)
library(FSA)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggsci)
library(magrittr)
library(sjPlot)
library(survival)
library(survminer)
library(flexsurv)
library(mice)

# Análisis exploratorio y limpieza de datos ########
## Visualizar los 6 primeros datos del dataset:

head(MCV1)

## Visualizar los 6 ultimos datos del dataset:

tail(MCV1)

## Visualizar los nombres de las variables:

names(MCV1)

## Identificar tipos de variables:

str(MCV1)

## Visualizar el contenido de una variable:

MCV1$sexo

summary(MCV1$edad)

table(MCV1$sexo)

# Tabla de doble entrada #######

table(MCV1$sexo, MCV1$clasificacion_rcv)

prop.table(table(MCV1$sexo, MCV1$clasificacion_rcv))

tab * 100 

# Limpieza de variables #######

## Estandarizar nombres de variables:

MCV1 <- clean_names(MCV1)

names(MCV1)

# Análisis de normalidad #######
## test de shapiro wilk para una variable

shapiro.test(MCV1$trigliceridos_ml_dl)

## test de Kolmogorov-Smirnov
lillie.test(MCV1$trigliceridos_ml_dl)

# Pruebas estadísticas - Pruebas bivariadas #######

## Chi cuadrado # variables categóricas

str(MCV1)

chisq.test(table(MCV1$sexo,MCV1$fumador), correct = FALSE)

## U de Mann Whitney # var-numérica y var-categórica

wilcox.test(MCV1$colesterol_total_mg_dl~MCV1$fumador)

## T de student para muestras independientes # var-numérica y var-categórica

t.test(MCV1$colesterol_total_mg_dl~MCV1$fumador)

## Correlacion Pearson 

cor.test(MCV1$colesterol_total_mg_dl,MCV1$pas_mm_hg) # var-dependiente y var-independiente

## Correlacion de Spearman # var-independiente y var-dependiente

lillie.test(MCV1$glucosa_mg_dl) # ver normalidad

cor.test(MCV1$colesterol_total_mg_dl,MCV1$pas_mm_hg,method="spearman",data=MCV1)

cor.test(MCV1$glucosa_mg_dl,MCV1$hb_a1c,method="spearman",data=MCV1)

names(MCV1)
# Graficos de dispersion #######
glu <- MCV1[,5]
hbA1c <- MCV1[,9]

## Graficos de dispersion
*pch: permite dar la forma de los puntos depende de codigos "21=bolas"
*bg: color del contenido del punto
*col: color del borde del punto
*cex: tamaño del punto
*legend : permite agragar leyendas al grafico
# var-independiente y var-dependiente
plot(glu, hbA1c, main= "Correlacion glu-hba1c",
     xlab="Glucosa (mg/dL)",
     ylab="HbA1c (%)",
     xlim= NULL,
     ylim=NULL,
     col="blue",
     bg="black",
     pch=20,
     cex=1.0)

# Analisis de la varianza (ANOVA) #########
str(data)
## categorizar variable numerica
table(MCV1$imc)
table(MCV1$cc_cm)
MCV1$imc_cat <- cut(MCV1$imc,
                    breaks = c(0, 25, 30, 100),
                    labels = c("normal", "sobrepeso", "obesidad"), 
                    include.lowest = T,
                    right = F) 
table(MCV1$imc_cat)
names(MCV1$colesterol_total_mg_dl)

ANOVA <- aov(cc_cm~imc_cat,data=MCV1) # efecto (var-numérica) y tratamiento (var-categorica-politomica)
summary(ANOVA)
TukeyHSD(ANOVA) # análisis post-hoc

# Test de Kruskal wallis #######
kruskal.test(cc_cm~imc_cat,data=MCV1) # efecto (var-numérica) y tratamiento (var-categorica-politomica)
pairwise.wilcox.test(x = MCV1$cc_cm, g = MCV1$imc_cat, p.adjust.method = "holm" ) # análisis post-hoc


# Graficos de cajas y bigotes #######
#convertir variables
names(MCV1)
cc <- MCV1[,3]
imc <- MCV1[,21]
# Gráfico
boxplot(cc~imc,
        main= "Wata e IMC",
        xlab="IMC",
        ylab="Circunferencia cintura (cm)",
        col=c("skyblue2","blue","yellow"))


# R Quinta clase
# Instalar paquetes necesarios #########
install.packages("readxl") # Te permite abrir archivos de exceñ 
install.packages("tidyverse") # herramientas para limpieza de datos 
install.packages("janitor") # herramientas para limpieza de datos - clean names 
install.packages("compareGroups") # tablas descriptivas y comparativas 
install.packages("naniar") # complemento de limpieza de datos 
install.packages("nortest") # para usar test normalidad
install.packages("sjPlot") # para usar test normalidad

library(readxl)
library(tidyverse)
library(janitor)
library(compareGroups)
library(naniar)
library(nortest)
library(sjPlot)

# Ruta de trabajo ########

setwd ("C:/Users/MATEO/OneDrive/Documentos/Mateo Pre examen/Data de trabajo")

# Ingresar data set 2 #######
MCV2 <- read.csv("data_cvs.csv", header=T, sep=",", dec=",")

# Analisis exploratorio ########
head(MCV2) # Observar las primeras 6 variables 
names(MCV2) # Visualizar las variables disponibles 
MCV2 <- clean_names(MCV2) # Unificar nombres variables 
str(MCV2) # Identificar tipos de variables

# Analisis bivariado - factores asociados a RCV ######
names(MCV2)

table (MCV2$clasificacion_rcv)

table2 <- compareGroups(data=MCV2, formula=clasificacion_rcv ~.,
                        byrow=T,
                        method = NA)

table3 <- createTable(table2)

table3

## Regresion de de Poisson con var robusta bivariada y multivariable

str(MCV2$clasificacion_rcv)

MCV2[MCV2$clasificacion_rcv=="Bajo",]$clasificacion_rcv <- 0

MCV2[MCV2$clasificacion_rcv=="Alto",]$clasificacion_rcv <- 1

MCV2$clasificacion_rcv <- as.numeric(MCV2$clasificacion_rcv)

names (MCV2)

model <- glm (data = MCV2,
              formula= clasificacion_rcv ~ diabetes+sexo+edad+cc_cm+colesterol_total_mg_dl+trigliceridos_ml_dl+dheas_ng_ml,
              family = quasipoisson)

tab_model(model)