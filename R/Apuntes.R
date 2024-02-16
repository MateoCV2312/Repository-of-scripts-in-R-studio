# importantes para R (tidyverse)

# Eliminar "." y juntar nombres (sin espacios) #

colnames(Data) <- gsub("\\.", "", colnames(Data))

# Cambiar " " por "_" (sin espacios) #

names(Data) <- gsub("[[:space:]+]", "_", names(Data))

# Reemplazar "%" por nada para correr la columna como num y no chr #

Data$CocoaPercent <- sapply(Data$CocoaPercent, function(x) gsub("%", "", x))

# Función del "tidyverse" para corregir datos automaticamente #

Data <- type_convert(Data)

# Agrupación por colummna específica #

Data %>%
  group_by(ReviewDate) %>%
  summarise(averageRating = mean(CocoaPercent),
            sdRating = sd(CocoaPercent))

# Eliminar el conjunto de datos #

rm(mi_dataset)

#funciones para crear un grafico de dsipersión#

ggplot(chocolateData, aes(x= ReviewDate, y = Rating))
ggplot(chocolateData, aes(x= ReviewDate, y = Rating)) + geom_point()
ggplot(chocolateData, aes(x= ReviewDate, y = Rating)) + geom_point() + geom_jitter()

#Función para colocar un línea de acuerdo al número de participantes en el gráfico de dispersión#

ggplot(chocolateData, aes(x= ReviewDate, y = Rating)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm')

# añadir otro argumento a la función aes(), diciéndole que trace la columna Porcentaje_Cacao en color #

ggplot(chocolateData, aes(x= ReviewDate, y = Rating, color = CocoaPercent)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm')

# save our plot to a variable with an informative name #

chocolateRatingByReviewDate <- ggplot(chocolateData, aes(x= Review_Date, y = Rating, color = Cocoa_Percent)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = 'lm')

# save our plot #

ggsave("chocolateRatingByReviewDate.png", # the name of the file where it will be save
       plot = chocolateRatingByReviewDate, # what plot to save
       height=6, width=10, units="in") # the size of the plot & units of the size

# Cambiar el nombre de una columna #

# Cambiando el nombre de una columna llamada "old_name" a "new_name" #

names(dataframe)["old_name"] <- "new_name"

# Cambiando el nombre de una columna llamada "old_name" a "new_name" #

dataframe$new_name <- dataframe$old_name

# Crear nuevo spript desde la consola #

usethis::use_r("Examenfinal")
