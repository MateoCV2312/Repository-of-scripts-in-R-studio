# Cargar CSV

titanic <- read.csv("C:/Users/MATEO/OneDrive/Documentos/Getting staRted Into R/Datasets/Titanicdata.csv")

# Mostrar filas iniciales

head(titanic)

# Mostrar nombres de columnas y tipos de variable

str(titanic)

# Número de filas y columnas

dim(titanic)

# Agregar índices para luego eliminarlos

titanic <- cbind(Índice = 1:nrow(titanic), titanic)

# Eliminar colummna Ïndice

titanic <- subset(titanic, select = -Índice)

# ¿Cuántas personas de sexo masculino y femenino están representadas en el dataset?


table(titanic$Sex)
314 + 577

# ¿Cuál es la edad promedio del sexo masculino?

aggregate(Age ~ Sex, data = titanic, FUN = mean)

# Convertit valores numericos a categoricos según el tipo de pasaje

titanic$Pclass <- factor(titanic$Pclass, levels = c(1, 2, 3), labels = c("Pobre", "Normal", "VIP"))

# Agrupar segun el tipo de pasaje on su respectivo nombre

titanic_pobre <- titanic[titanic$Pclass == "Pobre", c("Name", "Pclass")]
titanic_normal <- titanic[titanic$Pclass == "Normal", c("Name", "Pclass")]
titanic_vip <- titanic[titanic$Pclass == "VIP", c("Name", "Pclass")]

# Print

titanic_pobre
titanic_normal
titanic_vip

# Contar la cantidad de pasajeros en cada tipo de pasaje
cantidad_pobre <- nrow(titanic[titanic$Pclass == "Pobre", ])
cantidad_normal <- nrow(titanic[titanic$Pclass == "Normal", ])
cantidad_vip <- nrow(titanic[titanic$Pclass == "VIP", ])

# Calcular el total de pasajeros
total_pasajeros <- nrow(titanic)

# Calcular el porcentaje de cada tipo de pasaje
porcentaje_pobre <- (cantidad_pobre / total_pasajeros) * 100
porcentaje_normal <- (cantidad_normal / total_pasajeros) * 100
porcentaje_vip <- (cantidad_vip / total_pasajeros) * 100

# Imprimir los resultados
cat("Porcentaje de pasajeros Pobres:", porcentaje_pobre, "%\n")
cat("Porcentaje de pasajeros Normales:", porcentaje_normal, "%\n")
cat("Porcentaje de pasajeros VIP:", porcentaje_vip, "%\n")

# Promedio de edad por tipo de pasaje

aggregate(Age ~ Pclass, data = titanic, FUN = mean, na.rm = TRUE)

# Dividir los datos en subconjuntos basados en la letra inicial de 'Cabin'
subconjuntos <- split(titanic, substr(titanic$Cabin, 1, 1))

# Crear datasets distintos dependiendo de la letra inicial de 'Cabin'
for (letra_inicial in names(subconjuntos)) {
  assign(paste0("datos_", letra_inicial), subconjuntos[[letra_inicial]][, c("Cabin", "Name")])
}

# Verificar los nuevos datasets
print(datos_A)
print(datos_B)
print(datos_C)
print(datos_D)

# Cambiar datos de muerte

titanic$Survived <- factor(titanic$Survived, levels = c(0, 1), labels = c("No", "Sí"))

# Gráfica

titanic<-as.data.frame(table(titanic$Pclass))
titanic<-titanic[order(-titanic$Freq),] %>% top_n(10)
colnames(titanic)<-c('Class','Count')
str(titanic)
fig(12,8)

titanicgraph <- ggplot(titanic, aes(Class,Count,fill=Count))+
  geom_bar(stat="identity", width = 0.5)+
  geom_text(aes(label=Count), vjust=0) +
  scale_fill_gradient(low = "green", high = "red")+
  labs(x="Class",
       y="Count",
       title="Distribution of Playstore Genres ")+
  theme_bw()+
  theme(plot.title = element_text(size=10),axis.text.x= element_text(size=7,angle=90),
        axis.text.y= element_text(size=7), axis.title=element_text(size=10))
titanicgraph

ggsave("Count Vs Class titanic.png", 
       plot = titanicgraph, 
       width = 12, height = 8, units = "in")
