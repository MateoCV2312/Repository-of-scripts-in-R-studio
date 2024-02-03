# Animed graph GIF with gganimate and ggplot

#link https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

animated_graph <- c("gganimate","ggplot2","dplyr","gapminder","ggthemes","gifski","readr","tidyr")

ipak(animated_graph) # activar todos los paquetes

str(gapminder)
dim(gapminder)

setwd("C:/Users/profesor.salud/Desktop/gganimed and ggplot")

graph1 = gapminder %>% ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop)) + scale_size(range=c(2,12), guide="none")+ geom_point(alpha=0.7, stroke = 0) + theme_fivethirtyeight() + scale_x_log10() + labs(title = "Life Expectancy vs GDP",x = "Income",y = "Life Expectancy",color = "Continent",caption = "Source: Gapminder") + 
  theme(axis.title = element_text(),text = element_text(family = "Rubik"), legend.text = element_text(size=10)) + scale_color_brewer(palette = "Set2")

#grafica de gapminder

graph1

#agregar la animación dentro de la grafica ggplot

graph1.animation = graph1 + transition_time(year) + labs(subtitle = "Year: {frame_time}") + shadow_wake(wake_length = 0.1)

graph1.animation

animate(graph1.animation, height = 500, width = 800, fds = 30, duration = 10, end_pause =  60, res = 100)
anim_save("gapminder graph.gif")

#Getting Data
#extraer la database de https://www.kaggle.com/datasets/gregorut/videogamesales/data
game_sales = read_csv("C:/Users/profesor.salud/Desktop/gganimed and ggplot/vgsales.csv") %>% 
  mutate(Year = as.numeric(Year)) %>% filter(Platform == "PS3", Genre %in% c("Action","Shooter","Sports","Racing","Simulation")) %>% drop_na() %>%
  group_by(Year, Genre) %>%
  summarise(Sales = sum(Global_Sales, na.rm = TRUE))

View(game_sales)

graph2 <- game_sales %>%
  ggplot(aes(x=Year, y=Sales, color=Genre))+
  geom_line(size=2, alpha=0.75)+
  theme_solarized_2(light = FALSE)+
  labs(title="Video Game Sales",
       y = "Global Sales")+
  theme(text = element_text(family = "DM Sans Medium", colour = "#EEEEEE"),
        title = element_text(color = "#EEEEEE"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#111111"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))+ scale_color_brewer(palette = "Pastel1") + geom_point()+scale_x_continuous(breaks = 0:2100)

View(graph2)
graph2

graph2.animation <- graph2 + transition_reveal(Year) + view_follow(fixed_y = TRUE)

animate(graph2.animation, height = 500, width = 800, fds = 30, duration = 5, end_pause =  60, res = 100)
graph2.animation
anim_save("ps3 game sales.gif")
