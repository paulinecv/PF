library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(cowplot)
library(coche)
library(boot)
library(QuantPsyc)

setwd("C:/Users/Samuel/Desktop/Data.Science UCAB/ProyectoFinal")
data_2016 <- read_csv("2016.csv")%>% 
  rename( Calificacion = `Happiness Score`,
          PIB_per_capita = `Economy (GDP per Capita)`,
          Esperanza_vida = `Health (Life Expectancy)`,
          Corrupcion = `Trust (Government Corruption)`,
          Pais = `Country`,
          Ranking_felicidad = `Happiness Rank`,
          Familia = `Family`,
          Generosidad = `Generosity`,
          Libertades = `Freedom`) 
data_2016$`Lower Confidence Interval` = NULL
data_2016$`Upper Confidence Interval` = NULL


View(data_2016)
head(data_2016)

# Comparacion de niveles de felicidad entre regiones

tabla1 <- data_2016 %>% 
  group_by(Region) %>% 
  summarise(Medias = mean(Calificacion)) %>% 
  arrange(-Medias) %>% 
  ungroup()

mean_calificacion <- mean(data_2016$Calificacion)

grafica1 <- ggplot(data = tabla1, aes(x = Region, y = Medias, fill = Region))+
  geom_bar(stat = "summary", fun = mean)+
  geom_hline(yintercept=mean_calificacion, linetype="dashed", color = "#CD0000", size = 2)

grafica1 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, ))


# Distribucion normal del comportamiento de las calificaciones de Felicidad, con una recta horizontal de la media

grafica2 <- ggplot(data = data_2016, aes(Calificacion))+
  geom_histogram(color =  "#5F9EA0",  fill = "#8EE5EE",   alpha = .5,
                 position = "dodge", binwidth = .10 ,aes(y = ..density..))+
  stat_function(fun = dnorm,
                args = list(mean = mean(data_2016$Calificacion),
                            sd = sd(data_2016$Calificacion)),
                col = "#BF3EFF",
                size = 1)+
  geom_vline(aes(xintercept = mean(Calificacion)), 
             color = "#698B22", linetype = "dashed", size = 1.5)
grafica2


# Que variable tienen mayor ponderacion en el nivel de Felicidad

GDPporc <- mean(data_2016$PIB_per_capita) * 100 / mean_calificacion
familiaPorc <- mean(data_2016$Familia) * 100 / mean_calificacion
VidaPorc <- mean(data_2016$Esperanza_vida) * 100 / mean_calificacion
LibertadesPorc <- mean(data_2016$Libertades) * 100 / mean_calificacion
CorrupcionPorc <- mean(data_2016$Corrupcion) * 100 / mean_calificacion
GenerosidadPorc <- mean(data_2016$Generosidad) * 100 / mean_calificacion
DystopiaPorc <- mean(data_2016$`Dystopia Residual`) * 100 / mean_calificacion

tabla2 <- data.frame(Variables = c("GDP", "Familia", "Esperanza_vida", "Libertades", "Corrupcion",
                                   "Generosidad", "Dystopia"),
                     Porcentajes = c(GDPporc, familiaPorc, VidaPorc, LibertadesPorc,
                                     CorrupcionPorc, GenerosidadPorc, DystopiaPorc))

grafica3 <- ggplot(data = tabla2, aes(x = Variables, y = Porcentajes, fill = Variables))+
  geom_bar(stat = "summary", fun = mean)

grafica3 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, ))


# Graficas sobre la relacion del PIB per capita con las 4 variables encuestadas del infome de la felicidad "Familia, Libertades, Generosidad, Corrupcion"
# Con esto se busca hallar una relacion de las variables encuestas que respondieron las personas con la variable calculada del PIB per capita
# La comparacion se realiza un cuadro comparativo por cada region de los datos

tabla3 <- data_2016 %>% 
  group_by(Region, PIB_per_capita) %>% 
  summarise(puntaje = sum(Familia, Libertades, Generosidad, 
                          Corrupcion)) %>% 
  arrange(-puntaje) %>% 
  ungroup() 

grafica4 <- filter(tabla3, Region %in% c("Australia and New Zealand", "Western Europe",
                                         "North America","Latin America and Caribbean")) %>%
  ggplot(aes(PIB_per_capita, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica4

grafica5 <- filter(tabla3, Region %in% c("Eastern Asia", "Middle East and Northern Africa",
                                         "Central and Eastern Europe", "Southeastern Asia")) %>%
  ggplot(aes(PIB_per_capita, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica5

grafica6 <- filter(tabla3, Region %in% c("Southern Asia", "Sub-Saharan Africa")) %>%
  ggplot(aes(PIB_per_capita, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica6


# Graficas sobre la relacion de la Esperanza de vida con las 4 variables encuestadas del infome de la felicidad "Familia, Libertades, Generosidad, Corrupcion"
# Con esto se busca hallar una relacion de las variables encuestas que respondieron las personas con la variable calculada de la Esperanza de vida
# La comparacion se realiza un cuadro comparativo por cada region de los datos

tabla4 <- data_2016 %>% 
  group_by(Region, Esperanza_vida) %>% 
  summarise(puntaje = sum(Familia, Libertades, Generosidad, 
                          Corrupcion)) %>% 
  arrange(-puntaje) %>% 
  ungroup() 

grafica7 <- filter(tabla4, Region %in% c("Australia and New Zealand", "Western Europe",
                                         "North America","Latin America and Caribbean")) %>%
  ggplot(aes(Esperanza_vida, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica7

grafica8 <- filter(tabla4, Region %in% c("Eastern Asia", "Middle East and Northern Africa",
                                         "Central and Eastern Europe", "Southeastern Asia")) %>%
  ggplot(aes(Esperanza_vida, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica8

grafica9 <- filter(tabla4, Region %in% c("Southern Asia", "Sub-Saharan Africa")) %>%
  ggplot(aes(Esperanza_vida, puntaje, color = Region)) +
  geom_point() +
  facet_grid(. ~ Region)
grafica9


# Regresion lineal: puntaje = intercepto + B*PIB_per_capita 
modelo_1 <- lm(Calificacion ~ PIB_per_capita, data = data_2016, na.action = na.exclude)
summary(modelo_1)

#Grafico de dispersion
grafico10 <- ggplot(data_2016, aes(PIB_per_capita, Calificacion)) +
            geom_point() +
            geom_smooth(method = "lm", colour = "Red")
grafico10

#Aqui se hace un grafico con multiples variables

grafico_11<- data_2016 %>% ggplot() + 
  geom_point(aes(Familia , Calificacion)) +
  geom_smooth(aes(Familia,Calificacion),method=lm)+
  xlab('Familia')

grafico_11








