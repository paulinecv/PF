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

# Grafica1. Comparacion de niveles de felicidad entre regiones

tabla1 <- data_2016 %>% 
  group_by(Region) %>% 
  summarise(Medias = mean(Calificacion)) %>% 
  arrange(-Medias) %>% 
  ungroup()

mean_calificacion <- mean(data_2016$Calificacion)

grafica1 <- ggplot(data = tabla1, aes(x = Region, y = Medias, fill = Region))+
  geom_bar(stat = "summary", fun = mean)+
  geom_hline(yintercept=mean_calificacion, linetype="dashed", color = "#CD0000", size = 2)+
  ggtitle("Grafica1. Niveles de Felicidad por Regiones" )

grafica1 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


# Grafica2. Ponderacion de variable en el nivel de Felicidad

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

grafica2 <- ggplot(data = tabla2, aes(x = Variables, y = Porcentajes, fill = Variables))+
  geom_bar(stat = "summary", fun = mean)+
  ggtitle("Grafica2. Ponderacion en el nivel de Felicidad" )

grafica2 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, ))


# Grafica3. La relacion del PIB per capita con las 4 variables encuestadas

tabla3 <- data_2016 %>% 
  group_by(Region, PIB_per_capita) %>% 
  summarise(puntaje = sum(Familia, Libertades, Generosidad, 
                          Corrupcion)) %>% 
  arrange(-puntaje) %>% 
  ungroup() 

grafica3 <- filter(tabla3, Region %in% c("Australia and New Zealand", "Western Europe",
                                         "North America","Latin America and Caribbean")) %>%
  ggplot(aes(PIB_per_capita, puntaje, color = Region)) +
  geom_point(size = 2) +
  facet_grid(. ~ Region)+
  ggtitle("Grafica3. Relacion del PIB per capita con las 4 variables encuestadas" )
grafica3


# Grafica4. La relacion de la Esperanza de vida con las 4 variables encuestadas

tabla4 <- data_2016 %>% 
  group_by(Region, Esperanza_vida) %>% 
  summarise(puntaje = sum(Familia, Libertades, Generosidad, 
                          Corrupcion)) %>% 
  arrange(-puntaje) %>% 
  ungroup() 

grafica4 <- filter(tabla4, Region %in% c("Australia and New Zealand", "Western Europe",
                                         "North America","Latin America and Caribbean")) %>%
  ggplot(aes(Esperanza_vida, puntaje, color = Region)) +
  geom_point(size = 2) +
  facet_grid(. ~ Region)+
  ggtitle("Grafica4. Relacion de la Esperanza de vida con las 4 variables encuestadas" )
grafica4


# Regresion lineal: puntaje = intercepto + B*PIB_per_capita + e 
modelo_1 <- lm(Calificacion ~ PIB_per_capita, data = data_2016, na.action = na.exclude)
summary(modelo_1)

#Grafico de dispersion
grafica5 <- ggplot(data_2016, aes(PIB_per_capita, Calificacion)) +
            geom_point() +
            geom_smooth(method = "lm", colour = "Red")
grafica5

#Aqui se hace un grafico con multiples variables

#Creo que solo deberiamos usar el plot grid de abajo
#los tres primeros sugieren una relacion mas clara entre felicidad y las variables
#mientras que el ultimo "Libertades", no es tan clara porque parece ser mas dispersa.



grafica6<- data_2016 %>% ggplot() + 
  geom_point(aes(Familia , Calificacion)) +
  geom_smooth(aes(Familia,Calificacion),method=lm)+
  xlab('Familia')
grafica6

grafica7 <- data_2016 %>% ggplot() + 
  geom_point(aes(data_2016$`Esperanza_vida` , Calificacion)) +
  geom_smooth(aes(data_2016$`Esperanza_vida`, Calificacion), method=lm)+
  xlab('Salud/Esperanza de vida')
grafica7

grafica8 <- data_2016 %>% ggplot() + 
  geom_point(aes(data_2016$`PIB_per_capita` , Calificacion)) +
  geom_smooth(aes(data_2016$`PIB_per_capita`,Calificacion),method=lm)+
  xlab('GDP')
grafica8

grafica9 <- data_2016 %>% ggplot() + 
  geom_point(aes(data_2016$`Libertades` , Calificacion)) +
  geom_smooth(aes(data_2016$`Libertades`,Calificacion),method=lm)+
  xlab('Libertades')
grafica9

plot_grid(
  grafica6, 
  grafica7,
  grafica8,
  grafica9
)

# Regresion lineal: puntaje = intercepto + B*Libertades 
modelo_2 <- lm(Calificacion ~ Libertades, data = data_2016, na.action = na.exclude)
summary(modelo_2)

#el modelo 2 sugiere que aunque las libertades si son significativas, el error 
#standard es mucho mayor que con el PIb per capita.


