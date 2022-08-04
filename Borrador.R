library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)
library(cowplot)

setwd("C:/Users/Samuel/Desktop/Data.Science UCAB/ProyectoFinal")
data_2016 <- read_csv("2016.csv")%>% 
            rename( Calification = `Happiness Score`)
data_2016$`Lower Confidence Interval` = NULL
data_2016$`Upper Confidence Interval` = NULL

View(data_2016)
head(data_2016)

# Comparacion de niveles de felicidad entre regiones

tabla1 <- data_2016 %>% 
              group_by(Region) %>% 
              summarise(average = mean(Calification)) %>% 
              arrange(-average) %>% 
              ungroup()

mean_calification <- mean(data_2016$Calification)

grafica1 <- ggplot(data = tabla1, aes(x = Region, y = average, fill = Region))+
  geom_bar(stat = "summary", fun = mean)+
  geom_hline(yintercept=mean_calification, linetype="dashed", color = "#CD0000", size = 2)

grafica1 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, ))

# Distribucion normal del comportamiento de las calificaciones de Felicidad, con una recta horizontal de la media

grafica2 <- ggplot(data = data_2016, aes(Calification))+
  geom_histogram(color =  "#5F9EA0",  fill = "#8EE5EE",   alpha = .5,
                 position = "dodge", binwidth = .10 ,aes(y = ..density..))+
  stat_function(fun = dnorm,
                args = list(mean = mean(data_2016$Calification),
                            sd = sd(data_2016$Calification)),
                col = "#BF3EFF",
                size = 1)+
  geom_vline(aes(xintercept = mean(Calification)), 
             color = "#698B22", linetype = "dashed", size = 1.5)
grafica2


# Que variable tienen mayor ponderacion en el nivel de Felicidad

GDPporc <- mean(data_2016$`Economy (GDP per Capita)`) * 100 / mean_calification
familyporc <- mean(data_2016$Family) * 100 / mean_calification
healthporc <- mean(data_2016$`Health (Life Expectancy)`) * 100 / mean_calification
freedomporc <- mean(data_2016$Freedom) * 100 / mean_calification
corruptionporc <- mean(data_2016$`Trust (Government Corruption)`) * 100 / mean_calification
generosityporc <- mean(data_2016$Generosity) * 100 / mean_calification
dystopiaporc <- mean(data_2016$`Dystopia Residual`) * 100 / mean_calification

tabla2 <- data.frame(Variables = c("GDP", "Family", "Health", "Freedom", "Corruption",
                                 "Generosity", "Dystopia"),
                     Porcentajes = c(GDPporc, familyporc, healthporc, freedomporc,
                                     corruptionporc, generosityporc, dystopiaporc))

grafica3 <- ggplot(data = tabla2, aes(x = Variables, y = Porcentajes, fill = Variables))+
  geom_bar(stat = "summary", fun = mean)

grafica3 +
  theme(axis.text.x = element_text(angle = 70, hjust = 1, ))
