library(tidyverse)
library(readxl)
library(readr)
library(ggplot2)

install.packages('cowplot')
library(cowplot)

setwd("C:/Users/Samuel/Desktop/Data.Science UCAB/ProyectoFinal")
data_2016 <- read_csv("2016.csv")%>% 
            rename( Calification = `Happiness Score`)
View(data_2016)
head(data_2016)

# Comparar regiones entre si

tabla1 <- data_2016 %>% 
              group_by(Region) %>% 
              summarise(average = mean(Calification)) %>% 
              arrange(-average) %>% 
              ungroup()


# Que variable tienen mayor ponderacion en el nivel de Felicidad

mean_calification <- mean(data_2016$Calification)

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

ggplot(data = tabla2, aes(x = Variables, y = Porcentajes, fill = Variables))+
  geom_bar(stat = "summary", fun = mean)
