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
prueba

#Comparar regiones entre si

tabla1 <- data_2016 %>% 
              group_by(Region) %>% 
              summarise(average = mean(Calification)) %>% 
              arrange(-average) %>% 
              ungroup()
              
y <- mean(data_2016$Calification)
