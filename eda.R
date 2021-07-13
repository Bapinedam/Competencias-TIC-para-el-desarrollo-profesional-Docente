library(dplyr)
library(googlesheets4)
library(ggplot2)
library(scales)

# Autenticación de usuario

gs4_auth()

# Obtención de los datos

url = gs4_get("https://docs.google.com/spreadsheets/d/1ktKLnH8EcjV6fIBfQsfD6PZa7GKEWnvlshdGimlplJY/edit#gid=906242353")
data = read_sheet(url)

data$`Edad (en años)` = as.numeric(data$`Edad (en años)`)

# Análisis exploratorio de datos

columnas = data.frame(Columnas = colnames(data))

  ## Sexo

table(data$Sexo)
data_sexo = data %>% select(Sexo) %>% group_by(Sexo) %>% tally()
data_sexo$Porcentaje = data_sexo$n / sum(data_sexo$n) 

ggplot(data = data_sexo, aes(x = Sexo, y = Porcentaje)) + 
  geom_bar(stat = "identity", fill="#f68060", alpha=.6, width=.4) +
  scale_y_continuous(labels = percent_format()) +
  ylab("") +
  theme_bw()

  ## Edad

summary(data$`Edad (en años)`)

layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(data$`Edad (en años)`, horizontal=TRUE , ylim=c(20,60), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(data$`Edad (en años)`, breaks=40, col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="Edad", xlim=c(20,60))

  ## Edad x sexo

data_edad_sexo = data %>% select(Sexo, `Edad (en años)`) %>% filter(Sexo != "Prefiero no decirlo")

ggplot(data = data_edad_sexo, aes(x = Sexo, y = `Edad (en años)`, fill = Sexo)) +
  geom_boxplot(alpha=0.7, width=.4) + 
  theme_bw()
  

