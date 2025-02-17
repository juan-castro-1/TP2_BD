#==================================================#
#                                                  #
#               TRABAJO PRACTICO N�2               #
#                 REGULARIZACION                   #
#                                                  #
#==================================================#
library(readxl)
library(ggplot2)
library(dplyr)
library(xtable)

rm(list=ls())

setwd("C:/Users/juan_/Desktop/Big Data/TP_2")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir()

#aprender <- read.csv("Aprender-2018-primaria-6.csv",header=T,na.strings="?",sep = ";")
#aprender <- read.csv("Aprender-2018-primaria-6.csv",header=T,na.strings="?",sep = ";",dec = ",")

library(data.table)
aprender <- fread("Aprender-2018-primaria-6.csv",header=T,na.strings="?",sep = ";",dec = ",")

#a
tabla_missin <- sapply(aprender, function(x) sum(is.na(x)))
tabla_missin <-data.frame(tabla_missin)
print(xtable(tabla_missin, type = "csv"), file = "missin")

table(is.na(aprender$mpuntaje))
#b
aprender <- na.omit(aprender)
#aprender<-replace(aprender[1:124], aprender[1:124] < 0, NA)
table(aprender$ap2)

#c
io <- read_excel("aprender2018-diccionario-primaria-6.xlsx")
io <- io[,-3]
io <- io[,-3]
io <- na.omit(io)
library(dplyr)
names(io)[1] <- "Variable"
names(io)[2] <- "Etiqueta"
library(writexl)
write_xlsx(io, "C:/Users/juan_/Desktop/Big Data/TP_2/Varbl_Etiqueta.xlsx")

#d
#diccionario <- read_excel("variables.xlsx")
diccionario <- read_excel("Varbl_Etiqueta.xlsx")
diccionario <- data.frame(diccionario,stringsAsFactors=FALSE)
names(aprender)[match(diccionario[,"Variable"],names(aprender))] <- diccionario[,"Etiqueta"]


#3
aprender$`Puntaje en Matem�tica` <- as.numeric(aprender$`Puntaje en Matem�tica`)
aprender$`Puntaje en Lengua` <- as.numeric(aprender$`Puntaje en Lengua`)
aprender$Sexo <- as.factor(aprender$Sexo)
aprender$`Indice socioecon�mico del alumno` <- as.factor(aprender$`Indice socioecon�mico del alumno`)

ej3 <- aprender[!(aprender$Sexo==-6 | aprender$Sexo==-9 | aprender$Sexo==-8),]

a <- ggplot(ej3, aes(x=`Puntaje en Matem�tica`, fill=Sexo))
a + geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer"))

b <- ggplot(ej3, aes(x=`Puntaje en Lengua`, fill=Sexo))
b + geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Hombre", "Mujer"))

#4
library(corrplot)
library(dplyr)
M <- data.frame(aprender$`Puntaje en Lengua`, aprender$`Puntaje en Matem�tica`)
names(M)[1] <- "Lengua"
names(M)[2] <- "Matem�tica"
M <- cor(M)
corM <- corrplot(M, method = "number", order = "AOE", cl.pos = "n", tl.srt = 45)

correlations <- select(aprender, `Puntaje en Lengua`,`Puntaje en Matem�tica`)
cor(correlations)

#5
data <- data.frame(aprender)

colnames(data)[colnames(data) == 'Indice.socioecon�mico.del.alumno'] <- 'a'
data <- data %>% 
  group_by(a) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(a))
data$label <- scales::percent(data$per)
ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=a), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))+
  scale_fill_discrete(name = "Nivel", labels = c("N/A", "Bajo", "Medio", "Alto"))

#6
data <- data.frame(aprender)
soc_prov <- data %>%
  group_by(N�mero.de.jurisdicci�n, Indice.socioecon�mico.del.alumno) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
library(xtable)
print(xtable(soc_prov, type = "latex"), file = "provincias.tex")


##################
#### PUNTO 2 #####
##################

#1
aprender2 <- subset(aprender, select = -`Nivel de desempe�o en Matem�tica`)


aprender2 <- subset(aprender2, select = -c(`Indice socioecon�mico del alumno ponderador Lengua`,
                                           `Indice socioecon�mico del alumno ponderador Matem�tica`,
                                           `Factor de expansi�n (solo para variables cuestionario complementario)`,
                                           `Factor de expansi�n prueba de Matem�tica`,
                                           `Factor de expansi�n prueba de Lengua`) )

#2

library(Matrix) # Estos dos paquetes se necesitan para el paquete glmnet
library(foreach)
library(glmnet)
set.seed(101) # para reproducir los resultados

grid=10^seq(-3,10,length=100)

y <- aprender2$`Puntaje en Matem�tica`
x <- model.matrix(`Puntaje en Matem�tica` ~.,aprender2)[,-1]

cv.out.ridge=cv.glmnet(x ,y ,alpha=0, lambda = grid)
cv.out.lasso=cv.glmnet(x ,y ,alpha=1, lambda = grid)

# Gráfico del ECM y ln de lambda
plot(cv.out.ridge,ylab="Error Cuadr�tico Medio")
title("Ridge", line = 2.5)
plot(cv.out.lasso, ylab="Error Cuadr�tico Medio")
title("LASSO", line = 2.5)
# Una de las l�???neas indica el valor de lambda con menor MSE, la otra el mayor valor de
# lambda cuyo MSE está a un error estándar del menor MSE.

# Lambda óptimo
lam.ridge=cv.out.ridge$lambda.min
lam.ridge

lam.lasso=cv.out.lasso$lambda.min
lam.lasso


# Base de entrenamiento
train=sample(1:nrow(x), nrow(x)/2)
# Base de prueba
test=(-train)
# Vector de respuesta para testear el error de prediccion
y.test=y[test]



#3
mod.lasso <- glmnet(x[train,],y[train], alpha = 1, lambda = grid)
# Computamos los coeficientes para dicho lambda
lasso.coef=predict(mod.lasso,type="coefficients",s=lam.lasso)[1:118,]
# VARIABLES SELECCIONADAS POR LASSO USANDO 10-fold CV son:
length(round(lasso.coef[lasso.coef!=0],4))
abcd <- round(lasso.coef[lasso.coef==0])
View(abcd)


#4
pred.lasso <- predict(mod.lasso, s=lam.lasso, newx = x[test,])
#pred.lasso <- floor(pred.lasso)
#pred.lasso[pred.lasso==0] <- 1
#table_lasso<- table(pred.lasso);table_lasso
mean((pred.lasso-y.test)^{2})

mod.ridge <- glmnet(x[train,],y[train], alpha = 0, lambda = grid)
pred.ridge <- predict(mod.ridge, s=lam.ridge, newx = x[test,])
#pred.ridge <- floor(pred.ridge)
#pred.ridge[pred.ridge<0] <- 0
#table_ridge<- table(pred.ridge);table_ridge
mean((pred.ridge-y.test)^{2})



# El l2 norm 
pos_ridge <- match(lam.ridge, grid)
pos_lasso <- match(lam.lasso, grid)
sqrt(sum(coef(mod.ridge)[-1,pos_ridge]^2)) 
sqrt(sum(coef(mod.lasso)[-1,pos_lasso]^2))

# plots
plot(y.test[1:500], pred.lasso[1:500],ylim=c(250,700),xlim = c(250,700), main = "LASSO",xlab = "Predicci�n", ylab = "Real")
abline(a=0,b=1, col="blue")
plot(pred.lasso[1:500], y.test[1:500],ylim=c(250,700),xlim = c(250,700), main = "Ridge",xlab = "Predicci�n", ylab = "Real")
abline(a=0,b=1, col="red")



#5
plot(mod.ridge,xvar="lambda", label=FALSE , ylim=c(-15,20),xlim=c(-1,15), xlab="Log(Lambda)", ylab="Coeficientes")
title("Ridge", line = 2.5)

plot(mod.lasso,xvar="lambda", label=FALSE , ylim=c(-15,20),xlim=c(-1,15), xlab="Log(Lambda)", ylab="Coeficientes")
title("LASSO", line = 2.5)


#6
count_pred <- length(which(pred.ridge < floor(mean(pred.ridge))))
count_pred

count_pred <- length(which(pred.lasso < floor(mean(pred.lasso))))
count_pred


count_real <- length(which(y.test < floor(mean(y.test))))
count_real


count_pred - count_real




















