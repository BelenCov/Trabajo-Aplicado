

rm(list=ls())

setwd("~/DIPLOMADO BIG DATA/PROYECTO FINAL/BASE DATOS")

library(tidyverse)
library(ggplot2)
library(readxl)
library(outreg)
library(plyr)
library(compare)
library(psych) # Para usar describeBy para hacer summaries
library(doBy)
library(dplyr)
library(BalancedSampling) # Esto entrega muestras balanceadas

library(ggcorrplot) # Para graficar correlaciones (requiere ggplot2)

library(C50) # Decision Tree
library(gmodels) #Para hacer las tablas de contingencia



# Datos, ver la calidad de los datos, cuales no sirven (estan correlacionados)

# ver la y 

# Baseline: modelo random con metrica de interes
#o el modelo del experto,
#predecir con una sola varialbe (la mas correlacionada) usando arboles dedecision o regresion
# Regresion  por mayoria

bd <- readRDS("bd.rds")

#bd <- subset(bd,anno>2015)



bd$rut_op <- paste(bd$rut,bd$n_op, sep="_")
bd$dup <- duplicated(bd$rut_op)
bd1 <- subset(bd,!dup)
bd1 <- subset(bd1,!res_estado_op=="2 Terminada") #Elimina las observaciones term de pagar y en mora
bd1 <- subset(bd1,!res_estado_op=="3 En Mora")
bd1 <- subset(bd1,anno>2015) # Se eliminan los casos del anno 2015


#Generacion de "Yes"
###########################



# Numero de operaciones por beneficiario


bd1$contador <- 1
n_op_rut <- summaryBy( contador~ rut, FUN=sum, data=bd1)#cuenta numero de operaciones por rut
bd1$contador <-NULL
names(n_op_rut) <- c("rut","numero_operaciones")
summary(n_op_rut$numero_operaciones)
table(n_op_rut$numero_operaciones)

bd1_1 <- merge(bd1,n_op_rut,"rut")
bd1_1 <-subset(bd1_1,numero_operaciones>1) # Esto deja los casos con mas de una operacion
bd1_1 <- bd1_1[order(bd1_1$rut, bd1_1$periodo),] #Aca se ordenan por rut y periodo 

bd1_1$per1 <- as.numeric(bd1_1$periodo)

bd1_1$meses_pasados <- bd1_1$per1-lag(bd1_1$per1)
bd1_1$meses_pasados[bd1_1$rut!=lag(bd1_1$rut)] <- NA
bd1_1$meses_pasados2 <- bd1_1$meses_pasados+lag(bd1_1$meses_pasados)
bd1_1$meses_pasados3 <- bd1_1$meses_pasados+lag(bd1_1$meses_pasados2)
bd1_1$meses_pasados4 <- bd1_1$meses_pasados+lag(bd1_1$meses_pasados3)
bd1_1$meses_pasados5 <- bd1_1$meses_pasados+lag(bd1_1$meses_pasados4)

bd1_1$meses_pasados_tot <- ifelse(!is.na(bd1_1$meses_pasados5),
                                  bd1_1$meses_pasados5, 
                                  ifelse(!is.na(bd1_1$meses_pasados4),
                                         bd1_1$meses_pasados4,
                                         ifelse(!is.na(bd1_1$meses_pasados3),
                                                bd1_1$meses_pasados3,
                                                ifelse(!is.na(bd1_1$meses_pasados2),
                                                       bd1_1$meses_pasados2,
                                                       bd1_1$meses_pasados
                                                       
                                                ))))

bd1_1$meses_pasados_tot[is.na(bd1_1$meses_pasados_tot)] <- 0

table(duplicated(bd1_1$rut))
table(bd1_1$meses_pasados_tot)

# Generacion de df para reshape de ventas
bd_ventas <- data.frame(rut=bd1_1$rut, ventas_uf=bd1_1$ventas_uf, meses_pasados=bd1_1$meses_pasados_tot)

bd_ventas<-reshape(bd_ventas, timevar = "meses_pasados", idvar ="rut", direction = "wide")#Trasponer base
names(bd_ventas)

bd_ventas <- bd_ventas[c( "rut", "ventas_uf.0", "ventas_uf.1", "ventas_uf.2" , "ventas_uf.3",
                          "ventas_uf.4", "ventas_uf.5", "ventas_uf.6", "ventas_uf.7", 
                          "ventas_uf.8",  "ventas_uf.9", "ventas_uf.10", "ventas_uf.11", 
                          "ventas_uf.12",  "ventas_uf.13", "ventas_uf.14", "ventas_uf.15", 
                          "ventas_uf.16", "ventas_uf.17", "ventas_uf.18", "ventas_uf.19", 
                          "ventas_uf.20", "ventas_uf.21", "ventas_uf.22", "ventas_uf.23"  
  
)]

# Calculo de ventas promedio para diferentes periodos
bd_ventas$prom_vtas <- rowMeans(bd_ventas[,2:25], na.rm =TRUE)
bd_ventas$prom_vtas_post <- rowMeans(bd_ventas[,3:25], na.rm =TRUE) #promedio vtas 1 mes despu?s del otorgamiento del credito
bd_ventas$prom_vtas_6_11 <- rowMeans(bd_ventas[,8:13], na.rm =TRUE)
bd_ventas$prom_vtas_12_mas <- rowMeans(bd_ventas[,14:25], na.rm =TRUE)

bd_ventas$mediana_vtas_post <- apply(bd_ventas[,3:25],1, median, na.rm = TRUE) #Esto calcula la mediana

bd_ventas$prom_vtas_6_11[bd_ventas$prom_vtas_6_11==0] <- NA
bd_ventas$prom_vtas_12_mas[bd_ventas$prom_vtas_12_mas==0] <- NA


bd_ventas <- bd_ventas[c( "rut", "ventas_uf.0", "prom_vtas_6_11", "prom_vtas_12_mas",
                          "ventas_uf.1", "ventas_uf.2" , "ventas_uf.3",
                          "ventas_uf.4", "ventas_uf.5", "ventas_uf.6", "ventas_uf.7", 
                          "ventas_uf.8",  "ventas_uf.9", "ventas_uf.10", "ventas_uf.11", 
                          "ventas_uf.12",  "ventas_uf.13", "ventas_uf.14", "ventas_uf.15", 
                          "ventas_uf.16", "ventas_uf.17", "ventas_uf.18", "ventas_uf.19", 
                          "ventas_uf.20", "ventas_uf.21", "ventas_uf.22", "ventas_uf.23"  
                          
)]




# Creacion de bd con ventas promedio y calculo de diferencia relativas
bd_ventas1 <- data.frame(rut=bd_ventas$rut, ventas_inicio=bd_ventas$ventas_uf.0,
                         prom_vtas_6_11=bd_ventas$prom_vtas_6_11, 
                         prom_vtas_12_mas=bd_ventas$prom_vtas_12_mas)


bd_ventas1$dif_rel6 <- (bd_ventas1$prom_vtas_6_11/bd_ventas1$ventas_inicio)-1
bd_ventas1$dif_rel12 <- (bd_ventas1$prom_vtas_12_mas/bd_ventas1$ventas_inicio)-1

bd_ventas1$tipo_dif_6 <- ifelse(bd_ventas1$dif_rel6< -.05, "1. Menor a -5%",
                                ifelse(bd_ventas1$dif_rel6<.05, "2.-5% y 5%",
                                              "3. Mayor a 5%"
))


#Graficos incorporados por mi para ver el rango de crecimiento relativo a los 6 y 12 meses

ggplot(bd_ventas1, aes(dif_rel6)) +
  geom_histogram()+
  coord_cartesian(xlim = c(-.05, 7.5))+
ggtitle("Distribucion de Diferencias relativas ventas a los 6 meses")

ggplot(bd_ventas1, aes(dif_rel12)) +
  geom_histogram()+
  coord_cartesian(xlim = c(-.05, 15))+
ggtitle("Distribucion de Diferencias relativas ventas a los 6 meses")

bd_ventas1$tipo_dif_6 <- ifelse(bd_ventas1$dif_rel6< -.05, "1. Menor a -5%",
                                ifelse(bd_ventas1$dif_rel6<.05, "2. -5% y 5%",
                                       "3. Mayor a 5%"
                                ))

table(bd_ventas1$tipo_dif_6)

bd_ventas1$tipo_dif_12 <- ifelse(bd_ventas1$dif_rel12< -.05, "1. Menor a -5%",
                                       ifelse(bd_ventas1$dif_rel12<.05, "2. -5% y 5%",
                                                     "3. Mayor a 5%"
                                                     ))



#22737

table(bd_ventas1$tipo_dif_12)

#12509
  
summary(bd_ventas1)

hist(bd_ventas1$dif_rel6)
hist(bd_ventas1$dif_rel12)

table(bd_ventas1$tipo_dif_6)
table(bd_ventas1$tipo_dif_12)

# Cargando atributos por beneficiario
########################################

bd1_atrib <- data.frame(rut=bd1_1$rut, n_op=bd1_1$n_op, monto_op=bd1_1$monto_op,
                    n_cuotas=bd1_1$n_cuotas, tasa_anual=bd1_1$tasa_anual, region=bd1_1$region,tipo=bd1_1$tipo_benef,
                    acteco=bd1_1$acteco, objeto_op=bd1_1$objeto_op, plazo=bd1_1$plazo_total_op,
                     periodo=bd1_1$periodo, meses_pasados=bd1_1$meses_pasados_tot, mes=bd1_1$mes)
#inicialmente hay 107.139 filas

bd1_atrib <-subset(bd1_atrib, meses_pasados==0) # Los atributos que se usan son los del primer prestamo

#disminuy? a 45.739 filas
#ojo ac? se consider? el monto de la primera operaci?n, ?qu? pasa con las dem?s?
# Son del "futuro", por lo que no se deben usar en principio.Es posible generar un modelo que las incorpore, pero es otra cosa.

bd1_atrib$meses_pasados <- NULL


# Pegando las bases
####################

bd_ventas2 <-merge(bd_ventas1,bd1_atrib,"rut")


bd_ventas2 <- bd_ventas2[c( "rut", "tipo_dif_6", "tipo_dif_12", "ventas_inicio","monto_op", "tasa_anual","plazo","region", "tipo",
                            "acteco", "objeto_op", "mes" , "periodo", "prom_vtas_6_11", "prom_vtas_12_mas", "dif_rel6",         
                            "dif_rel12", "n_op","n_cuotas")]

#bd_ventas2$ventas_inicio <- log10(bd_ventas2$ventas_inicio)
#bd_ventas2$monto_op <- log10(bd_ventas2$monto_op)


# Generando las Variables de atributos

bd_ventas2$es_mujer <- 0
bd_ventas2$es_mujer[bd_ventas2$tipo=="PERSONA NATURAL MUJER"] <- 1
bd_ventas2$es_hombre <- 0
bd_ventas2$es_hombre[bd_ventas2$tipo=="PERSONA NATURAL HOMBRE"] <- 1
bd_ventas2$es_pj <- 0
bd_ventas2$es_pj[bd_ventas2$tipo=="PERSONA JURÍDICA"] <- 1

bd_ventas2$para_cap_trab <- 0
bd_ventas2$para_cap_trab[bd_ventas2$objeto_op=="CAPITAL DE TRABAJO"] <- 1
bd_ventas2$para_inv <- 0
bd_ventas2$para_inv[bd_ventas2$objeto_op=="INVERSIÓN"] <- 1
bd_ventas2$para_cap_trab_inv <- 0
bd_ventas2$para_cap_trab_inv[bd_ventas2$objeto_op=="INVERSIÓN Y CAPITAL DE TRABAJO"] <- 1


bd_ventas2$comercio <- 0
bd_ventas2$comercio[bd_ventas2$acteco=="COMERCIO AL POR MENOR, RESTAURANTES Y HOTELES"] <- 1

bd_ventas2$alimento <- 0
bd_ventas2$alimento[bd_ventas2$acteco=="INDUSTRIA DE PRODUCTOS ALIMENTICIOS, BEBIDAS Y TABACO"] <- 1

bd_ventas2$agricultura <- 0
bd_ventas2$agricultura[bd_ventas2$acteco=="AGRICULTURA Y GANADERÍA EXCEPTO FRUTICULTURA"] <- 1


# Estandarizando las variables
##############################


bd_ventas_norm <- as.data.frame(lapply(bd_ventas2[,4:7], scale))
bd_ventas_norm <- cbind(bd_ventas2[,1:3], bd_ventas_norm, bd_ventas2[8], bd_ventas2[,20:28])

bd_ventas_norm_6 <- subset(bd_ventas_norm,!is.na(tipo_dif_6))
bd_ventas_norm_6$tipo_dif_12 <- NULL
bd_ventas_norm_6$tipo_dif_6 <- as.factor(bd_ventas_norm_6$tipo_dif_6)

bd_ventas_norm_12 <- subset(bd_ventas_norm,!is.na(tipo_dif_12))
bd_ventas_norm_12$tipo_dif_6 <- NULL
bd_ventas_norm_12$tipo_dif_12 <- as.factor(bd_ventas_norm_12$tipo_dif_12)

bd_ventas_norm_6 <-bd_ventas_norm_6[c(1:6,8:16,7)]
bd_ventas_norm_12 <-bd_ventas_norm_12[c(1:6,8:16,7)]

# Creando grupos de entrenamiento, evaluacion y test
#####################################################

# Ventas a 6 meses
set.seed(12345)
N <-  nrow(bd_ventas_norm_6) # population size
n <-  round(nrow(bd_ventas_norm_6)*.15) # sample size
p = rep(n/N,N); # inclusion probabilities
var_dep <- as.numeric(bd_ventas_norm_6$tipo_dif_6)
X <- as.matrix(cbind(p,var_dep,bd_ventas_norm_6[3:15]))
s <-  cube(p,X) # select sample

bd_ventas_norm_6$test <- FALSE
for (i in s) {
  bd_ventas_norm_6$test[i] <- TRUE
}

bd_ventas_norm_6_test <- subset(bd_ventas_norm_6,test)
bd_ventas_norm_6_no_test <- subset(bd_ventas_norm_6,!test)
bd_ventas_norm_6_test$test <- NULL
bd_ventas_norm_6_no_test$test <- NULL

set.seed(12345)
N <-  nrow(bd_ventas_norm_6_no_test) # population size
n <-  round(nrow(bd_ventas_norm_6)*.15) # sample size (es el mismo numero anterior)
p = rep(n/N,N); # inclusion probabilities
var_dep <- as.numeric(bd_ventas_norm_6_no_test$tipo_dif_6)
X <- as.matrix(cbind(p,var_dep,bd_ventas_norm_6_no_test[3:15]))
s <-  cube(p,X) # select sample

bd_ventas_norm_6_no_test$eval <- FALSE
for (i in s) {
  bd_ventas_norm_6_no_test$eval[i] <- TRUE
}

bd_ventas_norm_6_eval <- subset(bd_ventas_norm_6_no_test,eval)
bd_ventas_norm_6_train <- subset(bd_ventas_norm_6_no_test,!eval)
rm(bd_ventas_norm_6_no_test)


# Ventas a 12 meses
set.seed(12345)
N <-  nrow(bd_ventas_norm_12) # population size
n <-  round(nrow(bd_ventas_norm_12)*.15) # sample size
p = rep(n/N,N); # inclusion probabilities
var_dep <- as.numeric(bd_ventas_norm_12$tipo_dif_12)
X <- as.matrix(cbind(p,var_dep,bd_ventas_norm_12[3:15]))
s <-  cube(p,X) # select sample

bd_ventas_norm_12$test <- FALSE
for (i in s) {
  bd_ventas_norm_12$test[i] <- TRUE
}

bd_ventas_norm_12_test <- subset(bd_ventas_norm_12,test)
bd_ventas_norm_12_no_test <- subset(bd_ventas_norm_12,!test)
bd_ventas_norm_12_test$test <- NULL
bd_ventas_norm_12_no_test$test <- NULL

set.seed(12345)
N <-  nrow(bd_ventas_norm_12_no_test) # population size
n <-  round(nrow(bd_ventas_norm_12)*.15) # sample size (es el mismo numero anterior)
p = rep(n/N,N); # inclusion probabilities
var_dep <- as.numeric(bd_ventas_norm_12_no_test$tipo_dif_12)
X <- as.matrix(cbind(p,var_dep,bd_ventas_norm_12_no_test[3:15]))
s <-  cube(p,X) # select sample

bd_ventas_norm_12_no_test$eval <- FALSE
for (i in s) {
  bd_ventas_norm_12_no_test$eval[i] <- TRUE
}

bd_ventas_norm_12_eval <- subset(bd_ventas_norm_12_no_test,eval)
bd_ventas_norm_12_train <- subset(bd_ventas_norm_12_no_test,!eval)
rm(bd_ventas_norm_12_no_test)

rm(X, i, n, N, p,s, var_dep)

# Chequeando balanceo en variable dependiente

todos_6 <- table(bd_ventas_norm_6$tipo_dif_6)
todos_6 <- todos_6/nrow(bd_ventas_norm_6)
train_6 <- table(bd_ventas_norm_6_train$tipo_dif_6)
train_6 <- train_6/nrow(bd_ventas_norm_6_train)
eval_6 <- table(bd_ventas_norm_6_eval$tipo_dif_6)
eval_6 <- eval_6/nrow(bd_ventas_norm_6_eval)
test_6 <- table(bd_ventas_norm_6_test$tipo_dif_6)
test_6 <- test_6/nrow(bd_ventas_norm_6_test)



todos_12 <- table(bd_ventas_norm_12$tipo_dif_12)
todos_12 <- todos_12/nrow(bd_ventas_norm_12)
train_12 <- table(bd_ventas_norm_12_train$tipo_dif_12)
train_12 <- train_12/nrow(bd_ventas_norm_12_train)
eval_12 <- table(bd_ventas_norm_12_eval$tipo_dif_12)
eval_12 <- eval_12/nrow(bd_ventas_norm_12_eval)
test_12 <- table(bd_ventas_norm_12_test$tipo_dif_12)
test_12 <- test_12/nrow(bd_ventas_norm_12_test)

todos_6
train_6
eval_6
test_6
todos_12
train_12
eval_12
test_12

rm (todos_6,
    train_6,
    eval_6,
    test_6,
    todos_12,
    train_12,
    eval_12,
    test_12)

bd_ventas_norm_6$test <- NULL
bd_ventas_norm_12$test <- NULL


# Guardando las bases para el analisis

saveRDS(bd_ventas_norm_12_train, "bd_ventas_norm_12_train.rds")
saveRDS(bd_ventas_norm_12_eval, "bd_ventas_norm_12_eval.rds")
saveRDS(bd_ventas_norm_12_test, "bd_ventas_norm_12_test.rds")

saveRDS(bd_ventas_norm_6_train, "bd_ventas_norm_6_train.rds")
saveRDS(bd_ventas_norm_6_eval, "bd_ventas_norm_6_eval.rds")
saveRDS(bd_ventas_norm_6_test, "bd_ventas_norm_6_test.rds")


# Estadisticas Descriptivas
##############################

# Descriptivos

res_bd_ventas_6 <- describeBy(bd_ventas_norm_6[,2:15],group="tipo_dif_6")
res_bd_ventas_6_t <- describeBy(bd_ventas_norm_6[,2:15])
res1_6 <- data.frame(res_bd_ventas_6[1])
res2_6 <- data.frame(res_bd_ventas_6[2])
res3_6 <- data.frame(res_bd_ventas_6[3])
res_bd_ventas_6_t$vars <- NULL
res1_6$X1.Menor.a..5..vars <- NULL
res2_6$X2.entre..5..y.5..vars<- NULL
res3_6$X3.Mayor.a.5..vars<- NULL

res_bd_ventas_6 <- cbind(res_bd_ventas_6_t,res1_6, res2_6, res3_6)
rm(res_bd_ventas_6_t,res1_6, res2_6, res3_6)


res_bd_ventas_12 <- describeBy(bd_ventas_norm_12[,2:15],group="tipo_dif_12")
res_bd_ventas_12_t <- describeBy(bd_ventas_norm_12[,2:15])
res1_12 <- data.frame(res_bd_ventas_12[1])
res2_12 <- data.frame(res_bd_ventas_12[2])
res3_12 <- data.frame(res_bd_ventas_12[3])
res_bd_ventas_12_t$vars <- NULL
res1_12$X1.Menor.a..5..vars <- NULL
res2_12$X2.entre..5..y.5..vars<- NULL
res3_12$X3.Mayor.a.5..vars<- NULL

res_bd_ventas_12 <- cbind(res_bd_ventas_12_t,res1_12, res2_12, res3_12)
rm(res_bd_ventas_12_t,res1_12, res2_12, res3_12)


# Pairs general

pairs(bd_ventas_norm_12[,3:15], col="blue", pch=16,  cex = 0.2)
pairs(bd_ventas_norm_6[,3:15], col="blue", pch=16,  cex = 0.2)

# Correlation matrix

summary(bd_ventas_norm_12)
summary(bd_ventas_norm_6)

bd_ventas_norm_12_c <- bd_ventas_norm_12
bd_ventas_norm_12_c$rut <- NULL
bd_ventas_norm_12_c$tipo_dif_12 <- NULL
bd_ventas_norm_12_c$es_pj <- NULL
bd_ventas_norm_12_c$es_hombre <- NULL
bd_ventas_norm_12_c$para_cap_trab_inv <- NULL
bd_ventas_norm_12_c$region <- NULL
bd_ventas_norm_12_c$alimento <- NULL
bd_ventas_norm_12_c$agricultura <- NULL

bd_ventas_norm_6_c <- bd_ventas_norm_6
bd_ventas_norm_6_c$rut <- NULL
bd_ventas_norm_6_c$tipo_dif_6 <- NULL
bd_ventas_norm_6_c$region <- NULL

corr12 <- round(cor(bd_ventas_norm_12_c), 1)
corr6 <- round(cor(bd_ventas_norm_6_c), 1)

# Plot
#tuve que correr dev.off() y luego correr el grafico para que no me arrojara error
#dev.off()
ggcorrplot(corr12, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma de Crecimiento a 12 meses", 
           ggtheme=theme_bw)

ggcorrplot(corr6, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma de Crecimiento a 6 meses", 
           ggtheme=theme_bw)


# Numero de creditos por beneficiario
table(n_op_rut$numero_operaciones)
summary(n_op_rut$numero_operaciones)

#Cantidad de Meses  entre creditos
summary(bd1_1$meses_pasados_tot)
table(bd1_1$meses_pasados_tot)

ggplot(subset(bd1_1,meses_pasados_tot>0), aes(meses_pasados_tot)) +
  geom_histogram()+
  scale_x_discrete(name ="Meses entre Creditos", 
                   limits=0:23)+
  labs(title="Distribucion de Cantidad de Meses entre creditos", 
       x="Meses entre creditos")
  
table(bd1_1$meses_pasados_tot)


# Distribucion de las tasas de crecimiento




ggplot( subset(bd_ventas1,!is.na(tipo_dif_6)), aes(dif_rel6)) +
  geom_histogram(binwidth = 0.005)+
  coord_cartesian(xlim = c(-.05, .05))+
  labs(title = "Distribucion de Crecimientos de Ventas a 6 meses",
       subtitle = "Detalle de Mayor Concentracion ",
       caption = "Fuente: CORFO", 
       x = "Tasa de crecimiento en tanto por 1", y = "Frecuencia") 


  ggplot( subset(bd_ventas1,!is.na(tipo_dif_6)), aes(dif_rel6)) +
    geom_histogram(binwidth = 0.01)+
    coord_cartesian(xlim = c(-1, 1))+
    labs(title = "Distribucion de Crecimientos de Ventas a 6 meses",
         subtitle = "Distribucion Completa",
         caption = "Fuente: CORFO", 
         x = "Tasa de crecimiento en tanto por 1", y = "Frecuencia") 
  

  ggplot( subset(bd_ventas1,!is.na(tipo_dif_12)), aes(dif_rel12)) +
    geom_histogram(binwidth = 0.005)+
    coord_cartesian(xlim = c(-.05, 0.05))+
    labs(title = "Distribucion de Crecimientos de Ventas a 12 meses",
         subtitle = "Detalle de Mayor Concentracion",
         caption = "Fuente: CORFO", 
         x = "Tasa de crecimiento en tanto por 1", y = "Frecuencia") 
  

ggplot( subset(bd_ventas1,!is.na(tipo_dif_12)), aes(dif_rel12)) +
    geom_histogram(binwidth = 0.01)+
    coord_cartesian(xlim = c(-1, 1))+
  labs(title = "Distribucion de Crecimientos de Ventas a 12 meses",
       subtitle = "Distribucion Completa",
       caption = "Fuente: CORFO", 
       x = "Tasa de crecimiento en tanto por 1", y = "Frecuencia") 
  
# Distribucion de categorias de variacion de ventas
ggplot(data = subset(bd_ventas1,!is.na(tipo_dif_6))) + 
  geom_bar(mapping = aes(x =tipo_dif_6, y=..prop.., group=1))+
labs(title="Distribucion de Tipos de Variaciones de Ventas a los 6 meses", 
     x="Tipo de variacion")

ggplot(data =  subset(bd_ventas1,!is.na(tipo_dif_12))) + 
  geom_bar(mapping = aes(x =tipo_dif_12, y=..prop.., group=1))+
  labs(title="Distribucion de Tipos de Variaciones de Ventas a los  12 meses", 
       x="Tipo de variacion")


# Relacion entre la varacion de ventas y monto de ventas

ggplot(data =  subset(bd_ventas2,!is.na(dif_rel12))) + 
  geom_point(mapping = aes(x=ventas_inicio , y =dif_rel12))+  
  geom_smooth(mapping = aes(x=ventas_inicio , y =dif_rel12))+
  scale_x_log10()+
  coord_cartesian(ylim = c(-1, 1))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de crecimiento a 12 meses y Nivel de Ventas")

ggplot(data =  subset(bd_ventas2,!is.na(tipo_dif_6))) + 
  geom_point(mapping = aes(x=ventas_inicio , y =dif_rel6))+  
  geom_smooth(mapping = aes(x=ventas_inicio , y =dif_rel6))+
  scale_x_log10()+
  coord_cartesian(ylim = c(-1, 1))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de crecimiento a 6 meses y Nivel de Ventas")

ggplot(data = subset(bd_ventas2,!is.na(dif_rel6)),
       mapping = aes(x = tipo_dif_6, y = ventas_inicio)) +
  scale_y_log10()+
  geom_boxplot()+
  ggtitle("Relacion entre Tipos de Tasa de crecimiento a 6 meses y Nivel de Ventas")

ggplot(data = subset(bd_ventas2,!is.na(dif_rel12)),
       mapping = aes(x = tipo_dif_12, y = ventas_inicio)) +
  scale_y_log10()+
  geom_boxplot()+
ggtitle("Relacion entre Tipos de Tasa de crecimiento a 12 meses y Nivel de Ventas")

# Relacion entre la varacion de ventas y monto de credito

ggplot(data =  subset(bd_ventas2,!is.na(tipo_dif_12))) + 
  geom_point(mapping = aes(x=monto_op , y =dif_rel12))+  
  geom_smooth(mapping = aes(x=monto_op , y =dif_rel12))+
  scale_x_log10()+
  coord_cartesian(ylim = c(-1, 1))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de crecimiento a 12 meses y Nivel de Ventas")

ggplot(data =  subset(bd_ventas2,!is.na(tipo_dif_6))) + 
  geom_point(mapping = aes(x=monto_op , y =dif_rel6))+  
  geom_smooth(mapping = aes(x=monto_op , y =dif_rel6))+
  scale_x_log10()+
  coord_cartesian(ylim = c(-1, 1))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre Tasa de crecimiento a 6 meses y Nivel de Ventas")

# Relacion entre variacion de ventas y variables categoricas

ggplot(data =  subset(bd_ventas2,!is.na(tipo_dif_12))) +
  geom_count(mapping = aes(x = tipo_dif_12, y =tipo))

ggplot(data =  subset(bd_ventas2,!is.na(tipo_dif_6))) +
  geom_count(mapping = aes(x = tipo_dif_6, y =tipo))

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_12))) +
  geom_bin2d(mapping = aes(y = tipo, x =tipo_dif_12))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre entre Tasa de crecimiento a 12 meses y Tipo de Benefciario")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_6))) +
  geom_bin2d(mapping = aes(y = tipo, x =tipo_dif_6))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre entre Tasa de crecimiento a 6 meses y Tipo de Benefciario")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_12))) +
  geom_bin2d(mapping = aes(y = as.factor(as.numeric(as.character(region))), x =tipo_dif_12))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre entre Tasa de crecimiento a 12 meses y Tipo de Beneficiario")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_6))) +
  geom_bin2d(mapping = aes(y = as.factor(as.numeric(as.character(region))), x =tipo_dif_6))+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  ggtitle("Relacion entre entre Tasa de crecimiento a 6 meses y Region")


ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_6))) + 
  geom_bar(mapping = aes(x =as.factor(as.numeric(as.character(region)))))+ 
  facet_wrap(~ tipo_dif_6, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 6 meses y Region")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_12))) + 
  geom_bar(mapping = aes(x =as.factor(as.numeric(as.character(region)))))+ 
  facet_wrap(~ tipo_dif_12, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 12 meses y Region")


ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_6))) + 
  geom_bar(mapping = aes(x =objeto_op))+ 
  facet_wrap(~ tipo_dif_6, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 6 meses y Objetivo")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_12))) + 
  geom_bar(mapping = aes(x =objeto_op))+ 
  facet_wrap(~ tipo_dif_12, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 12 meses y Objetivo")


ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_6))) + 
  geom_bar(mapping = aes(x =objeto_op))+ 
  facet_wrap(~ tipo_dif_6, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 6 meses y Objetivo")

ggplot(data = subset(bd_ventas2,!is.na(tipo_dif_12))) + 
  geom_bar(mapping = aes(x =acteco))+ 
  facet_wrap(~ tipo_dif_12, nrow = 3, scales = "free_y")+
  ggtitle("Relacion entre entre Tasa de crecimiento a 12 meses y Actividad Economica")


