library(rBayesianOptimization)
library(ggplot2)
library(GPfit)
library(survival)
require(survival)

source('binario a censura.R')


########Parte 1##########################
#Programa para estimar a y b de log(eta) = a1+a2x, donde x = 11605.53/(temperatura?C + 273.16)
#Esto con la finalidad de simular datos de respuestas de vida acelerada
#######
#NOTA: LO UNICO A CAMBIAR SERA, SI ACASO, LA EL VECTOR DE ETAS Y LA TEMPERATURA.
#SE PUEDE AGREGAR TEMPERATURA USUAL Y SU VALOR DE ETA PARA FORZAR A LA FUNCION
#OTRA NOTA: USAR TRES VECTORES DE DIAS DIFERENTES PARA CADA TEMPERATURA
relacion_AW <- function(temperatura, etas){
  #funcion relacionada a la relacion de Arrhenius-Weibul
  k <- length(temperatura)
  x <- matrix(data = 1,nrow = k, ncol =2)
  for( i in 1:k){x[i,2] <- 11605.53/(temperatura[i] + 273.16)}
  #  for( i in 1:k){x[i,2] <- temperatura[i] }
  l_eta <- matrix(data = log(etas), nrow = 3, ncol=1); A_est <- matrix(data=NA, nrow = 2, ncol = 1)
  t_x <- t(x) #transpuesta de la matriz x #print(x) #print(l_eta)
  A_est <- solve(t_x %*% x) %*% t_x %*% l_eta #estimador de regresion  #print(A_est)  #print(x %*% A_est)
  #plot(x[,2],x %*% A_est,pch=20,ylim=c(0,3), xlab = "x",ylab="log(eta)")
  #plot(temperatura, exp(x %*% A_est),pch=20,ylim=c(0,13), xlab = "esfuerzo",ylab="tiempo de vida")
  return(exp(x %*% A_est)) #nuevos etas
}
#aux_1 <- relacion_AW(temperatura,etas)
########Parte 2##########################
#termina otra funcion
#######################principio de funciones parte 2
#NOTA: sup hace referencia a supervivencia. Ademas, beta y eta son los emv respectivos
#sabemos que la supervivencia de una Weibull es
sup_weibull <- function(t, beta, eta){return( exp(-(t/eta)^(beta) ) )}

sup_en_tiempo <- function(beta,eta,tiempo,titulo,subtitulo){
  #esta funcion requiere de las estimaciones de beta y eta de una Weibull para determinar
  #la supervivencia en el vector tiempo y grafica la supervivencia
  tabla_sup <- matrix(data=1:1 , nrow=1, ncol = length(tiempo)) #matriz de unos 1 por 7
  for(j in 1:length(tiempo)){tabla_sup[1,j] <- sup_weibull(tiempo[j],beta,eta) }
  #plot(tiempo,tabla_sup, col = "black",pch=20,ylim=c(0,1),main = titulo, sub=subtitulo,xlab = "tiempo en dias",ylab="supervivencia")
  return(tabla_sup) #tabla de estimaciones de supervivencia en el tiempo
}
#funcion para simular las respuestas
simu_res <- function(tabla,consumidores,objeto,tiempo){
  #tabla es la tabla de confiabilidad con dimension: num de tostadas por num de dias (m)
  #consumidores es el numero total de consumidores
  #objeto es la etiqueta del obejto que se somente a estudio (1,2,3 en nuestro caso)
  #objeto tiene que estar en la dimension de las columnas de tabla
  # theta es el vector de los estimadores de los objetos
  m <- length(tabla[1,]) #numero de dias o pruebas
  mat_simu <- matrix(data=1:1 , nrow=consumidores, ncol = m)
  for(k in 1:consumidores){for(j in 1:m){
    p_simu <- tabla[objeto,j]
    mat_simu[k,j] <- rbinom(1,1,p=p_simu)
  }}
  return(mat_simu)
}
#######################terminan funciones parte 2
#hacer funcion
simu_temp <- function(i,aux_1,tiempo){
  temperatura <- c(30,40,50)
  #meter un ruido aleatorio para cada tostada tanto en beta como en eta
  est_beta <- c(1.5,1.5,1.5)
  #cambiar el  i de aux_1[i], de pendiendo de la temeratura a tomar
  #est_beta <- est_beta
  est_eta <- aux_1[i]*c(1,1,1)
  tabla_tipo1<- sup_en_tiempo(est_beta[1],est_eta[1],tiempo = tiempo,"supervivencia para la tostada tipo A", subtitulo= temperatura[i])
  tabla_tipo2<- sup_en_tiempo(est_beta[2],est_eta[2],tiempo = tiempo,"supervivencia para la tostada tipo B", subtitulo= temperatura[i])
  tabla_tipo3<- sup_en_tiempo(est_beta[3],est_eta[3],tiempo = tiempo,"supervivencia para la tostada tipo C", subtitulo= temperatura[i])
  tabla_sup <- rbind(tabla_tipo1,tabla_tipo2,tabla_tipo3)
  ###simulacion
  sim1 <- simu_res(tabla_sup,50,1,tiempo); sim2 <- simu_res(tabla_sup,50,2,tiempo); sim3 <- simu_res(tabla_sup,50,3,tiempo)
  return(list(sim1,sim2,sim3))
}

p_auxp <- 0.5
#Funciones cuantil
cuantil_1 <- function(beta,a1,a2,temp){
  #beta, a1 y a2 son los estimados del Arrhenius Weibull
  #temp es la temperatura en 30, 40 o 50 (si bien recuerdo)
  res <- exp(a1+a2*(11605.53/(temp + 273.16)) )*qweibull(p_auxp, beta, scale = 1, lower.tail = TRUE, log.p = FALSE)
  return(res)
}
cuantil_2 <- function(beta,eta){
  #beta, a1 y a2 son los estimados del Arrhenius Weibull
  #temp es la temperatura en 30, 40 o 50 (si bien recuerdo)
  res <- eta*qweibull(p_auxp, beta, scale = 1, lower.tail = TRUE, log.p = FALSE)
  return(res)
}
cuantil_3 <- function(beta,eta){
  #beta, a1 y a2 son los estimados del Arrhenius Weibull
  #temp es la temperatura en 30, 40 o 50 (si bien recuerdo)
  res <- eta*qweibull(0.9, beta, scale = 1, lower.tail = TRUE, log.p = FALSE)
  return(res)
}
cuantil_4 <- function(beta,eta,valor){
  #beta, a1 y a2 son los estimados del Arrhenius Weibull
  #temp es la temperatura en 30, 40 o 50 (si bien recuerdo)
  res <- eta*qweibull(valor, beta, scale = 1, lower.tail = TRUE, log.p = FALSE)
  return(res)
}

#generar tiempos de prueba (un vector)
genera_tiempos <- function(num_pruebas){
  #Genera los dias de pruebas (numeros) sin repeticiones
  #Regresa un total de num_pruebas dias de pruebas entre dia 0 al 30
  time <- 0:30
  s <-  sample(time,num_pruebas,replace=F)
  s <- sort(s,decreasing = FALSE)
  return(s)}
genera_tiempos1 <- function(num_pruebas,time){
  #Genera los dias de pruebas (numeros) sin repeticiones
  #Regresa un total de num_pruebas dias de pruebas entre dia 0 al 30
  s <-  sample(time,num_pruebas,replace=F)
  s <- sort(s,decreasing = FALSE)
  return(s)}

GenTodos <- function(m,ndias){
  #genera una matriz con m columnas y n dias de prueba entre 0 y 30
  vector1 <- c()
  for(i in 1:m){vector1 <- c(vector1,genera_tiempos(ndias))}
  tiempos <- matrix(data=vector1, nrow = m, ncol=ndias,byrow=TRUE)
  return(tiempos)}

# PARA REALIZAR OPTIMIZACION
# M es un numero, x es variable independiente , v es vector de cuantiles
f <- function(x,v) {  #es el ecm
  su <- 0; M <- length(v)
  for(i in 1:M){su <- (v[i]- x)^{2} + su }
  f <- (1/M)*su
  return(f)
}
#x <- seq(15,50,by=5)
#eval <- as.matrix(data.frame(x = x, y = f(x,c(16,20,30) ) ) )
#eval
#plot(x,-f(x,c(16,20,30)))
fup <- function(vector_cuantico){
  f3 <- function(x_r){
    list(Score = -f(x_r,vector_cuantico) ,Pred =0)
  }
  OPT_Res3 <- BayesianOptimization(f3,
                                   bounds = list(x_r = c(14,50)),
                                   init_points = 8, n_iter = 1,
                                   acq = "ucb", kappa = 2.576, eps = 0.0,
                                   verbose = TRUE)
  lala3 <- as.numeric(OPT_Res3$Best_Par)
  primer3 <- f(lala3,vector_cuantico)
  return(c(lala3,primer3))
}

# PEGADO VA tTemperaturaLetraDeTipoTostada. Temperatura en (30,40,50). TipoDeTostada en (a,b,c)
#################Datos generales

temperatura <- c(30,40,50); etas <- c(14,9,6) #ver valores tesis de alexis puede ayudar
DrS <- function(a,tiempo){
  aux_1 <- relacion_AW(temperatura,etas)  #etas de Arrhenius-Weibull
  #simulaciones para la entrada a en temperatura <- c(30,40,50)
  temp_simu <- simu_temp(a,aux_1 = aux_1,tiempo= tiempo)
  #cambio de lista a matriz, ya que R chafea en esta parte
  ta <- unlist(temp_simu[1]) #si es 1 es tostada tipo a, 2 es tipo b y 3 es tipo c
  ta <- matrix(ta,nrow = 50,ncol = length(tiempo))
  TOSTADA_aux <-  censura(ta,t= tiempo)
  return(TOSTADA_aux)
}

#traslacion del tiempo, probabilidad al tiempo t, y despues en el cuantil
mod_tiempo <- function(tiempo,et1,et2){
  valor <- pweibull(tiempo, 1.5, scale = et1, lower.tail = TRUE , log.p = FALSE)
  return(cuantil_4(1.5,et2,valor =valor))
}
#verificacion de la traslacion
df_tiempo <- function(vect,time1){
  if(length(which(table(vect)>1))){vect <- genera_tiempos1(7,time = time1)}
  return(vect)
}

pre_ecm <- function(tiempo){
  aux_1 <- relacion_AW(temperatura,etas)  #etas de Arrhenius-Weibull
  #print(aux_1) #30 es 12.70 i.e. aux_1[1]
  m <- 10 #50 es el ideal
  eta_n <- 16
  por_si <- c()
  guarda1 <- c() #vector para guardar las estimaciones de los cuantiles
  guarda2 <- c(); guarda3 <- c()
  # eta_media <- c(); beta_media <- c()
  #correccion en los tiempos (solo si se requiere)
  tiempo2 <- floor(mod_tiempo(tiempo= tiempo,eta_n,etas[2])); tiempo3 <- floor(mod_tiempo(tiempo= tiempo,eta_n,etas[3]))  #AQUI FUNCION PISO floor O TECHO ceiling
  tiempo1 <- unique(tiempo);tiempo2 <- unique(tiempo2); tiempo3 <- unique(tiempo3)
  for(j in 1:m){  #cambiar tiempo de ce2 y ce3 (rel. con la distribucion de los tiempos de inspeccion)
    #ver si son nulos, hacer algo
    ce1 <- DrS(a=1,tiempo = tiempo1); ce2 <- DrS(a=2,tiempo = tiempo2); ce3 <- DrS(a=3,tiempo = tiempo3)
    num_aux <- c(dim(ce1)[1],dim(ce2)[1],dim(ce3)[1]) #guardar dimensiones de los datos utiles
    num <- sum(num_aux)
    #print(num)
    TOSTADA <-rbind(ce1,ce2,ce3)
    for(i in 1:num){if(TOSTADA[i,1]<1){TOSTADA[i,1]<-0.0001}}
    #repetir temperaturas para Arrhenius- Weibull
    tem1 = rep(temperatura[1],num_aux[1]); tem2 = rep(temperatura[2],num_aux[2]); tem3 = rep(temperatura[3],num_aux[3])
    tem <- c(tem1,tem2,tem3)
    tem <- 11605.53/(tem + 273.16)
    t_data <- data.frame(start = TOSTADA[,1],stop=TOSTADA[,2],event=rep(3,num),TEM=tem,id = 1:num)
    
    Out <- try( survreg(Surv(start,stop,event,type='interval',origin = -0.0001) ~ TEM ,data=t_data ,dist="weibull", control = list(maxiter=1000)),
                silent = TRUE)
    
    A <- summary(Out)
    #print(A$table)
    if( sum(is.na(A$table[,4]))>0 | sum(is.infinite(A$table[,3]))>0){ #se puede agregar que si los p valores son mayores a 0.05 no tomar
      por_si[1]<-1
      break # funciona bien con break
    }else{
      #print(A)
      #  summary(A)
      A<-summary(Out)
      a1 <- as.numeric(A$coefficients[1]); a2 <- as.numeric(A$coefficients[2])
      eta <- exp(a1 + a2*(11605.53/(temperatura + 273.16))) #parametro de escala
      #  eta <- exp(a1 + a2*(11605.53/(20 + 273.16))) #parametro de escala a 20 grados
      beta <- as.numeric(1/A$scale) #parametro de forma
      #print(c('beta',beta,'eta',eta))
      if( anyNA(c(beta,eta)) ){ #esta parte despues de cambiar, ya no  es necesaria
        next }else{
          guarda1[j] <- cuantil_2(beta = beta,eta = eta[1])
          guarda2[j] <- cuantil_2(beta = beta,eta = eta[2])
          guarda3[j] <- cuantil_2(beta = beta,eta = eta[3])
          #  beta_media[j] <- beta
          #  eta_media[j] <- eta[1]
        }
    }
  }
  if(length(por_si[1]) > 0){
    fl <- 200000
  }else{
    real1 <- cuantil_2(beta = 1.5, aux_1[1])
    real2 <- cuantil_2(beta = 1.5,eta= aux_1[2])
    real3 <- cuantil_2(beta = 1.5,eta= aux_1[3])
    estimacion1 <- (1/length(guarda1))*sum( (guarda1-real1)^{2}  )  #estimacion del ecm tem 30
    estimacion2 <- (1/length(guarda2))*sum( (guarda2-real2)^{2}  )  #estimacion del ecm tem 40
    estimacion3 <- (1/length(guarda3))*sum( (guarda3-real3)^{2}  )  #estimacion del ecm tem 50
    fl <- mean(c(estimacion1,estimacion2,estimacion3))
  }
  return(fl)
  #  return(estimacion1)
}

f_min2 <- function(delta1,delta2,delta3,delta4,delta5,delta6,delta7){ #ti es tiempos de inspeccion   #cambiar t por delta
  ordenado <- floor(c(delta1,delta2,delta3,delta4,delta5,delta6,delta7))
  ordenado <- sort(ordenado)
  ordenado <- floor(mod_tiempo(tiempo= ordenado,16,etas[1])) #esto puede estar mal
  ordenado <- unique(ordenado)
  total_TI <- length(ordenado) #total de tiempos de
  aux_orde <- c()
  for(i in 2:total_TI){
    aux_orde[i-1] <- ordenado[i]-ordenado[i-1]
  }
  # Verificar si el número está en el vector
  if ( (ordenado[total_TI] > cuantil_4(1.5,etas[1],0.99) ) |(sum(aux_orde==1)>3)| (total_TI < 4)|(sum(aux_orde==0)>2) ) { paro<-NA} else {
    paro <- pre_ecm(tiempo=ordenado)  #PASO DE LA MUERTE ( cambiar funcion )
  }
  #paro <- pre_ecm(tiempo=ordenado)
  if( is.na(paro)){
    list(Score = - 100000,Pred =0) }else{  #hay que cambiar varle un valor menor tal vez -10000
      list(Score = - paro,Pred =0) #sirve
    }
}
#cuantil 0.5
FF1 <- data.frame(read.csv('historial5ecm1.csv'))
FF1 <- FF1[ , !(names(FF1) %in% c('X','Round') ) ]

n_opt <- 5
donde_busca <- list(delta1 =c(0,40), delta2 = c(0,40),delta3 = c(0,40),delta4 = c(0,40),delta5=c(0,40),delta6=c(0,40),delta7=c(0,40) )
op <-BayesianOptimization(f_min2,
                          bounds = donde_busca,init_grid_dt = FF1,
                          init_points = n_opt, n_iter = 1,
                          acq = "ucb", kappa=5, eps = 1,verbose = TRUE) # poner verbose = 1 mejor resultado
#init_grid_dt = FF1,

datos_a_guardar <- data.frame(op$History)
#datos_a_guardar<-datos_a_guardar[datos_a_guardar$Value != -100000.0000000, ]
#guardar cosas
#mejor_parametros <- write.csv(op$Best_Par,'mejores_parametros5ecmprueba1.csv')
#mejor_parametros
#mejor_valor <- write.csv(op$Best_Value,'mejor_valork5ecmprueba1.csv')
#mejor_valor
historial <- write.csv(datos_a_guardar,'historial5ecm1ka5.csv')
historial