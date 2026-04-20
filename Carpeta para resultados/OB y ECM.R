library(rBayesianOptimization)
library(ggplot2)
library(GPfit)
library(survival)
require(survival)

ruta_aux <- 'AQUI VA RUTA'

source(paste0(ruta_aux,'/binario a censura.R'))
source(paste0(ruta_aux,'/parte1.R'))
source(paste0(ruta_aux,'/parte2.R'))
source(paste0(ruta_aux,'/parte_3_seleccionar_cuantil.R'))
source(paste0(ruta_aux,'/parte4.R'))


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