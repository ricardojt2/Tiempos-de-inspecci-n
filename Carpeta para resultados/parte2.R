#parte 2
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
