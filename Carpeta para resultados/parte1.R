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