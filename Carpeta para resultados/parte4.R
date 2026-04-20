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
