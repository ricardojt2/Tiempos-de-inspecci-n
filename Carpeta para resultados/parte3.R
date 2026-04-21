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
