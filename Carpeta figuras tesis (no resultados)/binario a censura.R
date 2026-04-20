censura_funcion <- function(matriz, t) {
  # matriz: filas = sujetos, columnas = respuestas (1 = aceptación, 0 = rechazo)
  # t: vector de tiempos de inspección
  # salida: matriz con intervalos de censura/evento
  
  dimension_t <- length(t)
  Numero_aux <- 500
  num_total <- nrow(matriz)
  datos_cen <- matrix(ncol = 2, nrow = 0)
  
  for (num_fila in 1:num_total) {
    unos <- which(matriz[num_fila, ] == 1)
    ceros <- which(matriz[num_fila, ] == 0)
    
    # Caso: todos son aceptaciones (sin rechazo)
    if (length(ceros) == 0) {
      datos_cen <- rbind(datos_cen, c(t[dimension_t], Numero_aux))
      next
    }
    
    # Caso: censura por izquierda (primer rechazo aparece muy temprano)
    if (ceros[1] == 1) {
      # rechazo ya en la primera inspección
      datos_cen <- rbind(datos_cen, c(0, t[ceros[1]]))
      next
    }
    if (ceros[1] <= 2) {
      # rechazo en la segunda inspección → pudo ocurrir antes
      datos_cen <- rbind(datos_cen, c(0, t[ceros[1]]))
      next
    }
    
    # Caso: censura por intervalo
    A <- ceros[1]   # primer rechazo observado
    B <- A - 1      # último tiempo con aceptación antes del rechazo
    if (max(unos) > A) {
      # hay aceptaciones después del primer rechazo → inconsistencia
      C1 <- max(unos) + 1
      if (C1 <= dimension_t) {
        datos_cen <- rbind(datos_cen, c(t[B], t[C1]))
      }
    } else {
      # evento consistente: rechazo ocurre entre t[B] y t[A]
      datos_cen <- rbind(datos_cen, c(t[B], t[A]))
    }
  }
  return(datos_cen)
}


t1<-c(2,4,8,12,24,36,48)
ff<-t1>9
which(ff==TRUE)[1]

censura_funcion( matriz = matrix(data=c(1,0,1,1,0,0,1),byrow = TRUE,ncol=7),t=t1)


censura_funcion( matriz = matrix(data=c(0,0,1,1,0,0,0),byrow = TRUE,ncol=7),t=t1)

censura_funcion( matriz = matrix(data=c(1,1,1,1,1,1,1),byrow = TRUE,ncol=7),t=t1)
censura_funcion( matriz = matrix(data=c(1,1,1,1,0,0,0),byrow = TRUE,ncol=7),t=t1)

censura_funcion( matriz = matrix(data=c(1,1,0,1,0,0,0),byrow = TRUE,ncol=7),t=t1)
censura_funcion( matriz = matrix(data=c(1,0,1,0,1,0,0),byrow = TRUE,ncol=7),t=t1)

censura_funcion( matriz = matrix(data=c(0,0,1,1,1,1,0),byrow = TRUE,ncol=7),t=t1)
censura_funcion( matriz = matrix(data=c(0,0,0,0,0,0,0),byrow = TRUE,ncol=7),t=t1)


censura_por_izquierda <- function(tiempos, respuestas) {
  # Verificar que ambos vectores tengan la misma longitud
  if (length(tiempos) != length(respuestas)) {
    stop("Los vectores de tiempos y respuestas deben tener la misma longitud.")
  }
  
  # Buscar la primera ocurrencia de rechazo (0)
  for (i in seq_along(respuestas)) {
    if (respuestas[i] == 0) {
      return(list(indice = i, tiempo = tiempos[i]))  # Retorna el índice y el tiempo de la censura
    }
  }
  
  return(list(indice = -1, tiempo = NA))  # No se encontró rechazo
}

# Ejemplo de uso
tiempos <- c(1, 2, 3, 4, 5)
respuestas <- c(1, 0, 1, 0, 1)
resultado <- censura_por_izquierda(tiempos, respuestas)

cat("Censura por izquierda en el índice", resultado$indice, "a tiempo", resultado$tiempo, "\n")
