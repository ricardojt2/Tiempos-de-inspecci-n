library(ggplot2)

# Datos de ejemplo: un caso por tipo
datos <- data.frame(
  id = factor(c("Sin censura", "Censura por derecha", "Censura por izquierda", "Censura por intervalo"),
              levels = c("Sin censura", "Censura por derecha", "Censura por izquierda", "Censura por intervalo")),
  inicio = c(0, 0, 0, 0),
  fin    = c(3, NA, NA, NA),
  censura_inicio = c(NA, 5, NA, 2),
  censura_fin    = c(NA, NA, 4, 6),
  tipo = c("observado", "derecha", "izquierda", "intervalo")
)
datos

p <- ggplot() +
  # Sin censura: línea continua + punto
  geom_segment(data = subset(datos, tipo == "observado"),
               aes(x = inicio, xend = fin, y = id, yend = id),
               size = 1.2, linetype = "solid", color= "#585858") +
  geom_point(data = subset(datos, tipo == "observado"),
             aes(x = fin, y = id), shape = 16, size = 4, color="#09622A") +
  
  # Censura derecha: X + línea punteada hacia adelante
  geom_point(data = subset(datos, tipo == "derecha"),
             aes(x = censura_inicio, y = id), shape = 4, size = 6, stroke = 2, color ="#225188") +
  geom_segment(data = subset(datos, tipo == "derecha"),
               aes(x = censura_inicio, xend = censura_inicio + 3, y = id, yend = id),
               size = 1.2, linetype = "dashed", color= "#C11A3B") +
  geom_segment(data = subset(datos, tipo == "derecha"),
               aes(x = inicio, xend = censura_inicio, y = id, yend = id),
               size = 1.2, linetype = "solid", color = "#585858") +
  
  
  # Censura izquierda: línea punteada desde 0 hasta la X
  geom_segment(data = subset(datos, tipo == "izquierda"),
               aes(x = 0, xend = censura_fin, y = id, yend = id),
               size = 1.2, linetype = "dashed", color= "#C11A3B") +
  geom_point(data = subset(datos, tipo == "izquierda"),
             aes(x = censura_fin, y = id), shape = 4, size = 6, stroke = 2, color ="#225188") +
  geom_segment(data = subset(datos, tipo == "izquierda"),
               aes(x = censura_fin, xend = 8, y = id, yend = id),
               size = 1.2, linetype = "solid", color = "#585858")+
  # Censura intervalo: dos X con línea punteada entre ellas
  geom_segment(data = subset(datos, tipo == "intervalo"),
               aes(x = censura_inicio, xend = censura_fin, y = id, yend = id),
               size = 1.2, linetype = "dashed", color= "#C11A3B") +
  geom_point(data = subset(datos, tipo == "intervalo"),
             aes(x = censura_inicio, y = id), shape = 4, size = 6, stroke = 2, color ="#225188") +
  geom_point(data = subset(datos, tipo == "intervalo"),
             aes(x = censura_fin, y = id), shape = 4, size = 6, stroke = 2, color ="#225188") +
  geom_segment(data = subset(datos, tipo == "intervalo"),
               aes(x = inicio, xend = censura_inicio, y = id, yend = id),
               size = 1.2, linetype = "solid", color = "#585858")+
  geom_segment(data = subset(datos, tipo == "intervalo"),
               aes(x = censura_fin, xend = 8, y = id, yend = id),
               size = 1.2, linetype = "solid", color = "#585858")+
  labs(x = "Tiempo de inspección", y = NULL, title = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),   # elimina números eje x
    axis.ticks.x = element_blank(),
    legend.position = "none",         # elimina la leyenda
    axis.text.y = element_text(color = "#030303", size = 14), # eje Y valores
    axis.title.x  = element_text(color = "#030303", size = 18) # eje X titulo 
  )
p  
# Guardar como PNG cambiar ruta 
"
ruta <- 'C:/Users/1998R/OneDrive/Escritorio/Respaldo Tesis/tesis/img/'

ggsave(paste0(ruta,'tiposcensura.png'), plot = p, width = 10, height = 5, dpi = 300)
"
