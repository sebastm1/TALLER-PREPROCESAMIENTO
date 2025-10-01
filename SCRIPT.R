
library(dplyr)



datos <- readxl::read_excel("C:/Users/johnn/Desktop/GESTION DE DATOS/TALLER PREPROCESAMIENTO/DATOS/eva_df_2025.xlsx")
table(datos$CULTIVO)
level_CULTIVO <- c(MAIZ = "MAIZ", maiz= "MAIZ")
datos <- datos %>%
  mutate(CULTIVO = recode(CULTIVO, !!!level_CULTIVO))
datos <- datos %>%
  filter(CULTIVO == "MAIZ")
table(datos$DEPARTAMENTO)

ncol(datos)
nuevos_nombres <- c("departamento", "municipio", "grupo", "cultivo", "año", 
                    "area_sembrada", "area_cosechada", "t_produccion", 
                    "estado_fisico", "ciclo_cultivo")

names(datos) <- nuevos_nombres


summary(datos)
library(stringr)
library(ggplot2)


datos <- datos %>%
  mutate(departamento = str_to_upper(departamento))
table(datos$departamento)


datos <- datos %>%
  mutate(
    rendimiento = case_when(
      is.na(t_produccion) | is.na(area_cosechada) ~ NA_real_,
      area_cosechada == 0 ~ 0,
      TRUE ~ t_produccion / area_cosechada
    )
  )
p1 <-ggplot(data = datos, aes(x = area_cosechada, y = t_produccion)) +
  geom_point(aes(color = departamento), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(
    title = "Relación entre Área Cosechada y Producción de Maíz",
    x = "Área Cosechada (Hectáreas)",
    y = "Producción (Toneladas)",
    color = "Departamento"
  ) +
  theme_minimal()


datos_imputados <- datos_imputados %>%
  mutate(
    rendimiento = case_when(
      is.na(t_produccion) | is.na(area_cosechada) ~ NA_real_,
      area_cosechada == 0 ~ 0,
      TRUE ~ t_produccion / area_cosechada
    )
  )

datos_imputados <- datos_imputados %>%
  mutate(
    tipo_dato = if_else(is.na(datos$t_produccion), 
                        "Imputado", 
                        "Original"))
p2 <- ggplot(data = datos_imputados, aes(x = area_cosechada, y = t_produccion)) +
  geom_point(aes(color = departamento, shape = tipo_dato), alpha = 0.7, size = 3.5) +
geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  scale_shape_manual(values = c("Original" = 16, "Imputado" = 17)) +
  labs(
    title = "Relación entre Área Cosechada y Rendimiento",
    x = "Área Cosechada (Hectáreas)",
    y = "Rendimiento (Ton/Ha)",
    color = "Departamento",
    shape = "Tipo de Dato"
  ) +
  theme_minimal()
library(tidyverse)

print ( p1 + p2)


library(ggplot2)

# 2. Crear el gráfico
p4 <- ggplot(datos_imputados, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )

datos_limpios <- datos %>%
  filter(!is.na(datos$rendimiento))


p4 <- ggplot(Datos_ImputR, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )
vis_miss(Datos_ImputR)

p3 <- ggplot(datos_limpios, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )
print(p4+ p3)

library(mice)

# Usar el método por defecto (pmm), que es más robusto y no creará negativos
imputR = mice(datos, maxit = 5, seed = 123, print = F) # Aumenté maxit a 5, que es el default y es más estable

# Completar el dataset
Datos_ImputR = complete(imputR)

# Comprobar los resultados (ahora no deberían tener negativos)
summary(Datos_ImputR)

vis_miss(datos_imputados)
library(naniar)
vis_miss(datos)
library(dplyr)
datos_imputados <- datos %>%
  group_by(departamento) %>%
  mutate(
    t_produccion = ifelse(is.na(t_produccion), 
                                  median(t_produccion, na.rm = TRUE), 
                                  t_produccion)
  ) %>%
  ungroup()


vis_miss(datos_imputados)


library(corrplot)


datos %>% 
  filter(is.na(datos_imputados$t_produccion))

datos <- na.omit(datos)
vis_miss(datos)

library(dplyr)
library(plotly)

datos_freq <- datos %>%
  count(departamento)

Dist <- plot_ly(datos_freq, labels = ~departamento, values = ~n, type = "pie") %>%
  layout(title = "Distribución de la muestra por departamentos")

library(dplyr)

filas_faltantes_rendimiento <- datos %>% 
  filter(is.na(rendimiento))

vis_miss(datos)
print(filas_faltantes_rendimiento, n = Inf)


# Instala ggplot2 si no lo tienes, para gráficos más estéticos
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)
library(patchwork)

  
 

  
  # Imprimir un resumen estadístico de los datos limpios
  cat("Resumen estadístico del rendimiento (sin datos faltantes):\n")
  print(summary(datos_limpios$rendimiento))
  

datos_imputados <- datos %>%
  mutate(rendimiento = if_else(is.na(rendimiento),      # Condición: si rendimiento es NA...
                               mean(rendimiento, na.rm = TRUE), # ...reemplázalo con la media...
                               rendimiento))                  # ...de lo contrario, déjalo como está.


p4 <- ggplot(datos_imputados, aes(x = rendimiento)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", color = "white") +
  labs(title = "Histograma del Rendimiento",
       x = "Rendimiento (Ton/Ha)",
       y = "Frecuencia") +
  theme_minimal()

print(p4)

library(dplyr)
library(ggplot2)

# ---- PASO 1: Crear una columna para identificar los puntos imputados ----
# Usamos el data frame original 'datos_solo_maiz' para saber dónde estaban los NA
library(dplyr)
datos_para_grafico <- datos_imputados_por_depto %>%
  mutate(
    tipo_dato = if_else(is.na(datos$t_produccion), 
                        "Imputado", 
                        "Original")
  )



datos_para_grafico2 <- datos_imputados_lm %>%
  mutate( tipo_dato = if_else(is.na(datos$rendimiento), "Imputado", "Original"))

# ---- PASO 2: Crear el gráfico usando la nueva columna para la forma ----



p4 <- ggplot(data = datos_para_grafico2, aes(x = area_cosechada, y = rendimiento)) +
  geom_point(aes(color = departamento, shape = tipo_dato), alpha = 0.7, size = 3.5) +
  1
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  scale_shape_manual(values = c("Original" = 16, "Imputado" = 17)) +
  labs(
    title = "Relación entre Área Cosechada y Rendimiento",
    x = "Área Cosechada (Hectáreas)",
    y = "Rendimiento (Ton/Ha)",
    color = "Departamento",
    shape = "Tipo de Dato"
  ) +
  theme_minimal()

p5_modificado <- ggplot(data = datos_para_grafico2, aes(x = area_cosechada, y = rendimiento_)) +
  
  # La clave está aquí: mapeamos 'color' a departamento y 'shape' a tipo_dato
  geom_point(aes(color = departamento, shape = tipo_dato), alpha = 0.7, size = 4) +
  
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  
  # Agregamos una leyenda para la forma
  labs(
    title = "Relación entre Área Cosechada y Producción (con Datos Imputados)",
    subtitle = "Los puntos imputados se muestran con una forma diferente",
    x = "Área Cosechada (Hectáreas)",
    y = "Producción (Toneladas)",
    color = "Departamento",
    shape = "Tipo de Dato" # Título para la nueva leyenda
  ) +
  
  # Escala manual para que los puntos sean más claros (círculo y triángulo)
  scale_shape_manual(values = c("Imputado" = 17, "Original" = 16)) +
  
  theme_minimal()

# Mostrar el gráfico modificado
library(tidyverse)
print(p5_modificado + p5)

p5 <-ggplot(data = datos_limpios, aes(x = area_cosechada, y = t_produccion)) +
  geom_point(aes(color = departamento), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(
    title = "Relación entre Área Cosechada y Producción de Maíz",
    subtitle = "Se observa una tendencia lineal positiva",
    x = "Área Cosechada (Hectáreas)",
    y = "Producción (Toneladas)",
    color = "Departamento"
  ) +
  theme_minimal()



p6 <- ggplot(data = datos_limpios, aes(x = area_cosechada, y = t_produccion)) +
  geom_point(aes(color = departamento), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "firebrick", se = FALSE) +
  labs(
    title = "Relación entre Área Cosechada y Producción de Maíz",
    subtitle = "Se observa una tendencia lineal positiva",
    x = "Área Cosechada (Hectáreas)",
    y = "Producción (Toneladas)",
    color = "Departamento"
  ) +
  theme_minimal()

summary(datos)
print( p5_modificado +p5)

library(dplyr)


datos_imputados_por_depto %>%
  filter(is.na(datos$t_produccion)) %>%
  select(departamento, municipio, año, t_produccion, t_produccion_imputada)

datos_imputados_por_depto <- datos %>%
  group_by(departamento) %>%
  mutate(t_produccion_imputada = if_else(is.na(t_produccion),
                                         mean(t_produccion, na.rm = TRUE),
                                         t_produccion)) %>%
  ungroup()


# 1. Identificar las filas que tienen datos faltantes en 'rendimiento'
indices_faltantes <- which(is.na(datos$rendimiento))

# 2. Entrenar el modelo usando SOLAMENTE las filas completas
modelo_para_imputar <- lm(rendimiento ~ area_cosechada + departamento + año, 
                          data = datos[-indices_faltantes, ])

# 3. Predecir los valores de rendimiento para las filas que tenían NA
predicciones <- predict(modelo_para_imputar, newdata = datos[indices_faltantes, ])

# 4. Crear un nuevo data frame y rellenar los NA con las predicciones
datos_imputados_lm <- datos
datos_imputados_lm$rendimiento[indices_faltantes] <- predicciones



print(p6 + p5)

library(patchwork)



# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(patchwork) # La clave para unir los gráficos

# 1. Obtenemos una lista de los departamentos únicos
lista_departamentos <- unique(datos$departamento)

# 2. Creamos una lista vacía para guardar cada gráfico
lista_de_plots <- list()

# 3. Hacemos un bucle que recorra cada departamento
for (depto in lista_departamentos) {
  
  # Creamos un gráfico solo para el departamento actual
  p <- datos %>%
    filter(departamento == depto) %>%
    ggplot(aes(x = area_cosechada, y = rendimiento_imputado)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(depto) +
    theme_minimal()
  
  # 4. Guardamos el gráfico 'p' en nuestra lista
  lista_de_plots[[depto]] <- p
}

# 5. Usamos patchwork para organizar y mostrar todos los gráficos de la lista
wrap_plots(lista_de_plots)


vis_miss(datos)
