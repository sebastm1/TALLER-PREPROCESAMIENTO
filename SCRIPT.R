
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(tidyr)
library(naniar)
library(corrplot)
library(mice)
library(plotly)
datos <- readxl::read_excel("C:/Users/johnn/Desktop/GESTION DE DATOS/TALLER PREPROCESAMIENTO/DATOS/eva_df_2025.xlsx")
table(datos$CULTIVO)
level_CULTIVO <- c(MAIZ = "MAIZ", maiz= "MAIZ")
datos <- datos %>%
  mutate(CULTIVO = recode(CULTIVO, !!!level_CULTIVO))
datos <- datos %>%
  filter(CULTIVO == "MAIZ")
table(datos$DEPARTAMENTO)
ncol(datos)
nuevos_nombres <- c("departamento", "municipio", "grupo", "cultivo","año", 
                    "area_sembrada", "area_cosechada", "t_produccion", 
                    "estado_fisico", "ciclo_cultivo")

names(datos) <- nuevos_nombres
summary(datos)
vis_miss(datos)

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

filas_faltantes_rendimiento <- datos %>% 
  filter(is.na(rendimiento))
print(filas_faltantes_rendimiento, n = Inf)

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
datos_imputados <- datos %>%
  group_by(departamento) %>%
  mutate(
    t_produccion = ifelse(is.na(t_produccion), 
                          median(t_produccion, na.rm = TRUE), 
                          t_produccion)
  ) %>%
  ungroup()

datos_imputados <- datos_imputados %>%
  mutate(
    tipo_dato = if_else(is.na(datos$t_produccion), 
                        "Imputado", 
                        "Original"))

 ggplot(data = datos_imputados, aes(x = area_cosechada, y = t_produccion)) +
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


print ( p1 + p2)

ggplot(datos, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )


 ggplot(datos_imputados, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )

datos_limpios <- datos %>%
  filter(!is.na(datos$rendimiento))

ggplot(datos_limpios, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )

imputR = mice(datos, maxit = 5, seed = 123, print = F) 
Datos_ImputR = complete(imputR)
summary(Datos_ImputR)

p4 <- ggplot(Datos_ImputR, aes(x = departamento, y = rendimiento)) +
  geom_boxplot() +
  labs(
    title = "Distribución del Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  )
vis_miss(Datos_ImputR)


datos_freq <- Datos_ImputR %>%
  count(departamento)

Dist <- plot_ly(datos_freq, labels = ~departamento, values = ~n, type = "pie") %>%
  layout(title = "Distribución de la muestra por departamentos")

print(Dist)

ggplot(data = datos_limpios, aes(x = area_cosechada, y = t_produccion)) +
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



ggplot(data = datos_limpios, aes(x = area_cosechada, y = t_produccion)) +
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


datos_imputados_por_depto %>%
  filter(is.na(datos$t_produccion)) %>%
  select(departamento, municipio, año, t_produccion, t_produccion_imputada)

datos_imputados_por_depto <- datos %>%
  group_by(departamento) %>%
  mutate(t_produccion_imputada = if_else(is.na(t_produccion),
                                         mean(t_produccion, na.rm = TRUE),
                                         t_produccion)) %>%
  ungroup()


lista_departamentos <- unique(Datos_ImputR$departamento)


lista_de_plots <- list()


for (depto in lista_departamentos) {
  
  p <- Datos_ImputR %>%
    filter(departamento == depto) %>%
    ggplot(aes(x = area_cosechada, y = t_produccion)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(depto) +
    theme_minimal()
  
  lista_de_plots[[depto]] <- p
}

wrap_plots(lista_de_plots)



for (depto in lista_departamentos) {
  
  
  datos_filtrados <- subset(Datos_ImputR, departamento == depto)
  
  if (.Platform$OS.type == "windows") x11() else if (.Platform$OS.type == "unix") x11()
  
  par(mfrow = c(3, 1))
  
  with(datos_filtrados, {

    hist(rendimiento, 
         freq = FALSE, 
         col = "skyblue", 
         breaks = 15, 
         main = paste("Histograma de Rendimiento en", depto))
    
    boxplot(rendimiento, 
            horizontal = TRUE, 
            col = "orange",
            main = paste("Boxplot de Rendimiento en", depto))
  
    hist(scale(rendimiento), 
         freq = FALSE, 
         col = "lightgreen", 
         breaks = 15,
         main = paste("Rendimiento Estandarizado en", depto))
  })
}

ggplot(Datos_ImputR, aes(x = reorder(departamento, rendimiento, FUN = median), 
                          y = rendimiento, 
                          fill = departamento)) +
  geom_violin() + 
  geom_boxplot(width = 0.1, fill = "white") + 
  coord_flip() + 
  labs(
    title = "Comparación de Distribuciones de Rendimiento por Departamento",
    x = "Departamento",
    y = "Rendimiento (Ton/Ha)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


Datos_ImputR <- Datos_ImputR %>%
  mutate(id_fila = row_number())


UMBRAL_INFLUENCIA <- 5

# Creamos un data frame vacío para guardar los resultados
resultados_influencia <- data.frame()


# --- ANÁLISIS AUTOMATIZADO ---
# Loop que recorre cada departamento
for (depto_actual in unique(Datos_ImputR$departamento)) {
  
  # 1. Filtra los datos del departamento actual
  datos_depto <- Datos_ImputR %>% filter(departamento == depto_actual)
  
  # 2. Identifica los outliers solo para este departamento
  limite_superior <- quantile(datos_depto$rendimiento, 0.75, na.rm = TRUE) + 1.5 * IQR(datos_depto$rendimiento, na.rm = TRUE)
  outliers_depto <- datos_depto %>% filter(rendimiento > limite_superior)
  
  if (nrow(outliers_depto) == 0) next # Si no hay outliers, salta al siguiente depto
  
  # 3. Calcula la media original del grupo (con todos los datos)
  media_con_outliers <- mean(datos_depto$rendimiento, na.rm = TRUE)
  
  # 4. Loop que recorre cada outlier para medir su influencia
  for (id_outlier in outliers_depto$id_fila) {
    
    # Excluye el outlier actual y recalcula la media
    media_sin_outlier <- mean(datos_depto$rendimiento[datos_depto$id_fila != id_outlier], na.rm = TRUE)
    
    # Calcula el cambio porcentual que causó el outlier
    cambio_porcentual <- abs((media_con_outliers - media_sin_outlier) / media_con_outliers) * 100
    
    # Guarda el resultado
    info_outlier <- datos_depto %>% filter(id_fila == id_outlier)
    resultado_fila <- data.frame(
      id_fila = id_outlier,
      departamento = depto_actual,
      municipio = info_outlier$municipio,
      año = info_outlier$año,
      valor_outlier = info_outlier$rendimiento,
      influencia_pct = cambio_porcentual
    )
    resultados_influencia <- rbind(resultados_influencia, resultado_fila)
  }
}


# --- RESULTADO FINAL ---
# Filtramos la lista para mostrar solo los outliers que superan el umbral
outliers_a_revisar <- resultados_influencia %>%
  filter(influencia_pct > UMBRAL_INFLUENCIA) %>%
  arrange(desc(influencia_pct)) # Ordenamos por el de mayor impacto

print("--- Outliers a Revisar (Influencia > 5%) ---")
print(outliers_a_revisar)


datos_comparacion <- Datos_ImputR %>%
  filter(año %in% c(2007, 2017)) %>%
  group_by(departamento, año) %>%
  summarise(rendimiento_mediano = median(rendimiento, na.rm = TRUE), .groups = 'drop')
ggplot(data = datos_comparacion, aes(x = departamento, y = rendimiento_mediano, fill = as.factor(año))) +
  geom_col(position = "dodge") +
  coord_flip() + 
  scale_fill_manual(name = "Año", values = c("2007" = "orange", "2017" = "skyblue")) +
  labs(
    title = "Comparación del Rendimiento Mediano por Departamento",
    subtitle = "Años 2007 vs. 2017",
    x = "Departamento",
    y = "Rendimiento Mediano (Ton/Ha)"
  ) +
  theme_minimal()

tendencia_anual <- Datos_ImputR %>%
  group_by(año, departamento) %>%
  summarise(
    rendimiento_mediano = median(rendimiento, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(data = tendencia_anual, aes(x = año, y = rendimiento_mediano, color = departamento)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  
  scale_x_continuous(breaks = unique(tendencia_anual$año)) +
  
  labs(
    title = "Tendencia del Rendimiento Mediano del Maíz a través de los Años",
    subtitle = "Cada línea representa un departamento",
    x = "Año",
    y = "Rendimiento Mediano (Toneladas/Hectárea)",
    color = "Departamento"
  ) +
  theme_minimal() +
  
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 1))
