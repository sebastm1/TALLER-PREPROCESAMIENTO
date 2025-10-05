
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
table(datos$DEPARTAMENTO)
table(datos$ESTADO.FISICO.PRODUCCION)
level_CULTIVO <- c(MAIZ = "MAIZ", maiz= "MAIZ")
datos <- datos %>%
  mutate(CULTIVO = recode(CULTIVO, !!!level_CULTIVO))
datos <- datos %>%
  filter(CULTIVO == "MAIZ")
table(datos$DEPARTAMENTO)
table(datos$ESTADO.FISICO.PRODUCCION)
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
    title = "Relacion entre rea Cosechada y Producci de Maz",
    x = "rea Cosechada (Hectáreas)",
    y = "Producción (Toneladas)",
    color = "Departamento"
  ) +
  theme_minimal()
print(p1)

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
  ) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 1))


summary()
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


UMBRAL_INFLUENCIA <- 3

resultados_influencia <- data.frame()

for (depto_actual in unique(Datos_ImputR$departamento)) {
  
  datos_depto <- Datos_ImputR %>% filter(departamento == depto_actual)
  limite_superior <- quantile(datos_depto$rendimiento, 0.75, na.rm = TRUE) + 1.5 * IQR(datos_depto$rendimiento, na.rm = TRUE)
  outliers_depto <- datos_depto %>% filter(rendimiento > limite_superior)
  if (nrow(outliers_depto) == 0) next 
  media_con_outliers <- mean(datos_depto$rendimiento, na.rm = TRUE)
  
  for (id_outlier in outliers_depto$id_fila) {
    media_sin_outlier <- mean(datos_depto$rendimiento[datos_depto$id_fila != id_outlier], na.rm = TRUE)
    cambio_porcentual <- abs((media_con_outliers - media_sin_outlier) / media_con_outliers) * 100
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

outliers_a_revisar <- resultados_influencia %>%
  filter(influencia_pct > UMBRAL_INFLUENCIA) %>%
  arrange(desc(influencia_pct))

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




datos <- datos %>% 
  mutate(departamento = str_to_upper(departamento))
#omitir datos
datos_limpios <- datos %>%
  filter(!is.na(datos$rendimiento))
#imputar por departamento
datos_imputados <- datos %>%
  group_by(departamento) %>%
  mutate(
    rendimiento = ifelse(is.na(rendimiento), 
                          mean(rendimiento, na.rm = TRUE), 
                          rendimiento)
  ) %>%
  ungroup()
# Imputación de datos faltantes con MICE
imputR <- mice(datos, m = 5, maxit = 5, seed = 123, print = FALSE) 
Datos_ImputR <- complete(imputR)

datos_todos <- bind_rows(
  datos_limpios %>% mutate(metodo_imputacion = "Sin NA"),
  datos_imputados %>% mutate(metodo_imputacion = "Imputacion Dept"),
  Datos_ImputR %>% mutate(metodo_imputacion = "Mice")
)
ggplot(datos_todos, aes(x = departamento, y = rendimiento, fill = metodo_imputacion)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.3) +
  labs(
    title = "Comparación de Métodos de Imputación de Rendimiento",
    x = "Departamento",
    y = "Rendimiento (unidades)"
  ) +
  scale_fill_manual(values = c("Sin NA" = "#781C2E", "Imputacion Dept" = "#E86C1F", "Mice" = "#FFB347")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", color = "darkred", hjust = 0.5))

filas_originales <- nrow(Datos_ImputR)

Datos_Sin_Outliers_ZScore <- Datos_ImputR %>%
  group_by(departamento) %>%
  mutate(
    
    media_depto = mean(rendimiento, na.rm = TRUE),
    sd_depto = sd(rendimiento, na.rm = TRUE),
    
    limite_superior = media_depto + 3 * sd_depto,
    limite_inferior = media_depto - 3 * sd_depto
  ) %>%
  
  filter(rendimiento >= limite_inferior & rendimiento <= limite_superior) %>%
  
  ungroup() %>%
  select(-media_depto, -sd_depto, -limite_superior, -limite_inferior)


filas_finales <- nrow(Datos_Sin_Outliers_ZScore)

rango_rendimiento <- range(Datos_ImputR$rendimiento, na.rm = TRUE)

plot_antes <- ggplot(Datos_ImputR, aes(x = reorder(departamento, rendimiento, FUN = median), y = rendimiento)) +
  geom_boxplot(fill = "#E86C1F", alpha = 0.8) +
  coord_flip(ylim = rango_rendimiento) +
  labs(
    title = "ANTES de Eliminar Outliers",
    x = "Departamento",
    y = "Rendimiento (Ton/Ha)"
  ) +
  theme_minimal()

plot_despues <- ggplot(Datos_Sin_Outliers_ZScore, aes(x = reorder(departamento, rendimiento, FUN = median), y = rendimiento)) +
  geom_boxplot(fill = "skyblue", alpha = 0.8) +
  coord_flip(ylim = rango_rendimiento) + 
  labs(
    title = "DESPUÉS de Eliminar Outliers (Método Z-Score)",
    x = "Departamento",
    y = "Rendimiento (Ton/Ha)"
  ) +
  theme_minimal()
plot_antes / plot_despues

Datos_ImputR<- Datos_Sin_Outliers_ZScore


library(dplyr)
library(stringr)
library(ggplot2)
library(mice)

# 0) Preparar: estandarizar y crear id + indicador de NA original
datos <- datos %>%
  mutate(
    departamento = str_to_upper(departamento),
    .row_id = row_number(),
    orig_missing = is.na(rendimiento)
  )

# 1) Observados (Sin NA) --> mantengo sólo filas con rendimiento observado
df_sinNA <- datos %>%
  filter(!orig_missing) %>%
  transmute(departamento, rendimiento, imputed_flag = FALSE, metodo_imputacion = "Sin NA")

# 2) Imputación por media por departamento (sin perder indicador)
df_imput_media <- datos %>%
  group_by(departamento) %>%
  mutate(
    rendimiento_imput = if_else(orig_missing,
                                mean(rendimiento, na.rm = TRUE),
                                rendimiento)
  ) %>%
  ungroup() %>%
  transmute(departamento,
            rendimiento = rendimiento_imput,
            imputed_flag = orig_missing,
            metodo_imputacion = "Imputacion Dept")

# 3) Imputación con MICE (ejemplo: guardo la primera completación y conservo indicador orig_missing)
#    Nota: mice maneja factores; si tienes columnas problemáticas conviene pasar solo las columnas necesarias.
mice_input <- datos %>% select(.row_id, departamento, rendimiento) # puedes añadir más predictores si conviene
imputR <- mice(mice_input, m = 5, maxit = 5, seed = 123, print = FALSE)
Datos_ImputR_raw <- complete(imputR, 1)   # completado 1 (podrías probar otras completaciones o promediar resultados)
# unir el indicador orig_missing mediante .row_id
Datos_ImputR <- Datos_ImputR_raw %>%
  left_join(datos %>% select(.row_id, orig_missing), by = ".row_id") %>%
  transmute(departamento,
            rendimiento,
            imputed_flag = orig_missing,
            metodo_imputacion = "MICE")

# 4) Unir los 3 datasets (mismas columnas)
datos_todos <- bind_rows(df_sinNA, df_imput_media, Datos_ImputR)

# 5) Resumen: cuántas imputaciones por departamento / método
tabla_imput <- datos_todos %>%
  group_by(metodo_imputacion, departamento) %>%
  summarise(
    total = n(),
    n_imputados = sum(imputed_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(metodo_imputacion, departamento)

print(tabla_imput)

# 6) Resumen de medianas/varianzas por método (útil para comparar)
resumen_stats <- datos_todos %>%
  group_by(metodo_imputacion, departamento) %>%
  summarise(
    mediana = median(rendimiento, na.rm = TRUE),
    sd = sd(rendimiento, na.rm = TRUE),
    .groups = "drop"
  )
print(resumen_stats)

# 7) Gráfico: boxplot agrupado + puntos imputados destacados en rojo
ggplot(datos_todos, aes(x = departamento, y = rendimiento, fill = metodo_imputacion)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.25) +
  # puntos imputados (jitter sobre el dodge para que no se empalmen)
  geom_jitter(data = filter(datos_todos, imputed_flag == TRUE),
              aes(x = departamento, y = rendimiento),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
              color = "red", size = 1.2, alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("Sin NA" = "#781C2E",
                               "Imputacion Dept" = "#E86C1F",
                               "MICE" = "#FFB347")) +
  labs(title = "Comparación de Métodos de Imputación de Rendimiento",
       x = "Departamento",
       y = "Rendimiento (unidades)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "darkred", hjust = 0.5))

