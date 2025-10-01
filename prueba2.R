datos <- readxl::read_excel("C:/Users/johnn/Desktop/GESTION DE DATOS/TALLER PREPROCESAMIENTO/DATOS/eva_df_2025.xlsx")
table(datos$CULTIVO)
level_CULTIVO <- c(MAIZ = "MAIZ", maiz= "MAIZ")
datos <- datos %>%
  mutate(CULTIVO = recode(CULTIVO, !!!level_CULTIVO))
datos <- datos %>%
  filter(CULTIVO == "MAIZ")

table(datos$DEPARTAMENTO)
1
ncol(datos)
nuevos_nombres <- c("departamento", "municipio", "grupo", "cultivo", "año", 
                    "area_sembrada", "area_cosechada", "t_produccion", 
                    "estado_fisico", "ciclo_cultivo")

names(datos) <- nuevos_nombres