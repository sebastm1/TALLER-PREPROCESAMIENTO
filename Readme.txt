#################################################################
# INSTRUCCIONES PARA EL TALLER DE PREPROCESAMIENTO DE DATOS
#################################################################

Hola Profesor,

Gracias por tomarse el tiempo para evaluar nuestro trabajo.

-----------------------------------------------------------------
## Opción 1: Ver el Informe en Línea (Recomendado)
-----------------------------------------------------------------

Puede visualizar el informe interactivo final directamente en su navegador, sin necesidad de instalar nada, a través del siguiente enlace:

https://taller-preprocesamiento-n2li.vercel.app


-----------------------------------------------------------------
## Repositorio del Proyecto
-----------------------------------------------------------------

Todo el código fuente, los datos y el informe `.Rmd` están disponibles públicamente en nuestro repositorio de GitHub:

https://github.com/sebastm1/TALLER-PREPROCESAMIENTO


-----------------------------------------------------------------
## Opción 2: Ejecutar el Proyecto Localmente
-----------------------------------------------------------------

Si desea ejecutar el código usted mismo en RStudio, siga estos dos sencillos pasos:

### Paso 1: Instalar las Librerías Necesarias

Copie y pegue el siguiente bloque de código COMPLETO en su consola de R y presione Enter. 
El script verificará qué librerías le faltan y las instalará automáticamente.

####################### COPIAR DESDE AQUÍ #######################

lista_paquetes <- c("dplyr", "stringr", "ggplot2", "tidyverse", "knitr", 
                    "kableExtra", "RColorBrewer", "patchwork", "tidyr", 
                    "naniar", "corrplot", "htmltools", "crosstalk", "mice", 
                    "plotly", "readxl", "table1", "highcharter", "rmdformats")

nuevos_paquetes <- lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]

if(length(nuevos_paquetes)) {
  install.packages(nuevos_paquetes)
  cat("Las siguientes librerías han sido instaladas:", nuevos_paquetes, "\n")
} else {
  cat("Todas las librerías necesarias ya están instaladas.\n")
}

######################## HASTA AQUÍ ############################


### Paso 2: Generar el Informe

Una vez instaladas las librerías, el código del informe es secuencial.
1. Abra el archivo del proyecto `.Rmd` en RStudio.
2. Haga clic en el botón **Knit** en la parte superior del editor.


¡Gracias nuevamente y disfrute del análisis interactivo!