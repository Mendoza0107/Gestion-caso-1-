library(readxl)
library(tidyverse)
library(knitr)
library(plotly)

Tabla_modelo <- data.frame(
  Variable = c(
    "Generacion", "β₀", "Poblacion", "PIB",
    "Consumo", "Acceso", "Continente", "ε"
  ),
  
  Descripcion = c(
    "Generación total de electricidad del país",
    "Intercepto del modelo",
    "Población total del país",
    "Producto Interno Bruto total del país",
    "Consumo de electricidad per cápita",
    "Población con acceso a electricidad",
    "Continente al que pertenece el país",
    "Término de error aleatorio"
  ),
  
  Tipo = c(
    "Dependiente - cuantitativa continua",
    "Parámetro",
    "Cuantitativa",
    "Cuantitativa continua",
    "Cuantitativa continua",
    "Cuantitativa continua",
    "Categórica nominal",
    "Error"
  ),
  
  Unidad_Notas = c(
    "TWh",
    "Intercepto",
    "Habitantes",
    "USD",
    "kWh per cápita",
    "%",
    "Categoría de referencia",
    "Factores no observados"
  )
)

knitr::kable(
  Tabla_modelo,
  col.names = c(
    "Variable",
    "Descripción",
    "Tipo de variable",
    "Unidad / Notas"
  ),
  caption = "Variables del modelo de regresión"
)


gen = read.csv("Generacion de electricidad.csv", sep = ",") %>% 
  rename(Codigo = 2, Generacion = 4) %>% select(Codigo, Generacion)

pob = read.csv("Poblacion cada pais.csv", sep = ",") %>% 
  rename(Codigo = 2, Poblacion = 4) %>% select(Codigo, Poblacion)

cont = read.csv("Continente de cada pais.csv", sep = ",") %>% 
  rename(Codigo = 2, Continente = 4) %>% select(Codigo, Continente)

cons = read.csv("Cons energ per capita.xlsx - Data.csv") %>% 
  select("Country.Code", X2022) %>% 
  rename(Codigo = 1, Consumo = 2)

pib = read.csv("PIB total de cada pais.csv", sep = ",") %>% 
  rename(Codigo = 2, PIB = 4) %>% select(Codigo, PIB)

acc = read.csv("Acceso a electricidad de cada pais.csv", sep = ",") %>% 
  rename(Codigo = 2, Acceso = 4) %>% select(Codigo, Acceso)

Base_datos = gen  %>% inner_join(pob, by="Codigo") %>% 
  inner_join(cont, by="Codigo") %>% inner_join(cons, by="Codigo") %>% 
  inner_join(pib, by="Codigo") %>% inner_join(acc, by="Codigo")

# DEPURACION DE LA BASE 

# Covertirtir los espacios vacios de consumo en NA

Base_datos$Consumo[Base_datos$Consumo == ""] <- NA

# Cambiar la coma decimal por punto para que consumo sea variable numerica

Base_datos$Consumo <- as.numeric(
  gsub(",", ".", Base_datos$Consumo)
)

nrow(Base_datos)
colSums(is.na(Base_datos))
str(Base_datos)
Base_datos %>%
  filter(is.na(Consumo)) %>%
  select(Codigo, Continente)
sum(duplicated(Base_datos$Codigo))
summary(Base_datos)

# Porcentaje de datos faltantes en Consumo
mean(is.na(Base_datos$Consumo)) * 100

# Cantidad de datos disponibles y faltantes
table(is.na(Base_datos$Consumo))

# Faltantes de Consumo por continente
Base_datos %>%
  group_by(Continente) %>%
  summarise(
    Total = n(),
    Faltantes_Consumo = sum(is.na(Consumo)),
    Porcentaje_Faltante = round(mean(is.na(Consumo)) * 100, 2)
  )

# Eliminar los paises que tengan algun dato faltante
Base_datos = Base_datos %>%
  drop_na()




# Tabla estadistica descripriva (Generacion de electricidad)

Tabla_generacion <- Base_datos %>%
  group_by(Continente) %>%
  summarise(
    N = n(),
    Promedio = mean(Generacion),
    Desv_Estandar = sd(Generacion),
    Minimo = min(Generacion),
    Maximo = max(Generacion)
  )

kable(
  Tabla_generacion,
  digits = 2,
  col.names = c(
    "Continente",
    "N",
    "Generación promedio (TWh)",
    "Desv. Estándar",
    "Mínimo",
    "Máximo"
  ),
  caption = "Estadísticas descriptivas de la generación de electricidad por continente"
)




# Tabla estadistica descriptiva (Poblacion)

Tabla_poblacion <- Base_datos %>%
  group_by(Continente) %>%
  summarise(
    N = n(),
    Promedio = mean(Poblacion),
    Desv_Estandar = sd(Poblacion),
    Minimo = min(Poblacion),
    Maximo = max(Poblacion)
  )

kable(
  Tabla_poblacion,
  digits = 2,
  col.names = c(
    "Continente",
    "N",
    "Población promedio",
    "Desv. Estándar",
    "Mínimo",
    "Máximo"
  ),
  caption = "Estadísticas descriptivas de la población por continente"
)




#Tabla estadistica descriptiva (Consumo energia per capital)

Tabla_consumo <- Base_datos %>%
  group_by(Continente) %>%
  summarise(
    N = n(),
    Promedio = mean(Consumo),
    Desv_Estandar = sd(Consumo),
    Minimo = min(Consumo),
    Maximo = max(Consumo)
  )

kable(
  Tabla_consumo,
  digits = 2,
  col.names = c(
    "Continente",
    "N",
    "Consumo promedio",
    "Desv. Estándar",
    "Mínimo",
    "Máximo"
  ),
  caption = "Estadísticas descriptivas del consumo de electricidad per cápita por continente"
)




#Tabla estadistica descriptiva (PIB)

Tabla_pib <- Base_datos %>%
  group_by(Continente) %>%
  summarise(
    N = n(),
    Promedio = mean(PIB),
    Desv_Estandar = sd(PIB),
    Minimo = min(PIB),
    Maximo = max(PIB)
  )

kable(
  Tabla_pib,
  digits = 2,
  col.names = c(
    "Continente",
    "N",
    "PIB promedio",
    "Desv. Estándar",
    "Mínimo",
    "Máximo"
  ),
  caption = "Estadísticas descriptivas del PIB total por continente"
)

#Tabla estadistica descriptiva (Acceso a electricidad)

Tabla_acceso <- Base_datos %>%
  group_by(Continente) %>%
  summarise(
    N = n(),
    Promedio = mean(Acceso),
    Desv_Estandar = sd(Acceso),
    Minimo = min(Acceso),
    Maximo = max(Acceso)
  )

kable(
  Tabla_acceso,
  digits = 2,
  col.names = c(
    "Continente",
    "N",
    "Acceso promedio (%)",
    "Desv. Estándar",
    "Mínimo",
    "Máximo"
  ),
  caption = "Estadísticas descriptivas del acceso a electricidad por continente"
)





# Tabla de frecuencias para la variable continente

Tabla_continente <- Base_datos %>%
  count(Continente) %>%
  arrange(desc(n)) %>%
  mutate(
    Porcentaje = n / sum(n) * 100,
    Porcentaje_Acumulado = cumsum(Porcentaje)
  ) %>%
  rename(
    Frecuencia = n
  )

# ============================================================
# MOSTRAR TABLA
# ============================================================

knitr::kable(
  Tabla_continente,
  digits = 1,
  col.names = c(
    "Continente",
    "Frecuencia",
    "Porcentaje (%)",
    "Porcentaje Acumulado (%)"
  ),
  caption = "Distribución de frecuencias de los países según continente"
)


# Estimacion del modelo de regresion lineal multiple 

# Tabla

library(broom)
library(dplyr)

# ============================================================
# 1. ASEGURAR QUE CONTINENTE SEA CATEGÓRICA
# ============================================================

Base_datos$Continente <- factor(Base_datos$Continente)

# ============================================================
# 2. ESTIMAR EL MODELO DE REGRESIÓN LINEAL MÚLTIPLE
# ============================================================

Modelo_final <- lm(
  Generacion ~ Poblacion + PIB + Consumo + Acceso + Continente,
  data = Base_datos
)

# ============================================================
# 3. RESULTADOS DE LOS COEFICIENTES
# ============================================================

resumen_mod <- broom::tidy(Modelo_final) %>%
  mutate(
    Significancia = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE ~ ""
    ),
    
    `Valor p` = case_when(
      p.value < 0.0001 ~ "< 0.0001",
      TRUE ~ sprintf("%.4f", p.value)
    )
  ) %>%
  transmute(
    Variable = term,
    Coeficiente = round(estimate, 5),
    `Error Estándar` = round(std.error, 5),
    `Estadístico t` = round(statistic, 5),
    `Valor p`,
    Sig. = Significancia
  )

# ============================================================
# 4. RESULTADOS GENERALES DEL MODELO
# ============================================================

resumen_global <- broom::glance(Modelo_final)

R2 <- round(resumen_global$adj.r.squared, 4)
Fstat <- round(resumen_global$statistic, 2)

pvalor_modelo <- ifelse(
  resumen_global$p.value < 0.0001,
  "< 0.0001",
  sprintf("%.4f", resumen_global$p.value)
)

N <- nobs(Modelo_final)

# ============================================================
# 5. MOSTRAR RESULTADOS
# ============================================================

resumen_mod

cat(
  "\nR² ajustado:", R2,
  "\nF:", Fstat,
  "\nValor p del modelo:", pvalor_modelo,
  "\nN:", N
)



# Prediccion de la generacion de electricidad

# ============================================================
# PREDICCIONES DE GENERACIÓN DE ELECTRICIDAD
# ============================================================

library(dplyr)
library(knitr)
library(kableExtra)

# ============================================================
# 1. ASEGURAR QUE CONTINENTE SEA UNA VARIABLE CATEGÓRICA
# ============================================================

Base_datos$Continente <- factor(Base_datos$Continente)

# Verificar los nombres de los continentes
levels(Base_datos$Continente)


# ============================================================
# 2. ESTIMAR NUEVAMENTE EL MODELO
# ============================================================

Modelo_final <- lm(
  Generacion ~ Poblacion + PIB + Consumo + Acceso + Continente,
  data = Base_datos
)


# ============================================================
# 3. CREAR CINCO ESCENARIOS HIPOTÉTICOS
# ============================================================

escenarios <- data.frame(
  
  Poblacion = as.numeric(c(
    quantile(Base_datos$Poblacion, 0.10, na.rm = TRUE),
    quantile(Base_datos$Poblacion, 0.25, na.rm = TRUE),
    quantile(Base_datos$Poblacion, 0.50, na.rm = TRUE),
    quantile(Base_datos$Poblacion, 0.75, na.rm = TRUE),
    quantile(Base_datos$Poblacion, 0.90, na.rm = TRUE)
  )),
  
  PIB = as.numeric(c(
    quantile(Base_datos$PIB, 0.10, na.rm = TRUE),
    quantile(Base_datos$PIB, 0.25, na.rm = TRUE),
    quantile(Base_datos$PIB, 0.50, na.rm = TRUE),
    quantile(Base_datos$PIB, 0.75, na.rm = TRUE),
    quantile(Base_datos$PIB, 0.90, na.rm = TRUE)
  )),
  
  Consumo = as.numeric(c(
    quantile(Base_datos$Consumo, 0.10, na.rm = TRUE),
    quantile(Base_datos$Consumo, 0.25, na.rm = TRUE),
    quantile(Base_datos$Consumo, 0.50, na.rm = TRUE),
    quantile(Base_datos$Consumo, 0.75, na.rm = TRUE),
    quantile(Base_datos$Consumo, 0.90, na.rm = TRUE)
  )),
  
  Acceso = as.numeric(c(
    quantile(Base_datos$Acceso, 0.10, na.rm = TRUE),
    quantile(Base_datos$Acceso, 0.25, na.rm = TRUE),
    quantile(Base_datos$Acceso, 0.50, na.rm = TRUE),
    quantile(Base_datos$Acceso, 0.75, na.rm = TRUE),
    quantile(Base_datos$Acceso, 0.90, na.rm = TRUE)
  )),
  
  Continente = c(
    "Africa",
    "Asia",
    "Europe",
    "North America",
    "South America"
  )
)


# ============================================================
# 4. DAR A CONTINENTE LOS MISMOS NIVELES DE LA BASE ORIGINAL
# ============================================================

escenarios$Continente <- factor(
  escenarios$Continente,
  levels = levels(Base_datos$Continente)
)

# Verificar que NO aparezcan NA
escenarios$Continente


# ============================================================
# 5. CALCULAR LA GENERACIÓN PREDICHA
# ============================================================

escenarios$Generacion_predicha <- predict(
  Modelo_final,
  newdata = escenarios
)


# ============================================================
# 6. PREPARAR LA TABLA
# ============================================================

tabla_predicciones <- escenarios %>%
  mutate(
    Poblacion = round(Poblacion, 0),
    PIB = round(PIB, 0),
    Consumo = round(Consumo, 2),
    Acceso = round(Acceso, 2),
    Generacion_predicha = round(Generacion_predicha, 2)
  ) %>%
  rename(
    `Población` = Poblacion,
    `PIB total (USD)` = PIB,
    `Consumo (kWh per cápita)` = Consumo,
    `Acceso (%)` = Acceso,
    `Continente` = Continente,
    `Generación predicha (TWh)` = Generacion_predicha
  )


# ============================================================
# 7. MOSTRAR LA TABLA BONITA
# ============================================================

kbl(
  tabla_predicciones,
  caption = "Predicciones de generación de electricidad para diferentes escenarios",
  align = "c",
  format.args = list(
    big.mark = ",",
    decimal.mark = ".",
    scientific = FALSE
  )
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13,
    position = "center"
  ) %>%
  row_spec(
    0,
    background = "#1F3A5F",
    color = "white",
    bold = TRUE
  ) %>%
  footnote(
    general = paste(
      "Elaboración propia con base en Our World in Data, 2022.",
      "Las predicciones corresponden a escenarios hipotéticos."
    ),
    general_title = "Nota:",
    footnote_as_chunk = TRUE
  )




# 5 Resultados del modelo

## 5.1 Diagnostico grafico de supuestos


## 5.1.1 Lineal

library(broom)
library(ggplot2)
library(dplyr)

# ============================================================
# 1. OBTENER RESIDUOS Y VALORES AJUSTADOS
# ============================================================

res_df <- augment(Modelo_final)

# Ver los primeros resultados
head(res_df)


# ============================================================
# 2. GRÁFICO DE RESIDUOS VS VALORES AJUSTADOS
# ============================================================

resid_vs_ajustados <- ggplot(
  res_df,
  aes(x = .fitted, y = .resid)
) +
  
  # Puntos
  geom_point(
    aes(
      color = abs(.resid),
      size = abs(.resid)
    ),
    alpha = 0.6,
    show.legend = FALSE
  ) +
  
  # Línea horizontal en residuo = 0
  geom_hline(
    yintercept = 0,
    color = "#E27D60",
    linetype = "dashed",
    linewidth = 1.2,
    alpha = 0.8
  ) +
  
  # Línea de tendencia LOESS
  geom_smooth(
    method = "loess",
    color = "#1F3A5F",
    fill = "#4E79A7",
    alpha = 0.20,
    linewidth = 1.2,
    se = TRUE
  ) +
  
  # Colores de los puntos
  scale_color_gradient(
    low = "#4E79A7",
    high = "#E27D60"
  ) +
  
  # Tamaño de los puntos
  scale_size_continuous(
    range = c(2, 6)
  ) +
  
  # Títulos
  labs(
    title = "ANÁLISIS DE RESIDUOS VS VALORES AJUSTADOS",
    subtitle = "Validación de los supuestos de linealidad y homocedasticidad",
    x = "Valores ajustados de generación (TWh)",
    y = "Residuos",
    caption = paste(
      "Fuente: Elaboración propia con base en Our World in Data, 2022 |",
      "Línea azul: tendencia LOESS |",
      "Línea coral: residuo cero"
    )
  ) +
  
  # Diseño
  theme_minimal() +
  
  theme(
    text = element_text(
      family = "sans"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#1F3A5F",
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    
    plot.caption = element_text(
      size = 10,
      color = "gray50",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    panel.grid.major = element_line(
      color = "gray90"
    ),
    
    panel.grid.minor = element_line(
      color = "gray95"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# ============================================================
# 3. MOSTRAR GRÁFICO
# ============================================================

print(resid_vs_ajustados)




## 5.1.2 Normailidad

library(ggplot2)
library(broom)

# ============================================================
# GRÁFICO Q-Q DE LOS RESIDUOS
# ============================================================

G_qq <- ggplot(
  res_df,
  aes(sample = .resid)
) +
  
  # Puntos de los residuos
  geom_qq(
    alpha = 0.65,
    color = "#4E79A7",
    size = 2
  ) +
  
  # Línea de referencia de normalidad
  geom_qq_line(
    color = "#E27D60",
    linewidth = 1.2
  ) +
  
  # Títulos
  labs(
    title = "GRÁFICO Q-Q DE LOS RESIDUOS",
    subtitle = "Validación de normalidad - Residuos vs distribución normal teórica",
    x = "Cuantiles teóricos",
    y = "Cuantiles de los residuos",
    caption = "Fuente: Elaboración propia con base en Our World in Data, 2022"
  ) +
  
  # ==========================================================
# DISEÑO
# ==========================================================

theme_minimal() +
  
  theme(
    text = element_text(
      family = "sans"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#1F3A5F",
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    
    plot.caption = element_text(
      size = 10,
      color = "gray50",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    panel.grid.major = element_line(
      color = "gray90"
    ),
    
    panel.grid.minor = element_line(
      color = "gray95"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# ============================================================
# MOSTRAR GRÁFICO
# ============================================================

print(G_qq)






# Distribucion de los residuos del modelo

library(ggplot2)
library(broom)
library(dplyr)

# ============================================================
# 1. OBTENER LOS RESIDUOS DEL MODELO
# ============================================================

res_df <- augment(Modelo_final)

# Media y desviación estándar de los residuos
media_res <- mean(res_df$.resid, na.rm = TRUE)
sd_res <- sd(res_df$.resid, na.rm = TRUE)


# ============================================================
# 2. DISTRIBUCIÓN DE LOS RESIDUOS
# ============================================================

G_residuos <- ggplot(
  res_df,
  aes(x = .resid)
) +
  
  # Histograma en escala de densidad
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "#4E79A7",
    color = "white",
    alpha = 0.80
  ) +
  
  # Densidad observada de los residuos
  geom_density(
    color = "#E27D60",
    linewidth = 1.3,
    adjust = 1
  ) +
  
  # Distribución normal teórica
  stat_function(
    fun = dnorm,
    args = list(
      mean = media_res,
      sd = sd_res
    ),
    color = "#59A14F",
    linewidth = 1.3,
    linetype = "dashed"
  ) +
  
  # Línea vertical en residuo = 0
  geom_vline(
    xintercept = 0,
    color = "#1F3A5F",
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  
  # ==========================================================
# TÍTULOS
# ==========================================================

labs(
  title = "DISTRIBUCIÓN DE LOS RESIDUOS DEL MODELO",
  subtitle = paste0(
    "Validación del supuesto de normalidad de los errores\n",
    "Línea coral: densidad observada | ",
    "Línea verde: distribución normal teórica"
  ),
  x = "Residuos",
  y = "Densidad",
  caption = "Fuente: Elaboración propia con base en Our World in Data, 2022"
) +
  
  # ==========================================================
# DISEÑO
# ==========================================================

theme_minimal() +
  
  theme(
    text = element_text(
      family = "sans"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#1F3A5F",
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    
    plot.caption = element_text(
      size = 10,
      color = "gray50",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    panel.grid.major = element_line(
      color = "gray90"
    ),
    
    panel.grid.minor = element_line(
      color = "gray95"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


# ============================================================
# 3. MOSTRAR GRÁFICO
# ============================================================

print(G_residuos)




# 5.1.3 Homocedasticidad

library(ggplot2)
library(broom)
library(dplyr)

# ============================================================
# 1. OBTENER INFORMACIÓN DEL MODELO
# ============================================================

res_df <- broom::augment(Modelo_final)

# ============================================================
# 2. CALCULAR VARIABLE SCALE-LOCATION
# ============================================================

res_df <- res_df %>%
  mutate(
    sqrt_resid_std = sqrt(abs(.std.resid))
  )

# Media de la raíz de los residuos estandarizados
media_scale <- mean(
  res_df$sqrt_resid_std,
  na.rm = TRUE
)

# ============================================================
# 3. GRÁFICO SCALE-LOCATION
# ============================================================

G_scale <- ggplot(
  res_df,
  aes(
    x = .fitted,
    y = sqrt_resid_std
  )
) +
  
  # Puntos
  geom_point(
    aes(
      color = sqrt_resid_std,
      size = sqrt_resid_std
    ),
    alpha = 0.65,
    show.legend = FALSE
  ) +
  
  # Línea horizontal de referencia
  geom_hline(
    yintercept = media_scale,
    color = "#E27D60",
    linetype = "dashed",
    linewidth = 1.2,
    alpha = 0.8
  ) +
  
  # Tendencia LOESS
  geom_smooth(
    method = "loess",
    color = "#1F3A5F",
    fill = "#4E79A7",
    alpha = 0.20,
    linewidth = 1.2,
    se = TRUE
  ) +
  
  # Colores de los puntos
  scale_color_gradient(
    low = "#4E79A7",
    high = "#E27D60"
  ) +
  
  # Tamaño de puntos
  scale_size_continuous(
    range = c(2, 6)
  ) +
  
  # ==========================================================
# TÍTULOS
# ==========================================================

labs(
  title = "GRÁFICO SCALE-LOCATION - VERIFICACIÓN DE HOMOCEDASTICIDAD",
  subtitle = "Raíz de los residuos estandarizados vs valores ajustados",
  x = "Valores ajustados de generación (TWh)",
  y = expression(sqrt("|Residuos estandarizados|")),
  caption = paste(
    "Fuente: Elaboración propia con base en Our World in Data, 2022 |",
    "Línea coral: media |",
    "Línea azul: tendencia LOESS"
  )
) +
  
  # ==========================================================
# DISEÑO
# ==========================================================

theme_minimal() +
  
  theme(
    text = element_text(
      family = "sans"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5,
      color = "#1F3A5F",
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 12,
      hjust = 0.5,
      color = "gray40",
      margin = margin(b = 20)
    ),
    
    plot.caption = element_text(
      size = 10,
      color = "gray50",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10
    ),
    
    panel.grid.major = element_line(
      color = "gray90"
    ),
    
    panel.grid.minor = element_line(
      color = "gray95"
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )

# ============================================================
# 4. MOSTRAR GRÁFICO
# ============================================================

print(G_scale)




# 5.1.4 VIF

# ============================================================
# MULTICOLINEALIDAD - VIF / GVIF
# ============================================================

library(car)
library(dplyr)

# Asegurar que Continente sea categórica
Base_datos$Continente <- factor(Base_datos$Continente)

# ============================================================
# 1. MODELO FINAL
# ============================================================

Modelo_final <- lm(
  Generacion ~ Poblacion + PIB + Consumo + Acceso + Continente,
  data = Base_datos
)

# ============================================================
# 2. CALCULAR VIF / GVIF
# ============================================================

vif_resultados <- car::vif(Modelo_final)

# Ver resultado original
print(vif_resultados)


# ============================================================
# 3. PREPARAR RESULTADOS
# ============================================================

if (is.matrix(vif_resultados)) {
  
  # Cuando existen variables con varios grados de libertad,
  # como Continente, car::vif() devuelve GVIF.
  
  vif_df <- data.frame(
    Variable = rownames(vif_resultados),
    GVIF = vif_resultados[, "GVIF"],
    Df = vif_resultados[, "Df"],
    GVIF_ajustado = vif_resultados[, "GVIF^(1/(2*Df))"],
    row.names = NULL
  ) %>%
    mutate(
      GVIF = round(GVIF, 3),
      GVIF_ajustado = round(GVIF_ajustado, 3),
      
      Diagnostico = case_when(
        GVIF_ajustado < sqrt(5) ~ "ACEPTABLE",
        GVIF_ajustado < sqrt(10) ~ "REVISAR",
        TRUE ~ "ALTO"
      )
    ) %>%
    arrange(desc(GVIF_ajustado))
  
} else {
  
  # Por si R devuelve VIF convencional
  
  vif_df <- data.frame(
    Variable = names(vif_resultados),
    VIF = as.numeric(vif_resultados),
    row.names = NULL
  ) %>%
    mutate(
      VIF = round(VIF, 3),
      Tolerancia = round(1 / VIF, 4),
      
      Diagnostico = case_when(
        VIF < 5 ~ "ACEPTABLE",
        VIF < 10 ~ "REVISAR",
        TRUE ~ "ALTO"
      )
    ) %>%
    arrange(desc(VIF))
}


# ============================================================
# 4. MOSTRAR TABLA EN R
# ============================================================

print(vif_df)
