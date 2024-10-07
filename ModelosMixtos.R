# Cargar las librerias
library(completejourney)
library(dplyr)
library(lubridate)
library(lme4)
library(car)
library(MASS)
library(lmtest)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(performance)
library(ggplot2)
library(corrplot)
library(GGally)
library(tibble)
library(MuMIn)
library(arm)
library(caret)
library(lattice)
library(lmerTest)
library(pbapply)
library(car)
library(xtable)
library(robustlmm)
library(lattice)
library(lmtest)
library(sandwich)


# Carga de datos, en este caso demographics y transactions

data("demographics")
#data("products")
#data("transactions_sample")
transactions <- get_transactions()




# Exploración -------------------------------------------------------------


head(demographics)
str(demographics)

head(transactions)
str(transactions)


# % NA's
sapply(demographics, function(x) {
  sum(is.na(x)) / length(x) * 100
})

sapply(transactions, function(x) {
  sum(is.na(x)) / length(x) * 100
})


# Graficar las ventas totales mensuales
ggplot(ventas_mensuales_filtrado, aes(x = mes, y = ventas_totales)) +
  geom_line(color = "blue") +           # Línea para las ventas mensuales
  geom_point(color = "darkblue") +     # Puntos en cada mes
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(title = "Ventas Mensuales Totales",       # Título del gráfico
       x = "Mes",                        # Etiqueta del eje X
       y = "Ventas Totales") +           # Etiqueta del eje Y
  theme_minimal()+                 # Tema del gráfico
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar etiquetas del eje X

# Asegurarte de que la columna 'mes' está en formato Date
ventas_mensuales_filtrado$mes <- as.Date(ventas_mensuales_filtrado$mes)

# Graficar las ventas totales mensuales, con el eje X bien configurado mes a mes
ggplot(ventas_mensuales_filtrado, aes(x = mes, y = ventas_totales)) +
  geom_line(color = "blue") +           # Línea para las ventas mensuales
  geom_point(color = "darkblue") +      # Puntos en cada mes
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +  # Etiquetas mes a mes
  labs(title = "Ventas Mensuales",       # Título del gráfico
       x = "Mes",                        # Etiqueta del eje X
       y = "Ventas Totales") +           # Etiqueta del eje Y
  theme_minimal() +                      # Tema minimalista
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotar etiquetas del eje X


# Graficar la distribución del tamaño de hogar con un histograma
ggplot(demographics, aes(x = household_size)) +
  geom_histogram(binwidth = 1, fill = "green4", color = "black", boundary = 0) +  # Histograma con color verde
  labs(title = "Distribución del Tamaño de Hogar",       # Título
       x = "Tamaño de Hogar",                           # Etiqueta eje X
       y = "Frecuencia") +                              # Etiqueta eje Y
  theme_minimal()                                       # Tema minimalista


# Graficar la distribución del tamaño de hogar con un histograma
ggplot(demographics, aes(x = household_size)) +
  geom_histogram(binwidth = 1, fill = "green4", color = "black", boundary = 0) +  # Histograma con color verde
  labs(title = "Distribución del Tamaño de Hogar",       # Título
       x = "Tamaño de Hogar",                           # Etiqueta eje X
       y = "Frecuencia") +                              # Etiqueta eje Y
  theme_minimal()                                       # Tema minimalista


# Graficar la distribución del tamaño de hogar con un gráfico de barras
ggplot(demographics, aes(x = factor(household_size))) +
  geom_bar(fill = "red3", color = "black") +  # Gráfico de barras con color verde
  labs(title = "Distribución del Tamaño de Hogar",       # Título
       x = "Tamaño de Hogar",                           # Etiqueta eje X
       y = "Número de Hogares") +                       # Etiqueta eje Y
  theme_minimal()                                       # Tema minimalista

}


# Lectura y transformación ------------------------------------------------

dataTransactions <- transactions

# Convertir month y household_id a factores si es necesario
dataTransactions$mes <- lubridate::month(dataTransactions$transaction_timestamp)
dataTransactions$mes <- as.factor(dataTransactions$mes)
dataTransactions$household_id <- as.factor(dataTransactions$household_id)


# Filtrar para los household_id muestreados y agrupar por mes
ventasMensualesHogar <- dataTransactions %>%
  group_by(household_id, mes) %>%
  summarise(total_sales_value = sum(sales_value, na.rm = TRUE)) %>%
  arrange(household_id, mes)


muestraHogares <- dataTransactions %>%
  distinct(household_id) %>%
  sample_n(5) %>%
  pull(household_id)




# Introduccion por el modelo poolings -------------------------------------

set.seed(1234)  # Para reproducibilidad
# Seleccionar 20 hogares aleatorios
muestra_households <- sample(unique(ventasMensualesHogar$household_id), 20)

ventas_muestra_hogar <- ventasMensualesHogar %>%
  filter(household_id %in% muestra_households)


ventas_muestra_hogar1 <- ventas_muestra_hogar

ventas_muestra_hogar1$mes <- as.numeric(as.character(ventas_muestra_hogar1$mes))

# Gráfico con línea de regresión
ggplot(ventas_muestra_hogar1) +
  aes(x = mes, y = total_sales_value) +
  stat_smooth(method = "lm", se = FALSE) +  # Ajustar línea de regresión
  geom_point() +                            # Poner los puntos encima de las líneas
  facet_wrap(~ household_id) +              # Facetear por household_id
  labs(x = "Mes", y = "Valor Total de Ventas", title = "Análisis de Ventas mensuales por Hogar (no pooling)") +  # Añadir etiquetas de los ejes
  scale_x_continuous(breaks = 0:6 * 2) +
  theme_minimal()

#No pooling
df_no_pooling_1 <- lmList(total_sales_value ~ mes | household_id, ventas_muestra_hogar1) %>%
  coef() %>%
  # Hogares
  rownames_to_column("household_id") %>%
  rename(Intercept = `(Intercept)`, Slope_Months = mes) %>%
  add_column(Model = "No pooling")

#Complete pooling
df_complete_pooling_1 <- lm(total_sales_value ~ mes, ventas_muestra_hogar1)
# Mismo intecerpto y pendiente para todos
df_pooled_1 <- tibble(
  Model = "Complete pooling",
  household_id = unique(ventas_muestra_hogar1$household_id),
  Intercept = coef(df_complete_pooling_1)[1],
  Slope_Months = coef(df_complete_pooling_1)[2]
)

#Join para mostrar

df_models_2 <- bind_rows(df_pooled_1, df_no_pooling_1)%>% 
  left_join(ventas_muestra_hogar1, by = "household_id")


p_model_comparison <- ggplot(df_models_2) + 
  aes(x = mes, y = total_sales_value) + 
  # Set the color mapping in this layer so the points don't get a color
  geom_abline(
    aes(intercept = Intercept, slope = Slope_Months, color = Model),
    size = .75
  ) + 
  geom_point() +
  facet_wrap("household_id") +
  labs(x = xlab, y = ylab) + 
  scale_x_continuous(breaks = 0:6 * 2) + 
  # Fix the color palette 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "top", legend.justification = "left")

p_model_comparison

##### Partial pooling

mod_mixto_muestra1_pendientes <- lmer(total_sales_value ~ 1 + mes + (1 + mes | household_id), ventas_muestra_hogar1)
arm::display(mod_mixto_muestra1_pendientes)



df_partial_pooling_1 <- coef(mod_mixto_muestra1_pendientes)[["household_id"]] %>% 
  rownames_to_column("household_id") %>% 
  as_tibble() %>% 
  rename(Intercept = `(Intercept)`, Slope_Months = mes) %>% 
  add_column(Model = "Partial pooling")

head(df_partial_pooling_1)

df_models_3 <- bind_rows(df_pooled_1, df_no_pooling_1, df_partial_pooling_1) %>% 
  left_join(ventas_muestra_hogar1, by = "household_id")

p_model_comparison %+% df_models_3



####Graficos de tendencia
# Graficar los datos longitudinales con diferentes colores
ggplot(ventas_muestra_hogar1, aes(x = mes, y = total_sales_value, color = factor(household_id))) +
  geom_line() +                  # Línea para los datos longitudinales
  geom_point() +                 # Puntos para los datos
  labs(title = "Ventas Totales Mensuales por Hogar",
       x = "Mes",
       y = "Valor Total de Ventas",
       color = "Id. Hogares") +  # Leyenda para household_id
       scale_x_continuous(breaks = 0:6 * 2) +
  theme_minimal() 




# Dataset Ventas por Mes en los Hogares -----------------------------------

# Graficar los datos longitudinales por mes
ggplot(ventasMensualesHogar, aes(x = mes, y = total_sales_value, color = factor(household_id), group = household_id)) +
  geom_line() +                  # Línea para los datos longitudinales
  geom_point() +                 # Puntos en cada mes
  labs(title = "Ventas Totales por Mes para Diferentes Hogares",
       x = "Mes",
       y = "Valor Total de Ventas",
       color = "Household ID") +  # Leyenda para household_id
    # Mostrar los nombres de los meses
  theme_minimal()

# Modelo OLS

modelo_ventas_hogar_lm <- lm(total_sales_value ~ household_id + mes, data=ventasMensualesHogar)

summary(modelo_ventas_hogar_lm)
broom::glance(modelo_ventas_hogar_lm)
broom::tidy(modelo_ventas_hogar_lm)


# OLS no vale la pena


# Validacion cruzada modelo simple ----------------------------------------


set.seed(123) 
train_indices <- createDataPartition(ventasMensualesHogar$total_sales_value, p = 0.8, list = FALSE)

# Crear los conjuntos de entrenamiento y validación
train_data <- ventasMensualesHogar[train_indices, ]
test_data <- ventasMensualesHogar[-train_indices, ]

# Modelo mixto hogares aleatorios
modelo_mixto_hogar_aleat <- lmer(total_sales_value ~ mes + (1 | household_id), data = train_data)

predicciones_train <- predict(modelo_mixto_hogar_aleat, newdata = train_data)

#  Función para calcular RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calcular RMSE en los datos de entrenamiento
rmse_train <- calc_rmse(train_data$total_sales_value, predicciones_train)
cat("RMSE en los datos de entrenamiento: ", rmse_train, "\n")

#Test
predicciones_test <- predict(modelo_mixto_hogar_aleat, newdata = test_data, allow.new.levels = TRUE)


# Calcular RMSE en los datos de validación
rmse_test <- calc_rmse(test_data$total_sales_value, predicciones_test)
cat("RMSE en los datos de validación: ", rmse_test, "\n")

broom.mixed::glance(modelo_mixto_hogar_aleat)

#Grafico residuales
plot(modelo_mixto_hogar_aleat_mes, main = "Residuos vs. Valores Ajustados")


# QQ-Plot de residuos
qqnorm(resid(modelo_mixto_hogar_aleat_mes))
qqline(resid(modelo_mixto_hogar_aleat_mes))

# Histograma de residuos
hist(resid(modelo_mixto_hogar_aleat_mes), main = "Histograma de Residuos", xlab = "Residuos")
#Cola pesada

# Residuos vs. mes
plot(train_data$mes, resid(modelo_mixto_hogar_aleat_mes), main = "Residuos vs. Mes", xlab = "Mes", ylab = "Residuos")
#Muchos atipicos

# ACF de residuos
acf(resid(mm_log_var_hog_aleat), main = "Función de Autocorrelación de Residuos")


#lo utilizamos para más que todo lo de multiple efectos fijos

# ANOVA del modelo
anova(modelo_mixto_hogar_aleat_mes)



###### No vale la pena 


broom.mixed::glance(modelo_mixto_hogar_aleat)
broom.mixed::tidy(modelo_mixto_hogar_aleat)



#Grafico residuales
plot(modelo_mixto_hogar_aleat, main = "Residuos vs. Valores Ajustados")


# QQ-Plot de residuos
qqnorm(resid(modelo_mixto_hogar_aleat))
qqline(resid(modelo_mixto_hogar_aleat))

# Histograma de residuos
hist(resid(modelo_mixto_hogar_aleat), main = "Histograma de Residuos", xlab = "Residuos")
#Cola pesada

# Residuos vs. mes
plot(ventasMensualesHogar$mes, resid(modelo_mixto_hogar_aleat), main = "Residuos vs. Mes", xlab = "Mes", ylab = "Residuos")
#Muchos atipicos

# ACF de residuos
acf(resid(modelo_mixto_hogar_aleat), main = "Función de Autocorrelación de Residuos")


# Efectos aleatorios

dotplot(ranef(modelo_mixto_hogar_aleat, condVar = TRUE), main = "Efectos Aleatorios por Hogar")

# ANOVA del modelo
anova(modelo_mixto_hogar_aleat)

# El factor mes es significativo: El valor p extremadamente bajo (< 2.2e-16) 
# indica que los meses tienen un efecto estadísticamente significativo en las ventas totales.
# Esto significa que hay diferencias notables en las ventas mensuales que no pueden 
# explicarse por el azar.
# 
# Importancia del factor mes: Dado que el valor F es considerablemente alto, 
# esto sugiere que los meses explican una parte importante de la variabilidad en las 
# ventas mensuales, y probablemente algunos meses muestran ventas significativamente mayores 
# o menores que otros.

pairwise.t.test(ventasMensualesHogar$total_sales_value, ventasMensualesHogar$mes)

# lambda  0.2222222
r2 <- r.squaredGLMM(modelo_mixto_hogar_aleat)
print(r2)

# > r2 <- r.squaredGLMM(modelo_mixto_hogar_aleat)
# > print(r2)
# R2m       R2c
# [1,] 0.00139133 0.7005852

# Conclusiones:
#   R²m bajo: El hecho de que el R²m sea tan bajo indica que el factor mes por sí solo no 
# # explica una parte significativa de la varianza en las ventas totales. Aunque mes 
# es significativo (según el ANOVA anterior), su contribución en términos de la varianza 
# explicada es pequeña.
# 
# R²c alto: Un R²c del 70% indica que la mayor parte de la varianza en las ventas mensuales 
# está explicada por el modelo completo, y en particular, por la variabilidad entre los hogares. 
# Esto sugiere que las diferencias en las ventas entre hogares individuales son muy importantes 
# y que el uso de un efecto aleatorio para los hogares ha sido crucial para capturar estas diferencias.
# 

ggplot(ventasMensualesHogar, aes(x = mes, y = total_sales_value)) +
   # Línea para las ventas totales
  geom_point() + # Puntos en cada mes
  labs(title = "Ventas Totales por Mes", x = "Mes", y = "Valor Total de Ventas") +
  theme_minimal()  # Mostrar los meses de 1 a 12

demographics1 <- demographics

# Crear el gráfico de distribución de ingresos


# Crear un gráfico de barras para la distribución de ingresos
ggplot(demographics, aes(x = income)) +
  geom_bar(fill = "#1B4D3E", color = "black", alpha = 0.85) +  # Usando un verde más agradable
  labs(title = "Distribución de Ingresos por Categorías", x = "Ingreso (Categorías)", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x si son largas


#Modelo log

# Modelo log con 1 sola variable fija -------------------------------------


set.seed(123) 
train_indices2 <- createDataPartition(ventasMensualesHogarLog$log_sales_value, p = 0.8, list = FALSE)

# Crear los conjuntos de entrenamiento y validación
train_data2 <- ventasMensualesHogarLog[train_indices2, ]
test_data2 <- ventasMensualesHogarLog[-train_indices2, ]



# Modelo mixto hogares aleatorios, mes, edad, ingreso, tamaño
mm_log_sola_var_hog_aleat <- lmer(log_sales_value ~ mes + (1 | household_id), data = train_data2)

predicciones_train_log_sola <- predict(mm_log_sola_var_hog_aleat, newdata = train_data2)

# Calcular RMSE en los datos de entrenamiento
rmse_train_log_var_sola <- calc_rmse(train_data2$log_sales_value, predicciones_train_log_sola)
cat("RMSE en los datos de entrenamiento: ", rmse_train_log_var_sola, "\n")

#Test
predicciones_test_log_var_sola <- predict(mm_log_sola_var_hog_aleat, newdata = test_data2, allow.new.levels = TRUE)


# Calcular RMSE en los datos de validación
rmse_test <- calc_rmse(test_data1$log_sales_value, predicciones_test_log_var)
cat("RMSE en los datos de validación: ", rmse_test, "\n")

summary(mm_log_sola_var_hog_aleat)

r2_log_total_sola <- r.squaredGLMM(mm_log_sola_var_hog_aleat)
print(r2_log_total_sola)

broom::glance(mm_log_sola_var_hog_aleat)

#####

ventasMensualesHogarLog <- ventasMensualesHogar
# ventasMensualesHogarLog$log_sales_value <- log(ventasMensualesHogar$total_sales_value + 1)  # +1 para evitar log(0)
# modelo_transformado_log <- lmer(log_sales_value ~ mes + (1 | household_id), data = ventasMensualesHogarLog)

# plot(modelo_transformado_log, main = "Res vs. Val Ajustados Transf Log")

# Extraer los efectos aleatorios del modelo mixto
# ranef_model <- ranef(modelo_transformado_log)


# QQ-Plot de residuos
# qqnorm(resid(modelo_transformado_log), main = "QQ-Plot de Residuos Modelo (Log)")
# qqline(resid(modelo_transformado_log), col = "red")
# Agregar un título al gráfico

# QQ-plot para los residuos
# qqnorm(resid(modelo_transformado_log))
# qqline(resid(modelo_transformado_log), col = "red")

# Graficar Residuos vs Valores Ajustados para verificar homocedasticidad
# plot(fitted(modelo_mixto_hogar_aleat), resid(modelo_mixto_hogar_aleat),
#      main = "Residuos vs Valores Ajustados", 
#      xlab = "Valores Ajustados", ylab = "Residuos")
# abline(h = 0, col = "red")



# Histograma de residuos
hist(resid(modelo_transformado_log), main = "Histograma de Residuos Trans. Log", xlab = "Residuos")
#Cola pesada

# Residuos vs. mes
plot(ventasMensualesHogarLog$mes, resid(modelo_transformado_log), main = "Residuos vs. Mes", xlab = "Mes", ylab = "Residuos")
#Muchos atipicos

# ACF de residuos
acf(resid(modelo_transformado_log), main = "Función de Autocorrelación de Residuos Log")


# Efectos aleatorios

dotplot(ranef(modelo_transformado_log, condVar = TRUE), main = "Efectos Aleatorios por Hogar")

#grafico que no tiene interpretacion

# VIF para multicolinealidad


# print(vif_values)

#lo utilizamos para más que todo lo de multiple efectos fijos

# ANOVA del modelo
anova(modelo_transformado_log)




###Ahora revisamos con la información demográfica

ventasMensualesHogarDemog <- ventasMensualesHogar %>%
  right_join(demographics, by = "household_id")

###JOIN Log
ventasMensualesHogarLogDemo <- ventasMensualesHogarLog

ventasMensualesHogarLogDemo <- ventasMensualesHogarLogDemo %>%
  right_join(demographics, by = "household_id")

View(ventasMensualesHogarDemog)

sapply(ventasMensualesHogarDemog, function(x) {
  sum(is.na(x)) / length(x) * 100
})



# Validacion cruzada con log todas las variables --------------------------

vif_values <- vif(lm(total_sales_value ~ mes + income + mes + household_size, data = ventasMensualesHogarLogDemo))
vif_values

set.seed(123) 
train_indices1 <- createDataPartition(ventasMensualesHogarLogDemo$log_sales_value, p = 0.8, list = FALSE)

# Crear los conjuntos de entrenamiento y validación
train_data1 <- ventasMensualesHogarLogDemo[train_indices1, ]
test_data1 <- ventasMensualesHogarLogDemo[-train_indices1, ]

# Modelo mixto hogares aleatorios, mes, edad, ingreso, tamaño
mm_log_var_hog_aleat <- lmer(log_sales_value ~ income + mes + household_size + (1 | household_id), data = train_data1)

predicciones_train_log_total <- predict(mm_log_var_hog_aleat, newdata = train_data1)

# Calcular RMSE en los datos de entrenamiento
rmse_train_log_var <- calc_rmse(train_data1$log_sales_value, predicciones_train_log_total)
cat("RMSE en los datos de entrenamiento: ", rmse_train_log_var, "\n")

#Test
predicciones_test_log_var <- predict(mm_log_var_hog_aleat, newdata = test_data1, allow.new.levels = TRUE)

summary(mm_log_var_hog_aleat)

# Calcular RMSE en los datos de validación
rmse_test <- calc_rmse(test_data1$log_sales_value, predicciones_test_log_var)
cat("RMSE en los datos de validación: ", rmse_test, "\n")

r2_log_total <- r.squaredGLMM(mm_log_var_hog_aleat)
print(r2_log_total)


broom.mixed::glance(mm_log_var_hog_aleat)



# validacion cruzada sin log de todas las variables -----------------------

set.seed(123) 
train_indices4 <- createDataPartition(ventasMensualesHogarDemog$total_sales_value, p = 0.8, list = FALSE)

# Crear los conjuntos de entrenamiento y validación
train_data4 <- ventasMensualesHogarDemog[train_indices4, ]
test_data4 <- ventasMensualesHogarDemog[-train_indices4, ]

# Modelo mixto hogares aleatorios
modelo_mixto_hogar_aleat_todas <- lmer(total_sales_value ~ mes + income + household_size + (1 | household_id), 
                                       data = train_data4)

predicciones_train4 <- predict(modelo_mixto_hogar_aleat_todas, newdata = train_data4)


# Calcular RMSE en los datos de entrenamiento
rmse_train4 <- calc_rmse(train_data4$total_sales_value, predicciones_train4)
cat("RMSE en los datos de entrenamiento: ", rmse_train4, "\n")

#Test
predicciones_test4 <- predict(modelo_mixto_hogar_aleat_todas, newdata = test_data4, 
                             allow.new.levels = TRUE)


# Calcular RMSE en los datos de validación
rmse_test4 <- calc_rmse(test_data4$total_sales_value, predicciones_test4)
cat("RMSE en los datos de validación: ", rmse_test4, "\n")

broom.mixed::glance(modelo_mixto_hogar_aleat_todas)



# Comparacion modelos -----------------------------------------------------

# Extraer resúmenes de los modelos
modelo1_glance <- broom.mixed::glance(modelo_mixto_hogar_aleat)
modelo2_glance <- broom.mixed::glance(mm_log_sola_var_hog_aleat)
modelo3_glance <- broom.mixed::glance(mm_log_var_hog_aleat)
modelo4_glance <- broom.mixed::glance(modelo_mixto_hogar_aleat_todas)

# Agregar nombre del modelo a cada resumen
modelo1_glance <- modelo1_glance %>% add_column(Model = "Modelo 1 - Ventas por mes ~ Mes Fijo / Hogares Aleatorios")
modelo2_glance <- modelo2_glance %>% add_column(Model = "Modelo 2 - log(Ventas por mes) ~ Mes Fijo / Hogares Aleatorios")
modelo3_glance <- modelo3_glance %>% add_column(Model = "Modelo 3 - log(Ventas por mes) ~ Mes, Ingreso, Tamaño Hogar Fijos / Hogares Aleatorios ")
modelo4_glance <- modelo4_glance %>% add_column(Model = "Modelo 4 - Ventas por mes ~ Mes, Ingreso, Tamaño Hogar Fijos / Hogares Aleatorios")

# Combinar todos los resúmenes en una sola tabla
modelos_comparacion <- bind_rows(modelo1_glance, modelo2_glance, modelo3_glance, modelo4_glance)

# Mostrar la tabla
print(modelos_comparacion)

print(xtable(modelos_comparacion, type = "latex"), file = "tabla1.tex")

# Validacion  -------------------------------------------------------------


# Marginales --------------------------------------------------------------


predicciones_fijos <- predict(mm_log_var_hog_aleat, re.form = NA) #NA como fijos nada mas

residuales_marginales <- train_data1$log_sales_value - predicciones_fijos

plot(residuales_marginales)

ggplot(data = train_data1, aes(x = predicciones_fijos, y = residuales_marginales)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuales Marginales vs Valores Ajustados (Efectos Fijos)",
       x = "Valores Ajustados (Efectos Fijos)",
       y = "Residuales Marginales") +
  theme_minimal()


# Condicionales -----------------------------------------------------------


predicciones_completas <- predict(mm_log_var_hog_aleat)

residuales_condicionales <- train_data1$log_sales_value - predicciones_completas

plot(residuales_condicionales)

ggplot(data = train_data1, aes(x = predicciones_completas, y = residuales_condicionales)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuales Condicionales vs Valores Ajustados (Efectos Fijos y Aleatorios)",
       x = "Valores Ajustados (Fijos + Aleatorios)",
       y = "Residuales Condicionales") +
  theme_minimal()




# blup nuevo --------------------------------------------------------------


# Extraer los efectos aleatorios con sus errores estándar
efectos_aleatorios_blup <- as.data.frame(ranef(mm_log_var_hog_aleat, condVar = TRUE)$household_id)

# Extraer las varianzas condicionales (errores estándar)
errores_estandar <- attr(ranef(mm_log_var_hog_aleat, condVar = TRUE)$household_id, "postVar")

# Calcular los intervalos de confianza (95%)
efectos_aleatorios_blup$se <- sqrt(errores_estandar[1, , ])  # Error estándar
efectos_aleatorios_blup$ci_lower <- efectos_aleatorios_blup$`(Intercept)` - 1.96 * efectos_aleatorios_blup$se
efectos_aleatorios_blup$ci_upper <- efectos_aleatorios_blup$`(Intercept)` + 1.96 * efectos_aleatorios_blup$se

# Añadir la variable household_id
efectos_aleatorios_blup$household_id <- rownames(efectos_aleatorios_blup)

efectos_aleatorios_blup$tipo <- "BLUP"  # Puntos (BLUP)
efectos_aleatorios_blup$tipo_intervalo <- "Intervalos de confianza" 

# Graficar con intervalos de confianza

# Graficar con intervalos de confianza y puntos
ggplot(efectos_aleatorios_blup, aes(x = `(Intercept)`, y = reorder(household_id, `(Intercept)`))) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = "Intervalos de confianza"), height = 0.2) +  # Intervalos de confianza
  geom_point(aes(color = "BLUP"), size = 2) +  # Puntos de los efectos aleatorios (BLUP)
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Línea de referencia en x = 0
  labs(title = "Efectos Aleatorios por Hogar (BLUP)",
       x = "Intercepto (Efectos Aleatorios)",
       y = NULL) +  # Quitar etiqueta del eje Y
  theme_minimal() +
  theme(axis.text.y = element_blank()) +  # Quitar las etiquetas del eje Y
  scale_x_continuous(labels = scales::number_format()) +  # Mostrar unidades en el eje X
  scale_color_manual(name = "Leyenda", values = c("BLUP" = "black", "Intervalos de confianza" = "darkgray")) +  # Añadir leyenda
  theme(legend.position = "bottom")  # Posicionar la leyenda en la parte inferior


# Heterocedasticidad ------------------------------------------------------



white_test <- bptest(mm_log_var_hog_aleat, ~ fitted(mm_log_var_hog_aleat) + I(fitted(mm_log_var_hog_aleat)^2))
print(white_test)

# Extraer los residuos condicionales y valores ajustados del modelo mixto
residuos_cond <- residuals(mm_log_var_hog_aleat)
valores_ajustados <- fitted(mm_log_var_hog_aleat)

# Crear un dataframe con los residuos y valores ajustados
data_test6 <- data.frame(residuos_cond, valores_ajustados)

# Aplicar la prueba de White
white_test <- bptest(residuos_cond ~ valores_ajustados + I(valores_ajustados^2), data = data_test6)
print(white_test)

#robusto
# Extraer los residuos condicionales y valores ajustados del modelo mixto
residuos_cond_rob <- residuals(mm_log_var_hog_aleat_robust)
valores_ajustados_rob <- fitted(mm_log_var_hog_aleat_robust)

# Crear un dataframe con los residuos y valores ajustados
data_test7 <- data.frame(residuos_cond_rob, valores_ajustados_rob)

# Aplicar la prueba de White
white_test_rob <- bptest(residuos_cond_rob ~ valores_ajustados_rob + I(valores_ajustados_rob^2), data = data_test6)
print(white_test_rob)



# Graficar con puntos más visibles y una leyenda para los intervalos de confianza
ggplot(efectos_aleatorios_blup, aes(x = `(Intercept)`, y = reorder(household_id, `(Intercept)`))) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, color = "darkgray") +  # Intervalos de confianza
  geom_point(color = "blue") +  # Aumentar el tamaño de los puntos azules
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Línea de referencia en x = 0
  labs(title = "Efectos Aleatorios por Hogar (BLUP)",
       x = "Intercepto (Efectos Aleatorios)",
       y = NULL) +  # Quitar etiqueta del eje Y
  theme_minimal() +
  theme(axis.text.y = element_blank()) +  # Quitar las etiquetas del eje Y
  scale_x_continuous(labels = scales::number_format()) +  # Mostrar unidades en el eje X
  guides(color = guide_legend("Elementos")) +  # Añadir la leyenda
  theme(legend.position = "bottom") +  # Posicionar la leyenda abajo
  annotate("text", x = -2.5, y = 1000, label = "Intervalos de confianza al 95%", color = "darkgray", size = 4, angle = 0, alpha=0.1) +  # Explicación de los intervalos
  annotate("text", x = -2.5, y = 500, label = "Efectos aleatorios (BLUP)", color = "blue", size = 4, angle = 0)  # Explicación de los puntos





# Robusto -----------------------------------------------------------------


# Modelo mixto robusto, similar a tu modelo original pero usando rlmer()
mm_log_var_hog_aleat_robust <- robustlmm::rlmer(log_sales_value ~ income + mes + household_size + (1 | household_id), 
                                     data = train_data1)

# Resumen del modelo robusto
summary(mm_log_var_hog_aleat_robust)

# Predicciones en el conjunto de entrenamiento
predicciones_train_log_robust <- predict(mm_log_var_hog_aleat_robust, newdata = train_data1)

# Calcular RMSE en los datos de entrenamiento para el modelo robusto
rmse_train_log_robust <- calc_rmse(train_data1$log_sales_value, predicciones_train_log_robust)
cat("RMSE en los datos de entrenamiento (robusto): ", rmse_train_log_robust, "\n")

# Predicciones en el conjunto de test
predicciones_test_log_robust <- predict(mm_log_var_hog_aleat_robust, newdata = test_data1, allow.new.levels = TRUE)

# Calcular RMSE en los datos de validación para el modelo robusto
rmse_test_log_robust <- calc_rmse(test_data1$log_sales_value, predicciones_test_log_robust)
cat("RMSE en los datos de validación (robusto): ", rmse_test_log_robust, "\n")



broom.mixed::glance(mm_log_var_hog_aleat_robust)

# Extraer logLik, AIC y BIC del modelo estándar
logLik_standard <- logLik(mm_log_var_hog_aleat)
AIC_standard <- AIC(mm_log_var_hog_aleat)
BIC_standard <- BIC(mm_log_var_hog_aleat)

# Resumen del modelo estándar
summary(mm_log_var_hog_aleat)


# Extraer logLik, AIC y BIC del modelo robusto
logLik_robust <- logLik(mm_log_var_hog_aleat_robust)
AIC_robust <- AIC(mm_log_var_hog_aleat_robust)
BIC_robust <- BIC(mm_log_var_hog_aleat_robust)

# Resumen del modelo robusto
summary(mm_log_var_hog_aleat_robust)

#Condicionales 

# Cálculo de los residuales condicionales
predicciones_completas_rob <- predict(mm_log_var_hog_aleat_robust)
residuales_condicionales_rob <- train_data1$log_sales_value - predicciones_completas_rob

# Gráfico de residuales condicionales
ggplot(data = train_data1, aes(x = predicciones_completas_rob, y = residuales_condicionales_rob)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuales Condicionales vs Valores Ajustados (Efectos Fijos y Aleatorios) Modelo Robusto",
       x = "Valores Ajustados (Fijos + Aleatorios)",
       y = "Residuales Condicionales") +
  theme_minimal()



# Cálculo de los residuales marginales
predicciones_fijos_rob <- predict(mm_log_var_hog_aleat_robust, re.form = NA)
residuales_marginales_rob <- train_data1$log_sales_value - predicciones_fijos_rob

# Gráfico de residuales marginales
ggplot(data = train_data1, aes(x = predicciones_fijos_rob, y = residuales_marginales_rob)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuales Marginales vs Valores Ajustados (Efectos Fijos) Modelo Robusto",
       x = "Valores Ajustados (Efectos Fijos)",
       y = "Residuales Marginales") +
  theme_minimal()


ggplot(datos_grafico, aes(x = Predicciones_Completas, y = Residuales_BLUP)) +
  geom_point(color = "black", alpha = 0.8) +  # Puntos con un poco de transparencia
  labs(title = "Residuales BLUP vs. Predicciones Completas",
       x = "Predicciones Completas",
       y = "Residuales BLUP") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal()



# Exploratorio de nuevo ---------------------------------------------------





# Calcular los residuales marginales
residuales_marginales <- ventasMensualesHogar$total_sales_value - predicciones_fijos



# Modelo sin transformación logarítmica
modelo_sin_log <- lmer(sales_mensuales ~ age + income + household_size + mes + (1 | household_id), data = train_data)

# Modelo con transformación logarítmica
train_data$log_sales_mensuales <- log(train_data$sales_mensuales + 1)
modelo_con_log <- lmer(log_sales_mensuales ~ age + income + household_size + mes + (1 | household_id), data = train_data)




# Convertir la columna transaction_timestamp a fecha, si aún no lo está
transactionsV <- transactions

transactionsV$transaction_timestamp <- as.Date(transactionsV$transaction_timestamp)

# Agrupar las ventas totales por mes
ventas_mensuales <- transactionsV %>%
  mutate(mes = floor_date(transaction_timestamp, "month")) %>%  # Extraer mes
  group_by(mes) %>%
  summarise(ventas_totales = sum(sales_value, na.rm = TRUE))  # Sumar ventas por mes

ventas_mensuales_filtrado <- ventas_mensuales %>%
  filter(mes < as.Date("2018-01-01"))

