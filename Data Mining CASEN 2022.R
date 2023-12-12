install.packages("haven")
library(haven)
data_casen <- read_sav("C:/Users/DELL/Downloads/Base_de_datos_Casen_2022_SPSS.sav")
class(data_casen)
head(data_casen)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#PREGUNTA 1
#Porcentaje datos nulos
preguntas_interes <- c("ecivil","h7c", "e1", "e5b", "o18", "o28c", "y25ep", "y28_1g1", "s2","s13_fonasa", "r1cp", "r11", "v10", "v15")
data_casen %>%
  select(all_of(preguntas_interes)) %>%
  summarise_all(.funs = ~ mean(is.na(.))) %>%
  gather(key = "Variable", value = "Porcentaje_NA") %>%
  mutate(Porcentaje_NA = Porcentaje_NA * 100)

#PREGUNTA 2
#Porcentaje datos nulos variable Ingreso
head(data_casen$y1,100)
pregunta_interes <- c("y1")
data_casen %>%
  select(all_of(pregunta_interes)) %>%
  summarise_all(.funs = ~ mean(is.na(.))) %>%
  gather(key = "Ingreso", value = "Porcentaje_NA") %>%
  mutate(Porcentaje_NA = Porcentaje_NA * 100)


#Histograma variable ingreso
ggplot(data_casen, aes(x=df.y1)) + 
  geom_histogram(binwidth=500, fill="blue", color="black") +
  labs(title="Histograma de Ingresos", x="Ingreso", y="Frecuencia") +
  theme(axis.text=element_text(size=12))

#Porcentaje outliers Ingreso
data_casen[data_casen == -88] <- NaN
data_casen <- data_casen %>% filter(!is.na(data_casen$y1))
df.y1 <- as.numeric(data_casen$y1)
z_scores <- scale(df.y1, center = TRUE, scale = TRUE)
outliers <- data_casen[abs(z_scores) > 23, ]
outliers <- data_casen[abs(z_scores) > 6.7, ]
porcentaje_outliers_y1 <- length(outliers$y1)/length(data_casen$y1)*100
summary(outliers$y1)


#Grafico outliers
library(ggplot2)

data_casen$intervalo_ingreso <- ifelse(data_casen$y1 <= 5e6, "No outliers",
                                       ifelse(data_casen$y1 > 5e6 & data_casen$y1 <= 15e6, "Outliers Colectivos",
                                              "Outlier Global"))


ggplot(data_casen, aes(x = 1:length(df.y1), y = df.y1, color = intervalo_ingreso)) +
  geom_point() +
  scale_color_manual(values = c("No outliers" = "blue", "Outliers Colectivos" = "green", "Outlier Global" = "red")) +
  labs(title = "Gráfico de dispersión de Ingresos", x = "Observación", y = "Ingreso", color = "Outliers Ingreso") +
  theme_minimal()


#PREGUNTA 3
class(data_casen$y1)
levels(data_casen$y1)
head(data_casen$y1,100)

y <- as.data.frame(data_casen$y1)
y[y == -88] <- NaN
y.df <- as.data.frame(na.omit(y))
y.num <- as.numeric(unlist((y.df)))

install.packages("ggplot2")
library(ggplot2)

ggplot(y.df, aes(x = y.num)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución de densidad de la variable Ingreso",
       x = "Ingresos en CPL",
       y = "Densidad")

#transformación simetrica
y.num.tra <- sqrt(y.num)
ggplot(y.df, aes(x = y.num.tra)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Distribución de densidad de la variable Ingreso aplicada la raiz cuadrada",
       x = "Ingresos en CPL",
       y = "Densidad")

#PREGUNTA 4
library(mice)
library(dplyr)

variables_interes <- c("y1", "sexo", "region", "pco1_a", "e1", "e6a", "e8", "v12")
data_casen <- mutate_at(data_casen, vars(variables_interes), ~as.numeric(zap_labels(.)))

datacasen <- data_casen %>%
  select(edad, sexo, region, pco1_a, tot_per_h, e1,e6a,e8,v12, y1)
md.pattern(datacasen)
mice_imputed <- data.frame(
  original = data_casen$y1,
  imputed_pmm = complete(mice(datacasen, method = "pmm"))$y1,
  imputed_cart = complete(mice(datacasen, method = "cart"))$y1,
  imputed_lasso = complete(mice(datacasen, method = "lasso.norm"))$y1
)
View(mice_imputed)
library(ggplot2)
install.packages("cowplot")
library(cowplot)
h.orig <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h.pmm <- ggplot(mice_imputed, aes(x = imputed_pmm)) + 
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("Pmm-imputed distribution") +
  theme_classic()
h.cart <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()
h.lasso <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()
plot_grid(h.orig, h.pmm, h.cart, h.lasso, nrow = 2, ncol = 2)



#PREGUNTA 5
#Filtros de entropía
install.packages("FSelector")
library(FSelector)
weights <- information.gain(y1 ~ edad + sexo + region + pco1_a + tot_per_h + e1 + e6a + e8 + v12, data_casen) #Ganacia de información (Entropía)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "Ingreso")
print(f)

weights <- information.gain(y1 ~ edad + sexo + region + pco1_a + tot_per_h + e1 + e6a + e8 + v12, data_casen, unit="log2") #Ganacia de información (Entropía)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "Ingreso")
print(f)

weights <- gain.ratio(y1 ~ edad + sexo + region + pco1_a + tot_per_h + e1 + e6a + e8 + v12, data_casen) #Corrección de la ganancia de información por
print(weights)
subset <- cutoff.k(weights, 3)
f <- as.simple.formula(subset, "Ingreso")
print(f)

#Wrapper

library(dplyr)
# Seleccionar las columnas
df_filtrado <- select(data_casen, y1, edad, sexo, region,pco1_a, tot_per_h, e1, e6a, e8, v12)

install.packages("rpart")
library(rpart)
set.seed(1)
X <- 1:20
evaluator <- function(subset) {
  k <- 10
  splits <- runif(nrow(df_filtrado))
  results = sapply(X, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- df_filtrado[test.idx, , drop=FALSE]
    train <- df_filtrado[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "y1"), train)
    error.rate = sum(test$y1 != predict(tree, test, type="c"))/nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- forward.search(names(df_filtrado)[-1], evaluator)
f <- as.simple.formula(subset, "y1")
print(f)
  
subset <- backward.search(names(df_filtrado)[-1], evaluator)
f <- as.simple.formula(subset, "y1")
print(f)


