# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/X/Documents/.../Jusu_aplankas")

# Vartotojui draugiškas darbinio aplanko nustatymas:
# Session -> Set Working Directory -> Choose Directory...

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()


#######################################
# Paprastų koreliacijų pavyzdys #######
# Užkrauname built-in duomenų rinkinį #
#######################################
data(iris)
str(iris)

# Sukuriame pogrupius
iris_setosa <- subset(iris, Species == "setosa")
iris_versicolor <- subset(iris, Species == "versicolor")
iris_virginica <- subset(iris, Species == "virginica")

# Patikriname duomenų normalumą pogrupiuose
shapiro.test(iris_setosa$Petal.Length)
shapiro.test(iris_versicolor$Petal.Length)
shapiro.test(iris_virginica$Petal.Length)

shapiro.test(iris_setosa$Petal.Width)
shapiro.test(iris_versicolor$Petal.Width)
shapiro.test(iris_virginica$Petal.Width)

shapiro.test(iris_setosa$Sepal.Length)
shapiro.test(iris_versicolor$Sepal.Length)
shapiro.test(iris_virginica$Sepal.Length)

shapiro.test(iris_setosa$Sepal.Width)
shapiro.test(iris_versicolor$Sepal.Width)
shapiro.test(iris_virginica$Sepal.Width)

# Atliekame koreliacijos testus tarp žiedlapių ilgio ir pločio
# Galite pridėti ir savo testus - taip palyginti ir kitus kintamuosius tarpusavyje (pvz., taurėlapių ilgį ir plotį)
cor_setosa <- cor.test(
  iris_setosa$Petal.Length,
  iris_setosa$Petal.Width,
  method = "spearman"
)
cor_setosa

cor_versicolor <- cor.test(
  iris_versicolor$Petal.Length,
  iris_versicolor$Petal.Width,
  method = "spearman"
)
cor_versicolor

cor_virginica <- cor.test(
  iris_virginica$Petal.Length,
  iris_virginica$Petal.Width,
  method = "pearson"
)
cor_virginica

# Brėžiame sklaidos diagramas atskirai kiekvienai veislei
# Perspėjimas - tam, kad diagramų kodas veiktų,
# Prieš tai turi būti praleistas anksčiau esantis koreliacijų kodas
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(ggplot2)
library(dplyr)

# setosa
label_setosa <- paste0(
  "r = ", round(cor_setosa$estimate, 2),
  "\np = ", signif(cor_setosa$p.value, 3)
)

ggplot(iris_setosa, aes(Petal.Length, Petal.Width)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text",
    x = min(iris_setosa$Petal.Length),
    y = max(iris_setosa$Petal.Width),
    label = label_setosa,
    hjust = 0, vjust = 1
  ) +
  labs(
    title = "Iris Setosa",
    x = "Petal Length",
    y = "Petal Width"
  ) +
  theme_bw(base_size = 14)

# versicolor
label_versicolor <- paste0(
  "r = ", round(cor_versicolor$estimate, 2),
  "\np = ", signif(cor_versicolor$p.value, 3)
)

ggplot(iris_versicolor, aes(Petal.Length, Petal.Width)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text",
    x = min(iris_versicolor$Petal.Length),
    y = max(iris_versicolor$Petal.Width),
    label = label_versicolor,
    hjust = 0, vjust = 1
  ) +
  labs(
    title = "Iris Versicolor",
    x = "Petal Length",
    y = "Petal Width"
  ) +
  theme_bw(base_size = 14)

# virginica
label_virginica <- paste0(
  "r = ", round(cor_virginica$estimate, 2),
  "\np = ", signif(cor_virginica$p.value, 3)
)

ggplot(iris_virginica, aes(Petal.Length, Petal.Width)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate(
    "text",
    x = min(iris_virginica$Petal.Length),
    y = max(iris_virginica$Petal.Width),
    label = label_virginica,
    hjust = 0, vjust = 1
  ) +
  labs(
    title = "Iris Virginica",
    x = "Petal Length",
    y = "Petal Width"
  ) +
  theme_bw(base_size = 14)

# Brėžiame bendrą sklaidos diagramą visoms veislėms
# Perspėjimas - tam, kad diagramų kodas veiktų,
# Prieš tai turi būti praleistas anksčiau esantis koreliacijų kodas
library(ggplot2)
library(dplyr)

# Paruošiami aprašai iš turimų testų rezultatų
cor_labels <- data.frame(
  Species = c("setosa", "versicolor", "virginica"),
  label = c(
    paste0(
      "Spearman\nr = ", round(cor_setosa$estimate, 2),
      "\np = ", signif(cor_setosa$p.value, 3)
    ),
    paste0(
      "Spearman\nr = ", round(cor_versicolor$estimate, 2),
      "\np = ", signif(cor_versicolor$p.value, 3)
    ),
    paste0(
      "Pearson\nr = ", round(cor_virginica$estimate, 2),
      "\np = ", signif(cor_virginica$p.value, 3)
    )
  ),
  y = c(2.4, 1.8, 1.2)  # nurodomos teksto pozicijos
)

# Parenkamos spalvos
species_colors <- c(
  "setosa" = "darkred",
  "versicolor" = "darkgreen",
  "virginica" = "darkblue"
)

# Brėžiame patį grafiką
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = species_colors) +
  geom_text(
    data = cor_labels,
    aes(
      x = min(iris$Petal.Length),
      y = y,
      label = label,
      color = Species
    ),
    hjust = 0,
    show.legend = FALSE
  ) +
  labs(
    x = "Petal Length",
    y = "Petal Width",
    color = "Species"
  ) +
  theme_bw(base_size = 14)

#######################################
# Koreliacijų matricos pavyzdys #######
# Užkrauname built-in duomenų rinkinį #
#######################################
data(mtcars)
str(mtcars)

# Patikriname duomenų normalumą
shapiro.test(mtcars$mpg)
shapiro.test(mtcars$cyl)
shapiro.test(mtcars$disp)
shapiro.test(mtcars$hp)
shapiro.test(mtcars$drat)
shapiro.test(mtcars$wt)
shapiro.test(mtcars$qsec)
shapiro.test(mtcars$vs)
shapiro.test(mtcars$am)
shapiro.test(mtcars$gear)
shapiro.test(mtcars$carb)

# Atliekame statistinius testus ir sukuriam koreliacjų matricą
cor_matrix <- cor(
  mtcars,
  method = "spearman"
)

cor_matrix_rounded <- round(cor_matrix, 3) # < - rezultatai suapvalinami iki 3 skaičių po kablelio

# Naudojame write.csv funkciją tam, kad išsisaugotume rezultatus:
# (prisiminkite, kur jūsų darbinė direktorija - nusistatykite ją)
getwd()

write.csv(cor_matrix_rounded, "correlation_matrix_rho.csv", row.names = TRUE)

# p reikšmių koreliacijų matrica
cor_pvalues <- function(mat) {
  n <- ncol(mat)
  p_mat <- matrix(NA, n, n)
  colnames(p_mat) <- colnames(mat)
  rownames(p_mat) <- colnames(mat)
  
  for (i in 1:n) {
    for (j in 1:n) {
      test <- cor.test(mat[, i], mat[, j], method = "spearman")
      p_mat[i, j] <- test$p.value
    }
  }
  p_mat
}

p_matrix <- cor_pvalues(mtcars)

p_matrix_rounded <- round(p_matrix, 3)

# Naudojame write.csv funkciją tam, kad išsisaugotume rezultatus:
# (prisiminkite, kur jūsų darbinė direktorija - nusistatykite ją)
getwd()

write.csv(p_matrix_rounded, "correlation_matrix_p.csv", row.names = TRUE)

# Koreliacijų matricos  vizualizacija
# 1. pilna koreliacijų matrica - rodomi visi rezultatai
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 1
)

# 2. Pakoreguota koreliacijų matrica - rodomi tik
# statistiškai reikšmingi rezultatai
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 1,
  p.mat = p_matrix,
  sig.level = 0.05,
  insig = "blank"
)

# 3. Pakoreguota koreliacijų matrica - išmetame
# įstrižainę, kur kintamieji lyginami patys su savimi
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 1,
  p.mat = p_matrix,
  sig.level = 0.05,
  insig = "blank",
  diag = FALSE
)
