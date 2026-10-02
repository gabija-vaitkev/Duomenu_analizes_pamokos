#########################################
# Pradedame nuo bazinio R vizualizacijų #
#########################################

# Histograma
hist(iris$Sepal.Length, main="Taurėlapių ilgio pasiskirstymas", xlab="Ilgis (cm)")

# Stulpelinė diagrama
barplot(table(iris$Species), main="Mėginių kiekis pagal augalo rūšį", ylab="Kiekis")

# Dėžučių grafikas
boxplot(weight ~ group, data=PlantGrowth,
        main="Augalų svoris pagal grupes", xlab="Grupė", ylab="Svoris (g)")

# Sklaidos diagrama
# Bazinis R
plot(iris$Sepal.Length, iris$Petal.Length,
     col = c("red", "green", "blue")[iris$Species],
     pch = 19,
     main = "Ryšys tarp žiedlapių ir taurėlapių ilgio",
     xlab = "Taurėlapio ilgis (cm)",
     ylab = "Žiedlapio ilgis (cm)")

# Pridedame legendą
legend("topleft",
       legend = levels(iris$Species),
       col = c("red", "green", "blue"),
       pch = 19)

#####################################
# Pereiname prie ggplot2 ############
# Jei naudojama pirmą kartą #########
# Reikia suinstaliuoti ##############
# install.packages("ggplot2") #######
# instaliuojama tik vieną kartą #####
# užkraunama kaskart pradėjus darbą #
# library(ggplot2) ##################
#####################################

library(ggplot2)

# Histograma su ggplot2
ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(
    binwidth = 0.3,         # control bar width
    fill = "#D3D3FF",       # custom color
    color = "black"
  ) +
  labs(
    title = "Taurėlapių ilgio pasiskirstymas",
    x = "Taurėlapio ilgis (cm)",
    y = "Dažnis"
  ) +
  theme_minimal()

# Stulpelinė diagrama su ggplot2
ggplot(iris, aes(x = Species, fill = Species)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("#D3D3FF", "#C9F2C7", "#FFD6C9")) +
  labs(
    title = "Mėginių kiekis pagal augalo rūšį",
    x = "Rūšis",
    y = "Kiekis"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Dėžučių grafikas su ggplot2
ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("#D3D3FF", "#C9F2C7", "#FFD6C9")) +
  labs(
    title = "Augalų svoris pagal grupes",
    x = "Grupė",
    y = "Svoris (g)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Sklaidos diagrama su ggplot2
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 3) +
  labs(
    title = "Ryšys tarp žiedlapių ir taurėlapių ilgio",
    x = "Taurėlapio ilgis (cm)",
    y = "Žiedlapio ilgis (cm)"
  ) +
  theme_minimal()

# Kaip išsaugoti grafikus kaip aukštos raiškos paveikslėlius?
# Nepamirštame nusistatyti darbinės direktorijos
# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/4_pamoka")

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()

# Naudojame ankstesnį diagramos kodą, tik suteikiame jam pavadinimą
p1 <- ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(
    binwidth = 0.3,         # control bar width
    fill = "#D3D3FF",       # custom color
    color = "black"
  ) +
  labs(
    title = "Taurėlapių ilgio pasiskirstymas",
    x = "Taurėlapio ilgis (cm)",
    y = "Dažnis"
  ) +
  theme_minimal()

p1

# Panaudojame kodą išsaugojimui
ggsave("histograma.png", p1, 
       width = 6, height = 5, dpi = 300, units = "in")
