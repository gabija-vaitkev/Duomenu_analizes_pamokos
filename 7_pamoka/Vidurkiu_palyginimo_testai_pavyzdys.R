# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/X/Documents/.../Jusu_aplankas").

# Vartotojui draugiškas darbinio aplanko nustatymas:
# Session -> Set Working Directory -> Choose Directory...

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()

#######################################
# Neporinio T-testo pavyzdys ##########
# Užkrauname built-in duomenų rinkinį #
#######################################
?PlantGrowth
View(PlantGrowth)
head(PlantGrowth)

# Išsirenkame tik dvi grupes - kontrolę ir trt1
subset2 <- subset(PlantGrowth, group %in% c("ctrl", "trt2"))

# Atsiskiriame grupes, nes jose
# Tikrinsime duomenų normalumą
ctrl <- subset(subset2, subset2$group == "ctrl")
trt2 <- subset(subset2, subset2$group == "trt2")

# Pasitikriname duomenų normalumą
shapiro.test(ctrl$weight)
shapiro.test(trt2$weight)

# Vizualizuojame duomenis
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

ggplot(subset2, aes(x = group, y = weight, fill = group)) +
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.9) +
  labs(
    title = "Augalų svorio palyginimas tarp grupių",
    x = "Grupė",
    y = "Svoris"
  ) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("ctrl" = "#4E79A7", "trt2" = "#F28E2B")) +
  theme(legend.position = "none")

# Pasitikriname dispersijų homogeniškumą
if (!require("car")) install.packages("car")
library(car)
leveneTest(weight ~ group, data = subset2)

# T-testas taikomas tada kai duomenys pasiskirstę normaliai
# ir dispersijos homogeniškos
t.test(weight ~ group, data = subset2, var.equal = TRUE)

# Efekto dydžio matavimas
if (!require("effsize")) install.packages("effsize")
library(effsize)

# Cohen's d neporiniam palyginimui
cohen_d_ind <- cohen.d(
  ctrl$weight,
  trt2$weight,
  pooled = TRUE
)

cohen_d_ind

# Kai duomenys pasiskirstę normaliai,
# bet dispersijos nehomogeniškos
# taikomas T-testas su Welch korekcija
t.test(weight ~ group, data = subset2, var.equal = FALSE)

# Wilcoxon (Mann-Whitney U) testas taikomas
# kai duomenys nėra pasiskirstę normaliai
wilcox_res <- wilcox.test(weight ~ group, data = subset2)
wilcox_res

# r efektų dydis
z <- qnorm(wilcox_res$p.value / 2)
n <- nrow(subset2)

r_effect <- abs(z) / sqrt(n)
r_effect

####################
# Porinis t-testas #
####################
?sleep
View(sleep)
head(sleep)

# Vizualizuojame duomenis
ggplot(sleep, aes(x = group, y = extra, fill = group)) +
  geom_boxplot(alpha = 0.6, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.9) +
  labs(
    title = "Increase in sleep by drug",
    x = "Group",
    y = "Extra hours of sleep"
  ) +
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c("1" = "#7bc043", "2" = "#0392cf")) +
  theme(legend.position = "none")

# Išrikiuojame duomenis pagal ID
sleep_sorted <- sleep[order(sleep$ID, sleep$group), ]

# Atskiriame kiekvieno žmogaus matavimus
extra_1 <- sleep_sorted$extra[sleep_sorted$group == "1"]
extra_2 <- sleep_sorted$extra[sleep_sorted$group == "2"]

# Skirtumai (2 grupė - 1 grupė)
differences <- extra_2 - extra_1

differences

# Patikriname skirtumų duomenų normalumą vizualiai
hist(
  differences,
  main = "Skirtumų pasiskirstymas",
  xlab = "Skirtumas (grupė 2 – grupė 1)",
  col = "lightblue"
)

# qqplot
qqnorm(differences)
qqline(differences)

# Patikriname normalumą su statistiniu testu
shapiro.test(differences)

# Jei skirtumų duomenys normalūs
# Paruošiame duomenis analizei
sleep2 <- reshape(sleep, direction = "wide",
                  idvar = "ID", timevar = "group")

# Atliekame porinį t-testą
t.test(Pair(extra.1, extra.2) ~ 1, data = sleep2)

# Cohen's d poriniam t-testui (rankiniu būdu)
cohen_d_paired <- mean(differences) / sd(differences)
cat("Cohen's d (paired) =", round(cohen_d_paired, 3))

# Jei skirtumų duomenys nenormalūs
# Atliekame Wilcoxon testą
wilcox_paired <- wilcox.test(Pair(extra.1, extra.2) ~ 1, data = sleep2)
wilcox_paired

# r efekto dydis poriniam Wilcoxon testui
z <- qnorm(wilcox_paired$p.value / 2)
n <- length(differences)

r_effect_paired <- abs(z) / sqrt(n)
r_effect_paired

#####################
# ANOVA pavyzdys ####
# su post hoc testu #
#####################

# Naudosime chickwts duomenų rinkinį
head(chickwts)
View(chickwts)

# Susikuriam pogrupius tam, kad galėtume 
# patikrinti duomenų normalumą
horsebean<-subset(chickwts,chickwts$feed == "horsebean")
linseed<-subset(chickwts,chickwts$feed == "linseed")
soybean<-subset(chickwts,chickwts$feed == "soybean")
sunflower<-subset(chickwts,chickwts$feed == "sunflower")
meatmeal<-subset(chickwts,chickwts$feed == "meatmeal")
casein<-subset(chickwts,chickwts$feed == "casein")

# Paprastas ggplot2 
library(ggplot2)

p1 <- ggplot(chickwts, aes(x = feed, y = weight, fill = feed)) +
  geom_boxplot(alpha = 0.9, color = "grey30", linewidth = 0.5) +
  scale_fill_manual(values = c("#5C4B99", "#9AA0FF","#EABFFF", 
                               "#7ED3FF", "#3EA6FF", "#286DA8")) +
  labs(title = "Chicken Weight by Feed Type",
       x = "Feed Type",
       y = "Weight (g)") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(limits = c(100,450), breaks = seq(100,450,50))

p1

# Pasitikriname duomenų normalumą
shapiro.test(horsebean$weight)
shapiro.test(linseed$weight)
shapiro.test(soybean$weight)
shapiro.test(sunflower$weight)
shapiro.test(meatmeal$weight)
shapiro.test(casein$weight)

# Levene'o testas
if (!require("car")) install.packages("car")
library(car)

leveneTest(weight ~ feed, data = chickwts)

# Atliekame ANOVA
anova_result <- aov(weight ~ feed, data = chickwts)
summary(anova_result)

# Atliekame Tukey's HSD testą
TukeyHSD(anova_result)

# Eta squared
if (!require("effectsize")) install.packages("effectsize")
library(effectsize)

eta_squared(anova_result)

# ANOVA su Welch korekcija pavyzdys:
welch_anova <- oneway.test(
  weight ~ feed,
  data = chickwts,
  var.equal = FALSE
)

welch_anova

# Kruskal-Wallis testo pavyzdys:
kruskal_result <- kruskal.test(weight ~ feed, data = chickwts)
kruskal_result

# Dunn's post-hoc testo pavyzdys:
if (!require("FSA")) install.packages("FSA")
library(FSA)

dunn_result <- dunnTest(
  weight ~ feed,
  data = chickwts,
  method = "bonferroni"
)

dunn_result

#########################
# Dviejų veiksnių ANOVA #
#########################

# Užkrauname duomenis
View(ToothGrowth)
?ToothGrowth

my_data <- ToothGrowth
my_data

# Pasižiūrime duomenų struktūrą
str(my_data)

# Konverduojame dozę į faktorių ir rekoduojame lygius
# į "D0.5", "D1", "D2"
my_data$dose <- factor(my_data$dose, 
                       levels = c(0.5, 1, 2),
                       labels = c("D0.5", "D1", "D2"))
head(my_data)

# Patikriname duomenų normalumą
library(dplyr)

normality_tests <- my_data %>%
  group_by(supp, dose) %>%
  summarise(
    n = n(),
    shapiro_p = shapiro.test(len)$p.value
  )

normality_tests

# Dėžučių diagrama
library(ggplot2)

ggplot(my_data, aes(x = dose, y = len, fill = supp)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "dose", y = "len", fill = "supp") +
  theme_minimal(base_size = 14)

# Linijinė diagrama
library(ggplot2)

ggplot(my_data, aes(x = dose, y = len, group = supp, color = supp)) +
  # raw data (dotplot equivalent)
  geom_jitter(width = 0.1, alpha = 0.6) +
  # mean line
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  # mean points
  stat_summary(fun = mean, geom = "point", size = 3) +
  # mean ± SE
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(x = "dose", y = "len", color = "supp") +
  theme_minimal(base_size = 14)

# Dviejų veiksnių anova - * naudojame kai norime patikrinti
# ar yra sąveika tarp 2 nepriklausomų kintamųjų
res.aov3 <- aov(len ~ supp * dose, data = my_data)
summary(res.aov3)
