# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/X/Documents/.../Jusu_aplankas").

# Vartotojui draugiškas darbinio aplanko nustatymas:
# Session -> Set Working Directory -> Choose Directory...

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()


#####################
# Pirmasis pavyzdys #
#####################

# Naudosime "HairEyeColor" built-in duomenų rinkinį
# Apie jį galite pasiskaityti paleidę šį kodą:
?HairEyeColor

# Užkrauname HairEyeColor duomenų rinkinį
data(HairEyeColor)

# Pasižiūrime duomenis
HairEyeColor
str(HairEyeColor)

# Yra dvi lentelės su dviems lytims su akių ir plaukų spalvos duomenimis
# Sujungiame abiejų lyčių duomenis į kryžminę lentelę
# (norėsime patikrinti tik ryšį tarp akių ir plaukų spalvos)
# Plaukų spalva yra 1-as kintamasis, akių spalva - 2-as.
hair_eye <- margin.table(HairEyeColor, margin = c(1, 2))

# Pasižiūrime kryžminę lentelę
hair_eye

# Suskaičiuojame proporcijas
prop.table(hair_eye)

# Suskaičiuojame proporcijas pagal plaukų spalvą (eilutės)
# Sužinome, kokia akių spalva dažniausia kiekvienai plaukų spalvai
prop.table(hair_eye, margin = 1)

# Greita duomenų vizualizacija
barplot(
  hair_eye,
  beside = TRUE,
  col = c("#09090B", "#894B0A", "#FB2C36", "#FEF9C2"),
  legend = TRUE,
  main = "Distribution of hair colour by eye colour",
  xlab = "Eye colour",
  ylab = "Counts"
)
# Ar pasiskirstymas atrodo vienodas?

# Atliekame Chi-kvadrato testą
chi_test <- chisq.test(hair_eye)
chi_test
# Ką reiškia p vertė?

# Pasižiūrime tikėtinus dažnius
chi_test$expected

# ...ir stebėtus dažnius
chi_test$observed

# Patikriname ar rezultatai patikimi
# Tikėtini dažniai turi būti >=5
chi_test$expected >= 5

# Skaičiuojame Kramerio V koeficientą "rankiniu" būdu:
cramers_v_eye <- sqrt(chi_test$statistic / 
                        (sum(hair_eye) * (min(dim(hair_eye)) - 1)))

cramers_v_eye

# Panaudojame funkciją suskaičiuoti Cramer's V:
if (!require("lsr")) install.packages("lsr")
library(lsr)
cramersV(hair_eye)

# Ar rezultatai tarp "rankinio" apskaičiavimo
# Ir apskaičiavimo naudojant funkciją skiriasi?


#####################
# Antrasis pavyzdys #
#####################

# Naudojame tą patį HairEyeColor duomenų rinkinį,
# Tik patikrinsime sąsają tarp lyties ir plaukų spalvos

# Prisiminkime duomenų struktūrą
str(HairEyeColor)

# Plaukų spalva yra 1-as kintamasis, lytis - 3-ias.
hair_MF <- margin.table(HairEyeColor, margin = c(1, 3))
hair_MF

# Paskaičiuojame proporcijas
prop.table(hair_MF, margin = 1)

# Vizualizacija
barplot(
  hair_MF,
  beside = TRUE,
  col = c("#09090B", "#894B0A", "#FB2C36", "#FEF9C2"),
  legend.text = rownames(hair_MF),
  args.legend = list(x = "top"),
  main = "Distribution of hair colour by sex",
  xlab = "Sex",
  ylab = "Count"
)

# Chi-kvadrato testas
chi_MF <- chisq.test(hair_MF)
chi_MF

# Pasižiūrime tikėtinus dažnius
chi_MF$expected

# ...ir stebėtus dažnius
chi_MF$observed

# Patikriname ar rezultatai patikimi
# Tikėtini dažniai turi būti >=5
chi_MF$expected >= 5

# Kramerio V koeficientas
# Rankiniu būdu...
cramers_v_MF <- sqrt(chi_MF$statistic / 
                        (sum(hair_MF) * (min(dim(hair_MF)) - 1)))
cramers_v_MF

# ...Ir naudojant funkciją
cramersV(hair_MF)

# Ką reiškia testų rezultatai?

# Kaip duomenis eksportuoti iš R?
# 1. Tiesiog iškopijuojame rezultatus iš konsolės į Excel
hair_eye
chi_test
chi_test$expected

v_hair_eye <- cramersV(hair_eye)
v_hair_eye


hair_MF
chi_MF
chi_MF$expected

v_hair_MF <- cramersV(hair_MF)
v_hair_MF

# Ar gaunasi "patogiai" iškopijuoti? Galimai ne :(.

# 2. Naudojame write.csv funkciją:
# (prisiminkite, kur jūsų darbinė direktorija - nusistatykite ją)
getwd()

write.csv(hair_eye, "hair_eye_data.csv", row.names = FALSE)

# Prieš išsaugant Chi-kvadrato testo rezultatus,
# juos reikia perstruktūruoti į lentelę
chi_results <- data.frame(
  Test = chi_test$method,
  Chi_sq_result = chi_test$statistic,
  df = chi_test$parameter,
  p.value = chi_test$p.value
)
write.csv(chi_results, "hair_eye_chi_test.csv", row.names = FALSE)

write.csv(chi_test$expected, "hair_eye_expected.csv", row.names = TRUE)

write.csv(v_hair_eye, "hair_eye_cramers_v.csv", row.names = FALSE)


# 3. Naudojame "openxlsx" paketą
if (!require("openxlsx")) install.packages("openxlsx")
library(openxlsx)

# Perkeliame Chi testo rezultatus į letelę
chi_MF_results <- data.frame(
  Test = chi_MF$method,
  Chi_sq_result = as.numeric(chi_MF$statistic),
  df = as.numeric(chi_MF$parameter),
  p.value = chi_MF$p.value
)

# Sutvarkome stebėtų ir tikėtinų dažnių duomenis
observed_df <- cbind(
  Hair = rownames(chi_MF$observed),
  as.data.frame(chi_MF$observed)
)

expected_df <- cbind(
  Hair = rownames(chi_MF$expected),
  as.data.frame(chi_MF$expected)
)

# Sutvarkome Kramerio V duomenis
cramersV_df <- data.frame(
  Coefficient = "Cramer's V",
  Value = as.numeric(v_hair_MF)
)

# Viską įrašome į vieną xlsx dokumentą
write.xlsx(
  list(
    "Chi_sq_result" = chi_MF_results,
    "Obeserved" = observed_df,
    "Expected" = expected_df,
    "Cramers_V" = cramersV_df
  ),
  file = "hair_MF_chi_test.xlsx"
)

# Kuris būdas patogesnis ir praktiškesnis?


#################################################
# Kaip savo duomenis paversti kryžmine lentele? #
#################################################
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# Užkrauname mūsų duomenis (čia yra pavyzdys)
dat <- read.csv('C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/5_pamoka/pavyzdys_kryzminei_lentelei.csv', 
                   header=TRUE, sep = ",", stringsAsFactors=T)
head(dat)

# Pasirenkame mums rūpinčius stulpelius
dat2 <- dat %>% select(Lytis, Megstamiausias_uzkandis)

# Sukuriame kryžminę lentelę
c_table <- table(dat2$Lytis, dat2$Megstamiausias_uzkandis)

# Pasižiūrime kryžminę lentelę
print(c_table)

# Šią logiką galite pritaikyti ir savo surinktiems duomenims.
# "c_table" galite paduoti "chisq.test()" funkcijai
# ir taip atlikti chi-kvadrato testą
