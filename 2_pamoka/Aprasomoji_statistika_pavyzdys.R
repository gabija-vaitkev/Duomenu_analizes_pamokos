# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/X/Documents/.../Jusu_aplankas").

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()

# Dokumento įsikėlimas: File -> Import Dataset -> From Text (base)

# Pervadiname duomenis (jei duomenis įsikėlėme "User friendly" būdu)
data <- Reakcijos_laiko_sintetiniai_duomenys 

##################################################
# Kodu grįstas duomenų įkėlimo būdas, ############
# kuris yra naudingas ateityje norint išsaugoti, # 
# pakartoti ar automatizuoti analizes; ###########
# Nurodoma dokumento direktorija: ################
##################################################
dat <- read.csv('C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/2_pamoka/Egzaminu_pazymiai_sintetiniai_duomenys.csv', 
                 header=TRUE, sep = ",", stringsAsFactors=T)

# Duomenų santrauka:
head(dat)

# Vidurkio skaičiavimas:
mean(dat$Taskai)

# Pogrupių (Subsets) sukūrimas:
gr_A <- subset(dat, dat$Grupe == "A")
gr_B <- subset(dat, dat$Grupe == "B")

# Pogrupių santrauka:
head(gr_A)
head(gr_B)

# Vidurkių skaičiavimas pogrupiuose:
mean(gr_A$Taskai)
mean(gr_B$Taskai)

# Medianų skaičiavimas pogrupiuose:
median(gr_A$Taskai)
median(gr_B$Taskai)

# Minimumo skaičiavimas pogrupiuose:
min(gr_A$Taskai)
min(gr_B$Taskai)

# Maksimumo skaičiavimas pogrupiuose:
max(gr_A$Taskai)
max(gr_B$Taskai)

# Standartinio nuokrypio skaičiavimas pogrupiuose
# Pagal nutylėjimą funkcija skaičiuoja imties, ne populiacijos SD
# Jeigu norime populiacijos SD, nurodome "pop = TRUE"
sd(gr_A$Taskai)
sd(gr_B$Taskai)

# Dispersijos skaičiavimas pogrupiuose
# Pagal nutylėjimą funkcija skaičiuoja imties, ne populiacijos var:
var(gr_A$Taskai)
var(gr_B$Taskai)

# Paprastos histogramos:
hist(gr_A$Taskai)
hist(gr_B$Taskai)

# Informatyvesnės histogramos:
# abline prideda vertikalią liniją
# lines prideda tikimybės tankio (density) kreivę
# col nustato spalvą
# lwd nustato linijos storį
hist(gr_A$Taskai, probability = TRUE)
abline(v = mean(gr_A$Taskai), col='red', lwd = 3)
lines(density(gr_A$Taskai), col = 'green', lwd = 3)

hist(gr_B$Taskai, probability = TRUE)
abline(v = mean(gr_B$Taskai), col='red', lwd = 3)
lines(density(gr_B$Taskai), col = 'green', lwd = 3)

#############################################
# Naudosime built-in "iris" duomenų rinkinį #
#############################################
# Peržiūra
head(iris)

# Aprašomoji statistika
summary(iris$Sepal.Length)
mean(iris$Sepal.Length)

# Histograma
hist(iris$Sepal.Length,
     main = "Vilkdalgių žiedlapio ilgio pasiskirstymas",
     xlab = "Žiedlapio ilgis",
     ylab = "Dažnis",
     col = "lightblue", border = "black")

# Kokią dar aprašomąją statistiką galite apskaičiuoti? #
# Ar reikėtų duomenis išskirti į pogrupius (subset)? ###
# Gal galima patobulinti histogramą ? ##################
