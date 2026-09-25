# Kodinis darbinio aplanko nustatymas (reikia suvesti kelią į savo darbinį aplanką):
setwd("C:/Users/X/Documents/.../Jusu_aplankas").

# Norint patikrinti darbinę direktoriją, naudojame:
getwd()

# Dokumento įsikėlimas: File -> Import Dataset -> From Text (base)

# Pervadiname duomenis (jei duomenis įsikėlėme "User friendly" būdu)
zuvys1 <- fish_length1 

##################################################
# Kodu grįstas duomenų įkėlimo būdas, ############
# kuris yra naudingas ateityje norint išsaugoti, # 
# pakartoti ar automatizuoti analizes; ###########
# Nurodoma dokumento direktorija: ################
##################################################
zuvys1 <- read.csv('C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/3_pamoka/zuvu_ilgis_1.csv', 
                header=TRUE, sep = ",", stringsAsFactors=T)

zuvys2 <- read.csv('C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/3_pamoka/zuvu_ilgis_2.csv', 
                   header=TRUE, sep = ",", stringsAsFactors=T)

# Duomenų santrauka:
head(zuvys1)
head(zuvys2)

# Histogramos 
par(mfrow = c(1,2))

hist(zuvys1$ilgis_cm, main = "Zuvys1", col = "lightblue")
hist(zuvys2$ilgis_cm, main = "Zuvys2", col = "lightpink")

# Q-Q diagramos
par(mfrow = c(1,2))

qqnorm(zuvys1$ilgis_cm, main = "Zuvys1 Q-Q diagrama")
qqline(zuvys1$ilgis_cm)

qqnorm(zuvys2$ilgis_cm, main = "Zuvys2 Q-Q diagrama")
qqline(zuvys2$ilgis_cm)

# Shapiro-Wilk testas normalumui patikrinti
shapiro.test(zuvys1$ilgis_cm)
shapiro.test(zuvys2$ilgis_cm)
