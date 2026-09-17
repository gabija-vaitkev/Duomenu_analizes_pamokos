# Matematiniai ir loginiai uždaviniai programos pratestavimui
2 + 2
4 ^ 4
2 < 3
4 != 5

# Dokumento įsikėlimas: File -> Import Dataset -> From Text (base)

# Pervadiname duomenis
data <- Reakcijos_laiko_sintetiniai_duomenys 

# Sudėtingesnis duomenų įkėlimo būdas,
# kuris yra naudingas ateityje norint išsaugoti, 
# pakartoti ar automatizuoti analizes;
# Nurodoma dokumento direktorija:
data <- read.csv('C:/Users/User/Desktop/Duomenu analize/1_pamoka/Reakcijos_laiko_sintetiniai_duomenys.csv', 
                 header=TRUE, sep = ",", stringsAsFactors=T)

# Duomenų santrauka:
head(data)

# Vidurkio skaičiavimas:
mean(data$Atstumas_cm)

# Pogrupių (Subsets) sukūrimas:
d <- subset(data, data$Ranka == "Dominuojanti")
n <- subset(data, data$Ranka == "Nedominuojanti")

# Pogrupių santrauka:
head(d)
head(n)

# Vidurkių skaičiavimas pogrupiuose:
mean(d$Atstumas_cm)
mean(n$Atstumas_cm)
