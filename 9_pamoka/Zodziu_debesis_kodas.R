# Jeigu reikia, sudiegiame paketus
install.packages("wordcloud")
install.packages("tm")
install.packages("RColorBrewer")

# Užkrauname paketus
library(wordcloud)
library(tm)
library(RColorBrewer)

# Užkrauname duomenis (tekstinį failą)
# Čia turite nurodyti dokumento kelią savo kompiuteryje!
responses <- readLines('C:/Users/gabij/Documents/Biomatika/Pamokos_fortui/9_pamoka/moksleiviu_atsakymai.txt', encoding = "UTF-8", warn = FALSE)

# Sukuriame atsakymų "Corpus"
corpus <- VCorpus(VectorSource(responses))

# Teksto valymas
# Didžiasias raides paverčiame mažosiomis
corpus <- tm_map(corpus, content_transformer(tolower))

# Išimame skyrybos ženklus
corpus <- tm_map(corpus, removePunctuation)

# Išimame skaitmenis
corpus <- tm_map(corpus, removeNumbers)

# Surašome "stop words" (nereikalingus žodžius, kuriuos norėsime išmesti)
# Čia galima papildyti arba išmesti mums rūpinčius žodžius
custom_stopwords <- c(
  "man", "labiausiai", "patinka", "kai",
  "apie", "nes", "labai", "yra",
  "kaip", "galime", "tai",
  "pamokos", "mokytis", "įdomu",
  "įdomios", "įdomiausios"
)

# Panaikiname "stop words"
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Dokumento - termino matrica
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
word_freq <- sort(rowSums(matrix), decreasing = TRUE)

# Pasižiūrime dažniausius žodžius
head(word_freq, 10)

# Sukuriame žodžių debesį
set.seed(123)

wordcloud(
  names(word_freq),
  word_freq,
  min.freq = 3,
  random.order = FALSE,
  scale = c(4,0.8),
  colors = brewer.pal(8, "Dark2")
)