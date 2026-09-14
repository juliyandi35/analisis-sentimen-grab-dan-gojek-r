# Load required libraries
library(tm)
library(caret)
library(glmnet)

# Gojek
# Baca dataset Gojek
library(readxl)
dataGojek <- read_excel('Data Review Gojek.xlsx')
dataGojek <- dataGojek[,-1] #hapus username
dataGojek <- dataGojek[,-2] #hapus suka
dataGojek$score <- dataGojek$skor
dataGojek$content <- dataGojek$review
# Create a corpus from the text reviews
corpusGojek <- Corpus(VectorSource(dataGojek$content))

# Perform text preprocessing
preprocessData <- function(text) {
  # Spelling normalization
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeNumbers)
  text <- tm_map(text, removePunctuation)
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, stripWhitespace)
  myStopwords = read.csv("Stopwords.csv")
  myStopwords <- na.omit(myStopwords)
  # remove stopwords from corpus
  text<- tm_map(text, removeWords, myStopwords$Stopwords)
  # Remove your own stop word
  # specify your stopwords as a character vector
  text <- tm_map(text,removeWords,c("jam","minggu","full","jaya","ambil","nambah","tokopedia","shopee","dateng","gabisa","kota","dikasih","kemana","yah","yaa","jga","kena","back","lazada","negatif","emang","bagusup","indo","hrs","urus","rumah","seminggu","batas","tulis","nulis","dapatkan","gede","dipakai","setia","semenjak","namanya","rating","lakukan","job","smpai","pdahal","merah","kta","bintangnya","tulisan","tanda","edit","butuh","mah"))
  # Eliminate extra white spaces
  text<- tm_map(text, stripWhitespace)
  # Remove URL
  removeURL<- function(x) gsub("http[[:alnum:]]*", " ", x)
  text<- tm_map(text, removeURL)
  #Replace words
  text<- tm_map(text, gsub, pattern="ribet", replacement="rumit")
  text<- tm_map(text, gsub, pattern="ngawur", replacement="asalasalan")
  text<- tm_map(text, gsub, pattern="ngaco", replacement="asalasalan")
  text<- tm_map(text, gsub, pattern="top", replacement="bagus")
  text<- tm_map(text, gsub, pattern="keren", replacement="bagus")
  text<- tm_map(text, gsub, pattern="belom", replacement="belum")
  text<- tm_map(text, gsub, pattern="system", replacement="sistem")
  text<- tm_map(text, gsub, pattern="histori",replacement="history")
  text<- tm_map(text, gsub, pattern="sgen", replacement="agen")
  text<- tm_map(text, gsub, pattern="handphone", replacement="hp")
  text<- tm_map(text, gsub, pattern="kadaluarsa", replacement="kadaluwarsa")
  text<- tm_map(text, gsub, pattern="tlp", replacement="telepon")
  text<- tm_map(text, gsub, pattern="telpon", replacement="telepon")
  text<- tm_map(text, gsub, pattern="lelet", replacement="lambat")
  text<- tm_map(text, gsub, pattern="lemot", replacement="lambat")
  text<- tm_map(text, gsub, pattern="tf", replacement="transfer")
  text<- tm_map(text, gsub, pattern="cepet", replacement="cepat")
  text<- tm_map(text, gsub, pattern="duit", replacement="uang")
  text<- tm_map(text, gsub, pattern="nunggu", replacement="menunggu")
  text<- tm_map(text, gsub, pattern="ilang", replacement="hilang")
  text<- tm_map(text, gsub, pattern="males", replacement="malas")
  text<- tm_map(text, gsub, pattern="notif",replacement="notifikasi")
  text<- tm_map(text, gsub, pattern="ngasih", replacement="memberi")
  text<- tm_map(text, gsub, pattern="brg", replacement="barang")
  text<- tm_map(text, gsub, pattern="brang", replacement="barang")
  text<- tm_map(text, gsub, pattern="hhilang", replacement="hilang")
  text<- tm_map(text, gsub, pattern="nyesel", replacement="menyesal")
  text<- tm_map(text, gsub, pattern="komplen", replacement="komplain")
  text<- tm_map(text, gsub, pattern="nomer", replacement="nomor")
  text<- tm_map(text, gsub, pattern="voucer", replacement="voucher")
  text<- tm_map(text, gsub, pattern="apps", replacement="aplikasi")
  text<- tm_map(text, gsub, pattern="credits", replacement="kredit")
  text<- tm_map(text, gsub, pattern="blum", replacement="belum")
  text<- tm_map(text, gsub, pattern="pocher", replacement="voucher")
  text<- tm_map(text, gsub, pattern="vouchernya", replacement="voucher")
  text<- tm_map(text, gsub, pattern="ongkirnya", replacement="ongkir")
  text<- tm_map(text, gsub, pattern="negonya", replacement="nego")
  text<- tm_map(text, gsub, pattern="cpt", replacement="cepat")
  text<- tm_map(text, gsub, pattern="resinya", replacement="resi")
  text<- tm_map(text, gsub, pattern="pesen", replacement="pesan")
  text<- tm_map(text, gsub, pattern="cairin", replacement="cair")
  text<- tm_map(text, gsub, pattern="kesel", replacement="kesal")
  text<- tm_map(text, gsub, pattern="nyari", replacement="mencari")
  text<- tm_map(text, gsub, pattern="sempet", replacement="sempat")
  text<- tm_map(text, gsub, pattern="seneng", replacement="senang")
  text<- tm_map(text, gsub, pattern="tqut", replacement="takut")
  text<- tm_map(text, gsub, pattern="seneng", replacement="ketipu")
  text<- tm_map(text, gsub, pattern="menungguin", replacement="menunggu")
  text<- tm_map(text, gsub, pattern="ktipu", replacement="ketipu")
  text<- tm_map(text, gsub, pattern="bgus", replacement="bagus")
  text<- tm_map(text, gsub, pattern="baguss", replacement="bagus")
  text<- tm_map(text, gsub, pattern="mantap", replacement="bagus")
  text<- tm_map(text, gsub, pattern="lemooottt", replacement="lama")
  text<- tm_map(text, gsub, pattern="menuaskan",replacement="memuaskan")
  text<- tm_map(text, gsub, pattern="top",replacement="bagus")
  text<- tm_map(text, gsub, pattern="cape",replacement="lelah")
  text<- tm_map(text, gsub, pattern="nipu",replacement="tipu")
  text<- tm_map(text, gsub, pattern="tunggu",replacement="menunggu")
  text<- tm_map(text, gsub, pattern="tranfer",replacement="transfer")
  text<- tm_map(text, gsub, pattern="simpel",replacement="mudah")
  text<- tm_map(text, gsub, pattern="photo",replacement="foto")
  text<- tm_map(text, gsub, pattern="gambar",replacement="foto")
  text<- tm_map(text, gsub, pattern="blanja",replacement="belanja")
  text<- tm_map(text, gsub, pattern="belanjanya",replacement="belanja")
  text<- tm_map(text, gsub, pattern="sdah",replacement="sudah")
  text<- tm_map(text, gsub, pattern="repot",replacement="rumit")
  text <- tm_map(text, stemDocument)
  return(text)
}
corpusGojek <- preprocessData(corpusGojek)
# Create a document-term matrix
dtmGojek <- DocumentTermMatrix(corpusGojek)

# Convert the matrix to a data frame
dtm_dfGojek <- as.data.frame(as.matrix(dtmGojek))
dtm_dfGojek$sentiment <- dataGojek$score

# Split the data into training and testing sets
set.seed(123)
trainIndexGojek <- createDataPartition(dtm_dfGojek$sentiment, p = 0.7, list = FALSE)
trainGojek <- dtm_dfGojek[trainIndexGojek, ]
testGojek <- dtm_dfGojek[-trainIndexGojek, ]

# Train a maximum entropy model using glmnet
x_trainGojek <- as.matrix(trainGojek[, -ncol(trainGojek)])
y_trainGojek <- as.factor(trainGojek$sentiment)

modelGojek <- glmnet(x_trainGojek, y_trainGojek, family = "multinomial")

# Prepare the test data for prediction
x_testGojek <- as.matrix(testGojek[, -ncol(testGojek)])

# Predict on the test set
predictionsGojek <- predict(modelGojek, newx = x_testGojek, type = "class")
predictionsGojek <- as.factor(predictionsGojek)
testGojek$sentiment <- as.factor(testGojek$sentiment)
# Evaluate the model
confusionMatrix(predictionsGojek[117702:118900], testGojek$sentiment) #ambil kelas terakhir dari prediksi sebagai baris pembanding

#Grab
# Baca dataset Gojek
library(readxl)
dataGrab <- read_excel('Data Review Grab.xlsx')
dataGrab <- dataGrab[,-1] #hapus username
dataGrab <- dataGrab[,-3] #hapus suka
dataGrab$score <- dataGrab$skor
dataGrab$content <- dataGrab$review
# Create a corpus from the text reviews
corpusGrab <- Corpus(VectorSource(dataGrab$content))

# Perform text preprocessing
corpusGrab <- preprocessData(corpusGrab)
# Create a document-term matrix
dtmGrab <- DocumentTermMatrix(corpusGrab)

# Convert the matrix to a data frame
dtm_dfGrab <- as.data.frame(as.matrix(dtmGrab))
dtm_dfGrab$sentiment <- dataGrab$score

# Split the data into training and testing sets
set.seed(123)
trainIndexGrab <- createDataPartition(dtm_dfGrab$sentiment, p = 0.7, list = FALSE)
trainGrab <- dtm_dfGrab[trainIndexGrab, ]
testGrab <- dtm_dfGrab[-trainIndexGrab, ]

# Train a maximum entropy model using glmnet
x_trainGrab <- as.matrix(trainGrab[, -ncol(trainGrab)])
y_trainGrab <- as.factor(trainGrab$sentiment)

modelGrab <- glmnet(x_trainGrab, y_trainGrab, family = "multinomial")

# Prepare the test data for prediction
x_testGrab <- as.matrix(testGrab[, -ncol(testGrab)])

# Predict on the test set
predictionsGrab <- predict(modelGrab, newx = x_testGrab, type = "class")
predictionsGrab <- as.factor(predictionsGrab)
testGrab$sentiment <- as.factor(testGrab$sentiment)
# Evaluate the model
confusionMatrix(predictionsGrab[117702:118900], testGrab$sentiment)  #ambil kelas terakhir dari prediksi sebagai baris pembanding

# Model Evaluation Comparison Between Gojek and Grab
confusionMatrix(predictionsGojek[117702:118900], testGojek$sentiment)
confusionMatrix(predictionsGrab[117702:118900], testGrab$sentiment)

library(wordcloud)
par(mfrow=c(1,2))
wordcloud(corpusGojek,min.freq=4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))
wordcloud(corpusGrab,min.freq=4,max.words=100,random.order=F,colors=brewer.pal(8,"Dark2"))