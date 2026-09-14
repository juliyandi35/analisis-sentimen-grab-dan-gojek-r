#Step 0: Load the data
library(readxl)
dataGojek <- read_excel('GOJEK.xlsx')
dataGojek <- dataGojek[,-1]
#dataGojek <- dataGojek[,-2]
# Step 1: Descriptive Analysis of the data
# Load the necessary libraries
library(tm)
library(SnowballC)
library(MASS)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(e1071)
library(nnet)

# Step 2: Preprocessing Data
# Preprocess the text data
preprocessData <- function(text) {
  # Spelling normalization
  text <- tm_map(text, content_transformer(tolower))
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

# Step 3: Sentiment class labelling
#dataGojek$suka <- ifelse(dataGojek$suka == 0, 'negative', 'positive')

# Step 4: Training and testing data determination
# Split the data into training and testing sets
set.seed(123)
trainIndexGojek <- sample(1:nrow(dataGojek), 0.8 * nrow(dataGojek))
trainDataGojek <- dataGojek[trainIndexGojek, ]
testDataGojek <- dataGojek[-trainIndexGojek, ]

# Step 5: Positive and negative review classification using maximum entropy
# Combine training and test data for preprocessing
allDataGojek <- rbind(trainDataGojek, testDataGojek)

# Apply preprocessing to the combined data
corpusGojek <- Corpus(VectorSource(allDataGojek$content))
corpusGojek <- preprocessData(corpusGojek)

# Re-split the preprocessed data into training and testing sets
preprocessedTrainDataGojek <- allDataGojek[1:nrow(trainDataGojek), ]
preprocessedTestDataGojek <- allDataGojek[(nrow(trainDataGojek) + 1):nrow(allDataGojek), ]

# Create a document-term matrix for the training data
dtmGojek <- DocumentTermMatrix(corpusGojek[1:nrow(trainDataGojek)])

# Convert the dtm to a matrix
dtmMatrixGojek <- as.matrix(dtmGojek)
#preprocessedTrainDataGojek$skor<-as.numeric(preprocessedTrainDataGojek$skor)

# Train the maximum entropy model
maxEntModelGojek <- multinom(score ~ at + content,  data = preprocessedTrainDataGojek)

# Step 6: Result interpretation
# Preprocess the test data
testCorpusGojek <- Corpus(VectorSource(preprocessedTestDataGojek$review))
testCorpusGojek <- preprocessData(testCorpusGojek)

# Create a document-term matrix for the test data
testDtmGojek <- DocumentTermMatrix(testCorpusGojek)

# Convert the test dtm to a matrix
testDtmMatrixGojek <- as.matrix(testDtmGojek)
dataGojek$review <- as.factor(dataGojek$review)
# Predict sentiment for test data using the trained model
testPredictionsGojek <- predict(maxEntModelGojek, newdata = preprocessedTestDataGojek, type = "class")

# Evaluate the accuracy of the predictions
testPredictionsGojek <- as.numeric(testPredictionsGojek)
preprocessedTestDataGojek$skor <- as.vector(preprocessedTestDataGojek$skor)
max_lengthGojek <- max(length(testPredictionsGojek), length(preprocessedTestDataGojek$skor))
testPredictionsGojek <- rep(testPredictionsGojek, length.out = max_lengthGojek)
preprocessedTestDataNewGojek <- rep(preprocessedTestDataGojek$skor, length.out = max_lengthGojek)
MSEGojek <- mean((testPredictionsGojek - preprocessedTestDataNewGojek)^2)
MSEGojek
# Step 7: Conclusion
# Print the confusion matrix
confusionMatrixGojek <- table(data.frame(testPredictionsGojek, preprocessedTestDataNewGojek))
print(confusionMatrixGojek)

# Repeat the step using Grab Review Data
# Step 0: Load the data
library(readxl)
dataGrab <- read_excel('Data Review Grab.xlsx')
dataGrab <- dataGrab[,-1]
dataGrab <- dataGrab[,-3]
# Step 1: Descriptive Analysis of the data
# Load the necessary libraries
library(tm)
library(SnowballC)
library(MASS)
library(e1071)
library(nnet)

# Step 2: Preprocessing Data
# Preprocess the text data
preprocessData <- function(text) {
  # Spelling normalization
  text <- tm_map(text, content_transformer(tolower))
  text <- tm_map(text, removeWords, stopwords("english"))
  text <- tm_map(text, stripWhitespace)
  text <- tm_map(text, stemDocument)
  
  return(text)
}

# Step 3: Sentiment class labelling

# Step 4: Training and testing data determination
# Split the data into training and testing sets
set.seed(123)
trainIndexGrab <- sample(1:nrow(dataGrab), 0.8 * nrow(dataGrab))
trainDataGrab <- dataGrab[trainIndexGrab, ]
testDataGrab <- dataGrab[-trainIndexGrab, ]

# Step 5: Positive and negative review classification using maximum entropy
# Combine training and test data for preprocessing
allDataGrab <- rbind(trainDataGrab, testDataGrab)

# Apply preprocessing to the combined data
corpusGrab <- Corpus(VectorSource(allDataGrab$review))
corpusGrab <- preprocessData(corpusGrab)

# Re-split the preprocessed data into training and testing sets
preprocessedTrainDataGrab <- allDataGrab[1:nrow(trainDataGrab), ]
preprocessedTestDataGrab <- allDataGrab[(nrow(trainDataGrab) + 1):nrow(allDataGrab), ]

# Create a document-term matrix for the training data
dtmGrab <- DocumentTermMatrix(corpusGrab[1:nrow(trainDataGrab)])

# Convert the dtm to a matrix
dtmMatrixGrab <- as.matrix(dtmGrab)

# Train the maximum entropy model
maxEntModelGrab <- multinom(skor ~ `waktu review`, data = preprocessedTrainDataGrab)

# Step 6: Result interpretation
# Preprocess the test data
testCorpusGrab <- Corpus(VectorSource(preprocessedTestDataGrab$review))
testCorpusGrab <- preprocessData(testCorpusGrab)

# Create a document-term matrix for the test data
testDtmGrab <- DocumentTermMatrix(testCorpusGrab)

# Convert the test dtm to a matrix
testDtmMatrixGrab <- as.matrix(testDtmGrab)
dataGrab$review <- as.factor(dataGrab$review)
# Predict sentiment for test data using the trained model
testPredictionsGrab <- predict(maxEntModelGrab, newdata = preprocessedTrainDataGrab, type = "class")

# Evaluate the accuracy of the predictions
testPredictionsGrab <- as.numeric(testPredictionsGrab)
preprocessedTestDataGrab$skor <- as.vector(preprocessedTestDataGrab$skor)
max_lengthGrab <- max(length(testPredictionsGrab), length(preprocessedTestDataGrab$skor))
testPredictionsGrab <- rep(testPredictionsGrab, length.out = max_lengthGrab)
preprocessedTestDataNewGrab <- rep(preprocessedTestDataGrab$skor, length.out = max_lengthGrab)
MSEGrab <- mean((testPredictionsGrab - preprocessedTestDataNewGrab)^2)
MSEGrab
# Step 7: Conclusion
# Print the confusion matrix
confusionMatrixGrab <- table(data.frame(testPredictionsGrab, preprocessedTestDataNewGrab))
print(confusionMatrixGrab)

#Comparison
print(confusionMatrixGojek)
print(confusionMatrixGrab)
