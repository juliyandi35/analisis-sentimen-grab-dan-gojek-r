#Step 0: Load the data
library(readxl)
dataGojek <- read_excel('Data Review Gojek.xlsx')
dataGojek <- dataGojek[,-1]
dataGojek <- dataGojek[,-2]
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
# Create a sentiment label column
#dataGojek$suka <- ifelse(dataGojek$suka == 0, 1, 2)

# Step 4: Training and testing data determination
# Split the data into training and testing sets
set.seed(123)
trainIndexGojek <- sample(1:nrow(dataGojek), 0.7 * nrow(dataGojek))
trainDataGojek <- dataGojek[trainIndexGojek, ]
testDataGojek <- dataGojek[-trainIndexGojek, ]

# Step 5: Positive and negative review classification using maximum entropy
# Combine training and test data for preprocessing
allDataGojek <- rbind(trainDataGojek, testDataGojek)

# Apply preprocessing to the combined data
corpusGojek <- Corpus(VectorSource(allDataGojek$review))
corpusGojek <- preprocessData(corpusGojek)

# Re-split the preprocessed data into training and testing sets
preprocessedTrainDataGojek <- allDataGojek[1:nrow(trainDataGojek), ]
preprocessedTestDataGojek <- allDataGojek[(nrow(trainDataGojek) + 1):nrow(allDataGojek), ]

# Create a document-term matrix for the training data
dtmGojek <- DocumentTermMatrix(corpusGojek[1:nrow(trainDataGojek)])

# Convert the dtm to a matrix
dtmMatrixGojek <- as.matrix(dtmGojek)

# Train the maximum entropy model
maxEntModelGojek <- multinom(skor ~ ., data = preprocessedTrainDataGojek)

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
testPredictionsGojek <- predict(maxEntModelGojek, newdata = preprocessedTrainDataGojek, type = "class")

# Evaluate the accuracy of the predictions
testPredictionsGojek <- as.numeric(testPredictionsGojek)
preprocessedTestDataGojek$suka <- as.vector(preprocessedTestDataGojek$suka)
max_lengthGojek <- max(length(testPredictionsGojek), length(preprocessedTestDataGojek$suka))
testPredictionsGojek <- rep(testPredictionsGojek, length.out = max_lengthGojek)
preprocessedTestDataNewGojek <- rep(preprocessedTestDataGojek$suka, length.out = max_lengthGojek)
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
# Create a sentiment label column
dataGrab$suka <- ifelse(dataGrab$suka == 0, 1, 2)

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
maxEntModelGrab <- multinom(suka ~ ., data = preprocessedTrainDataGrab)

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
preprocessedTestDataGrab$suka <- as.vector(preprocessedTestDataGrab$suka)
max_lengthGrab <- max(length(testPredictionsGrab), length(preprocessedTestDataGrab$suka))
testPredictionsGrab <- rep(testPredictionsGrab, length.out = max_lengthGrab)
preprocessedTestDataNewGrab <- rep(preprocessedTestDataGrab$suka, length.out = max_lengthGrab)
MSEGrab <- mean((testPredictionsGrab - preprocessedTestDataNewGrab)^2)
MSEGrab
# Step 7: Conclusion
# Print the confusion matrix
confusionMatrixGrab <- table(data.frame(testPredictionsGrab, preprocessedTestDataNewGrab))
print(confusionMatrixGrab)

#Comparison
print(confusionMatrixGojek)
print(confusionMatrixGrab)
