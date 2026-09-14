#Preprocessing Data
# Install
install.packages("tm") #for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator
install.packages("RColorBrewer") # color palettes
# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(readxl)

docs<-read_excel("Data Review Gojek 1.xlsx")
docs <- docs$review
# Load the data as a corpus
docs<- Corpus(VectorSource(docs))
#Inspect the content of the document
inspect(docs)
#Replacing “/”, “@” and “|” with space:
toSpace<- content_transformer(function (x , pattern )
  gsub(pattern, " ", x))
docs<- tm_map(docs, toSpace, "/")
docs<- tm_map(docs, toSpace, "@")
docs<- tm_map(docs, toSpace, "\\|")
#Cleaning the text
#Convert the text to lower case
docs<- tm_map(docs, content_transformer(tolower))
#Remove punctuation
docs<- tm_map(docs, toSpace, "[[:punct:]]")
#Remove numbers
docs<- tm_map(docs, toSpace, "[[:digit:]]")
# add two extra stop words: "available" and "via"
93
myStopwords = read.csv("Stopwords.csv")
myStopwords <- na.omit(myStopwords)
# remove stopwords from corpus
docs<- tm_map(docs, removeWords, myStopwords$Stopwords)
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs,removeWords,c("jam","minggu","full","jaya","ambil","nambah","tokopedia","shopee","dateng","gabisa","kota","dikasih","kemana","yah","yaa","jga","kena","back","lazada","negatif","emang
","bagusup","indo","hrs","urus","rumah","seminggu","batas","tulis","nulis","dapatkan","gede","dipakai","setia","semenjak","namanya","rating","lakukan","job","smpai","pdahal","merah","kta","bintangnya","tulisan","tanda","edit","butuh","mah"))
# Eliminate extra white spaces
docs<- tm_map(docs, stripWhitespace)
# Remove URL
removeURL<- function(x) gsub("http[[:alnum:]]*", " ", x)
docs<- tm_map(docs, removeURL)
#Replace words
docs<- tm_map(docs, gsub, pattern="ribet", replacement="rumit")
docs<- tm_map(docs, gsub, pattern="ngawur", replacement="asalasalan")
docs<- tm_map(docs, gsub, pattern="ngaco", replacement="asalasalan")
docs<- tm_map(docs, gsub, pattern="top", replacement="bagus")
docs<- tm_map(docs, gsub, pattern="keren", replacement="bagus")
docs<- tm_map(docs, gsub, pattern="belom", replacement="belum")
docs<- tm_map(docs, gsub, pattern="system", replacement="sistem")
docs<- tm_map(docs, gsub, pattern="histori",replacement="history")
docs<- tm_map(docs, gsub, pattern="sgen", replacement="agen")
docs<- tm_map(docs, gsub, pattern="handphone", replacement="hp")
docs<- tm_map(docs, gsub, pattern="kadaluarsa",
replacement="kadaluwarsa")
docs<- tm_map(docs, gsub, pattern="tlp", replacement="telepon")
docs<- tm_map(docs, gsub, pattern="telpon", replacement="telepon")
docs<- tm_map(docs, gsub, pattern="lelet", replacement="lambat")
docs<- tm_map(docs, gsub, pattern="lemot", replacement="lambat")
docs<- tm_map(docs, gsub, pattern="tf", replacement="transfer")
docs<- tm_map(docs, gsub, pattern="cepet", replacement="cepat")
docs<- tm_map(docs, gsub, pattern="duit", replacement="uang")
docs<- tm_map(docs, gsub, pattern="nunggu",
replacement="menunggu")
docs<- tm_map(docs, gsub, pattern="ilang", replacement="hilang")
docs<- tm_map(docs, gsub, pattern="males", replacement="malas")
docs<- tm_map(docs, gsub,
pattern="notif",replacement="notifikasi")
docs<- tm_map(docs, gsub, pattern="ngasih", replacement="memberi")
docs<- tm_map(docs, gsub, pattern="brg", replacement="barang")
docs<- tm_map(docs, gsub, pattern="brang", replacement="barang")
docs<- tm_map(docs, gsub, pattern="hhilang", replacement="hilang")
docs<- tm_map(docs, gsub, pattern="nyesel",
replacement="menyesal")
docs<- tm_map(docs, gsub, pattern="komplen",
replacement="komplain")
docs<- tm_map(docs, gsub, pattern="nomer", replacement="nomor")
docs<- tm_map(docs, gsub, pattern="voucer", replacement="voucher")
docs<- tm_map(docs, gsub, pattern="apps", replacement="aplikasi")
docs<- tm_map(docs, gsub, pattern="credits", replacement="kredit")
docs<- tm_map(docs, gsub, pattern="blum", replacement="belum")
docs<- tm_map(docs, gsub, pattern="pocher", replacement="voucher")
docs<- tm_map(docs, gsub, pattern="vouchernya",
replacement="voucher")
docs<- tm_map(docs, gsub, pattern="ongkirnya",
replacement="ongkir")
docs<- tm_map(docs, gsub, pattern="negonya", replacement="nego")
docs<- tm_map(docs, gsub, pattern="cpt", replacement="cepat")
docs<- tm_map(docs, gsub, pattern="resinya", replacement="resi")
docs<- tm_map(docs, gsub, pattern="pesen", replacement="pesan")
docs<- tm_map(docs, gsub, pattern="cairin", replacement="cair")
docs<- tm_map(docs, gsub, pattern="kesel", replacement="kesal")
docs<- tm_map(docs, gsub, pattern="nyari", replacement="mencari")
docs<- tm_map(docs, gsub, pattern="sempet", replacement="sempat")
docs<- tm_map(docs, gsub, pattern="seneng", replacement="senang")
docs<- tm_map(docs, gsub, pattern="tqut", replacement="takut")
docs<- tm_map(docs, gsub, pattern="seneng", replacement="ketipu")
docs<- tm_map(docs, gsub, pattern="menungguin",
replacement="menunggu")
docs<- tm_map(docs, gsub, pattern="ktipu", replacement="ketipu")
docs<- tm_map(docs, gsub, pattern="bgus", replacement="bagus")
docs<- tm_map(docs, gsub, pattern="baguss", replacement="bagus")
docs<- tm_map(docs, gsub, pattern="mantap", replacement="bagus")
docs<- tm_map(docs, gsub, pattern="lemooottt", replacement="lama")
docs<- tm_map(docs, gsub,
pattern="menuaskan",replacement="memuaskan")
docs<- tm_map(docs, gsub, pattern="top",replacement="bagus")
docs<- tm_map(docs, gsub, pattern="cape",replacement="lelah")
docs<- tm_map(docs, gsub, pattern="nipu",replacement="tipu")
docs<- tm_map(docs, gsub, pattern="tunggu",replacement="menunggu")
95
docs<- tm_map(docs, gsub,
pattern="tranfer",replacement="transfer")
docs<- tm_map(docs, gsub, pattern="simpel",replacement="mudah")
docs<- tm_map(docs, gsub, pattern="photo",replacement="foto")
docs<- tm_map(docs, gsub, pattern="gambar",replacement="foto")
docs<- tm_map(docs, gsub, pattern="blanja",replacement="belanja")
docs<- tm_map(docs, gsub,
pattern="belanjanya",replacement="belanja")
docs<- tm_map(docs, gsub, pattern="sdah",replacement="sudah")
docs<- tm_map(docs, gsub, pattern="repot",replacement="rumit")
#Build a term-document matrix
dtm<- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 25)
dataframe<-data.frame(text=unlist(sapply(docs,
`[`)),stringsAsFactors=F)
write.csv(dataframe,"C:/Users/JULI YANDI RAHMAN/Downloads/Kerjaan/Selesai/Project 2338 DL Jum'at/cleaning.csv")
save.image()

# Pelabelan Kelas Sentimen
library(tm)
kalimat2<-read.csv("cleaning.csv",header=TRUE)
#skoring
positif<- scan("E:/BISMILLAH TUGAS AKHIR/Bismillah TA
                             Ditia/Analisis/s-pos.txt",what="character",comment.char=";")
negatif<- scan("E:/BISMILLAH TUGAS AKHIR/Bismillah TA
                             Ditia/Analisis/s-neg.txt",what="character",comment.char=";")
kata.positif = c(positif, "senang")
kata.negatif = c(negatif, "kecewa")
score.sentiment = function(kalimat2, kata.positif, kata.negatif,
.progress='none')
{
require(plyr)
require(stringr)
scores = laply(kalimat2, function(kalimat, kata.positif,
kata.negatif)
{
kalimat = gsub('[[:punct:]]', '', kalimat)
kalimat = gsub('[[:cntrl:]]', '', kalimat)
kalimat = gsub('\\d+', '', kalimat)
kalimat = tolower(kalimat)
list.kata = str_split(kalimat, '\\s+')
kata2 = unlist(list.kata)
positif.matches = match(kata2, kata.positif)
negatif.matches = match(kata2, kata.negatif)
positif.matches = !is.na(positif.matches)
negatif.matches = !is.na(negatif.matches)
score = sum(positif.matches) - (sum(negatif.matches))
return(score)
}, kata.positif, kata.negatif, .progress=.progress )
scores.df = data.frame(score=scores, text=kalimat2)
return(scores.df)
}
hasil = score.sentiment(kalimat2$text, kata.positif, kata.negatif)
View(hasil)
#CONVERT SCORE TO SENTIMENT
hasil$klasifikasi<- ifelse(hasil$score<0, "Negatif","Positif")
hasil$klasifikasi
View(hasil)
#EXCHANGE ROW SEQUENCE
data<- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "hasil_pelabelan.csv")