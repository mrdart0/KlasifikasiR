library(textclean)
library(devtools)
install_github("nurandi/katadasaR")
library(katadasaR)
library(tokenizers)
library(stopwords)
library(dplyr)


#preprocessing
#data_latih
data_latih <- read.csv('C:\\Users\\Mrdart0\\Documents\\websiteR\\MulticlassSVM\\svm\\data-latih.csv')
data_latih <- data_latih$Pernyataan %>%
  as.character()
data_latih <- strip(data_latih)
data_latih <- data_latih %>%
  as.data.frame() %>%
  distinct()
data_latih <- as.character(data_latih$.)
stemming_latih <- function(x){
  paste(lapply(x,katadasar),collapse = " ")
}
data_latih <- lapply(tokenize_words(data_latih[]),stemming_latih)
hasil_preprocessing_latih <- tokenize_words(data_latih)
#data_uji
data_uji <- read.csv('C:\\Users\\Mrdart0\\Documents\\websiteR\\MulticlassSVM\\svm\\data-uji.csv')
data_uji <- data_uji$Pernyataan %>%
  as.character()
data_uji <- strip(data_uji)
data_uji <- data_uji %>%
  as.data.frame() %>%
  distinct()
data_uji <- as.character(data_uji$.)
stemming_uji <- function(x){
  paste(lapply(x,katadasar),collapse = " ")
}
data_uji <- lapply(tokenize_words(data_uji[]),stemming_uji)
hasil_preprocessing_uji <- tokenize_words(data_uji)
myStopwords <- readLines("C:\\Users\\Mrdart0\\Documents\\websiteR\\MulticlassSVM\\svm\\idstopwords.txt")
hasil_preprocessing_uji <- as.character(hasil_preprocessing_uji)
hasil_preprocessing_uji <- tokenize_words(hasil_preprocessing_uji, stopwords = myStopwords)

library(proxy)
library(tm)

#tf Idf data latih
TFIDF_latih <- function(hasil_preprocessing_latih){
  #tf
  tf_latih <- Corpus(VectorSource(hasil_preprocessing_latih))
  
  tf_latih <- TermDocumentMatrix(tf_latih) %>% as.matrix()
  #idf
  idf_latih <- log(ncol(tf_latih) /  (1 + rowSums(tf_latih != 0))) %>% diag()
  tf_idf_latih <- crossprod(tf_latih,idf_latih)
  colnames(tf_idf_latih) <- rownames(tf_latih)
  return(tf_idf_latih / sqrt(rowSums(tf_idf_latih^2)))
}
TFIDF_latih <- TFIDF_latih(hasil_preprocessing_latih)
#tf idf data uji
TFIDF_Uji <- function(hasil_preprocessing_uji){
  #tf
  tf_uji <- Corpus(VectorSource(hasil_preprocessing_uji))
  tf_uji <- TermDocumentMatrix(tf_uji) %>% as.matrix()
  #idf
  idf_uji <- log(ncol(tf_uji) / (1 + rowSums(tf_uji !=0))) %>% diag()
  tf_idf_uji <- crossprod(tf_uji, idf_uji)
  colnames(tf_idf_uji) <- rownames(tf_uji)
  return(tf_idf_uji / sqrt(rowSums(tf_idf_uji^2)))
}
TFIDF_Uji <- TFIDF_Uji(hasil_preprocessing_uji)
TFIDF_latih
# multiclass svm
library(e1071)
library(kernlab)
dataklasifikasi <- read.csv('C:\\Users\\Mrdart0\\Documents\\websiteR\\MulticlassSVM\\svm\\trainingfix.csv')
#view(dataklasifikasi)
dataklasifikasi$Pernyataan = factor(dataklasifikasi$Pernyataan)
attach(dataklasifikasi)
model_svm_latih <- svm(Pernyataan ~ ., data = dataklasifikasi)
x <- subset(dataklasifikasi, select = -Pernyataan)
y <- Pernyataan
model_svm_latih <- svm(x, y)

