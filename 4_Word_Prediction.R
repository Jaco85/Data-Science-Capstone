library(tm) # Tekst mining
library(stringr)
library(stylo)

#load files
bi_words <- readRDS("CleanCorpus_all_Bigram.rds")
tri_words  <- readRDS("CleanCorpus_all_Trigram.rds")
quad_words <- readRDS("CleanCorpus_all_Quadgram.rds")

cleanInput <- function(text){
  textInput <- tolower(text)
  textInput <- removeWords(textInput, stopwords("english"))
  textInput <- removePunctuation(textInput)
  textInput <- removeNumbers(textInput)
  textInput <- stripWhitespace(textInput)
  textInput <- txt.to.words.ext(textInput, language="English.all", preserve.case = TRUE)
  return(textInput)
}

predictNextWord <- function(textInput) {
  wordCount <- length(textInput) #length of input
  if (wordCount >= 3) { textInput <- textInput[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    word <- as.character(filter(quad_words, Word1 ==textInput[1] , Word2 == textInput[2], Word3 == textInput[3][1,4]))

    if(nrow(word)==0) { #if output (word) is na then try trigrams
      word <- as.character(filter(tri_words, Word1 ==textInput[2] , Word2 == textInput[3][1,3]))
      
      if(nrow(word)==0) {#if output (word) is na then try Bigrams
        word <- as.character(filter(bi_words, Word1 ==textInput[3][1,2]))
      }
    }
  } else if (length(textInput)==2) {#if input is two words then check input words on ngrams, first trigram
    word <- as.character(filter(tri_words, Word1 ==textInput[2] , Word2 == textInput[3][1,3]))
    
    if(nrow(word)==0) {
      word <- as.character(filter(bi_words, Word1 ==textInput[3][1,2]))
    }
  } else {#else input word is only one words then check input word on bigram
    word <- as.character(filter(bi_words, Word1 ==textInput[3][1,2]))
  }
  word
  return (word)
}

predictNextWord <- function(numPredictWord, textInput) {
  wordCount <- length(textInput) #length of input
  if (wordCount >= 3) { textInput <- textInput[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    word <- as.character(quad_words[quad_words$Word1==textInput[1] & 
                                      quad_words$Word2==textInput[2] & 
                                      quad_words$Word3==textInput[3],]
                         [1:numPredictWord,]$Word4)
    if(is.na(word[1])) { #if output (word) is na then try trigrams
      word <- as.character(tri_words[tri_words$Word1==textInput[2] 
                                     & tri_words$Word2==textInput[3],][1:numPredictWord,]$Word3)
      if(is.na(word[1])) {#if output (word) is na then try Bigrams
        word <- as.character(bi_words[bi_words$Word1==textInput[3],][1:numPredictWord,]$Word2)
      }
    }
  } else if (length(textInput)==2) {#if input is two words then check input words on ngrams, first trigram
    word <- as.character(tri_words[tri_words$Word1==textInput[1] & 
                                     tri_words$Word2==textInput[2],][1:numPredictWord,]$Word3)
    if(is.na(word[1])) {
      word <- as.character(bi_words[bi_words$Word1==textInput[2],][1:numPredictWord,]$Word2)
    }
  } else {#else input word is only one words then check input word on bigram
    word <- as.character(bi_words[bi_words$Word1==textInput[1],][1:numPredictWord,]$Word2)
  }
  return (word)
}

textInput <- "New York"
textInput <- cleanInput(textInput)

predictNextWord(1,textInput)

