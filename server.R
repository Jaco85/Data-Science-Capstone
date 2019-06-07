library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

bi_words_Blogs <- readRDS("CleanCorpus_blogs_Bigram.rds") 
tri_words_Blogs  <- readRDS("CleanCorpus_blogs_Trigram.rds")
quad_words_Blogs <- readRDS("CleanCorpus_blogs_Quadgram.rds")

bi_words_Twitter <- readRDS("CleanCorpus_twitter_Bigram.rds") 
tri_words_Twitter  <- readRDS("CleanCorpus_twitter_Trigram.rds")
quad_words_Twitter <- readRDS("CleanCorpus_twitter_Quadgram.rds")

bi_words_News <- readRDS("CleanCorpus_news_Bigram.rds") 
tri_words_News  <- readRDS("CleanCorpus_news_Trigram.rds")
quad_words_News <- readRDS("CleanCorpus_news_Quadgram.rds")

bi_words_General <- readRDS("CleanCorpus_all_Bigram.rds") 
tri_words_General  <- readRDS("CleanCorpus_all_Trigram.rds")
quad_words_General <- readRDS("CleanCorpus_all_Quadgram.rds")



cleanInput <- function(text){
  textInput_clean <- tolower(text)
  textInput_clean <- removePunctuation(textInput_clean)
  textInput_clean <- removeNumbers(textInput_clean)
  textInput_clean <- str_replace_all(textInput_clean, "[^[:alnum:]]", " ")
  textInput_clean <- stripWhitespace(textInput_clean)
  textInput_clean <- txt.to.words.ext(textInput_clean, language="English.all", preserve.case = TRUE)
  return(textInput_clean)
}

predictNextWord_Blogs <- function(textInput_clean) {
  wordCount <- length(textInput_clean) #length of input
  if (wordCount >= 3) { textInput_clean <- textInput_clean[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput_clean)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    predicted_word <- as.character(quad_words_Blogs[quad_words_Blogs$Word1==textInput_clean[1] & 
                                      quad_words_Blogs$Word2==textInput_clean[2] & 
                                      quad_words_Blogs$Word3==textInput_clean[3],]
                         [1:1,]$Word4)
    if(is.na(predicted_word[1])) { #if output (predicted_word) is na then try trigrams
      predicted_word <- as.character(tri_words_Blogs[tri_words_Blogs$Word1==textInput_clean[2] 
                                     & tri_words_Blogs$Word2==textInput_clean[3],][1:1,]$Word3)
      if(is.na(predicted_word[1])) {#if output (predicted_word) is na then try Bigrams
        predicted_word <- as.character(bi_words_Blogs[bi_words_Blogs$Word1==textInput_clean[3],][1:1,]$Word2)
      }
    }
  } else if (length(textInput_clean)==2) {#if input is two words then check input words on ngrams, first trigram
    predicted_word <- as.character(tri_words_Blogs[tri_words_Blogs$Word1==textInput_clean[1] & 
                                     tri_words_Blogs$Word2==textInput_clean[2],][1:1,]$Word3)
    if(is.na(predicted_word[1])) {
      predicted_word <- as.character(bi_words_Blogs[bi_words_Blogs$Word1==textInput_clean[2],][1:1,]$Word2)
    }
  } else {#else input text input is only one word then check words on bigram
    predicted_word <- as.character(bi_words_Blogs[bi_words_Blogs$Word1==textInput_clean[1],][1:1,]$Word2)
  }
  return (predicted_word)
}

predictNextWord_Twitter <- function(textInput_clean) {
  wordCount <- length(textInput_clean) #length of input
  if (wordCount >= 3) { textInput_clean <- textInput_clean[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput_clean)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    predicted_word <- as.character(quad_words_Twitter[quad_words_Twitter$Word1==textInput_clean[1] & 
                                              quad_words_Twitter$Word2==textInput_clean[2] & 
                                              quad_words_Twitter$Word3==textInput_clean[3],]
                         [1:1,]$Word4)
    if(is.na(predicted_word[1])) { #if output (predicted_word) is na then try trigrams
      predicted_word <- as.character(tri_words_Twitter[tri_words_Twitter$Word1==textInput_clean[2] 
                                             & tri_words_Twitter$Word2==textInput_clean[3],][1:1,]$Word3)
      if(is.na(predicted_word[1])) {#if output (predicted_word) is na then try Bigrams
        predicted_word <- as.character(bi_words_Twitter[bi_words_Twitter$Word1==textInput_clean[3],][1:1,]$Word2)
      }
    }
  } else if (length(textInput_clean)==2) {#if input is two words then check input words on ngrams, first trigram
    predicted_word <- as.character(tri_words_Twitter[tri_words_Twitter$Word1==textInput_clean[1] & 
                                             tri_words_Twitter$Word2==textInput_clean[2],][1:1,]$Word3)
    if(is.na(predicted_word[1])) {
      predicted_word <- as.character(bi_words_Twitter[bi_words_Twitter$Word1==textInput_clean[2],][1:1,]$Word2)
    }
  } else {#else input textinput is only one word then check input word on bigram
    predicted_word <- as.character(bi_words_Twitter[bi_words_Twitter$Word1==textInput_clean[1],][1:1,]$Word2)
  }
  return (predicted_word)
}

predictNextWord_News <- function(textInput_clean) {
  wordCount <- length(textInput_clean) #length of input
  if (wordCount >= 3) { textInput_clean <- textInput_clean[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput_clean)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    predicted_word <- as.character(quad_words_News[quad_words_News$Word1==textInput_clean[1] & 
                                            quad_words_News$Word2==textInput_clean[2] & 
                                            quad_words_News$Word3==textInput_clean[3],]
                         [1:1,]$Word4)
    if(is.na(predicted_word[1])) { #if output (predicted_word) is na then try trigrams
      predicted_word <- as.character(tri_words_News[tri_words_News$Word1==textInput_clean[2] 
                                           & tri_words_News$Word2==textInput_clean[3],][1:1,]$Word3)
      if(is.na(predicted_word[1])) {#if output (predicted_word) is na then try Bigrams
        predicted_word <- as.character(bi_words_News[bi_words_News$Word1==textInput_clean[3],][1:1,]$Word2)
      }
    }
  } else if (length(textInput_clean)==2) {#if input is two words then check input words on ngrams, first trigram
    predicted_word <- as.character(tri_words_News[tri_words_News$Word1==textInput_clean[1] & 
                                           tri_words_News$Word2==textInput_clean[2],][1:1,]$Word3)
    if(is.na(predicted_word[1])) {
      predicted_word <- as.character(bi_words_News[bi_words_News$Word1==textInput_clean[2],][1:1,]$Word2)
    }
  } else {#else input is only one words then check input word on bigram
    predicted_word <- as.character(bi_words_News[bi_words_News$Word1==textInput_clean[1],][1:1,]$Word2)
  }
  return (predicted_word)
}


predictNextWord_General <- function(textInput_clean) {
  wordCount <- length(textInput_clean) #length of input
  if (wordCount >= 3) { textInput_clean <- textInput_clean[(wordCount-2):wordCount] } #select last three words
  
  if (length(textInput_clean)==3) { #if (new) input is three words then check input words on ngrams, first quadgram
    predicted_word <- as.character(quad_words_General[quad_words_General$Word1==textInput_clean[1] & 
                                            quad_words_General$Word2==textInput_clean[2] & 
                                            quad_words_General$Word3==textInput_clean[3],]
                         [1:1,]$Word4) #select first (most frequent) word
    if(is.na(predicted_word[1])) { #if output (predicted_word) is NA then try trigrams
      predicted_word <- as.character(tri_words_General[tri_words_General$Word1==textInput_clean[2] & 
                                               tri_words_General$Word2==textInput_clean[3],]
                           [1:1,]$Word3)
      if(is.na(predicted_word[1])) {#if output (predicted_word) is na then try Bigrams
        predicted_word <- as.character(bi_words_General[bi_words_General$Word1==textInput_clean[3],][1:1,]$Word2)
      }
    }
  } else if (length(textInput_clean)==2) {#if input is two words then check input words on ngrams, first trigram
    predicted_word <- as.character(tri_words_General[tri_words_General$Word1==textInput_clean[1] & 
                                           tri_words_General$Word2==textInput_clean[2],][1:1,]$Word3)
    if(is.na(predicted_word[1])) {
      predicted_word <- as.character(bi_words_General[bi_words_General$Word1==textInput_clean[2],][1:1,]$Word2)
    }
  } else {#else input word is only one words then check input word on bigram
    predicted_word <- as.character(bi_words_General[bi_words_General$Word1==textInput_clean[1],][1:1,]$Word2)
  }
  return (predicted_word)
}

shinyServer <- function (input, output) {
  output$html1 <- renderUI({
    textInput <- input$Text
    textInput <- cleanInput(textInput)
    TypeText <- input$TypeText
    if (input$TypeText == "Blogs") {
      predictword <- predictNextWord_Blogs(textInput)
      } else if (input$TypeText == "Twitter") {
        predictword <- predictNextWord_Twitter(textInput) 
      } else if (input$TypeText == "News") {
        predictword <- predictNextWord_News(textInput) 
      } else {
        predictword <- predictNextWord_General(textInput) 
      }
        
    #predictword <- predictNextWord(1, textInput, TypeText)  
    str1 <- "The predicted word is: "
    strNA <- 
    if(is.na(predictword[1])) {
      HTML('<p style="color:red; font-size: 18pt">No word can be predicted, please try something else!</p>')
    } else
      HTML('<p style="color:black; font-size: 16pt">The predicted word is:</p>')
    })
  output$html2 <- renderUI({ 
    textInput <- input$Text
    textInput <- cleanInput(textInput)
    TypeText <- input$TypeText
    if (input$TypeText == "Blogs") {
      predictword <- predictNextWord_Blogs(textInput)
    } else if (input$TypeText == "Twitter") {
      predictword <- predictNextWord_Twitter(textInput) 
    } else if (input$TypeText == "News") {
      predictword <- predictNextWord_News(textInput) 
    } else {
      predictword <- predictNextWord_General(textInput) 
    }
    strNA1 <- ""
    if(is.na(predictword[1])) {
      HTML("")
    } else
      HTML(predictword)
  })
}


