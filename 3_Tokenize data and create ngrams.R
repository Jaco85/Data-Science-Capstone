ptm3 <- proc.time()

CleanCorpus_all <- readRDS("./Sample_Dataset_clean.Rdata")
CleanCorpus_blogs <- readRDS("./Blogs_sub_clean.Rdata")
CleanCorpus_twitter <- readRDS("./Twitter_sub_clean.Rdata")
CleanCorpus_news <- readRDS("./News_sub_clean.Rdata")

#Function for Count 1) total words, 2) "two", 3) three  and four 4) words combinations of sample data and filter low frequencies:
ngram <- function(source){
  library(tokenizers)
  library(dplyr) #filtering
  library(data.table) #setDT

  Bigram_tokens <- tokenize_ngrams(paste(source,"$rawtext", sep=""),n=2,n_min=2) # Tokenize source
  Bigram_word_count <- table(unlist(Bigram_tokens)) # Make columns
  Bigram <- data.frame(Bigram_word_count) #Dataframe
  Bigram <- Bigram[order(-Bigram$Freq),] #Start with highest freq
  Bigram <- filter(Bigram, Freq >= 2) #filter low frequencies
  
  Bigrams <- na.omit(setDT(Bigram)[, paste0("Word", 1:2) := tstrsplit(Var1, " ")])
  Bigrams <- data.frame(Bigrams$Word1,Bigrams$Word2,Bigrams$Freq)
  names(Bigrams) <- c("Word1","Word2","Freq")
  sourcename <- paste(deparse(match.call()$source),"_Bigram", sep="") #Get name of source and paste with "_Bigram)
  eval(call("<<-", as.name(sourcename), Bigrams)) #assign source name to global object (<<-) and fill with values from function
  saveRDS(eval(call("<<-", as.name(sourcename), Bigrams)),file = paste(sourcename,".rds", sep="")) #save file for later use
  
  
  Trigram_tokens <- tokenize_ngrams(paste(source,"$rawtext", sep=""),n=3,n_min=3)
  Trigram_word_count <- table(unlist(Trigram_tokens))
  Trigram <- data.frame(Trigram_word_count)
  Trigram <- Trigram[order(-Trigram$Freq),]
  Trigram <- filter(Trigram, Freq >= 2)
  
  Trigrams <- na.omit(setDT(Trigram)[, paste0("Word", 1:3) := tstrsplit(Var1, " ")])
  Trigrams <- data.frame(Trigrams$Word1,Trigrams$Word2,Trigrams$Word3,Trigrams$Freq)
  names(Trigrams) <- c("Word1","Word2","Word3","Freq")
  sourcename <- paste(deparse(match.call()$source),"_Trigram", sep="") 
  eval(call("<<-", as.name(sourcename), Trigrams)) 
  saveRDS(eval(call("<<-", as.name(sourcename), Trigrams)),file = paste(sourcename,".rds", sep="")) 
  
  Quadgram_tokens <- tokenize_ngrams(paste(source,"$rawtext", sep=""),n=4,n_min=4)
  Quadgram_word_count <- table(unlist(Quadgram_tokens))
  Quadgram <- data.frame(Quadgram_word_count)
  Quadgram <- Quadgram[order(-Quadgram$Freq),]
  Quadgram <- filter(Quadgram, Freq >= 2)
  
  Quadgrams <- na.omit(setDT(Quadgram)[, paste0("Word", 1:4) := tstrsplit(Var1, " ")])
  Quadgrams <- data.frame(Quadgrams$Word1,Quadgrams$Word2,Quadgrams$Word3,Quadgrams$Word4,Quadgrams$Freq)
  names(Quadgrams) <- c("Word1","Word2","Word3","Word4","Freq")
  sourcename <- paste(deparse(match.call()$source),"_Quadgram", sep="") 
  eval(call("<<-", as.name(sourcename), Quadgrams)) 
  saveRDS(eval(call("<<-", as.name(sourcename), Quadgrams)),file = paste(sourcename,".rds", sep="")) 
  
}

#rm(list=ls())

ptm3 <- proc.time() - ptm3

ngram(CleanCorpus_all)
ngram(CleanCorpus_blogs)
ngram(CleanCorpus_twitter)
ngram(CleanCorpus_news)

