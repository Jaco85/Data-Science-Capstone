################################### Session 3 ###################################
# tm explaination: https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
  
  ptm2 <- proc.time()

Sample_Dataset <- readRDS("Sample_Dataset.Rdata")
Blogs_sub <- readRDS("Blogs_sub.Rdata")
Twitter_sub <- readRDS("Twitter_sub.Rdata")
News_sub <- readRDS("News_sub.Rdata")

library(tm)
library(stringr)#replaceall
Clean <- function(source2clean){
  Vector <- VectorSource(source2clean)
  Corpus <- VCorpus(Vector)
  Corpus <- tm_map(Corpus, content_transformer(tolower))
  Corpus <- tm_map(Corpus, content_transformer(stripWhitespace))
  Corpus <- tm_map(Corpus, removeWords, stopwords('english'))    #remove stopwords
  Corpus <- tm_map(Corpus, content_transformer(removePunctuation))
  Corpus <- tm_map(Corpus, content_transformer(removeNumbers))
  Corpus <- tm_map(Corpus, content_transformer(gsub), pattern =  "[^[:alnum:]]", " ")

  CleanCorpus <- data.frame(rawtext = sapply(Corpus, as.character), stringsAsFactors=FALSE)
  sourcename <- paste(deparse(match.call()$source2clean),"_clean", sep="") #Get name of source and paste with "_clean)
  eval(call("<<-", as.name(sourcename), CleanCorpus)) #assign source name to global object (<<-) and fill with values from function
  saveRDS(eval(call("<<-", as.name(sourcename), CleanCorpus)),file = paste(sourcename,".Rdata", sep="")) #save file for later use
  }

Clean(Sample_Dataset)
Clean(Blogs_sub)
Clean(Twitter_sub)
Clean(News_sub)

ptm2 <- proc.time() - ptm2
print("Session 1: Loading the raw data:")
print(ptm2)