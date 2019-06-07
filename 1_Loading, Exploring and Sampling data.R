
# The data was loaded from Coursera Link (https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip) 
# to local machine and will be read from local disk.


################################### Session 1 ###################################
ptm <- proc.time()

#files info --> Size (mb)
Blogs_size <- file.info("en_US/en_US.blogs.txt")$size / 1024^2 
Twitter_size <- file.info("en_US/en_US.twitter.txt")$size  / 1024^2 
News_size <- file.info("en_US/en_US.news.txt")$size / 1024^2 

#load Data
Blogs <- suppressWarnings(readLines("en_US/en_US.blogs.txt", skipNul = TRUE, encoding = "UTF-8"))
Twitter <- suppressWarnings(readLines("en_US/en_US.twitter.txt", skipNul = TRUE, encoding = "UTF-8"))
News <- suppressWarnings(readLines("en_US/en_US.news.txt", skipNul = TRUE, encoding = "UTF-8"))

ptm1 <- proc.time() - ptm
print("Session 1: Loading the raw data:")
print(ptm1)

################################### Session 2 ###################################
ptm <- proc.time()

#Subsetting the data for building the model with good performance:

Sample_Data <- function(Source){
  set.seed(1234)
  Source <- Source %>% 
  sample(size = length(Source)*0.3, replace = TRUE) #30% sample
  sourcename <- paste(deparse(match.call()$Source),"_sub", sep="") #Get name of source and paste with "_sub)
  eval(call("<<-", as.name(sourcename), Source)) #assign source name to global object (<<-) and fill with values from function
  saveRDS(eval(call("<<-", as.name(sourcename), Source)),file = paste(sourcename,".Rdata", sep="")) #save file for later use
}

Sample_Data(Blogs)
Sample_Data(Twitter)
Sample_Data(News)

#Union the datasets
Sample_Dataset <- c(Blogs_sub, Twitter_sub, News_sub)

#Write compressed file to disk 
saveRDS(Sample_Dataset,"Sample_Dataset.Rdata")

Sample_size <- file.info("Sample_Dataset.Rdata")$size / 1024^2 

#Load sample data
Sample_Dataset <- readRDS("Sample_Dataset.Rdata")


# Number of lines / records
Blogs.length <-length(Blogs)/1000000
Twitter.length <-length(Twitter)/1000000
News.length <-length(News)/1000000
Sample.length <-length(Sample_Dataset)/1000000

# Total (in millions) and average number of words per line using stringi
library(stringi)
Blogs.words <- sum(stri_count_words(Blogs))/1000000
Twitter.words <- sum(stri_count_words(Twitter))/1000000
News.words <- sum(stri_count_words(News))/1000000
Sample.words <- sum(stri_count_words(Sample_Dataset))/1000000

lineBlogs.words <- mean(stri_count_words(Blogs))
lineTwitter.words <- mean(stri_count_words(Twitter))
lineNews.words <- mean(stri_count_words(News))
lineSample.words <- mean(stri_count_words(Sample_Dataset))

#Create dataframe with dataset info
File_Summary <- data.frame(
  fileName = c("Blogs","News","Twitter", "Sample_Dataset"),
  fileSize = c(round(Blogs_size), 
               round(Twitter_size),
               round(News_size), 
               round(Sample_size)),
  lineCount = c(round(Blogs.length,2), 
                round(Twitter.length,2), 
                round(News.length,2), 
                round(Sample.length,2)),
  wordCount = c(round(Blogs.words,2), 
                round(Twitter.words,2), 
                round(News.words,2), 
                round(Sample.words,2)),
  wordCount_avg = c(round(lineBlogs.words), 
                    round(lineTwitter.words), 
                    round(lineNews.words), 
                    round(lineSample.words))  
)

#change column names
colnames(File_Summary) <- c("File Name", 
                            "File Size in Megabyte", 
                            "Records (Million)", 
                            "Word Count (Millon)", 
                            "AVG words per line")

print (File_Summary)

ptm2 <- proc.time() - ptm
print("Session 2: Subsetting and summarizing the data:")

print(ptm2)

#Clean memory except File_summary and the sample dataset
rm(list=ls()[!(ls() %in% c("File_Summary","Sample_Dataset"))])

