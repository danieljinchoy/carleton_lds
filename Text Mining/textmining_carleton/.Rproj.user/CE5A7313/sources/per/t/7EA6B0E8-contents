###-----------------------------------------------------------###
###           Text Mining with R for Non-coders               ###
###            R script created by Daniel Choy                ###
###   Carleton College Gould Library Research Data Service    ###
###     Please Contact Kristin Partlo with any Questions      ###
###-----------------------------------------------------------###


### Text Handling and Basic Text Visuzalization for Windows   ###



##################################################################
### 1. Install Necessary Packages for Text Handling            ###
##################################################################


install.packages("tm") #Package for text mining
install.packages("slam")
install.packages("dplyr")
install.packages("readr") #Package to read file

library(tm)
library(slam)
library(dplyr)
library(readr)
library(NLP4kec)

##################################################################
### 2. Text Handling - Call the file that consists the text    ###
##################################################################

#Launch Morpheme Analyzer
parsedData = text_parser(path = "/Oatmeal_Cookies.xlsx"
                         ,language = "en")

# If your data file (csv or xlsx) is located in the same directory folder with your project file,
# just set your path as the above. Note the PowerPoint Slides for more detail.


# Run through the lines below until line 121

## Add Space between words ##
parsedData = gsub(" ","  ",parsedData)

##################################################################
### 3. Text Handling - Text Pre-processing                     ###
##################################################################

#Create Corpus
corp = VCorpus(VectorSource(parsedData))

#Remove Puncutuation and Special Characters
corp = tm_map(corp, removePunctuation)

#Remove Numbers
corp = tm_map(corp, removeNumbers)

#Chnage to lower case letters
corp = tm_map(corp, tolower)

#Remove particular words
corp = tm_map(corp, removeWords, c("is", "the","or","and","for","that","this","more","so"))

#manage synonyms
for (j in seq(corp))
{
  corp[[j]] <- gsub("like", "love", corp[[j]])
  corp[[j]] <- gsub("hate", "dislike", corp[[j]])
}
##################################################################

#change to text document form 
corp = tm_map(corp, PlainTextDocument)

#create Document Term Matrix (DTM) - (length of the word is set as 2)
dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

#remove space next to the words
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#create Term Document Matirx
tdm = TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))

#remove Sparse Terms 
dtm = removeSparseTerms(dtm, as.numeric(0.99))

#find word frequency 
freq = colSums(as.matrix(dtm))

#save DTM as frame form
dtm_df = as.data.frame(as.matrix(dtm))

#extract DTM as CSV
write_excel_csv(dtm_df, "./dtm.csv")

#word frequency
length(freq)

#sort words in descending order
freq[head(order(-freq), 5)]

#sort words in ascending order
freq[head(order(freq), 10)]

#Find Frequent Terms (for example greater than 20 and lower than 341) )
findFreqTerms(dtm, lowfreq = 20, highfreq = 341)

#word frequency visualization
wordDf = data.frame(word=names(freq), freq=freq)

##################################################################
### 4. Text Data Visualization - Basic visualization           ###
##################################################################

# BAR CHART

library(ggplot2)

#show word frequency through bar chart 
ggplot(wordDf, aes(x=word, y=freq)) + geom_bar(stat = "identity")

#show 30 words through bar chart 
ggplot(head(wordDf,10), aes(x=word, y=freq)) + geom_bar(stat = "identity")

#show top 20 words through bar chart 
ggplot(head(arrange(wordDf,-freq),20), aes(x=reorder(word,-freq), y=freq)) + geom_bar(stat = "identity")


# WORD CLOUD 
install.packages("wordcloud")
library(wordcloud)
pal = brewer.pal(n = 3, name = "Set2") # n:number of colors you want to use, name:the set of color

wordcloud(wordDf$word # word
          , wordDf$freq # frequency
          , min.freq = 5 # minimum frequency
          , colors = pal # palette information
          , rot.per = 0 # word rotation degree
          , random.order = F # decision of word appearance in random way (False or True)
          , scale = c(3,1)) # the front number should be large so the most freqeuntly appeared number can show up in greater size


# TREE MAP
install.packages("treemap")
library(treemap)
treemap(wordDf # set data
        ,title = "Word Tree Map"
        ,index = c("word") # set variable that will go inside the box
        ,vSize = "freq"  # set box size
        ,fontsize.labels = 12 # set font size
        ,palette=pal # palette information
        ,border.col = "white") # set border color
