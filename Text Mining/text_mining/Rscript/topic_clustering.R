###-----------------------------------------------------------###
###           Text Mining with R for Non-coders               ###
###            R script created by Daniel Choy                ###
###   Carleton College Gould Library Research Data Service    ###
###     Please Contact Kristin Partlo with any Questions      ###
###-----------------------------------------------------------###

###           Topic Clustering for Mac/Windows users          ###


##################################################################
### 1. Install Necessary Packages for Text Handling / Analysis ###
##################################################################


install.packages("topicmodels")
install.packages("LDAvis")
install.packages("servr")
library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(tm)
library(slam)
library(dplyr)
library(NLP4kec)


install.packages("rJava")
library(rJava)
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')


##################################################################
### 2. Text Handling - Call the file that consists the text    ###
##################################################################

#Launch Morpheme Analyzer (use Standford Core NLP )
parsedData = text_parser(path = "./Rscript/Oatmeal_Cookies.xlsx"
                         ,language = "en")

parsedData


##################################################################
### 3. Text Handling - Text Pre-processing                     ###
##################################################################


## Add Space between words ##
parsedData = gsub(" ","  ",parsedData)


#Create Corpus 
corp=VCorpus(VectorSource(parsedData))

#corp=VCorpus(VectorSource(parsedDataRe$parsedContent))

#Remove Puncutuation and Special Characters
corp = tm_map(corp, removePunctuation)

#Chnage to lower case letters
corp = tm_map(corp, tolower)

#Remove particular words
corp = tm_map(corp, removeWords, c("is", "the","or","and","for","that","this","more","so", "be"))


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
dtm

#Remove one letter word
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]

#Remove Sparse Terms
dtm = removeSparseTerms(dtm, as.numeric(0.997))
dtm

## When using LDA, control the size of DTM
#Find the value of Tf-Idf for each word
term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))

#Check the distribution using boxplot
boxplot(term_tfidf)

# Based on the value of Tf-Idf, reduce the size of dtm and create new_dtm 
new_dtm = dtm[,term_tfidf >= 0.1]
new_dtm = new_dtm[row_sums(new_dtm) > 0,]

############################################
## Running LDA
############################################
#set up random seed, name of the analysis, and the number of cluster
name = "OatmealCookies" #give a name
SEED = 2017 #set up ramdom seed
k = 10 # set up the number of lcuster

#launch LDA
lda_tm = LDA(new_dtm, control=list(seed=SEED), k)
lda_tm

#Save important words for each topic
term_topic = terms(lda_tm, 30)

#Print the important words for each topic through file
filePathName = paste0("./LDA_output/",name,"_",k,"_LDA_Result.csv")
write.table(term_topic, filePathName, sep=",", row.names=FALSE)

# Save the topic number of each file 
doc_topic = topics(lda_tm, 1)
doc_topic_df = as.data.frame(doc_topic)
doc_topic_df$rown = as.numeric(row.names(doc_topic_df))

#calculate the probability of  topic of each file
doc_Prob = posterior(lda_tm)$topics
doc_Prob_df = as.data.frame(doc_Prob)

#Find the maximum probability value
doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)

#Find topic number for each file and extract the probablity value
doc_Prob_df$rown = doc_topic_df$rown
parsedData = as.data.frame(parsedData)
parsedData$rown = as.numeric(row.names(parsedData))
id_topic = merge(doc_topic_df, doc_Prob_df, by="rown")
id_topic = merge(id_topic, parsedData, by="rown", all.y = TRUE)
id_topic = subset(id_topic,select=c("rown","doc_topic","maxProb"))

#Extract the probability value of each topic number
filePathName = paste0("./LDA_output/",name,"_",k,"_DOC","_LDA_Result.csv",sep="")
write.table(id_topic, filePathName, sep=",", row.names=FALSE)

#Print the probability value of each topic word 
posterior(lda_tm)$terms

##################################################################
### 3. Data Visualization of Topic Clustering                  ###
##################################################################

# phi is the probability value that is included in the topics of each word.
phi = posterior(lda_tm)$terms %>% as.matrix

# theta is the probability value that is included in the topics of each file.
theta = posterior(lda_tm)$topics %>% as.matrix

# vocab is the list of all the words.
vocab = colnames(phi)

# Find the length of the file of each file.
doc_length = vector()
doc_topic_df=as.data.frame(doc_topic)

for( i in as.numeric(row.names(doc_topic_df))){
  temp = corp[[i]]$content
  doc_length = c(doc_length, nchar(temp[1]))
}

# Find the freqency of each word.
new_dtm_m = as.matrix(new_dtm)
freq_matrix = data.frame(ST = colnames(new_dtm_m),
                          Freq = colSums(new_dtm_m))

# We change the value above to a parameter value in order to create a data for visualization.
source("./Rscript/createChoyJson_v2.R")
json_lda = createChoyJson(phi = phi, theta = theta,
                          vocab = vocab,
                          doc.length = doc_length,
                          term.frequency = freq_matrix$Freq,
                          #mds.method = jsPCA #canberraPCA <- use this when it doesnt work
                          mds.method = canberraPCA
)

# send it to tomcat
serVis(json_lda, out.dir = paste("C:/apache-tomcat-8.5.11/webapps/",name,"_",k,sep=""), open.browser = FALSE) # Use this when you are using Windows
serVis(json_lda, open.browser = T) # Use this when you are using mac 

