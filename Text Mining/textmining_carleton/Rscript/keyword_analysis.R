###-----------------------------------------------------------###
###           Text Mining with R for Non-coders               ###
###            R script created by Daniel Choy                ###
###   Carleton College Gould Library Research Data Service    ###
###     Please Contact Kristin Partlo with any Questions      ###
###-----------------------------------------------------------###

###              Keyword Analysis for Mac users               ###


##################################################################
### 1. Install Necessary Packages for Text Handling / Analysis ###
##################################################################


install.packages("tm") #Package for text mining
install.packages("slam")
install.packages("dplyr")
install.packages("readr") #Package to read file

library(tm)
library(slam)
library(dplyr)
library(readr)

install.packages("rJava")
library(rJava)
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre")
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home/jre/lib/server/libjvm.dylib')

library(NLP4kec)


##################################################################
#2. Text Handling - Call the file that consists the text you want to analyze
##################################################################

#Launch Morpheme Analyzer (use Standford Core NLP )
parsedData = text_parser(path = "./Rscript/Oatmeal_Cookies.xlsx"
                         ,language = "en")

parsedData

# If your data file (csv or xlsx) is located in the same directory folder with your project file,
# just set your path as the above. Note the PowerPoint Slides for more detail.


# You need to produce a DTM in order to do keyword analysis. Note your PowerPoint Slide if you have any confusion

##################################################################
### 3. Text Handling - Text Pre-processing                     ###
##################################################################


#change to text document form
corp = tm_map(corp, PlainTextDocument)

#create Document Term Matrix (DTM) - (length of the word is set as 2)
dtm = DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

#create Term Document Matirx
tdm = TermDocumentMatrix(corp, control=list(removeNumbers=TRUE, wordLengths=c(2,Inf)))

#remove Sparse Terms 
dtm = removeSparseTerms(dtm, as.numeric(0.98))


##################################################################
### 3. Keyword Analysis                                        ###
##################################################################

# Extract your Keyword

# "냉장고"의 연관 키워드 구하기
findAssocs(dtm, terms = "cookie", corlimit = 0.2)  #insert your keyword here. For example I have put cookie here

# Find correlations 
dtm_m = as.matrix(dtm)
dtm_m
cor_term = cor(dtm_m)
cor_term
cor_ref = cor_term[,"cookie"]
cor_ref

#Using the value of TF-IDF, find related keyword
dtmW = DocumentTermMatrix(corp, control=list(wordLengths=c(2,Inf),
                                            weighting = function(x) weightTfIdf(x, normalize = TRUE))) 
#Give a "weight" value to Tf-Idf

dtmW



## Remove spaces next to the words
colnames(dtmW) = trimws(colnames(dtmW))
dtmW = dtmW[,nchar(colnames(dtm)) > 1]


dtmW = removeSparseTerms(dtmW, as.numeric(0.96))
dtmW

findAssocs(dtmW, "cookie", 0.01)

##################################################################
### 3. Data Visualization of Keyword Analysis                  ###
##################################################################


install.packages(c("igraph", "network", "sna", "GGally")) 
install.packages("ggplot2")
library(igraph)
library(network)
library(sna)
library(ggplot2)
library(GGally)

# Create a Data of Network Map version ( This creates a matrix of correlation coefficient between words)

dtmW_m = as.matrix(dtmW)
cor_termW = cor(dtmW_m)

# Control the number of "Edge"
cor_termW[cor_termW < 0.25] = 0

# Create an object to draw a Network Map
net = network(cor_termW, directed = FALSE)
net

# Find the value of the "betweenness of network"Network의 betweenness and color yellow with the nodes that have the higher rank (10%) 
net %v% "mode" <- ifelse(betweenness(net) > quantile(betweenness(net), 0.9), "big", "small")
node_color = c("small" = "grey", "big" = "gold")

# Set the value of Network edge size
set.edge.value(net, "edgeSize", cor_termW * 2)




###Draw Network Map  ###


ggnet2(net # Network
       ,label=TRUE # Node label - true or false
       ,label.size = 3 # label font size
       ,color = "mode" # node color standard
       ,palette = node_color # node color
       ,size = "degree" # the size of the node is differed based on the degree cetrality value
       ,edge.size = "edgeSize" # 
       ,family="AppleGothic") # mac users need this


word_network = data.frame(word = rownames(cor_termW),
                          centrality = degree(net),
                          betweenness = betweenness(net),
                          eigenvector = evcent(net))

## Draw Network Map based on certain keywords ##


keyword = c("grain","baked","sample")  #This is just an example
sub_cor_term = cor_termW[,keyword]
head(sub_cor_term)
sub_cor_term = sub_cor_term[!(rownames(sub_cor_term) %in% keyword),]
head(sub_cor_term)
sub_cor_term = sub_cor_term[rowSums(sub_cor_term)>0,]
head(sub_cor_term)
net2 = network(sub_cor_term, directed = FALSE, matrix.type="bipartite")

ggnet2(net2 
       ,label=TRUE
       ,label.size = 3 
       ,edge.size = sub_cor_term[sub_cor_term>0] * 2
       ,size = degree(net2) 
       ,family="AppleGothic")
