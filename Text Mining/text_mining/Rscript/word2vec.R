###-----------------------------------------------------------###
###           Text Mining with R for Non-coders               ###
###            R script created by Daniel Choy                ###
###   Carleton College Gould Library Research Data Service    ###
###     Please Contact Kristin Partlo with any Questions      ###
###-----------------------------------------------------------###

install.packages("tsne")
install.packages("devtools")
library(devtools)   #Rtools needs to be downloaded separately in Windows
install_github("bmschmidt/wordVectors")
library(wordVectors)
library(tsne)
library(readr)

#Launch Morpheme Analyzer
system("tctStart")
targetData = read_csv("./Rscript/Oatmeal_Cookies.xlsx")

#Create TXT file for word2vec Train
write.table(targetData$parsedContent,file = "./trainTxt.txt", row.names = FALSE, col.names = FALSE, quote = F)

#Model Training
model = train_word2vec("./trainTxt.txt", output_file = "w2vModel.bin", 
                       threads=3, vectors=100, force = T)

#confirm word2vector
read.vectors("./w2vModel.bin")

#Extract associated keyword
nearest_to(model,model[["cookies"]], 20)

#Extract associated keywords of 2 or more words
nearest_to(model,model[[c("cookies","oatmeal")]], 20)

#Calculate the words
subVec = model[rownames(model)=="cookies",] - model[rownames(model) == "amazing",] + model[rownames(model) == "good",]
nearest_to(model, subVec, 20)

#Visualize the entire relationship
library(extrafont) 
par(family="AppleGothic") 

plot(model)

######### Text Pre-Processing ############
#Change to lower case letters
targetData$parsedContent = tolower(targetData$parsedContent)




#Cosine similarity (= 1 - Cosine distance)
cosineSimilarity(model[["cookies"]], model[["amazing"]])

#Euclidean Distance
dist(model[(row.names(model)=="cookies" | row.names(model)=="amazing"),])

