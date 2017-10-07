rm(list=ls())
library(ggplot2)

SONNETS.df <- read.table(
  "http://history.emory.edu/RAVINA/HIST_582/Data/Shakespeare.txt", 
  header = TRUE, stringsAsFactors = FALSE)

## paste the text into one long string
big.string <- paste(SONNETS.df$text, collapse = " ")
## split the string into words
big.string <- str_split(big.string,"\\W")
## get a dataframe of word frequency
Word.list.df <- data.frame(table(big.string))
## give the dataframe some nice names
colnames(Word.list.df) <- c("word","freq")
## remove blanks
Word.list.df <- Word.list.df[which(!Word.list.df$word==""),]
## add \\b so the words are ready for regex searches
target.list <- paste("\\b",Word.list.df$word,"\\b", sep="")

count.matrix <- 
  sapply(X = target.list, FUN = function(x) str_count(SONNETS.df$text, x))
## lines below are clean up
DTM.df <- data.frame(count.matrix)    ##converts matrix to df
colnames(DTM.df) <- Word.list.df$word ## corrects column names

DTM.matrix <- apply(X = DTM.df, MARGIN = 1, FUN = function (x) x/sum(x))
DTM.matrix <- t(DTM.matrix*100)            ## convert to percent and transpose
DTM.perc.df <- data.frame(DTM.matrix)      ## list to dataframe


ggplot(data=DTM.perc.df, aes(eye,heart)) + geom_point()


rownames(DTM.perc.df) <- SONNETS.df$title
DTM.perc.smaller.df <- DTM.perc.df[which(DTM.perc.df$eye > 0),] 


dist <- dist(DTM.perc.smaller.df, method = "euclidean")
cluster <- hclust(dist, method="ward.D2")
plot(cluster)
plot(as.dendrogram(cluster, hang=0.05),horiz=T)


library(ggplot2)
library(ggdendro)
ddata <- dendro_data(cluster, type="rectangle")

ggdendrogram(ddata, leaf_labels = TRUE, rotate = TRUE) + 
  theme(axis.text= element_text(color="black", size=12))
