rm(list=ls())
library(stringr)
##read in "The Sonnets" again
SONNETS.df <- read.table(
        "http://history.emory.edu/RAVINA/HIST_582/Data/Shakespeare.txt",
        header = TRUE, stringsAsFactors = FALSE)
SONNETS.df$love <-
        str_count(string = SONNETS.df$text, pattern = "\\blove\\b")


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


DTM.df <- data.frame(matrix(nrow =length(SONNETS.df$text), ncol = length(target.list)))


for (i in seq_along(target.list))
{
        DTM.df[,i] <- str_count(SONNETS.df$text, target.list[i])
}


colnames(DTM.df) <- Word.list.df$word
TDM.df <- data.frame(t(DTM.df))
rownames(TDM.df) <- Word.list.df$word

count.matrix <-
        sapply(X = target.list, FUN = function(x) str_count(SONNETS.df$text, x))
## lines below are clean up
DTM.df <- data.frame(count.matrix) ##converts matrix to df
colnames(DTM.df) <- Word.list.df$word ## corrects column names


total.words <- rowSums(DTM.df)


start.time <- proc.time()
DTM.matrix <- apply(X = DTM.df, MARGIN = 1, FUN = function (x) x/sum(x))
DTM.matrix <- t(DTM.matrix*100) ## convert to percent and transpose
DTM.norm.df <- data.frame(DTM.matrix) ## list to dataframe
apply.time <- proc.time() - start.time

DTM.norm.df.sweep <- sweep(x = DTM.df, MARGIN = 1,
                           STATS = rowSums(DTM.df), FUN="/")
DTM.norm.df.sweep <- DTM.norm.df.sweep*100
colnames(DTM.norm.df.sweep) <- Word.list.df$word



short.list <- c("love","powerful", "abhor","thou","you","muse","argument")
DTM.norm.mini.df <- DTM.norm.df[,which(colnames(DTM.norm.df)%in%short.list)]



cor.matrix.mini <- cor(DTM.norm.mini.df)
round(cor.matrix.mini, 2) ## rounds off at 2 places


select.terms <- c("love","hate")
Corr.matrix.df <- data.frame(cor(DTM.norm.df))
diag(Corr.matrix.df) <- NA
Corr.matrix.select.df <-
        Corr.matrix.df[,which(colnames(Corr.matrix.df)%in%select.terms)]


library(tidyr)
Corr.matrix.df$first.word <- colnames(Corr.matrix.df) ##makes new var
Corr.rank.df <- gather(Corr.matrix.df, key=second.word,
                       value=cor, 1:(ncol(Corr.matrix.df)-1), na.rm=TRUE)





total.count <- sum(Word.list.df$freq)
Word.list.df$norm <- Word.list.df$freq/total.count*100

x <- 10
TFM.norm.df <- data.frame(t(DTM.norm.df))
Subtract.df <- data.frame(TFM.norm.df[,x]-Word.list.df$norm)
Subtract.df$words <- row.names(TFM.norm.df)
subtract.ten <- Subtract.df[order(-Subtract.df[1]),][c(1:10),2]
subtract.ten <- str_replace(subtract.ten, "\\.", "")
subtract.ten

TDM.df[which(rownames(TDM.df)%in%subtract.ten),][,x]



Divide.df <- data.frame(TFM.norm.df[,x]/Word.list.df$norm)
Divide.df$words <- row.names(TFM.norm.df)
divide.ten <- Subtract.df[order(-Divide.df[1]),][c(1:10),2]
divide.ten <- str_replace(divide.ten, "\\.", "")
divide.ten


TDM.df[which(rownames(TDM.df)%in%divide.ten),][,x]

sum(DTM.norm.df$a != 0)

doc.count <- sum(DTM.norm.df$a != 0) + 1 ## add one and assign value
log(nrow(DTM.norm.df)/doc.count) ## divide total docs by doc frequency


SONNETS.tfidf.df1 <- data.frame(apply(DTM.df, 2,function(x) x*log(nrow(DTM.norm.df)/(sum(x!=0)+1))))
SONNETS.tfidf.df <- data.frame(apply(DTM.norm.df, 2,function(x) x*log(nrow(DTM.norm.df)/(sum(x!=0)+1))))

## select row for sonnet x
Tfidf.ten.df <- data.frame(SONNETS.tfidf.df[x,])
## transpose for easier sorting
Tfidf.ten.df <- data.frame(t(Tfidf.ten.df))
## add words
Tfidf.ten.df$words <- row.names(Tfidf.ten.df)
## sort and get top ten
tfidf.ten <- Tfidf.ten.df[order(-Tfidf.ten.df[1]),][c(1:10),]
tfidf.ten$words

SONNETS.tfidf.df.new <- data.frame(t(SONNETS.tfidf.df))

a <- TDM.df[,x]
b <- rowSums(TDM.df[,-x])
c <- colSums(TDM.df[x]) - a
d <- sum(colSums(TDM.df[-x])) - b
E1 <- c*(a+b)/(c+d)
E2 <- d*(a+b)/(c+d)
Dunning <- 2*((a*log(a/E1)) + (b*log(b/E2)))
Dunning.df <- as.data.frame(Dunning)
Dunning.df$words <- row.names(Dunning.df)
Dunning.df <- Dunning.df[order(-Dunning.df$Dunning),]
Dunning.df[c(1:10),"words"]

intersect(Tfidf.ten.df[c(1:10),"words"],Dunning.df[c(1:10),"words"])





    