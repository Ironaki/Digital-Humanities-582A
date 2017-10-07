rm(list=ls())
library(stringr)
library(tidyr)
library(plyr)

##read in "The Sonnets" again
SONNETS.df <- read.table(
        "http://history.emory.edu/RAVINA/HIST_582/Data/Shakespeare.txt",
        header = TRUE, stringsAsFactors = FALSE)

SONNETS.df$love <- str_count(string = SONNETS.df$text, pattern = "\\blove\\b")


big.string <- paste(SONNETS.df$text, collapse = " ")
big.string <- str_split(big.string, "\\W")
Word.list.df <- data.frame(table(big.string))
colnames(Word.list.df) <- c("word","freq")
Word.list.df <- Word.list.df[which(Word.list.df$word != ""),]
target.list <- paste("\\b", Word.list.df$word, "\\b", sep = "")


DTM.df <- data.frame(matrix(nrow = length(SONNETS.df$number), ncol = length(target.list)))

for(i in seq_along(target.list)){
        DTM.df[,i] <- str_count(SONNETS.df$text, target.list[i])
}

colnames(DTM.df) <- Word.list.df$word


## Transpose
TDM.df <- data.frame(t(DTM.df))


### Try to do the same thing with apply
function(x) str_count(SONNETS.df$text, x)


count.matrix <- sapply(X = target.list, FUN = function(x) str_count(SONNETS.df$text, x))
DTM.new.df <- data.frame(count.matrix)
colnames(DTM.new.df) <- Word.list.df$word


### Total word count
total.words <- rowSums(DTM.df)

# Use apply to find the percentage of word in each sonnet.

start.time <- proc.time()
DTM.matrix <- apply(X = DTM.df, MARGIN = 1, FUN = function(x) x/total.words)
DTM.matrix <- t(DTM.matrix * 100) # Apply flip the matrix so we flip it back
DTM.norm.df <- data.frame(DTM.matrix)
apply.time <- proc.time() - start.time

# Try to use sweep
DTM.perc.df.sweep <- sweep(x = DTM.df, MARGIN = 1, STATS = rowSums(DTM.df), FUN = "/")
DTM.perc.df.sweep <- DTM.perc.df.sweep * 100


### Do correlation with with a small sample of selected words
short.list <- c("love", "powerful", "abhor", "thou", "you", "muse", "argument")
DTM.per.mini.df <- DTM.perc.df[,which(colnames(DTM.perc.df) %in% short.list)]
#? I don't really understand why we must use %in%


cor.matrix.mini <- cor(DTM.per.mini.df)
round(cor.matrix.mini, 2)


select.terms <- c("love", "hate")
Corr.matrix.df <- data.frame(cor(DTM.norm.df))
diag(Corr.matrix.df) <- NA
Corr.matrix.select.df <- Corr.matrix.df[, which(colnames(Corr.matrix.df) %in% select.terms)]

Corr.matrix.df$first.word <- colnames(Corr.matrix.df)
Corr.rank.df <- gather(Corr.matrix.df, key = second.word, value = cor, 1:(ncol(Corr.matrix.df) - 1), na.rm = TRUE)
#? I don't really understand the gather function here

#? I basically copied the code. Haven't read it.
reserved.words <- c("if", "else"," repeat", "while", "function","for", "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf","NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
Word.list.df$matching <- ifelse(Word.list.df$word%in%reserved.words,paste(as.character(Word.list.df$word), ".", sep=""),as.character(Word.list.df$word))
Corr.rank.df$first.word.freq <- mapvalues(x=Corr.rank.df$first.word,
                                          from=Word.list.df$matching, to=as.numeric(Word.list.df$freq))
Corr.rank.df$second.word.freq <- mapvalues(x=Corr.rank.df$second.word,
                                           from=Word.list.df$matching, to=as.numeric(Word.list.df$freq))
Corr.rank.df[,4] <- as.numeric(Corr.rank.df[,4])
Corr.rank.df[,5] <- as.numeric(Corr.rank.df[,5])
head(Corr.rank.df)

Smaller.corr.rank.df <- Corr.rank.df[which(Corr.rank.df$cor>0.8 &Corr.rank.df$first.word.freq>=3 & Corr.rank.df$second.word.freq>=3),]


##### Chapter 6.2
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