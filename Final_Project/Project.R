#####A bunch of useful code and packages
rm(list=ls())
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggmap)
library(maps)
library(mapdata)
library(plyr)
library(geosphere)
Sys.setlocale("LC_ALL", "English")
Sys.getlocale()

##### Read in the file
Ningenshikaku.df <- read.delim("D:/Google Drive/JPN_LIT/Dasai_Osamu/ningenshikaku.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "CP932")
Ningenshikaku.text <- paste(Ningenshikaku.df[,1],collapse = "")
Ningenshikaku.splited <- unlist(str_split(Ningenshikaku.text, pattern = ""))

###grep
## Take out ruby and style notation
# Find out where to start and end
start <- grep(pattern = "《|［", Ningenshikaku.splited)
end <- grep(pattern = "》|］", Ningenshikaku.splited)
from <- end + 1
to <- start - 1
real.from <- c(1, from)
real.to <- c(to, length(Ningenshikaku.splited))
CUT.df <- data.frame("from" = real.from, "to"= real.to,"text" = NA)

# Solve the situation when form > end
row <- 1
CUT.fine.df  <- data.frame("from" = 0, "to" = 0, "text" = NA)
for(row in 1:length(CUT.df$from)){
        if(CUT.df$from[row] <= CUT.df$to[row]){
                CUT.fine.df<- rbind(CUT.fine.df, CUT.df[row,])
        }
}

i <- 1
for(i in 1:length(CUT.fine.df$from)){
        text <- Ningenshikaku.splited[CUT.fine.df$from[i]:CUT.fine.df$to[i]]
        CUT.fine.df$text[i] <- paste(text, collapse = "")
}

Ningenshikaku.cleaned.text <- paste(CUT.fine.df$text, collapse = "") #cleaned up text, without ruby and style notations.
Ningenshikaku.cleaned.split <- unlist(str_split(Ningenshikaku.cleaned.text, pattern = ""))

## A simple word count here (All punctuation, white spaces in English or Japanese format)
Ningenshikaku.wordcount <- str_replace_all(Ningenshikaku.cleaned.split, "[:punct:]", " ")
Ningenshikaku.wordcount <- Ningenshikaku.wordcount[which(Ningenshikaku.wordcount != " ")]
Ningenshikaku.wordcount <- Ningenshikaku.wordcount[which(Ningenshikaku.wordcount != "　")]
NGSK.freq <- data.frame(table(Ningenshikaku.wordcount))
NGSK.freq.ord <- NGSK.freq[order(-NGSK.freq$Freq),]
write.table(NGSK.freq.ord, "NGSK_freq.txt",row.names = FALSE, sep = "\t")

# Grouping according to character length
Ningenshikaku.cleaned.split.group1 <- Ningenshikaku.cleaned.split
g2 <- 2
Ningenshikaku.cleaned.split.group2 <- paste(Ningenshikaku.cleaned.split[1:2], collapse = "")
for (g2 in 2:(length(Ningenshikaku.cleaned.split) - 1)){
        group2 <- paste(Ningenshikaku.cleaned.split[g2:(g2+1)], collapse = "")
        Ningenshikaku.cleaned.split.group2 <- c(Ningenshikaku.cleaned.split.group2,group2)
}
g3 <- 2
Ningenshikaku.cleaned.split.group3 <- paste(Ningenshikaku.cleaned.split[1:3], collapse = "")
for (g3 in 2:(length(Ningenshikaku.cleaned.split) - 2)){
        group3 <- paste(Ningenshikaku.cleaned.split[g3:(g3+2)], collapse = "")
        Ningenshikaku.cleaned.split.group3 <- c(Ningenshikaku.cleaned.split.group3,group3)
}

Hanzai <- grep(pattern = "犯罪", Ningenshikaku.cleaned.split.group2)
Kanashi <- grep(pattern = "悲し", Ningenshikaku.cleaned.split.group2)
Kana <- grep(pattern = "悲", Ningenshikaku.cleaned.split.group1)

