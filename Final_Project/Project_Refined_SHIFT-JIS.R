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
library(RMeCab)
Sys.setlocale("LC_ALL", "English")
Sys.setlocale("LC_ALL", "Japanese")
Sys.getlocale()

##### Read in the file
Text.df <- read.delim("D:/Google Drive/JPN_LIT/Natsume/kokoro/kokoro.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "SHIFT-JIS")

###Here I am trying to cut the dataframe into chapeters
chapter.pos <-  c(grep("m”‚TŽš‰º‚°n",Text.df$V1),length(Text.df$V1)+1)
for (ch in 1:(length(chapter.pos)-1)){
        Text.text <- paste(Text.df[(chapter.pos[ch]+1):(chapter.pos[ch+1]-1),1],collapse = "")
        Text.splited.raw <- unlist(str_split(Text.text, pattern = ""))
        Text.splited <- str_replace_all(Text.splited.raw, "b", "") # Take out all "b"
        
        
        
        
        ###grep
        ## Take out ruby and style notation
        # Find out where to start and end
        start <- grep(pattern = "s|m", Text.splited)
        end <- grep(pattern = "t|n", Text.splited)
        from <- end + 1
        to <- start - 1
        real.from <- c(1, from)
        real.to <- c(to, length(Text.splited))
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
                text <- Text.splited[CUT.fine.df$from[i]:CUT.fine.df$to[i]]
                CUT.fine.df$text[i] <- paste(text, collapse = "")
        }
        Text.cleaned.text <- paste(CUT.fine.df$text, collapse = "") #cleaned up text, without ruby and style notations.
        write.table(Text.cleaned.text, paste("kokoro_jyou",as.character(ch),".txt",sep = ""),row.names = FALSE, col.names = FALSE)
}




##Old Code
Text.text <- paste(Text.df[,1],collapse = "")
Text.splited.raw <- unlist(str_split(Text.text, pattern = ""))
Text.splited <- str_replace_all(Text.splited.raw, "b", "") # Take out all "b"




###grep
## Take out ruby and style notation
# Find out where to start and end
start <- grep(pattern = "s|m", Text.splited)
end <- grep(pattern = "t|n", Text.splited)
from <- end + 1
to <- start - 1
real.from <- c(1, from)
real.to <- c(to, length(Text.splited))
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
        text <- Text.splited[CUT.fine.df$from[i]:CUT.fine.df$to[i]]
        CUT.fine.df$text[i] <- paste(text, collapse = "")
}

Text.cleaned.text <- paste(CUT.fine.df$text, collapse = "") #cleaned up text, without ruby and style notations.
Text.cleaned.split <- unlist(str_split(Text.cleaned.text, pattern = ""))

##### New Code with Japanese Tokenizer
Text.cleaned.tokenized <-  RMeCabC(Text.text)
Text.tokenized.unlisted <- unlist(Text.cleaned.tokenized)

###Run Code if you want a cleaned txt of the text
###Change name if needed
write.table(Text.cleaned.text,"Kokoro.txt",row.names = FALSE, col.names = FALSE)

### Run code if you want a wordcount table.
### Change name if needed
## A simple word count here (All punctuation, white spaces in English or Japanese format)
Text.wordcount <- str_replace_all(Text.cleaned.split, "[:punct:]", " ")
Text.wordcount <- Text.wordcount[which(Text.wordcount != " ")]
Text.wordcount <- Text.wordcount[which(Text.wordcount != "ã€€")]
Text.freq <- data.frame(table(Text.wordcount))
Text.freq.ord <- Text.freq[order(-Text.freq$Freq),]
# write.table(Text.freq.ord, "Shayo_freq.txt",row.names = FALSE, sep = "\t")

### Grouping according to character length
## Runs slowly. 
## Do not use unless necessary. Uncomment before use.
Text.cleaned.split.group1 <- Text.cleaned.split
g2 <- 2
Text.cleaned.split.group2 <- paste(Text.cleaned.split[1:2], collapse = "")
for (g2 in 2:(length(Text.cleaned.split) - 1)){
        group2 <- paste(Text.cleaned.split[g2:(g2+1)], collapse = "")
        Text.cleaned.split.group2 <- c(Text.cleaned.split.group2,group2)
}

# g3 <- 2
# Text.cleaned.split.group3 <- paste(Text.cleaned.split[1:3], collapse = "")
# for (g3 in 2:(length(Text.cleaned.split) - 2)){
#         group3 <- paste(Text.cleaned.split[g3:(g3+2)], collapse = "")
#         Text.cleaned.split.group3 <- c(Text.cleaned.split.group3,group3)
# }
# 
# g4 <- 2
# Text.cleaned.split.group4 <- paste(Text.cleaned.split[1:4], collapse = "")
# for (g4 in 2:(length(Text.cleaned.split) - 3)){
#         group4 <- paste(Text.cleaned.split[g4:(g4+3)], collapse = "")
#         Text.cleaned.split.group4 <- c(Text.cleaned.split.group4,group4)
# }

#Word with length one
ç¬? <- grep(pattern = "ç¬?", Text.cleaned.split.group1)

#Word with length two
ç¬‘ã† <- grep(pattern = "ç¬‘ã†", Text.cleaned.split.group2)
ç¬‘ã„ <- grep(pattern = "ç¬‘ã„", Text.cleaned.split.group2)
ç¬‘ã£ <- grep(pattern = "ç¬‘ã£", Text.cleaned.split.group2)
ç¬‘ã‚ <- grep(pattern = "ç¬‘ã‚", Text.cleaned.split.group2)
ç¬‘ãˆ <- grep(pattern = "ç¬‘ãˆ", Text.cleaned.split.group2)
ç¬‘ãŠ <- grep(pattern = "ç¬‘ãŠ", Text.cleaned.split.group2)
ç¬‘ã?€ <- grep(pattern = "ç¬‘ã?€", Text.cleaned.split.group2)
ç¬‘ã‚“ <- grep(pattern = "ç¬‘ã‚“", Text.cleaned.split.group2)
ç¬‘é¡? <- grep(pattern = "ç¬‘é¡?", Text.cleaned.split.group2)
ç¬‘è©± <- grep(pattern = "ç¬‘è©±", Text.cleaned.split.group2)
ç¬‘å£° <- grep(pattern = "ç¬‘å£°", Text.cleaned.split.group2)
å¾®ç¬? <- grep(pattern = "å¾®ç¬?", Text.cleaned.split.group2) + 1L
å˜²ç¬? <- grep(pattern = "å˜²ç¬?", Text.cleaned.split.group2) + 1L
è‹¦ç¬? <- grep(pattern = "è‹¦ç¬?", Text.cleaned.split.group2) + 1L
åªšç¬? <- grep(pattern = "åªšç¬?", Text.cleaned.split.group2) + 1L
å¯ç¬? <- grep(pattern = "å¯ç¬?", Text.cleaned.split.group2) + 1L
ä¸€ç¬? <- grep(pattern = "ä¸€ç¬?", Text.cleaned.split.group2) + 1L
æ†«ç¬? <- grep(pattern = "æ†«ç¬?", Text.cleaned.split.group2) + 1L
åŸç¬? <- grep(pattern = "åŸç¬?", Text.cleaned.split.group2) + 1L # ForåŒ—åŸç¬‘ã?€
å¤±ç¬? <- grep(pattern = "å¤±ç¬?", Text.cleaned.split.group2) + 1L
ã®ç¬? <- grep(pattern = "ä¸€ç¬?", Text.cleaned.split.group2) + 1L # This is the case when ç¬? stands alone

# #Word with length three
# ç¬‘ã‚ã‚? <- grep(pattern = "ç¬‘ã‚ã‚?", Text.cleaned.split.group3)
# ç¬‘ã‚ã? <- grep(pattern = "ç¬‘ã‚ã?", Text.cleaned.split.group3)
# 
# #Word with length four
# ç¬‘ã„ã¾ã? <- grep(pattern = "ç¬‘ã„ã¾ã?", Text.cleaned.split.group4)


ç¬?.two.all <- c(ç¬‘ã†,ç¬‘ã„,ç¬‘ã£,ç¬‘ã‚,ç¬‘ã?€,ç¬‘å£°,å¤±ç¬?,ç¬‘ã‚“,ç¬‘è©±,å¾®ç¬?,å˜²ç¬?,è‹¦ç¬?,ç¬‘é¡?,åªšç¬?,å¯ç¬?,ä¸€ç¬?,ã®ç¬?,ç¬‘ãˆ,æ†«ç¬?,åŸç¬?,ç¬‘ãŠ)
ç¬?.two.positive <- c(ç¬‘ã†,ç¬‘ã„,ç¬‘ã£,ç¬‘ã?€,ç¬‘å£°,å¤±ç¬?,ç¬‘ã‚“,å¾®ç¬?,ç¬‘é¡?,ç¬‘è©±,ä¸€ç¬?,ç¬‘ãˆ,åŸç¬?,ç¬‘ãŠ)
ç¬?.two.negative <- c(ç¬‘ã‚,å˜²ç¬?,è‹¦ç¬?,åªšç¬?,æ†«ç¬?)
ç¬?.two.neutral <- c(å¯ç¬?,ã®ç¬?)                   
ç¬?.others <- setdiff(ç¬?,ç¬?.two.all)

##### Graph Section of the code
###ç¬? divided positive and negative as frequecy in the novel
postive.freq <- length(ç¬?.two.positive) / length(Text.wordcount)
negative.freq <- - length(ç¬?.two.negative) / length(Text.wordcount)
Freq.novel.df <- data.frame ("type"= c("Positive", "Negative"), "value" = c(postive.freq,negative.freq),color = c("1","2"))
Freq.novel.df$type <- as.character(Freq.novel.df$type)
ggplot(Freq.novel.df,aes(x = reorder(type,-value), y= value, fill = color))+
        geom_bar(stat="identity",color="grey50",position = "dodge", width = 1) +
        xlab("Type") + ylab("") +
        scale_fill_manual(values=c("dodgerblue2","firebrick3"),guide = FALSE) +
        ggtitle("1948_6_Ningenshikkaku")

