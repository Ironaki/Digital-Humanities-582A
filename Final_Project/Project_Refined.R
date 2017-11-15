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
Text.df <- read.delim("D:/Google Drive/JPN_LIT/Dasai_Osamu/1948_6_Ningenshikkaku.txt", header = FALSE, stringsAsFactors = FALSE, encoding = "CP932")
Text.text <- paste(Text.df[,1],collapse = "")
Text.splited.raw <- unlist(str_split(Text.text, pattern = ""))
Text.splited <- str_replace_all(Text.splited.raw, "｜", "") # Take out all "｜"

###grep
## Take out ruby and style notation
# Find out where to start and end
start <- grep(pattern = "《|［", Text.splited)
end <- grep(pattern = "》|］", Text.splited)
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

###Run Code if you want a cleaned txt of the text
###Change name if needed
# write.table(Text.cleaned.text,"shayo.txt",row.names = FALSE, col.names = FALSE)

### Run code if you want a wordcount table.
### Change name if needed
## A simple word count here (All punctuation, white spaces in English or Japanese format)
Text.wordcount <- str_replace_all(Text.cleaned.split, "[:punct:]", " ")
Text.wordcount <- Text.wordcount[which(Text.wordcount != " ")]
Text.wordcount <- Text.wordcount[which(Text.wordcount != "　")]
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
笑 <- grep(pattern = "笑", Text.cleaned.split.group1)

#Word with length two
笑う <- grep(pattern = "笑う", Text.cleaned.split.group2)
笑い <- grep(pattern = "笑い", Text.cleaned.split.group2)
笑っ <- grep(pattern = "笑っ", Text.cleaned.split.group2)
笑わ <- grep(pattern = "笑わ", Text.cleaned.split.group2)
笑え <- grep(pattern = "笑え", Text.cleaned.split.group2)
笑お <- grep(pattern = "笑お", Text.cleaned.split.group2)
笑む <- grep(pattern = "笑む", Text.cleaned.split.group2)
笑ん <- grep(pattern = "笑ん", Text.cleaned.split.group2)
笑顔 <- grep(pattern = "笑顔", Text.cleaned.split.group2)
笑話 <- grep(pattern = "笑話", Text.cleaned.split.group2)
笑声 <- grep(pattern = "笑声", Text.cleaned.split.group2)
微笑 <- grep(pattern = "微笑", Text.cleaned.split.group2) + 1L
嘲笑 <- grep(pattern = "嘲笑", Text.cleaned.split.group2) + 1L
苦笑 <- grep(pattern = "苦笑", Text.cleaned.split.group2) + 1L
媚笑 <- grep(pattern = "媚笑", Text.cleaned.split.group2) + 1L
可笑 <- grep(pattern = "可笑", Text.cleaned.split.group2) + 1L
一笑 <- grep(pattern = "一笑", Text.cleaned.split.group2) + 1L
憫笑 <- grep(pattern = "憫笑", Text.cleaned.split.group2) + 1L
叟笑 <- grep(pattern = "叟笑", Text.cleaned.split.group2) + 1L # For北叟笑む
失笑 <- grep(pattern = "失笑", Text.cleaned.split.group2) + 1L
の笑 <- grep(pattern = "一笑", Text.cleaned.split.group2) + 1L # This is the case when 笑 stands alone

# #Word with length three
# 笑われ <- grep(pattern = "笑われ", Text.cleaned.split.group3)
# 笑わせ <- grep(pattern = "笑わせ", Text.cleaned.split.group3)
# 
# #Word with length four
# 笑いませ <- grep(pattern = "笑いませ", Text.cleaned.split.group4)


笑.two.all <- c(笑う,笑い,笑っ,笑わ,笑む,笑声,失笑,笑ん,笑話,微笑,嘲笑,苦笑,笑顔,媚笑,可笑,一笑,の笑,笑え,憫笑,叟笑,笑お)
笑.two.positive <- c(笑う,笑い,笑っ,笑む,笑声,失笑,笑ん,微笑,笑顔,笑話,一笑,笑え,叟笑,笑お)
笑.two.negative <- c(笑わ,嘲笑,苦笑,媚笑,憫笑)
笑.two.neutral <- c(可笑,の笑)                   
笑.others <- setdiff(笑,笑.two.all)

##### Graph Section of the code
###笑 divided positive and negative as frequecy in the novel
postive.freq <- length(笑.two.positive) / length(Text.wordcount)
negative.freq <- - length(笑.two.negative) / length(Text.wordcount)
Freq.novel.df <- data.frame ("type"= c("Positive", "Negative"), "value" = c(postive.freq,negative.freq),color = c("1","2"))
Freq.novel.df$type <- as.character(Freq.novel.df$type)
ggplot(Freq.novel.df,aes(x = reorder(type,-value), y= value, fill = color))+
        geom_bar(stat="identity",color="grey50",position = "dodge", width = 1) +
        xlab("Type") + ylab("") +
        scale_fill_manual(values=c("dodgerblue2","firebrick3"),guide = FALSE) +
        ggtitle("1948_6_Ningenshikkaku")

