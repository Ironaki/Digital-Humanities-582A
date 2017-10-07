#####A bunch of useful code
rm(list=ls())
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)


##### PART I: DIVEDE INTO CHAPTERS
### Function to read the text
my.scan <- function(x){
        Raw_Text.scan <- scan(paste("/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W3/Raw Text/", x, sep = ""),what="character",sep = "\n")
        Raw_Text.df <- data.frame(Raw_Text.scan, stringsAsFactors = FALSE)
}

Pride.raw <- my.scan("Pride.txt")
Pride.cut <- Pride.raw[c(16:10734),]

### Find the start and the end line
chapter.line <- grep("^Chapter\\s[[:digit:]]+", Pride.cut)
start.line <- chapter.line + 1
end.line <- chapter.line[2:length(chapter.line)] - 1
end.line <- c(end.line, length(Pride.cut))

Pride.df <- data.frame("Start" = start.line,"End" = end.line, "Text" = NA)

i <- 1
for (i in 1:length(Pride.df$End)){
        Pride.df$Text[i] <- paste(Pride.cut[Pride.df$Start[i]:Pride.df$End[i]], collapse = " ")
}

### Now the text is nicely cut in chapters

##### PART II: SCATTERPLOT of USE OF THE WORD IN EACH CHAPTER
Pride.df$letter <- str_count(Pride.df$Text,"\\bletter\\b | \\bletters\\b | \\bLetter\\b | \\bLetters\\b")
Pride.df$count <- str_count(Pride.df$Text,"[[:alpha:]]+")
Pride.df$Chapter <- 1:61

ggplot(Pride.df, aes(Chapter, letter,color=non.null)) +
        geom_point(color = "orange") +
        xlab("Chapter Number") +
        ylab("Use of 'letter(s)'")
        
Pride.df$letter.per <- Pride.df$letter/Pride.df$count*100

###Another graph in percentage
ggplot(Pride.df, aes(Chapter, letter.per,color=non.null)) +
        geom_point(color = "orange") +
        xlab("Chapter Number") +
        ylab("Use of 'letter(s)' in percentage")


##### PART3: KWIC OF THE WORD LETTER
Pride.kwic <- paste(Pride.cut, collapse = " ")
Pride.kwic.fine <- unlist(str_split(Pride.kwic , "\\W"))
location.kwic <- which(Pride.kwic.fine == "letter" | Pride.kwic.fine == "letters"| Pride.kwic.fine == "Letters" | Pride.kwic.fine == "Letter")

start.kwic <- location.kwic - 5 ## Change 5 to any numbers
end.kwic <- location.kwic + 5 ## Change 5 to any numbers
start.kwic <- ifelse(start.kwic > 0, start.kwic, 0)
end.kwic <- ifelse(end.kwic < length(Pride.kwic.fine), end.kwic, length(Pride.kwic.fine))

KWIC.letter.df <- data.frame("Start" = start.kwic, "End" = end.kwic, "Text" =NA)
k <- 1
for(k in 1:length(KWIC.letter.df$End)){
        text <- Pride.kwic.fine[KWIC.letter.df$Start[k]:KWIC.letter.df$End[k]]
        KWIC.letter.df$Text [k] <- paste(text, collapse = " ")
}

write.table(KWIC.letter.df,"/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W3/KWIC_letter.txt",sep = "\t")
