rm (list = ls())
library(stringr)
PRIDE.scan <- scan("/Volumes/Files/Learning Material/HIST582A/W2/Raw Text/Pride.txt",what="character",sep = "\n")
PRIDE.df <- data.frame(PRIDE.scan, stringsAsFactors = FALSE)

PRIDE.t <- PRIDE.df[c(16:10734),]
PRIDE.string <- paste(PRIDE.t, collapse= " ")

PRIDE.words <- str_split(string = PRIDE.string, pattern = " ")
PRIDE.words.good <- unlist(PRIDE.words) 

# Take out punctuation before take out empty string ""
# Since there are words consist only punctuations
PRIDE.words.good1 <- str_replace_all(PRIDE.words.good,"[:punct:]","")
PRIDE.words.good2 <- PRIDE.words.good1[which(PRIDE.words.good1 != "")]
PRIDE.words.goodF <- tolower(PRIDE.words.good2)

PRIDE.df <- data.frame(table(PRIDE.words.goodF))
PRIDE.ord.df <- PRIDE.df [order(-PRIDE.df$Freq),]
colnames(PRIDE.ord.df)[1] <- "Word"

# For some reason, the first column of the df is factor. Next line tries to
# convert it into character.
PRIDE.ord.df$Word <- as.character(PRIDE.ord.df$Word)


write.table(PRIDE.ord.df,"C:/Users/klijia/Desktop/HIST582A/W2/Freq/A Tale_Freq.txt",sep = "\t")
