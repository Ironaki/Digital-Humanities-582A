SONNETS.df <- read.table("http://history.emory.edu/RAVINA/HIST_582/Data/Shakespeare.txt",header = TRUE, stringsAsFactors = FALSE)
SONNETS.df$title

typeof(SONNETS.df)

SONNETS.df$number

klijia.df <- 5
typeof(klijia.df)

str_count(SONNETS.df$text, "love")

library('stringr')
str_count(SONNETS.df$text, "love")

str_count(SONNETS.df$text,"the")  

str_count(SONNETS.df$text,'\\wthe\\w')

SONNETS.df$love <- str_count(SONNETS.df$text,"love")

SONNETS.df$fear <- str_count(SONNETS.df$text,"fear")

SONNETS.df$definite_article <- str_count(SONNETS.df$text,"\\wthe\\w")

typeof(SONNETS.df$definite_article)

SONNETS.df[1,2]

SONNETS.df[1, "title"]

SONNETS.df[c(1,3),"title"]

SONNETS.df[c(3:7),"title"]

SONNETS.df[-c(3:148),"title"]

SONNETS.df[-c(3:148),c(1)]

SONNETS.df[c(1:3,5:8),"title"]

SONNETS.df[c(1:5),]

SONNETS.df[,"title"]

SONNETS.df[c(7:9),"title"]

SONNETS.df[16,"text"]

SONNETS.df[-c(4:54),"fear"]

SONNETS.df$definite_article

SONNETS.df[which(SONNETS.df$definite_article == 3), c(2,7)]

SONNETS.df_subset <- SONNETS.df[which(SONNETS.df$love>=3),]

typeof(SONNETS.df_subset)

SONNETS.df_subset

View(SONNETS.df_subset)

States <- c("Montana","California","New Jersey","Delaware")
No_Counties <- c(56,58,21,3)

typeof(States)
typeof(No_Counties)

length(States)

POLITICS.df <- cbind.data.frame(States,No_Counties)

View(POLITICS.df)

No_haha <- c(13,23,42,14)
SHOULDNOTWORK.df <- cbind.data.frame(States,No_haha)

nrow(POLITICS.df)
## [1] 4
ncol(POLITICS.df)
## [1]

dim(POLITICS.df)

dim(SONNETS.df)
States[2]

STATES.df <- cbind.data.frame(States)
View(STATES.df)

STATES.df[2,1]

typeof(STATES.df$States)

STATES.df

STATES.df[3,]

MIX <- c("xj",1,TRUE)
rm(MIX)

View(SONNETS.df)
write.table(SONNETS.df,"SONNETS.txt",sep="\t")

write.table(SONNETS.df,"SONNETS.txt",sep="\t",row.names = F)

write.table(POLITICS.df,"Pol.txt",sep="\t",row.names = F) #Note the row.names should be false

3 <- m
3 -> m

str_count(SONNETS.df$text,"eye")

SONNETS.eye.df <- SONNETS.df[which(str_count(SONNETS.df$text,"eye") >= 1),c(1:5)]
View(SONNETS.eye.df)

SONNETS.eye.love.df <- SONNETS.eye.df[which(SONNETS.eye.df$love >= 1),]
View(SONNETS.eye.love.df)

SONNETS.eye.love.df.thy <- SONNETS.eye.love.df[which((str_count(SONNETS.eye.love.df$text,"thy")) >= 1),]
View(SONNETS.eye.love.df.thy)



SONNETS.df <- read.delim("/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W2/SONNETS.txt",stringsAsFactors=FALSE)

write.table(SONNETS.df,"/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W2/try1.txt",sep="\t")

SONNETS.df <- read.delim("/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W2/SONNETS.txt",stringsAsFactors=FALSE)

SONNETS.df <- read.table("/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W2/SONNETS.txt", sep="\t",stringsAsFactors=FALSE,header = T)




SONNETS.df <- read.table("/Users/Armstrong/Google_Drive/Learning_Material/HIST582A/Class_Code/W2/SONNETS.txt", sep="\t",stringsAsFactors=FALSE,header = T)

table(SONNETS.df$love, SONNETS.df$fear)

xtabs(~SONNETS.df$fear+SONNETS.df$love)

xtabs(data=SONNETS.df, ~ love + fear) 
xtabs(~ SONNETS$love + SONNETS$fear)

SONNET_TABLE <- table(SONNETS.df$love, SONNETS.df$fear)
SONNET_TABLE
write.table(SONNET_TABLE,"SONNETS.csv",sep=",",col.names = NA)


SONNETS.lines.scan <- scan("http://history.emory.edu/RAVINA/HIST_582/Data/Sonnets_raw.txt",what="character", sep="\n")

SONNETS.lines.df <- data.frame(SONNETS.lines.scan,stringsAsFactors = FALSE)


SONNETS.lines <- SONNETS.lines.df[c(166:2474),]

Sonnets.string <- paste(SONNETS.lines, collapse=" ")



Sonnets.words <- str_split(string=Sonnets.string, pattern = " ")

Sonnets.words <- unlist(Sonnets.words)

Sonnets.freq.df <- data.frame(table(Sonnets.words))

Sonnets.words <- Sonnets.words[which(Sonnets.words !="")]


SONNETS.words.df <- data.frame(Sonnets.words)
SONNETS.words.df$lower <- tolower(SONNETS.words.df[,1])

colnames(SONNETS.words.df)[1] <-"words"

SONNETS.words.df$clean_text <- str_replace_all(SONNETS.words.df$words,"[:punct:]","")

SONNETS.words.df$cleaned <- str_replace_all(SONNETS.words.df$lower,"[:punct:]","")
SONNETS.clean.tbl.df <- data.frame(table(SONNETS.words.df$cleaned))

SONNETS.clean.tbl.ord.df <- SONNETS.clean.tbl.df[order(-SONNETS.clean.tbl.df$Freq),]
View(SONNETS.clean.tbl.ord.df)
SONNETS.clean.tbl.ord.df[1,1]

HAMLET.cleaned.tbl.ord.df <-read.table(
  "http://history.emory.edu/RAVINA/HIST_582/Data/HAMLET_table.txt",
  header = TRUE, stringsAsFactors = FALSE)

View(HAMLET.cleaned.tbl.ord.df)

colnames(SONNETS.clean.tbl.ord.df) <- c("Words","Freq")
colnames(HAMLET.cleaned.tbl.ord.df) <- c("Words","Freq")



SONNETS.cleaned.tbl.ord.df <- SONNETS.clean.tbl.df

colnames(SONNETS.cleaned.tbl.ord.df) <- c("Words","Freq")
SONNETS.cleaned.tbl.ord.df <- SONNETS.cleaned.tbl.ord.df[order(-SONNETS.cleaned.tbl.ord.df$Freq),]

class(SONNETS.clean.tbl.ord.df$Var1)



intersect(SONNETS.cleaned.tbl.ord.df$Words[1:50],HAMLET.cleaned.tbl.ord.df$Words[1:50])

setdiff(HAMLET.cleaned.tbl.ord.df$Words[1:50],
        SONNETS.cleaned.tbl.ord.df$Words[1:50])

setdiff(SONNETS.cleaned.tbl.ord.df$Words[1:50],
        HAMLET.cleaned.tbl.ord.df$Words[1:50])

SONNETS.cleaned.tbl.ord.df[which(SONNETS.cleaned.tbl.ord.df$Words[1:20]
                                 %in% HAMLET.cleaned.tbl.ord.df$Words[1:20]),]

SONNETS.cleaned.tbl.ord.df[which(!SONNETS.cleaned.tbl.ord.df$Words[1:20]
                                 %in% HAMLET.cleaned.tbl.ord.df$Words[1:20]),]

HAMLET.cleaned.tbl.ord.df[1,1]
class(HAMLET.cleaned.tbl.ord.df$Words)
