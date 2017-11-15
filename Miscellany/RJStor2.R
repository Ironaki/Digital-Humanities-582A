
rm(list=ls())
library(stringr)

##http://dfr.jstor.org/
##"Dataset reqeusts"

##read in the file with the metadata
citations <- read.delim("C:/Users/klijia/Desktop/HIST582A/JSTOR/Dazai Osamu/citations1.txt", stringsAsFactors=FALSE,encoding = "UTF-8") 

##extract the year
citations$pubyear <- str_extract(citations$pubdate, "^\\d{4}")

##set folder name as variable
x_name <- "C:/Users/klijia/Desktop/HIST582A/JSTOR/Dazai Osamu/wordcounts"

##get all files in that folder
files <- list.files(path = x_name)
setwd("C:/Users/klijia/Desktop/HIST582A/JSTOR/Dazai Osamu/wordcounts")


##creates a doi variable from the file name by removing prefix and suffix
word.counter <- function(x){
WORD_counter.df <- read.table(x, sep=",", stringsAsFactors=FALSE, header=TRUE)
WORD_counter.df$doi <- str_replace(x, "wordcounts_", "")
WORD_counter.df$doi <- str_replace(WORD_counter.df$doi, ".CSV", "")
WORD_counter.df$doi <- str_replace(WORD_counter.df$doi, "_", "/")
return <- WORD_counter.df
}



ptm <- proc.time()
Tokugawa.list <- lapply(files, word.counter)
proc.time() - ptm
Tokugawa.list[1]

# this method is TOO SLOW
# ptm <- proc.time()
# Tokugawa.df <- do.call(rbind, Tokugawa.list)
# proc.time() - ptm

library(data.table)
ptm <- proc.time()
Tokugawa.df <- rbindlist(Tokugawa.list)
proc.time() - ptm


library(plyr)
##map the citation information in 
colnames(Tokugawa.df) <- c("word","count","doi")

Tokugawa.df$pubyear <- Tokugawa.df$doi
Tokugawa.df$pubyear <- mapvalues(Tokugawa.df$pubyear, from=citations$doi, to=citations$pubyear)
Tokugawa.df$pubyear <- as.numeric(Tokugawa.df$pubyear)

Tokugawa.df$title <- Tokugawa.df$doi
Tokugawa.df$title <- mapvalues(Tokugawa.df$title, from=citations$doi, to=citations$title)




Tokugawa.agg.df <- aggregate(Tokugawa.df$count, by=list(Tokugawa.df$pubyear, Tokugawa.df$word), FUN=sum, na.rm=TRUE)
colnames(Tokugawa.agg.df) <- c("pubyear","word","count")
Tokugawa.agg.df$pubyear <- as.numeric(Tokugawa.agg.df$pubyear)


library(tidyr)
##fill = 0 gives 0 instead of NA
Tokugawa.agg.df <- spread(Tokugawa.agg.df, key=word, value=count, fill=0)
library(ggplot2)
ggplot(data = Tokugawa.agg.df, aes(pubyear, translation)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, shakespeare)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, seidensticker)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, keene)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, mcclellan)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, mclinney)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, a)) + geom_point()
ggplot() + geom_line(data = Tokugawa.agg.df, aes(pubyear, dazai), color = "red") +
        geom_line(data = Tokugawa.agg.df, aes(pubyear, kawabata), color = "blue") +
        geom_line(data = Tokugawa.agg.df, aes(pubyear, natsume), color = "green") +
        geom_line(data = Tokugawa.agg.df, aes(pubyear, mishima), color = "orange") +
        geom_line(data = Tokugawa.agg.df, aes(pubyear, murasaki), color = "purple") +
        geom_line(data = Tokugawa.agg.df, aes(pubyear, matsuo), color = "black")
ggplot() +geom_point(data = Tokugawa.agg.df, aes(pubyear, natsume), color = "green")

rowSums(Tokugawa.agg.df[c(2:ncol(Tokugawa.agg.df))])

## Get a word frequency table from the agg table
new.df  <- Tokugawa.agg.df
new.df  <- subset(new.df, select = -pubyear)
new.df <- data.frame(t(new.df))
new.df$sum <- rowSums(new.df)

Tokugawa.agg.perc.df <- Tokugawa.agg.df[,-1]
##too slow
#Tokugawa.agg.perc.df <- sweep(x = Tokugawa.agg.perc.df, MARGIN = 1, STATS = rowSums(Tokugawa.agg.perc.df), FUN="/", check.margin = FALSE)

ptm <- proc.time()
Tokugawa.agg.perc.df <- apply(X = Tokugawa.agg.perc.df, MARGIN = 1, FUN = function (x) x/sum(x))
Tokugawa.agg.perc.df <- t(Tokugawa.agg.perc.df*100)
Tokugawa.agg.perc.df <- data.frame(Tokugawa.agg.perc.df)
run_time <- proc.time() - ptm
run_time

Tokugawa.agg.perc.df <- cbind.data.frame(Tokugawa.agg.df[,1], Tokugawa.agg.perc.df)
colnames(Tokugawa.agg.perc.df)[1] <- "pubyear"
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, seidensticker)) + geom_line()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, war)) + geom_line() + geom_smooth()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, zen)) + geom_point() + geom_smooth()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, samurai)) + geom_line() + geom_smooth()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, guilt)) + geom_line() + geom_smooth()

# Tokugawa.agg.perc.df <-Tokugawa.agg.df[,-1]
##get rolling averages, but what about missing years?
full_seq(Tokugawa.agg.perc.df$pubyear,1) ##trick for sequence

Complete.year.df <- data.frame("pubyear"=full_seq(Tokugawa.agg.perc.df$pubyear,1))
Tokugawa.full.perc.df <- merge(Tokugawa.agg.perc.df, Complete.year.df, by = "pubyear", all = TRUE)

keepers <- c("translation","seidensticker","keene","mcclellan")
Tokugawa.full.smaller <- Tokugawa.full.perc.df[,keepers]



# code is fine BUT SLOWER than gdata option
ptm <- proc.time()
Tokugawa.full.smaller[is.na(Tokugawa.full.smaller)] <- 0
run_time <- proc.time() - ptm
run_time

##faster alternative
# library(gdata)
# ptm <- proc.time()
# Tokugawa.full.perc.NA.df <- NAToUnknown(Tokugawa.full.perc.df[,c(1:10000)], unknown = 0, force=TRUE)
# run_time <- proc.time() - ptm
# run_time

library(zoo)
Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear

mathching <- c("translation" = "black","seidensticker" = "red","keene" = "blue","mcclellan" = "green")
ggplot(Tokugawa.smaller.roll.5, aes(x=pubyear)) + 
        geom_line(aes(y = translation, color = "translation")) +
        geom_line(aes(y = seidensticker, color = "seidensticker"))+
        geom_line(aes(y = keene, color = "keene"))+
        geom_line(aes(y = mcclellan, color = "mcclellan")) +
        scale_colour_manual(name="Keywords",values = mathching)+
        xlab("Year") + ylab("Rolling Mean of Percentage over Five Years") +
        guides(col = guide_legend(reverse = TRUE))
                  

###Codes are replications from this line

ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, daimyo)) + geom_line()
ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, war)) + geom_point()
ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, zen)) + geom_point()
ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, samurai)) + geom_line() + geom_line(data = Tokugawa.full.perc.df, aes(pubyear, samurai), color="red")

##################################################


keepers <- c("japanese","american","english","chinese","korean")
Tokugawa.full.smaller <- Tokugawa.full.perc.df[,keepers]




ptm <- proc.time()
Tokugawa.full.smaller[is.na(Tokugawa.full.smaller)] <- 0
run_time <- proc.time() - ptm
run_time

Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear
mathching <- c("japanese" = "black","english" = "blue","chinese" = "red","korean" = "green")
ggplot(Tokugawa.smaller.roll.5, aes(x=pubyear)) + 
        geom_line(aes(y = japanese, color = "japanese")) +
        geom_line(aes(y = english, color = "english"))+
        geom_line(aes(y = chinese, color = "chinese")) +
        geom_line(aes(y = korean, color = "korean")) +
        scale_colour_manual(name="Keywords",values = mathching)+
        xlab("Year") + ylab("Rolling Mean of Percentage over Five Years") +
        guides(col = guide_legend(reverse = FALSE))

###################################################

keepers <- c("war","moral","love","death")
Tokugawa.full.smaller <- Tokugawa.full.perc.df[,keepers]

Tokugawa.full.smaller[is.na(Tokugawa.full.smaller)] <- 0


Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear

mathching <- c("death" = "black","moral" = "blue","love" = "red","war" = "green")
ggplot(Tokugawa.smaller.roll.5, aes(x=pubyear)) + 
        geom_line(aes(y = death, color = "death")) +
        geom_line(aes(y = moral, color = "moral"))+
        geom_line(aes(y = love, color = "love")) +
        geom_line(aes(y = war, color = "war")) +
        scale_colour_manual(name="Keywords",values = mathching)+
        xlab("Year") + ylab("Rolling Mean of Percentage over Five Years") +
        guides(col = guide_legend(reverse = FALSE))

##########################################################


keepers <- c("social","political","economic","historical")
Tokugawa.full.smaller <- Tokugawa.full.perc.df[,keepers]

Tokugawa.full.smaller[is.na(Tokugawa.full.smaller)] <- 0


Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear

mathching <- c("social" = "black","political" = "blue","economic" = "red","historical" = "green")
ggplot(Tokugawa.smaller.roll.5, aes(x=pubyear)) + 
        geom_line(aes(y = social, color = "social")) +
        geom_line(aes(y = political, color = "political"))+
        geom_line(aes(y = economic, color = "economic")) +
        geom_line(aes(y = historical, color = "historical")) +
        scale_colour_manual(name="Keywords",values = mathching)+
        xlab("Year") + ylab("Rolling Mean of Percentage over Five Years") +
        guides(col = guide_legend(reverse = FALSE))


##########################################################


keepers <- c("natsume","dazai","tanizaki","akutagawa","kawabata","matsuo","chikamatsu","murasaki")
Tokugawa.full.smaller <- Tokugawa.full.perc.df[,keepers]

Tokugawa.full.smaller[is.na(Tokugawa.full.smaller)] <- 0


Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear

mathching <- c("natsume" = "red4","dazai" = "black","tanizaki" = "orange4","akutagawa" = "orange", "kawabata" = "red", "matsuo" = "seagreen4","chikamatsu" = "palegreen","murasaki" = "royalblue")
ggplot(Tokugawa.smaller.roll.5, aes(x=pubyear)) + 
        geom_line(aes(y = natsume, color = "natsume")) +
        geom_line(aes(y = dazai, color = "dazai"))+
        geom_line(aes(y = tanizaki, color = "tanizaki")) +
        geom_line(aes(y = akutagawa, color = "akutagawa")) +
        geom_line(aes(y = kawabata, color = "kawabata")) +
        geom_line(aes(y = matsuo, color = "matsuo")) +
        geom_line(aes(y = chikamatsu, color = "chikamatsu")) +
        geom_line(aes(y = murasaki, color = "murasaki")) +
        scale_colour_manual(name="Keywords",values = mathching)+
        xlab("Year") + ylab("Rolling Mean of Percentage over Five Years") +
        guides(col = guide_legend(reverse = FALSE))