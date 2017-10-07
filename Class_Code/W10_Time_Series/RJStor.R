
rm(list=ls())
library(stringr)

##http://dfr.jstor.org/
##"Dataset reqeusts"

##read in the file with the metadata
citations <- read.delim("~/Downloads/2016.8.23.eBAEvJN2/citations.tsv", stringsAsFactors=FALSE)
##extract the year
citations$pubyear <- str_extract(citations$pubdate, "^\\d{4}")

##set folder name as variable
x_name <- "~/Downloads/2016.8.23.eBAEvJN2/wordcounts"

##get all files in that folder
files <- list.files(path = x_name)
setwd("~/Downloads/2016.8.23.eBAEvJN2/wordcounts/")


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


Tokugawa.agg.df <- aggregate(Tokugawa.df$count, by=list(Tokugawa.df$pubyear, Tokugawa.df$word), FUN=sum, na.rm=TRUE)
colnames(Tokugawa.agg.df) <- c("pubyear","word","count")
Tokugawa.agg.df$pubyear <- as.numeric(Tokugawa.agg.df$pubyear)


library(tidyr)
##fill = 0 gives 0 instead of NA
Tokugawa.agg.df <- spread(Tokugawa.agg.df, key=word, value=count, fill=0)
library(ggplot2)
ggplot(data = Tokugawa.agg.df, aes(pubyear, religion)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, war)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, zen)) + geom_point()
ggplot(data = Tokugawa.agg.df, aes(pubyear, a)) + geom_point()
rowSums(Tokugawa.agg.df[c(2:ncol(Tokugawa.agg.df))])

Tokugawa.agg.perc.df <- Tokugawa.agg.df[,-1]
##too slow
# Tokugawa.agg.perc.df <- sweep(x = Tokugawa.agg.perc.df, MARGIN = 1, STATS = rowSums(Tokugawa.agg.perc.df), FUN="/", check.margin = FALSE)

ptm <- proc.time()
Tokugawa.agg.perc.df <- apply(X = Tokugawa.agg.perc.df, MARGIN = 1, FUN = function (x) x/sum(x))
Tokugawa.agg.perc.df <- t(Tokugawa.agg.perc.df*100)
Tokugawa.agg.perc.df <- data.frame(Tokugawa.agg.perc.df)
run_time <- proc.time() - ptm
run_time

Tokugawa.agg.perc.df <- cbind.data.frame(Tokugawa.agg.df[,1], Tokugawa.agg.perc.df)
colnames(Tokugawa.agg.perc.df)[1] <- "pubyear"
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, religion)) + geom_point()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, war)) + geom_point()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, zen)) + geom_point()
ggplot(data = Tokugawa.agg.perc.df, aes(pubyear, samurai)) + geom_point()

# Tokugawa.agg.perc.df <-Tokugawa.agg.df[,-1]
##get rolling averages, but what about missing years?
full_seq(Tokugawa.agg.perc.df$pubyear,1) ##trick for sequence

Complete.year.df <- data.frame("pubyear"=full_seq(Tokugawa.agg.perc.df$pubyear,1))
Tokugawa.full.perc.df <- merge(Tokugawa.agg.perc.df, Complete.year.df, by = "pubyear", all = TRUE)

keepers <- c("war","zen","samurai","daimyo","bushido","industry","finance")
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

Tokugawa.smaller.roll.5 <- data.frame(rollmean(Tokugawa.full.smaller, k=5, fill = list(NA, NULL, NA)))


Tokugawa.smaller.roll.5$pubyear <- Tokugawa.full.perc.df$pubyear

ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, war)) + geom_point()
ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, zen)) + geom_point()
ggplot(data = Tokugawa.smaller.roll.5, aes(pubyear, samurai)) + geom_line() + geom_line(data = Tokugawa.full.perc.df, aes(pubyear, samurai), color="red")





