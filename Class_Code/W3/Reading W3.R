#####A bunch of useful code
rm(list=ls())
library(stringr)

###CH 3.1 Data Types
#Create the POLITICS data frame
States <- c("Montana","California","New Jersey","Delaware")
No_Counties <- c(56,58,21,3)
POLITICS.df <- cbind.data.frame(States, No_Counties)

# try to plot. Sequence is decided by alphabetical order
plot(POLITICS.df$States,POLITICS.df$No_Counties)

# plot with order of number of counties
POLITICS.df$States.ord <- factor(POLITICS.df$States, levels = POLITICS.df$States[order(POLITICS.df$No_Counties)])
plot(POLITICS.df$States.ord,POLITICS.df$No_Counties)

###CH 3.2 Data Structure
vec_one <- c(1,5,7)
vec_two <- c(1,"5",7)

list_one <- list("a",19,vec_one,vec_two)
str(list_one)

list_two <- list("a",19,vec_one,vec_two,list_one)
str(list_two)

##? I want to know the difference between single and double brackets.
list_one[[1]]
list_two[[5]][[2]]

###CH 3.3 Regex
# Take out the punctuation and spce from the string
example <- "My brother's dog hates semi-colons; others love them. Is it fate?"
some.text <- str_split(example,"[:punct:]|[:space:]")

# The example shows that [] has a implicit | for objects inside
example <- "There is strange nonsense Z in the middle F of this sentence"
unlist(str_split(string = example, pattern = '[ZF]'))
unlist(str_split(string = example, pattern = '(Z|F)'))

#Note the difference when I extract with and without a bracket
str_extract_all(example,"m.")
str_extract_all(example, "m[.]")

###Play with sonnets again
#Find the line numbers of number of sonnets
SONNETS.lines.scan <- scan("http://history.emory.edu/RAVINA/HIST_582/Data/Sonnets_raw.txt",what="character", sep="\n")
SONNETS.lines.df <- data.frame(SONNETS.lines.scan[c(166:2474)])
grep(pattern="[0-9]", SONNETS.lines.df[,1])

#Another experiment with the Time Machine
text.scan <- scan("http://history.emory.edu/RAVINA/HIST_582/Data/Time_Machine.txt",what="character", sep="\n")

start.line <- which(text.scan=="The Time Machine, by H. G. Wells [1898]")
end.line <- which(text.scan=="of man.")
novel.lines <- text.scan[start.line:end.line]


#We are finding chapter title number lines, ^ means start of a line and $ means end of a line.
length(grep("^[I|V|X]*$",novel.lines))

###CH 3.4
# Try to break the book into chapters
chapter.headings <- grep("^[I|V|X]*$",novel.lines)
start.lines <- chapter.headings + 1

#It's trickier to find the endline of the last chapter.
end.lines <- chapter.headings[2:length(chapter.headings)]-1
end.lines <- c(end.lines, length(novel.lines))

chapter.one <- novel.lines[start.lines[1]:end.lines[1]]
chapter.one <- paste(novel.lines[start.lines[1]:end.lines[1]],collapse = " ")

#loop
# for(i in start.number:end.number){function.to.execute}
Time.machine.df <- data.frame("start" = start.lines, "end" = end.lines, "text" = NA)

i <- 1
for (i in 1:length(Time.machine.df$end))
  {Time.machine.df$text[i] <- paste(
    novel.lines[Time.machine.df$start[i]:Time.machine.df$end[i]],
    collapse = " ")
  }

Time.machine.df$time <- str_count(Time.machine.df$text,"\\btime\\b|\\bTime\\b")

Time.machine.df$chapter <- seq(1,12,1)
plot(Time.machine.df$chapter,Time.machine.df$time)


###CH 3.5 KWIC and loops
novel.total <- paste(novel.lines, collapse =" ")
length(novel.total)

nchar(novel.total)
novel.total <- tolower(novel.total)

novel.words <- unlist (str_split(novel.total,"\\W"))
length(novel.words)
novel.words <- novel.words[which(novel.words !="")]
length(novel.words)

#now we try to locate the words before and after the key word "man"
location.kwic <- which(novel.words == "man")

start.kwic <- location.kwic - 5
end.kwic <-  location.kwic +5

start.kwic <- ifelse(start.kwic > 0, start.kwic, 0)
end.kwic <-  ifelse(end.kwic<length(novel.words),end.kwic,length(novel.words))

KWIC.df <- data.frame("start" = start.kwic, "end" = end.kwic, "text" = NA)
i <- 1
for(i in 1:length(KWIC.df$start)){
  text <- novel.words[KWIC.df$start[i]:KWIC.df$end[i]]
  KWIC.df$text[i] <- paste(text, collapse = " ")
}


# Another Graph
index.no <- which(novel.words == "man")
context.count <- str_count(KWIC.df$text, "medical|silent")

plot(index.no,context.count)

###CH 3.6 User-defined Functions
plus.seven <- function(x){
        x + 7
}

#Try to create a function that create KWICth
KWIC <- function(x, y, z)  {
        locations.kwic <- which(x == y)
        start.kwic <- locations.kwic - z
        end.kwic <- locations.kwic + z
        starts.kwic <- ifelse(start.kwic > 0, start.kwic, 0)
        ends.kwic <- ifelse(end.kwic < length(x), end.kwic, length(x))
        
        KWIC.df <- data.frame("start" = start.kwic, "end" = end.kwic, "text" = NA)
        i <- 1
        i <- for (i in 1:length(KWIC.df$start)){
                text <- x[KWIC.df$start[i]:KWIC.df$end[i]]
                KWIC.df$text[i] <- paste(text, collapse = " ")
        }
        return(KWIC.df)
}


kwic.woman <- KWIC(novel.words, "woman", 5)

# Create a function that allow not so strict search. We basically
# change the second line, "which" to "grep"

KWIC.NS <- function(x, y, z)  {
        locations.kwic <- grep(y, x)
        start.kwic <- locations.kwic - z
        end.kwic <- locations.kwic + z
        starts.kwic <- ifelse(start.kwic > 0, start.kwic, 0)
        ends.kwic <- ifelse(end.kwic < length(x), end.kwic, length(x))
        
        KWIC.df <- data.frame("start" = start.kwic, "end" = end.kwic, "text" = NA)
        i <- 1
        i <- for (i in 1:length(KWIC.df$start)){
                text <- x[KWIC.df$start[i]:KWIC.df$end[i]]
                KWIC.df$text[i] <- paste(text, collapse = " ")
        }
        return(KWIC.df)
}

KWIC.df <- KWIC.NS(novel.words, "child.*",5)
kwic.string <- paste(KWIC.df$text, collapse =" ")
kwic.words <- unlist(str_split(kwic.string, "\\W"))
KWIC.freq.df <- data.frame(table(kwic.words))
KWIC.freq.df <- KWIC.freq.df[order(-KWIC.freq.df$Freq),]
KWIC.freq.df[c(1:30),]
