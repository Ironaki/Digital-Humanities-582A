#####A bunch of useful code
rm(list=ls())
library(stringr)
library(ggplot2)
library(tidyr)
library(plotly)

## CH 4.3 Visualization
# Let me get my SONNETS.df
SONNETS.df <- read.table("http://history.emory.edu/RAVINA/HIST_582/Data/Shakespeare.txt",header = TRUE, stringsAsFactors = FALSE)


SONNETS.df$eye <- str_count(SONNETS.df$text,"\\beye\\b")
SONNETS.df$heart <- str_count(SONNETS.df$text,"\\bheart\\b")
SONNETS.df$count <- str_count(SONNETS.df$text,"[[:alpha:]]+") #? Why is this world count

SONNETS.df$eye.per <- (SONNETS.df$eye/SONNETS.df$count)*100
SONNETS.df$heart.per <- (SONNETS.df$heart/SONNETS.df$count)*100

#Graph, pay attention to the axes

ggplot(data = SONNETS.df, aes(x=heart.per,y=eye.per)) + geom_point(color = "red")

# The one above is not fitted into a linear model, while the one below is. The one below also have titles
# SE in the last argument is for confidence interval
# You should put the "+" at the end of each line
ggplot(data = SONNETS.df, aes(x=heart.per,y=eye.per)) + 
        geom_point(color = "orange") +
        geom_smooth(method = "lm", se = FALSE) + 
        xlab("'Heart' as Percentage of Text") + 
        ylab("'Eye' as Percentage of Text") + 
        ggtitle (("Word Frequency in the Shakespeare Sonnets"))

# Try group the result into two groups "N" and "Y"
SONNETS.df$non.null <- ifelse((SONNETS.df$eye.per > 0 & SONNETS.df$heart.per > 0),"Y","N")

ggplot(data = SONNETS.df, aes(x=heart.per,y=eye.per, color = non.null)) +
        geom_point(color = "orange") +
        geom_smooth(method = "lm", se = FALSE) + 
        xlab("'Heart' as Percentage of Text") + 
        ylab("'Eye' as Percentage of Text") + 
        theme(legend.position = "none" ) + 
        scale_color_manual(values = c("blue", "red")) + 
        ggtitle (("Word Frequency in the Shakespeare Sonnets"))

# Trying to draw a hisogram
ggplot(data = SONNETS.df, aes(heart.per)) +
        geom_histogram(binwidth = 0.35)+
        xlab("'Heart' as Percentage of Text") +
        ylab("Number of Sonnets") +
        ggtitle("Word Frequency in the Shakespeare Sonnets") 
        
# Doing something new. Note that its not percentage here.
SONNETS.smaller.df <- SONNETS.df [, c(2:3)]
SONNETS.smaller.df$count <-
        str_count(SONNETS.smaller.df$text, "[[:alpha:]]+") ##word count
SONNETS.smaller.df$thy <-
        str_count(SONNETS.smaller.df$text, "\\bthy\\b")*100/SONNETS.smaller.df$count
SONNETS.smaller.df$love <-
        str_count(SONNETS.smaller.df$text, "\\blove\\b")*100/SONNETS.smaller.df$count
SONNETS.smaller.df$eye <-
        str_count(SONNETS.smaller.df$text, "\\beye\\b")*100/SONNETS.smaller.df$count
SONNETS.smaller.df$heart <-
        str_count(SONNETS.smaller.df$text, "\\bheart\\b")*100/SONNETS.smaller.df$count
SONNETS.smaller.df$heart <-
        str_count(SONNETS.smaller.df$text, "\\bheart\\b")*100/SONNETS.smaller.df$count

#? Graph, I don't really understand the gather function here
SONNETS.long.df <- gather(data=SONNETS.smaller.df, key=word, value=freq, thy:heart)


# Three different graphs
ggplot(data=SONNETS.long.df, aes(x=word, y=freq)) +
        geom_point() +coord_flip()

ggplot(data=SONNETS.long.df, aes(x=word, y=freq)) +
        geom_boxplot() +coord_flip()

# Export the last one
pdf("test.pdf", width = 7.5, height = 10)
ggplot(data=SONNETS.long.df, aes(x=word, y=freq)) +
        geom_violin() +coord_flip()
dev.off()

# Using plotly
plot_ly(data = SONNETS.df, x = eye.per, y = heart.per, type = "scatter", mode = "markers", text = title)




