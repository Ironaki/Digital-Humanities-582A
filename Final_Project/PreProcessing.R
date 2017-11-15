##### This is just a loop for reading multiple original files and processing

file.org.dir<- list.files("D:/Google Drive/JPN_LIT/Natsume/original")
file.dir <- paste("D:/Google Drive/JPN_LIT/Natsume/original/",list.files("D:/Google Drive/JPN_LIT/Natsume/original"), sep ="")

file.i <- 1
for (file.i in 1:length(file.dir)){
        Text.df <- read.delim(file.dir[file.i], header = FALSE, stringsAsFactors = FALSE, encoding = "SHIFT-JIS")
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
        
        Text.cleaned.text <- paste(CUT.fine.df$text, collapse = "") 
        
        write.table(Text.cleaned.text,file.org.dir[file.i],row.names = FALSE, col.names = FALSE)
}