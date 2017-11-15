## Use RMeCabFreq to read in a file and create a frequency data frame
Kokoro_ge.df <- RMeCabFreq("D:/Google Drive/JPN_LIT/Natsume/combine/kokoro_ge_cleaned.txt")
Kokoro_chuu.df <- RMeCabFreq("D:/Google Drive/JPN_LIT/Natsume/combine/kokoro_chuu_cleaned.txt")
Kokoro_jyou.df <- RMeCabFreq("D:/Google Drive/JPN_LIT/Natsume/combine/kokoro_jyou_cleaned.txt")


Kokoro.df <- RMeCabFreq("D:/Google Drive/JPN_LIT/Natsume/combine/kokoro.txt")


death.jyou.df <- data.frame("chapter" = 0,"freq" = 0,"perc" =0)
for (ch in 1:36){
        Kokoro_jyoutemp.df <- RMeCabFreq(paste("D:/Google Drive/JPN_LIT/Natsume/kokoro_jyou",ch,".txt",sep =""))
        Kokoro_jyoutemp_reduced.df <- Kokoro_jyoutemp.df[which(Kokoro_jyoutemp.df$Info1 != "記号"),]
        death.jyoutemp.sum <- sum(Kokoro_jyoutemp_reduced.df$Freq[grep(pattern = "死|亡",Kokoro_jyoutemp_reduced.df$Term)])
        death.jyoutemp.perc <- death.jyoutemp.sum/sum(Kokoro_jyoutemp_reduced.df$Freq)*100
        death.jyou.df <- rbind(death.jyou.df, c(ch,death.jyoutemp.sum,death.jyoutemp.perc))
}
death.jyou.df <- death.jyou.df[-1,]



ggplot(death.jyou.df,aes(chapter,perc)) + geom_point()

# kokoro_ge.adj.df <- Kokoro_ge.df[which(Kokoro_ge.df$Info1 == "形容詞"),]
# kokoro_ge.adj.df <- Kokoro_ge.df[which(Kokoro_ge.df$Info1 == "形容詞"),]


## The reduced table takes out space and punctuations
Kokoro_jyou_reduced.df <- Kokoro_jyou.df[which(Kokoro_jyou.df$Info1 != "記号"),]
Kokoro_chuu_reduced.df <- Kokoro_chuu.df[which(Kokoro_chuu.df$Info1 != "記号"),]
Kokoro_ge_reduced.df <- Kokoro_ge.df[which(Kokoro_ge.df$Info1 != "記号"),]
Kokoro_reduced.df <- Kokoro.df[which(Kokoro.df$Info1 != "記号"),]

Kokoro_jyou_reduced_ordered.df <- Kokoro_jyou_reduced.df[order(-Kokoro_jyou_reduced.df$Freq),]
Kokoro_chuu_reduced_ordered.df <- Kokoro_chuu_reduced.df[order(-Kokoro_chuu_reduced.df$Freq),]
Kokoro_ge_reduced_ordered.df <- Kokoro_ge_reduced.df[order(-Kokoro_ge_reduced.df$Freq),]
Kokoro_reduced_ordered.df <- Kokoro_reduced.df[order(-Kokoro_reduced.df$Freq),]

death.jyou.sum <- sum(Kokoro_jyou_reduced.df$Freq[grep(pattern = "死",Kokoro_jyou_reduced.df$Term)])
perc.death.jyou <- death.jyou.sum/sum(Kokoro_jyou_reduced.df$Freq)
death.chuu.sum <- sum(Kokoro_chuu_reduced.df$Freq[grep(pattern = "死",Kokoro_chuu_reduced.df$Term)])
perc.death.chuu <- death.chuu.sum /sum(Kokoro_chuu_reduced.df$Freq)
death.ge.sum <- sum(Kokoro_ge_reduced.df$Freq[grep(pattern = "死",Kokoro_ge_reduced.df$Term)])
perc.death.ge <- death.ge.sum/sum(Kokoro_ge_reduced.df$Freq)


setdiff(Kokoro_ge_reduced_ordered.df$Term[1:100],Kokoro_chuu_reduced_ordered.df$Term[1:100])
setdiff(Kokoro_chuu_reduced_ordered.df$Term[1:100],Kokoro_ge_reduced_ordered.df$Term[1:100])
setdiff(Kokoro_jyou_reduced_ordered.df$Term[1:100],Kokoro_ge_reduced_ordered.df$Term[1:100])
intersect(Kokoro_jyou_reduced_ordered.df$Term[1:100],Kokoro_chuu_reduced_ordered.df$Term[1:100])
intersect(Kokoro_jyou_reduced_ordered.df$Term[1:100],Kokoro_ge_reduced_ordered.df$Term[1:100])
intersect(intersect(Kokoro_jyou_reduced_ordered.df$Term[1:100],Kokoro_chuu_reduced_ordered.df$Term[1:100]),Kokoro_ge_reduced_ordered.df$Term[1:100])



#####Attempt to do a document term matrix for kokoro
Kokoro.DTM.df <- Kokoro_reduced.df
Kokoro.DTM.df$Freq1 <- 0 
Kokoro.DTM.df$Freq2 <- 0
Kokoro.DTM.df$Freq3 <- 0

Kokoro.DTM.df  <- rbind(Kokoro.DTM.df[1:3140,],c("液","名詞" ,"一般",1,0,0,0),Kokoro.DTM.df[3141:6074,])
Kokoro.DTM.df  <- rbind(Kokoro.DTM.df[1:4260,],c("唾","名詞" ,"一般",1,0,0,0),Kokoro.DTM.df[4261:6075,])

z <- 1
y <- 1
while (z <= length(Kokoro.DTM.df$Term)){
        if(Kokoro_jyou_reduced.df$Term[y] == Kokoro.DTM.df$Term[z] & Kokoro_jyou_reduced.df$Info1[y] == Kokoro.DTM.df$Info1[z] & Kokoro_jyou_reduced.df$Info2[y] == Kokoro.DTM.df$Info2[z]){
                Kokoro.DTM.df$Freq1[z] <- Kokoro_jyou_reduced.df$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}


z <- 1
y <- 1
while (z <= length(Kokoro.DTM.df$Term)){
        if(Kokoro_chuu_reduced.df$Term[y] == Kokoro.DTM.df$Term[z] & Kokoro_chuu_reduced.df$Info1[y] == Kokoro.DTM.df$Info1[z] & Kokoro_chuu_reduced.df$Info2[y] == Kokoro.DTM.df$Info2[z]){
                Kokoro.DTM.df$Freq2[z] <- Kokoro_chuu_reduced.df$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(Kokoro.DTM.df$Term)){
        if(Kokoro_ge_reduced.df$Term[y] == Kokoro.DTM.df$Term[z] & Kokoro_ge_reduced.df$Info1[y] == Kokoro.DTM.df$Info1[z] & Kokoro_ge_reduced.df$Info2[y] == Kokoro.DTM.df$Info2[z]){
                Kokoro.DTM.df$Freq3[z] <- Kokoro_ge_reduced.df$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

Real.TMD <- Kokoro.DTM.df[,c(5:7)]
Real.TMD$Dfreq <- apply(Real.TMD, 1, function(x) (1+length(which(x != 0))))
Real.TMD$Dfnorm <- log(3/Real.TMD$Dfreq +1)
Kokoro.TFIDF.df <- data.frame(t(apply(Real.TMD[,1:3], 1, function(x) log(x)+1)))
Kokoro.TFIDF.df <- Kokoro.TFIDF.df*Real.TMD$Dfnorm
Kokoro.TFIDF.df$Term <- Kokoro.DTM.df$Term
Kokoro.TFIDF.df[Kokoro.TFIDF.df == -Inf] <- 0

Kokoro.TFIDF.df.ord1 <- Kokoro.TFIDF.df[order(-Kokoro.TFIDF.df$Freq1),]
Kokoro.TFIDF.df.ord2 <- Kokoro.TFIDF.df[order(-Kokoro.TFIDF.df$Freq2),]
Kokoro.TFIDF.df.ord3 <- Kokoro.TFIDF.df[order(-Kokoro.TFIDF.df$Freq3),]
