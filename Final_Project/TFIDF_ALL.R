na.zzz <- RMeCabFreq("D:/Google Drive/JPN_LIT/Natsume/zzz.txt")
na.zzz.reduced <- na.zzz[which(na.zzz$Info1 != "‹L†"),]

files.cle.dir<- list.files("D:/Google Drive/JPN_LIT/Natsume/cleaned")

for (n.i in 1:length(files.cle.dir)){
        assign(paste0("n.", files.cle.dir[n.i]), RMeCabFreq(paste0("D:/Google Drive/JPN_LIT/Natsume/cleaned/",files.cle.dir[n.i]))[which(RMeCabFreq(paste0("D:/Google Drive/JPN_LIT/Natsume/cleaned/",files.cle.dir[n.i]))$Info1 != "‹L†"),])
}

na.zzz.reduced$bocchan <- 0
na.zzz.reduced$gubijinso <- 0
na.zzz.reduced$higansugimade <- 0
na.zzz.reduced$kofu <- 0
na.zzz.reduced$kojin <- 0
na.zzz.reduced$kokoro <- 0
na.zzz.reduced$kusamakura <- 0
na.zzz.reduced$meian <- 0
na.zzz.reduced$michikusa <- 0
na.zzz.reduced$mon <- 0
na.zzz.reduced$nihyakutoka <- 0
na.zzz.reduced$nowaki <- 0
na.zzz.reduced$sanshiro <- 0
na.zzz.reduced$sorekara <- 0
na.zzz.reduced$wagahaiwa_nekodearu <- 0

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.bocchan.txt$Term[y] == na.zzz.reduced$Term[z] & n.bocchan.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.bocchan.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$bocchan[z] <- n.bocchan.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.gubijinso.txt$Term[y] == na.zzz.reduced$Term[z] & n.gubijinso.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.gubijinso.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$gubijinso[z] <- n.gubijinso.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.higansugimade.txt$Term[y] == na.zzz.reduced$Term[z] & n.higansugimade.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.higansugimade.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$higansugimade[z] <- n.higansugimade.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.kofu.txt$Term[y] == na.zzz.reduced$Term[z] & n.kofu.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.kofu.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$kofu[z] <- n.kofu.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.kojin.txt$Term[y] == na.zzz.reduced$Term[z] & n.kojin.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.kojin.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$kojin[z] <- n.kojin.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.kokoro.txt$Term[y] == na.zzz.reduced$Term[z] & n.kokoro.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.kokoro.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$kokoro[z] <- n.kokoro.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.kusamakura.txt$Term[y] == na.zzz.reduced$Term[z] & n.kusamakura.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.kusamakura.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$kusamakura[z] <- n.kusamakura.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}


z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.meian.txt$Term[y] == na.zzz.reduced$Term[z] & n.meian.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.meian.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$meian[z] <- n.meian.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.michikusa.txt$Term[y] == na.zzz.reduced$Term[z] & n.michikusa.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.michikusa.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$michikusa[z] <- n.michikusa.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.mon.txt$Term[y] == na.zzz.reduced$Term[z] & n.mon.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.mon.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$mon[z] <- n.mon.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.nihyakutoka.txt$Term[y] == na.zzz.reduced$Term[z] & n.nihyakutoka.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.nihyakutoka.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$nihyakutoka[z] <- n.nihyakutoka.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.nowaki.txt$Term[y] == na.zzz.reduced$Term[z] & n.nowaki.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.nowaki.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$nowaki[z] <- n.nowaki.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.sanshiro.txt$Term[y] == na.zzz.reduced$Term[z] & n.sanshiro.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.sanshiro.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$sanshiro[z] <- n.sanshiro.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.sorekara.txt$Term[y] == na.zzz.reduced$Term[z] & n.sorekara.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.sorekara.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$sorekara[z] <- n.sorekara.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

z <- 1
y <- 1
while (z <= length(na.zzz.reduced$Term)){
        if(n.wagahaiwa_nekodearu.txt$Term[y] == na.zzz.reduced$Term[z] & n.wagahaiwa_nekodearu.txt$Info1[y] == na.zzz.reduced$Info1[z] & n.wagahaiwa_nekodearu.txt$Info2[y] == na.zzz.reduced$Info2[z]){
                na.zzz.reduced$wagahaiwa_nekodearu[z] <- n.wagahaiwa_nekodearu.txt$Freq[y]
                y <-  y + 1
        }
        z <- z + 1
}

n.TMD <- na.zzz.reduced[,4:19]
# write.table(na.zzz.reduced, "Natsume_all.txt", sep ="\t")
# write.table(n.TMD, "Natsume_all_wo_term.txt", sep = "\t")


n.TMD$Dfreq <- apply(n.TMD, 1, function(x) length(which(x != 0)))
n.TMD$Dfnorm <- log(15/n.TMD$Dfreq +1)

n.TFIDF.df <- data.frame(t(apply(n.TMD[,2:16], 1, function(x) log(x)+1)))
n.TFIDF.df <- n.TFIDF.df*n.TMD$Dfnorm
n.TFIDF.df[n.TFIDF.df == -Inf]  <- 0

Final.TFIDF.df <- cbind(na.zzz.reduced[,1:3],n.TFIDF.df)

##### Make another table of Final percentage

n.PERC.df<- data.frame(apply(n.TMD[,1:16], 2,function(x) x/sum(x)*100))
Final.PERC.df <- cbind(na.zzz.reduced[,1:3],n.PERC.df)







ord.kokoro.TFIDF.df <- Final.TFIDF.df[order(-Final.TFIDF.df$kokoro),]
ord.bocchan.TFIDF.df <- Final.TFIDF.df[order(-Final.TFIDF.df[,4]),]

ord.kokoro.TFIDF20.df <- ord.kokoro.TFIDF.df[1:20,]

ord.kokoro.TFIDF20.df$novel <- "kokoro"
ord.kokoro.TFIDF20.df$temp <- 2


###Make a dataframe of top 20 tfidf for each novel
all.TFIDF20 <- data.frame(Term = NA,tfidf = NA, novel = NA)
for (col.i in 4:18){
        all.TFIDF20 <- rbindlist(list(all.TFIDF20, cbind(Final.TFIDF.df[order(-Final.TFIDF.df[,col.i]),][1:20,][,c(1,col.i)],colnames(Final.TFIDF.df)[col.i])))
}
all.TFIDF20 <- all.TFIDF20[-1,]
all.TFIDF20$novel  <- as.character(all.TFIDF20$novel)
all.TFIDF20[all.TFIDF20 == "wagahaiwa_nekodearu"] <- "wagahaiwa"

all.TFIDF5 <- data.frame(Term = NA,tfidf = NA, novel = NA)
for (col.i in 4:18){
        all.TFIDF5 <- rbindlist(list(all.TFIDF5, cbind(Final.TFIDF.df[order(-Final.TFIDF.df[,col.i]),][1:5,][,c(1,col.i)],as.character(colnames(Final.TFIDF.df)[col.i]))))
}
all.TFIDF5 <- all.TFIDF5[-1,]
all.TFIDF5$novel  <- as.character(all.TFIDF5$novel)

library(ggrepel)

#### Graphing Part

ggplot(all.TFIDF20, aes(x = novel, y = tfidf))+
        geom_point(size = 2) +
        scale_x_discrete(limits=c("wagahaiwa","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian")) +
        

ggplot(all.TFIDF20, aes(x = novel, y = tfidf))+
        geom_point(size = 2) +
        scale_x_discrete(limits=rev(c("wagahaiwa","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        geom_violin()+
        coord_flip()
        geom_boxplot() +
     #   geom_text_repel(aes(label=Term), size =4, segment.color = 'grey60',nudge_x =0.03)

        
        
all.TFIDF5[all.TFIDF5 == "wagahaiwa_nekodearu"] <- "wagahaiwa"
ggplot(all.TFIDF5, aes(x = novel, y = tfidf))+
        geom_point(size = 2) +
        scale_x_discrete(limits=c("wagahaiwa","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian")) +
        geom_text_repel(aes(label=Term), size =4, segment.color = 'grey60',nudge_x =0.05)

ggplot(subset(Final.TFIDF.df, Term != "‚È‚¢" & Info1 == ",Œ`—eŽŒ" & kokoro >=3),aes(x = kokoro, y = reorder(Term, kokoro))) +
        geom_point()

### Gun
gun.Freq.df <- Final.PERC.df[grep(pattern = "ŒR", Final.TFIDF.df$Term),]
gun.Freq.df <- data.frame(apply(gun.Freq.df[,5:19], 2, function(x) sum(x)))
gun.Freq.df$novel <- row.names(gun.Freq.df)
colnames(gun.Freq.df)[1] <- "Freq"

ggplot(gun.Freq.df, aes(x=novel,y=Freq))+
        geom_point(size = 2)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        coord_flip()
### Sen
sen.Freq.df <- Final.PERC.df[grep(pattern = "í", Final.TFIDF.df$Term),]
sen.Freq.df <- data.frame(apply(sen.Freq.df[,5:19], 2, function(x) sum(x)))
sen.Freq.df$novel <- row.names(sen.Freq.df)
colnames(sen.Freq.df)[1] <- "Freq"

ggplot(sen.Freq.df, aes(x=novel,y=Freq))+
        geom_point(size = 2)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        coord_flip()

gunsenall.Freq.df <- cbind(gun.Freq.df, sen.Freq.df$Freq, ai.Freq.df$Freq, shi.Freq.df$Freq)
colnames(gunsenall.Freq.df)[3:5] <- c("sen","ai","shi")
gunsenall.Freq.df$novel <- c("bocchan 1906-4",
                          "gubijinso 1907-6",
                          "higansugimade 1912-1",
                          "kofu 1908-1",
                          "kojin 1912-11",
                          "kokoro 1914-4",
                          "kusamakura 1906-9",
                          "meian 1916-5",
                          "michikusa 1915-6",
                          "mon 1910-3",
                          "nihyakutoka 1906-10",
                          "nowaki 1907-1",
                          "sanshiro 1908-9",
                          "sorekara 1909-6",
                          "wagahaiwa_nekodearu 1905-1")
ggplot(gunsenall.Freq.df, aes(x=novel))+
        geom_point(aes(y = Freq, color = "Military"), shape =3, size = 3)+
        geom_point(aes(y = sen, color = "War"), shape =4, size = 3)+
        geom_point(aes(y = ai, color = "Love"), shape =0, size = 3)+
        geom_point(aes(y = shi, color = "Death"), shape =2, size = 3)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu 1905-1",
                                      "bocchan 1906-4",
                                      "kusamakura 1906-9",
                                      "nihyakutoka 1906-10",
                                      "nowaki 1907-1",
                                      "gubijinso 1907-6",
                                      "kofu 1908-1",
                                      "sanshiro 1908-9",
                                      "sorekara 1909-6",
                                      "mon 1910-3",
                                      "higansugimade 1912-1",
                                      "kojin 1912-11",
                                      "kokoro 1914-4",
                                      "michikusa 1915-6",
                                      "meian 1916-5"))) +
        labs(color="Keywords")+
        coord_flip()


### Ai
ai.Freq.df <- Final.PERC.df[grep(pattern = "ˆ¤", Final.TFIDF.df$Term),]
ai.Freq.df <- ai.Freq.df[which(ai.Freq.df$Term != "ˆ¤“†" ),]
ai.Freq.df <- ai.Freq.df[which(ai.Freq.df$Term != "ˆ¤“†ŽR" ),]
ai.Freq.df <- data.frame(apply(ai.Freq.df[,5:19], 2, function(x) sum(x)))
ai.Freq.df$novel <- row.names(ai.Freq.df)
colnames(ai.Freq.df)[1] <- "Freq"

ggplot(ai.Freq.df, aes(x=novel,y=Freq))+
        geom_point(size = 2)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        coord_flip()

### Laugh
shi.Freq.df <- Final.PERC.df[grep(pattern = "Ž€", Final.TFIDF.df$Term),]
shi.Freq.df <- data.frame(apply(shi.Freq.df[,5:19], 2, function(x) sum(x)))
shi.Freq.df$novel <- row.names(shi.Freq.df)
colnames(shi.Freq.df)[1] <- "Freq"

ggplot(shi.Freq.df, aes(x=novel,y=Freq))+
        geom_point(size = 2)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        coord_flip()

### Death

xiao.Freq.df <- Final.PERC.df[grep(pattern = "Î", Final.TFIDF.df$Term),]
xiao.Freq.df <- data.frame(apply(xiao.Freq.df[,5:19], 2, function(x) sum(x)))
xiao.Freq.df$novel <- row.names(xiao.Freq.df)
colnames(xiao.Freq.df)[1] <- "Freq"

ggplot(xiao.Freq.df, aes(x=novel,y=Freq))+
        geom_point(size = 2)+
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu","bocchan","kusamakura","nihyakutoka","nowaki","gubijinso","kofu","sanshiro","sorekara","mon","higansugimade","kojin","kokoro","michikusa","meian"))) +
        coord_flip()

ggplot(Final.TFIDF.df[grep(pattern = "ŒR", Final.TFIDF.df$Term),],aes(x = kokoro, y = reorder(Term, kokoro))) +
        geom_point()

ggplot(subset(Final.TFIDF.df, Term != "‚È‚¢" & Info1 == "Œ`—eŽŒ" & kofu >=3),aes(x = kofu, y = reorder(Term, kofu))) +
        geom_point()

ggplot(subset(Final.TFIDF.df, Info1 == "“®ŽŒ" & kokoro >=4),aes(x = kokoro, y = reorder(Term, kokoro))) +
        geom_point()

ggplot(subset(Final.TFIDF.df, Info1 == "“®ŽŒ" & kokoro >=4),aes(x = kokoro, y = reorder(Term, kokoro))) +
        geom_point() +
        geom_text(aes(label=Info2),size=4)

ggplot(subset(Final.TFIDF.df, Info1 == "“®ŽŒ" & kokoro >=4),aes(x = kokoro, y = colnames(Final.TFIDF.df)[4:18])) +
        geom_point()


### Gendai & Kodai
gendai.all.PERC.df <- Final.PERC.df[which(Final.PERC.df$Term == "ˆÛV"| Final.PERC.df$Term == "Œ»‘ã" |Final.PERC.df$Term =="ŠJ‰»"|Final.PERC.df$Term =="“Æ—§"),]
gendai.all.PERC.df <- gendai.all.PERC.df[,-1:-4]
gendai.all.PERC.df[2,8] <- gendai.all.PERC.df[2,8] + gendai.all.PERC.df[5,8] 
gendai.all.PERC.df <- gendai.all.PERC.df[1:4,]
t.gendai.df <- data.frame(t(gendai.all.PERC.df))
colnames(t.gendai.df) <- c("ŠJ‰»", "“Æ—§", "ˆÛV", "Œ»‘ã")
t.gendai.df$novel <- row.names(t.gendai.df)

t.gendai.df$novel <- c("bocchan 1906-4",
                             "gubijinso 1907-6",
                             "higansugimade 1912-1",
                             "kofu 1908-1",
                             "kojin 1912-11",
                             "kokoro 1914-4",
                             "kusamakura 1906-9",
                             "meian 1916-5",
                             "michikusa 1915-6",
                             "mon 1910-3",
                             "nihyakutoka 1906-10",
                             "nowaki 1907-1",
                             "sanshiro 1908-9",
                             "sorekara 1909-6",
                             "wagahaiwa_nekodearu 1905-1")


ggplot(t.gendai.df, aes(x = novel))+
        geom_point(aes(y=ŠJ‰», color ="ŠJ‰»"), shape = 3, size =3) +
        geom_point(aes(y=“Æ—§, color = "“Æ—§"), shape = 4, size =3) +
        geom_point(aes(y=ˆÛV, color = "ˆÛV"), shape = 0, size =3) +
        geom_point(aes(y=Œ»‘ã, color = "Œ»‘ã"), shape = 2, size =3) +
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu 1905-1",
                                      "bocchan 1906-4",
                                      "kusamakura 1906-9",
                                      "nihyakutoka 1906-10",
                                      "nowaki 1907-1",
                                      "gubijinso 1907-6",
                                      "kofu 1908-1",
                                      "sanshiro 1908-9",
                                      "sorekara 1909-6",
                                      "mon 1910-3",
                                      "higansugimade 1912-1",
                                      "kojin 1912-11",
                                      "kokoro 1914-4",
                                      "michikusa 1915-6",
                                      "meian 1916-5"))) +
        labs(color="Keywords")+
        ylab("Freq") +
        coord_flip()
        
gvk.all.PERC.df <- Final.PERC.df[which(Final.PERC.df$Term == "Œ»‘ã" |Final.PERC.df$Term =="ŒÃ‘ã"),]
gvk.all.PERC.df <- gvk.all.PERC.df[,-1:-4]
t.gvk.df <- data.frame(t(gvk.all.PERC.df))
colnames(t.gvk.df) <- c("Œ»‘ã", "ŒÃ‘ã")
t.gvk.df$novel <- row.names(t.gvk.df)

t.gvk.df$novel <- c("bocchan 1906-4",
                       "gubijinso 1907-6",
                       "higansugimade 1912-1",
                       "kofu 1908-1",
                       "kojin 1912-11",
                       "kokoro 1914-4",
                       "kusamakura 1906-9",
                       "meian 1916-5",
                       "michikusa 1915-6",
                       "mon 1910-3",
                       "nihyakutoka 1906-10",
                       "nowaki 1907-1",
                       "sanshiro 1908-9",
                       "sorekara 1909-6",
                       "wagahaiwa_nekodearu 1905-1")

ggplot(t.gvk.df, aes(x = novel))+
        geom_point(aes(y=ŒÃ‘ã, color = "ŒÃ‘ã"), shape = 8, size =3) +
        geom_point(aes(y=Œ»‘ã, color = "Œ»‘ã"), shape = 1, size =3) +
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu 1905-1",
                                      "bocchan 1906-4",
                                      "kusamakura 1906-9",
                                      "nihyakutoka 1906-10",
                                      "nowaki 1907-1",
                                      "gubijinso 1907-6",
                                      "kofu 1908-1",
                                      "sanshiro 1908-9",
                                      "sorekara 1909-6",
                                      "mon 1910-3",
                                      "higansugimade 1912-1",
                                      "kojin 1912-11",
                                      "kokoro 1914-4",
                                      "michikusa 1915-6",
                                      "meian 1916-5"))) +
        labs(color="Keywords")+
        ylab("Freq") +
        coord_flip()


### Chinese and English
chn.all.PERC.df <- Final.PERC.df[which(Final.PERC.df$Term == "´‘" |
                                       Final.PERC.df$Term == "’†‘"|
                                       Final.PERC.df$Term == "Š¿Šw"|
                                       Final.PERC.df$Term =="Š¿Œê"|
                                       Final.PERC.df$Term =="Š¿Ž"|
                                       Final.PERC.df$Term =="Š¿Ð"|
                                       Final.PERC.df$Term =="Š¿“y"|
                                       Final.PERC.df$Term =="Š¿"|
                                       Final.PERC.df$Term =="Š¿l"),]

eng.all.PERC.df <- Final.PERC.df[which(Final.PERC.df$Term == "ƒCƒMƒŠƒX" |
                                               Final.PERC.df$Term == "‰p‘"|
                                               Final.PERC.df$Term == "‰p–ó"|
                                               Final.PERC.df$Term =="‰pŒê"|
                                               Final.PERC.df$Term =="‰p•¶"|
                                               Final.PERC.df$Term =="‰p˜a"),]
chn.eng.df <- data.frame(apply(chn.all.PERC.df[,5:19], 2, function(x) sum(x)))
chn.eng.df <- cbind(chn.eng.df, data.frame(apply(eng.all.PERC.df[,5:19], 2, function(x) sum(x))))

colnames(chn.eng.df) <- c("CHINESE", "ENGLISH")

chn.eng.df$novel <- row.names(chn.eng.df)

chn.eng.df$novel <- c("bocchan 1906-4",
                    "gubijinso 1907-6",
                    "higansugimade 1912-1",
                    "kofu 1908-1",
                    "kojin 1912-11",
                    "kokoro 1914-4",
                    "kusamakura 1906-9",
                    "meian 1916-5",
                    "michikusa 1915-6",
                    "mon 1910-3",
                    "nihyakutoka 1906-10",
                    "nowaki 1907-1",
                    "sanshiro 1908-9",
                    "sorekara 1909-6",
                    "wagahaiwa_nekodearu 1905-1")

ggplot(chn.eng.df, aes(x = novel))+
        geom_point(aes(y=CHINESE, color = "CHINESE"), shape = 8, size =3) +
        geom_point(aes(y=ENGLISH, color = "ENGLISH"), shape = 1, size =3) +
        scale_x_discrete(limits=rev(c("wagahaiwa_nekodearu 1905-1",
                                      "bocchan 1906-4",
                                      "kusamakura 1906-9",
                                      "nihyakutoka 1906-10",
                                      "nowaki 1907-1",
                                      "gubijinso 1907-6",
                                      "kofu 1908-1",
                                      "sanshiro 1908-9",
                                      "sorekara 1909-6",
                                      "mon 1910-3",
                                      "higansugimade 1912-1",
                                      "kojin 1912-11",
                                      "kokoro 1914-4",
                                      "michikusa 1915-6",
                                      "meian 1916-5"))) +
        labs(color="Keywords")+
        ylab("Freq") +
        coord_flip()

ggplot(ord.kokoro.TFIDF20.df, aes(x = kokoro, y = reorder(Term, kokoro) ))+
        geom_point() +
        xlim(5.2,7.5) +
        ylab("Term") +
        xlab("tf-idf")
        
