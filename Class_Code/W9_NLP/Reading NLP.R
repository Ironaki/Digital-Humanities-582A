strIn <-  "Mother died today. Or, maybe, yesterday; I can't be sure."

strsplit(strIn, split=" ")
str_split(strIn, " ")

library(coreNLP)
initCoreNLP()

annotation  <-  annotateString(strIn)
annotation

getToken(annotation)$token

anno <-  readRDS("C:/Users/klijia/Desktop/HIST582A/humanitiesDataInR/data/ch09/holmes_anno/01_a_scandal_in_bohemia.Rds")
sentLen = table(getToken(anno)$sentence)
hist(sentLen,breaks=30)

token <- getToken(anno)
token[token$sentence == 2,c(1:4,7)]

table(token$POS)

ut  <-  universalTagset(token$POS)
unique(token$POS[ut == "NOUN"])
unique(token$POS[ut == "VERB"])


nounCnt = tapply(ut == "NOUN", token$sentence, sum)
pronCnt = tapply(ut == "PRON", token$sentence, sum)
adjCnt = tapply(ut == "ADJ", token$sentence, sum)
verbCnt = tapply(ut == "VERB", token$sentence, sum)
posDf = data.frame(nounCnt,pronCnt,adjCnt,verbCnt)
head(posDf)


par(mar=c(4,4,2,1))
par(mfrow=c(1,2))
plot(nounCnt+pronCnt,adjCnt,pch=19,cex=2,col=rgb(0,0,1,0.02),xlim=c(0,13),ylim=c(0,13),asp=1)
plot(nounCnt+pronCnt,verbCnt,pch=19,cex=2,col=rgb(0,0,1,0.02),xlim=c(0,13),ylim=c(0,13),asp=1)



index <-  which(ut=="NOUN")
tab <-  table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)

index <-  which(token$POS == "NNP")
tab <-  table(token$lemma[index])
head(sort(tab,decreasing=TRUE),25)

### Dependencies

parseTree <-  getParse(anno)
length(parseTree)
cat(parseTree[1])

dep <-  getDependency(anno)
dep[dep$sentence == 1,]

par(mar=c(0,0,0,0))
plot(anno,5) ## For some reason this code does not work

index <-  which(token$lemma[dep$depIndex] == "I")
depSelf <-  dep[index,]
depSelf <-  depSelf[depSelf$type == "nsubj",]
sort(table(depSelf$governor),decreasing=TRUE)[1:10]

index <-  which(dep$type == "nn" &
                      token$POS[dep$govIndex] == "NNP" &
                      token$POS[dep$depIndex] == "NNP" &
                      (toupper(token$word) != token$word)[dep$govIndex] &
                      (toupper(token$word) != token$word)[dep$depIndex])
nnDep <-  dep[index,]

pname <-  data.frame(startIndex = NULL, endIndex = NULL)
for (g in unique(nnDep$govIndex)) {
        these = c(which(nnDep$depIndex == g),
                  which(nnDep$govIndex == g))
        these = range(c(nnDep$depIndex[these],nnDep$govIndex[these]))
        out = paste(token$word[these[1]:these[2]],collapse=" ")
        pname = c(pname, out)
        startIndex = c(startIndex, these[1])
        endIndex = c(endIndex, these[2])
}
pnames <-  data.frame(pname,startIndex,endIndex,stringsAsFactors=FALSE)
unique(pnames$pname)
### Part of the code above does not work; Start with new topic



token = getToken(anno)
table(token$NER)

unique(token$lemma[token$NER=="LOCATION"])

# Example date
token[485:490,]

# Example time
token[6991:6994,]


