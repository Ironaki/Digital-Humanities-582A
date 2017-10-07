library(coreNLP)
sIn <- "Mother died today. Or, maybe, yesterday; I can’t be sure."
# downloadCoreNLP()
initCoreNLP()

annotation <- annotateString(sIn)
annotation

##look at structure of annotation

getToken(annotation)$token


Edlestein <- "The traditional “lit review” bears more than a passing resemblance to a landscape painting. In both cases, the artist or scholar scans the horizon to trace the outlines of what appear to be the major summits, filling in the hills and valleys below to a varying and limited degree. In both cases as well, the selection of prominent features is largely dependent on perspective: one artist’s mountain is another’s molehill. But there are still some constants, particularly in the scholarly world. Those figures who loom over the others tend to be at the peak of their careers. As for the rest, whether or not they make it into the picture at all is to a considerable degree a matter of chance and space. And, of course, of time: we can read only so many books and articles. Sometimes excellent works simply don’t cross our desks (or, these days, our desktops). But the rise of scholarly reference databases, either of citations or of full-text documents, has made it possible to identify, and harder to ignore, many sources that might previously have flown beneath our radars. These databases are not limited to articles, either: JSTOR now includes some books; and the MLA International Bibliography catalogs monographs. In itself, this newfound access does not per se solve any problems. If anything, it can make matters worse, by calling our attention to the inconvenient fact that an impossibly large number of sources could be read and evaluated before we write our review. No matter how well intentioned we are, it is often unfeasible—and possibly unnecessary—to plow through this forest. But the availability of all this metadata (and data, in the case of full-text works) also raises another possibility: that of using quantitative measures to assess the impact of certain arguments and ideas. Just as literary scholars have adopted text-mining techniques to explore the “great unread” of literary history, humanists can now use data-mining tools to structure their lit review.Now, I am not suggesting that thoughtful analysis and engagement with individual works can be replaced with number crunching. Because the lit review is ultimately about assessing the quality of other people’s arguments, it will and should remain a fundamentally qualitative exercise. The question is, are we always assessing the right works? Are we missing important trends? Might we be granting more weight to certain authors than they deserve, in light of their limited impact on the scholarship? And could we be ignoring parallel debates and arguments that are taking place in neighboring fields? What data mining can offer, I suggest, is a broad yet detailed backdrop that helps guide our analyses of secondary sources. It can reveal trends about the evolution of a field that might in turn lead us to pay more attention to a particular discipline, time period, theorist, or argument. It is also a tricky exercise, which can easily be misleading. In this article, I experiment with the potential of JSTOR’s “data for research” portal (hereafter abbreviated as DfR) for discovering trends in scholarship. Data-mining JSTOR yields rows and columns of neatly structured metadata, which any Excel user can turn into scientific-looking charts. But the data themselves are often quite messy. Consider the following example: a scholar interested in the place-names mentioned in documents discussing “the Enlightenment” might be surprised to learn that “Princeton” appears more often than “Paris.” This is not because Princeton was a hotbed of the Enlightenment (which would have been a curious finding, since the university was still named the College of New Jersey in the eighteenth century), but because Princeton is a place where many twentieth-century books have been published on the Enlightenment."


Edlestein.anno <- annotateString(Edlestein)



getToken(Edlestein.anno)$token

getToken(Edlestein.anno)$sentence  ## which word in which sentence

mean(getToken(Edlestein.anno)$sentence) ## mean sentence length

##grab all the token data as a dataframe
Edlestein.token.df <- getToken(Edlestein.anno)
##can be TOO detailed, so here's the alternative
##question: what is the function?
Edlestein.token.df$POS_basic <- universalTagset(Edlestein.token.df$POS)
##maybe sort by POS?

Edlestein.adj.df <- Edlestein.token.df[which(Edlestein.token.df$POS_basic=="ADJ" | Edlestein.token.df$POS_basic=="ADV"),]
table(Edlestein.adj.df$lemma)
sort(table(Edlestein.adj.df$lemma))

##dependency parsing
parseTree <- getParse(Edlestein.anno)
length(parseTree) ## same as number of sentences
## is this useful?
cat(parseTree[1]) ## index by sentence


Edlestein.depend <- getDependency(Edlestein.anno)
## check out that dataframe!!
## what does this mean?
## http://thomaslebarbe.free.fr/Linguistic_Lexicon/ll_g.html#government


Edlestein.depend[which(Edlestein.depend$dependent=="I"),"governor"]
Edlestein.depend[which(Edlestein.depend$dependent=="humanists"),"governor"]
Edlestein.depend[which(Edlestein.depend$dependent=="it"),"governor"]



Cubs <- "If you are going to endure years — no, generations — of futility and heartbreak, when you do finally win a World Series championship, it may as well be a memorable one. The Chicago Cubs did just that, shattering their 108-year championship drought in epic fashion: with an 8-7, 10-inning victory over the Cleveland Indians in Game 7, which began on Wednesday night, carried into Thursday morning and seemed to end all too soon. When the Indians rallied with three runs in the eighth inning — including a two-out, two-strike, two-run thunderbolt of a home run by Rajai Davis off closer Aroldis Chapman — the Cubs found a way to beat back the ghosts of playoffs past. After a brief rain delay following the ninth inning, they pushed two runs across in the 10th inning on a double by Ben Zobrist, the Series’s most valuable player, and a single by Miguel Montero. The Cubs then had to hold their breath in the bottom of the inning when Davis hit a run-scoring single to pull the Indians to a run behind. But reliever Mike Montgomery replaced Carl Edwards and got Michael Martinez to hit a slow roller into the infield. Third baseman Kris Bryant scooped it up and threw across to first baseman Anthony Rizzo. As the ball made its flight across the diamond, the stadium went silent for one of only a few times all night — and only until it settled into Rizzo’s glove."

Cubs.anno <- annotateString(Cubs)


Cubs.depend <- getDependency(Cubs.anno)
Cubs.depend[which(Cubs.depend$dependent=="Cubs"),"governor"]


