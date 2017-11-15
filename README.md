# Digital_Humanities_582A
Project of History 582A Digital Humanities at Emory University, Fall 2016

## Final Paper
[Final Paper Here](Final%20Paper%20HIST582A%20Zixuan%20(Armstrong)%20Li.pdf)

The purpose of this course is to incorporate digital humanities methods (Data Visualization, Text Mining and Natural Language Processing) into history research for PhD students. 

However, I was an undergraduate student, and decided not to pursue further historical research. Therefore, I tried to applied what I learned to a small project. The result is this 25 page paper.

I started with the idea of doing text mining on the early 20th century Japanese Literature, but realized that would be too much for a paper of this length. Therefor after a brief historiographical introduction, I focused on text mining on works of [Natsume Soseki](https://en.wikipedia.org/wiki/Natsume_S%C5%8Dseki) and especially his [*Kokoro*](https://en.wikipedia.org/wiki/Kokoro)

## Course Infomation and Site
[Course Site](http://history.emory.edu/RAVINA/Syllabi/History_582A_2016.htm)

[Blog Site](https://scholarblogs.emory.edu/582atextmining/)

Two of my articles related to my final project are about [Natsume Soseki](https://scholarblogs.emory.edu/582atextmining/2016/11/29/the-search-for-modernity-and-tradition-in-fifteen-novels-of-natsume-soseki/) and [Historiographical Resarch on Natsume Soseki and Dazai Osamu](https://scholarblogs.emory.edu/582atextmining/2016/11/11/historiographical-research-and-text-analysis-on-natsume-soseki-and-dazai-osamu/)

## Tools and Packages

### A list of useful R packages for textming and data visulization

#### General
* stringr
* tidyr
* plyr
* dplyr
* data.table

#### Visulization
* ggplot2
* plotly

#### Geographical Data
* ggmap
* maps
* mapdata
* geosphere

#### Network
* igrph

#### Natural Language Processing
* coreNLP
* mallet

## Japanese Tokenization
[MeCab](http://taku910.github.io/mecab/) is what I used for this project.

You need [RMeCab](http://rmecab.jp/wiki/index.php?RMeCab), if you want to use MeCab with R.

You can use [UniDic](https://sites.google.com/site/rmecab/home/unidic) instead of the default dictionary for better result.

I recommend not to use Windows for Japanese or Chinese text mining with RStudio, because of the encoding problem. The unicode characters do not display well on Windows RStudio. I experienced diffculties and sometimes has to switch encoding between UTF-8 and SHIFT-JIS, but it's not a good practice in general.


## Selected Visualization
![Google Ngram for Natsume Soseki and Murakami Haruki](Final_Project/Final%20Project%20Visualization/Ngram3.PNG)
Google Ngram
