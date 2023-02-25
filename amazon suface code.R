library(rvest)
library(XML)
library(magrittr)

aurl <- "https://www.amazon.com/Microsoft-Surface-Laptop-Touch-Screen-Alcantara/dp/B07YNK3R68/ref=sr_1_3?dchild=1&keywords=surface%2Blaptop&qid=1614416199&sr=8-3&th=1"

reviews <- NULL

for (i in 1:20){
  tab <- read_html(as.character(paste(aurl,i,sep ="=")))
  rev <- tab %>% html_nodes(".review-text") %>% html_text()
  reviews <- c(reviews,rev)
}

write.table(reviews,"tablet.txt")
getwd()

txt <- reviews

str(txt)
length(txt)

library(tm)

x <- Corpus(VectorSource(txt))

inspect(x[1])
inspect(x[260])

x <- tm_map(x, function(x) iconv(enc2utf8(x), sub='byte'))

x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])


tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm) # transpose
dtm <- DocumentTermMatrix(x1)

tdm <- as.matrix(tdm)
dim(tdm)

tdm[1:20, 1:20]

inspect(x[3])        

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 65)
w_sub        

barplot(w_sub, las=1, col = rainbow(30))

x1 <- tm_map(x1, removeWords, c('laptop'))
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 50)
w_sub
sort(w_sub)

barplot(w_sub, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_sub), w_sub)
colnames(w1) <- c('word', 'freq')

wordcloud2(w1, size=0.4, shape='circle')

wordcloud2(w1, size=0.3, shape = 'square')
