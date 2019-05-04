library(tm)
#install.packages("readtext")
#library(readtext)
DATA_DIR <- system.file("texts", "tweet", package = "tm")
tweet <- VCorpus(DirSource(DATA_DIR),readerControl = list(reader = readPlain))
tweet
meta(tweet[[1]])

getTransformations()
clean_corpus <- tm_map(tweet, content_transformer(tolower))
writeLines(as.character(clean_corpus[[1]]))
clean_corpus <- tm_map(clean_corpus, removeNumbers)
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
stopwords()
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords())
clean_corpus <- tm_map(clean_corpus, removeWords, c("that"))
#clean_corpus <- tm_map(clean_corpus, removeWords, c("reuter"))
library(SnowballC) 
clean_corpus <- tm_map(clean_corpus, stemDocument)
dtm <- DocumentTermMatrix(clean_corpus) 
inspect(dtm[1:10, 50:100])
findFreqTerms(dtm, 1, 5)
findAssocs(dtm, "buy", .7)
dtms <- removeSparseTerms(dtm, 0.5)
dtmr <-DocumentTermMatrix(clean_corpus, control=list(wordLengths=c(4, 20), bounds = list(global = c(3,27))))
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 50:100])
#inspect(DocumentTermMatrix(crude,list(dictionary = c("coward", "kawhi", "trade"))))
 #install.packages("wordcloud",dependencies = TRUE)
 library(wordcloud)
 wordcloud(tweet, min.freq=500)
 wordcloud(tweet, min.freq=500, colors=brewer.pal(6, "Dark2"))
 
 library(ggplot2)
 freq <- colSums(as.matrix(dtm)) 
 wf <- data.frame(word=names(freq), freq=freq)
 p <- ggplot(subset(wf, freq>300), aes(word, freq))
 p <- p + geom_bar(stat="identity")
 p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
 p
 
 dtm <- TermDocumentMatrix(clean_corpus, control=list(wordLengths=c(5, 7), bounds = list(global = c(15,20)))) 
 termDocMatrix <- as.matrix(dtm) 
   # change it to a Boolean matrix
     termDocMatrix[termDocMatrix>=1] <- 1
   # transform into a term-term adjacency matrix
     termMatrix <- termDocMatrix %*% t(termDocMatrix)
   # inspect terms numbered 5 to 10
     termMatrix[5:10,5:10]
 
  library(igraph)
  # build a graph from the above matrix
    g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
  # remove loops
    g <- simplify(g)
  # set labels and degrees of vertices
    V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
 
  # set seed to make the layout reproducible
    set.seed(3952)
  layout1 <- layout.fruchterman.reingold(g)
  plot(g, layout=layout1)
 
 
 
 
 
 #Prepare the data
 m <- as.matrix(dtm_tfxidf)
 norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
 m_norm <- norm_eucl(m)
 #Clustering using kmeans
 cl <- kmeans(m_norm, 5)
 cl
 plot(prcomp(m_norm)$x, col=cl$cl)
 inspect(tweet[which(cl$cluster==3)])

