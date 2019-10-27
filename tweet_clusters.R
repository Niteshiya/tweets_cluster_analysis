#get libraries
library(tm)
tweets <- readLines(".//tweets_cluster_analysis//tweets.txt")
tweets

#build corpus

corpus <- Corpus(VectorSource(tweets))
corpus

#create term doucment matrix
tdm <- TermDocumentMatrix(corpus,
                          control = list(minWordLength=c(1,Inf)))
tdm

t <- removeSparseTerms(tdm,sparse = 0.98)
t
m <- as.matrix(t)
m

#plot frequency terms

freq <- rowSums(m)
freq <- subset(freq,freq>=50)
barplot(freq,las=2,col=rainbow(25))

#Create a dendo grams
distance <- dist(scale(m))
print(distance,digits = 2)
hc <- hclust(distance,method="ward.D")
plot(hc,hang=-1)
rect.hclust(hc,k=10)

#non-hierarchical clustering using k means

m1 <- t(m)
set.seed(222)
k <- 12
kc <- kmeans(m1,k)
kc$cluster
tweets[kc$cluster==11]
