### Analysis

#Explore terms in the documents: delete sparse ones (weird symbols...) and maybe some of high ocurrence ones
freq <- colSums(as.matrix(dtms))
length(freq)
ord <- order(freq)
hist(prueba,1000)
# most and less common words
freq[tail(ord, 20)]
freq[head(ord)]

#########################################################################################################
###################################### SUMMARIZING #######################################################
#########################################################################################################



## Explore some words with high ocurrence that are not relevant: also, came, first, must...
findFreqTerms(dtm, 3000)

### Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.
# Weight terms as tf-idf
library(tm)
dtm_tfxidf <- weightTfIdf(dtm)
dtms_tfxidf <- weightTfIdf(dtms)



#########################################################################################################
###################################### WORD CLOUD #######################################################
#########################################################################################################

library(wordcloud)
library(RColorBrewer)
WordFrec.sort <- sort(colSums(m), decreasing = T)
WordFrec.sort <- sort(m[1,], decreasing = T)
set.seed(NULL)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_packages.png", width=1280,height=800)
#grayLevels <- gray( (WordFrec.sort + 10) / (max(WordFrec.sort) + 10))
wordcloud(words=names(WordFrec.sort), freq=WordFrec.sort, min.freq=50,
                        random.order=F, max.words = Inf,
                        scale=c(5,.2),rot.per=.15, colors=pal2)
dev.off()

?wordcloud
png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, colors=pal2)


#########################################################################################################
############################### HIERARCHICAL CLUSTERING #################################################
#########################################################################################################

#convert dtm to matrix
m <- as.matrix(dtms)
m_tfidf <- as.matrix(dtms_tfxidf)
m_short_tfidf <- as.matrix(dtms_tfxidf)
# Name the documents as the books
rownames(m) <- BookNamesShorter
rownames(m_tfidf) <- BookNamesShorter
rownames(m_short_tfidf) <- BookNamesShortest

# #shorten rownames for display purposes
# rownames(m) <- paste(substring(rownames(m),1,3),rep('..',nrow(m)),
#                       substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))

# Distance matrix
distMatrix <- dist(scale(m))
#run hierarchical clustering using Ward’s method
groups <- hclust(distMatrix,method='ward.D')
#plot dendogram, use hang to ensure that labels fall below tree
plot(groups, hang=-1, main='Hierarchical Clustering: Euler distance')
rect.hclust(groups,k=2)

############### TRY WITH DIFFERENT METHODS!!!

# Cosine distance matrix: better results
library(proxy)
cosMatrix <- dist(m, method="cosine")
hc <- hclust(cosMatrix, method="average")
plot(hc)

heatmap(as.matrix(cosMatrix))
heatmap(as.matrix(distMatrix))

cosMatrix_tfidf <- dist(m_tfidf,method='cosine')
cosMatrix_short_tfidf <- dist(m_short_tfidf,method='cosine')

hc2 <- hclust(cosMatrix_tfidf, method='average')
plot(hc2, main='Hierarchical Clustering', xlab = '', ylab = '', sub='')



# 
# 
# ###################################################################################
# # Convert into sparse matrix from "Matrix" package
# matrix <- Matrix::sparseMatrix(i = dtm$i, j = dtm$j, x = dtm$v,
#                           dims = c(dtm$nrow, dtm$ncol)) 
# 
# # Normalise lengths of documents
# row_norms <- sqrt(rowSums(matrix ^ 2))
# row_norms <- t(crossprod(sign(matrix), Diagonal(x = row_norms)))
# row_norms@x <- 1/row_norms@x
# m_norm <- matrix * row_norms
# 
# # Finally, we can find cosine similarity
# sim <- tcrossprod(m_norm)
# # colnames(sim) <- metadata$shortname
# # rownames(sim) <- metadata$shortname
# dissim <- 1 - sim
# ###################################################################################
# 
# 


#########################################################################################################
#################################### K-MEANS CLUSTERING #################################################
#########################################################################################################

# #k means algorithm, 6 clusters, 100 starting configurations
# kfit <- kmeans(distMatrix, 6, nstart=100)
# #plot – need library cluster
# library(cluster)
# clusplot(m, kfit$cluster, color=T, shade=T, labels=2, lines=0)

# First normalize distances
norm_eucl <- function(m_tfidf) m_tfidf/apply(m_tfidf, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m_tfidf)

norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

### cluster into 6 clusters
cl <- kmeans(m_norm, 6)
table(cl$cluster)
### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl, cex = 2, pch = 21, bg = cl$cl, main='k-means')
# Add labels to the points 
text(prcomp(m_norm)$x, BookNamesShort)




#kmeans – determine the optimum number of clusters (elbow method)
#look for “elbow” in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:23
for (i in 2:23) wss[i] <- sum(kmeans(distMatrix,centers=i,nstart=25)$withinss)
plot(2:23, wss[2:23], type='b', xlab='Number of Clusters',ylab='Within groups sum of squares')



# 
# #k means algorithm, 2 clusters, 100 starting configurations
# kfit <- kmeans(distMatrix, 6, nstart=100)
# #plot – need library cluster
# library(cluster)
# clusplot(m, kfit$cluster, color=T, shade=T, labels=2, lines=0)


#########################################################################################################
#################################### MultiDimensional Scaling #################################################
#########################################################################################################

fit <- cmdscale(distMatrix, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

plot(x, y, pch = 19, xlim = range(x) + c(0, 600), main ='MultiDimensional Scaling')
text(x, y, pos = 4, labels = BookNamesShorter)


#########################################################################################################
#################################### Network Map #################################################
#########################################################################################################

library(igraph)
corrMatrix_short <- 1 - as.matrix(cosMatrix_short_tfidf)
#diag(corrMatrix_short)<-0
graph<-graph.adjacency(corrMatrix_short,weighted=TRUE,mode="lower", diag = F)
graph <- delete.edges(graph, E(graph)[ weight < 0.6 ])
E(graph)$width <- E(graph)$weight + min(E(graph)$weight) + 3
plot(graph, vertex.color = 'lightblue', vertex.frame.color = 'white', vertex.label.color = 'black', vertex.label.family = 'sans')




#########################################################################################################
#################################### Word Frequencies #################################################
#########################################################################################################




freq <- colSums(m))
length(freq)
ord <- order(freq)
hist(freq[ord],300)

# most and less common words
freq[tail(ord, 20)]
freq[head(ord)]
wordfreq <- m_short[,ord[1745:1731]]
write.csv(wordfreq, 'wordFreq.csv')
write.table(wordfreq, 'clipboard', sep='\t')
