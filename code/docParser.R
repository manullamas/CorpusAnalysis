rm(list = ls())

library(XML)
library(tm)

# parsing html files. Output: list with words
path <- 'C:/Users/Manuel/Desktop/Southampton/Data Mining/Text Analysis/gap-html/'
Books <- list.files(path)
# Create empty list for the books
BookLists <- as.list(NULL)
for (i in 1:length(Books)) {
  Pages <- list.files(paste0(path,Books[i]))
  for (j in 1:length(Pages)) {
    if (j == 1) {
      html.raw<-htmlTreeParse(paste0(path,Books[i], '/', Pages[j]), useInternalNodes=T)
      page <- xpathApply(html.raw, "//p/span/span", xmlValue)
    } else {
      html.raw<-htmlTreeParse(paste0(path,Books[i], '/', Pages[j]), useInternalNodes=T)
      pageN <- xpathApply(html.raw, "//p/span/span", xmlValue)
      page <- c(page, pageN)
      print(paste0('Book ', i, ':   page ', j, '/', length(Pages)))
    }
  }
  if (i == 1) {
    BookLists <- page
  } else {
    # populate book lists with the lists of words
    BookLists <- list(BookLists,  page)
  }
}

BookList <- list(Book1,Book2,Book3,Book4,Book5,Book6,Book7,Book8,Book9,Book10,Book11,Book12,
                 Book13,Book14,Book15,Book16,Book17,Book18,Book19,Book20,Book21,Book22,Book23,Book24)

###############################################################################
#              PREPROCESSING    
library(tm)
docs <- Corpus(VectorSource(BookList))

###### To check how are things going after each step:
inspect(docs[i])

# remove punctuation
docs <- tm_map(docs, removePunctuation)

# rempve numbers
docs <- tm_map(docs, removeNumbers)
# numbers attached to words removed???????

# convert capital letters to lowercase: to group words we need them to be exactly the same
docs <- tm_map(docs, tolower)

# remove stopwords:  words with no analytic value (a, the, also.....). 
docs <- tm_map(docs, removeWords, stopwords("english"))
# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   


############################################################################################
############################################################################################
############################################################################################
# Remove specific symbols/strange words: •, •■vtane, €atooaij....
# However taking out sparse terms later in analysis will remove almost all these terms
############################################################################################
############################################################################################
############################################################################################
# # remove strange symbols
# toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, ” “, x))})
# docs <- tm_map(docs, toSpace, “-“)
# docs <- tm_map(docs, toSpace, “:”)
# docs <- tm_map(docs, toSpace, “‘”)
# docs <- tm_map(docs, toSpace, “•”)
# docs <- tm_map(docs, toSpace, “■”)
# docs <- tm_map(docs, toSpace, “—”)
# docs <- tm_map(docs, toSpace, “►”)
# docs <- tm_map(docs, toSpace, “€”)



# remove specific words:
#docs <- tm_map(docs, removeWords, c("word1", "word1" ...))   

# classify different words as the same (acronyms...)
# for (j in seq(docs))
# {
#   docs[[j]] <- gsub("qualitative research", "QDA", docs[[j]])
#   docs[[j]] <- gsub("qualitative studies", "QDA", docs[[j]])
#   docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
#   docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
# }


# steaming document: (removing common endings: -ing, -es, -s ...)
library(SnowballC)
docs <- tm_map(docs, stemDocument)

# remove white spaces (many as from the processing before we are generating a lot of them)
docs <- tm_map(docs, stripWhitespace)


###### Treat the preprocessed file as text document
docs <- tm_map(docs, PlainTextDocument)



#                    END OF PREPROCESSING

############################################################################################

# create a document term matrix: each term in column and row the document 
dtm <- DocumentTermMatrix(docs)
# maybe later we need this matrix transposed
tdm <- TermDocumentMatrix(docs)


# always try if it worked!

############################################################################################
############################################################################################
#                EXPLORE DATA

# Organize terms by frequency
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing = F)
head(ord)
tail(ord)
hist(freq[ord],1000)
hist(as.matrix(dtm),100)

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.
inspect(dtms)

dtms[,1:10]


# TODO:

#### ELIMINAR LOS TERMINOS QUE SE REPITEN MUCHO: (50/60 %???? probar y aplicar clustering y algoritmos para comprobar el funcionamiento)

#### td - idf
#   https://en.wikipedia.org/wiki/Tf%E2%80%93idf
#### cosine similarity


#### hierarquical clustering y otros algoritmos
#### Nombre de los libros para comprobar resultados!!!




freq <- colSums(as.matrix(dtms))
length(freq)
ord <- order(freq)
hist(freq[ord],300)

rownames()

# most and less common words
freq[tail(ord, 20)]
freq[head(ord)]
as.matrix(dtms[,ord[1730:1745]])














# wf <- data.frame(word=names(freq), freq=freq)
# head(wf)
# # plot word frequencies (fancier)
# library(ggplot2)
# p <- ggplot(subset(wf, freq>50), aes(word, freq))
# p <- p + geom_bar(stat="identity")
# p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
# p






##         READING THE DOCS???
#################################################################
## Remove special characters:
# for(j in seq(docs))   
# {   
#   docs[[j]] <- gsub("/", " ", docs[[j]])   
#   docs[[j]] <- gsub("@", " ", docs[[j]])   
#   docs[[j]] <- gsub("\\|", " ", docs[[j]])   
# }   


### TITULOS DE LOS LIBROS PARA PONERLES TAGS A LOS DOCUMENTOS!: en las primeras paginas en mayuscula los podemos encontrar

