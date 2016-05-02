library(tm)

#Remove stop words, white space, punctuation, lower case, and stem document
cleanCorpus <- function(vc) {
	vc <- tm_map( vc, content_transformer(tolower) )
	vc <- tm_map( vc, removeWords, stopwords("SMART") )
	vc <- tm_map( vc, removeWords, stopwords("english") )
	vc <- tm_map( vc, stripWhitespace )
	vc <- tm_map( vc, removePunctuation )
	vc <- tm_map( vc, stemDocument )
	tdm <- TermDocumentMatrix(vc)
	inspectedTDM <- inspect( removeSparseTerms(tdm, 0.4) )
	tdmFrame <- data.frame( dimnames(inspectedTDM), inspectedTDM )
	names(tdmFrame) <- c( 'Terms', 'Docs', 'Counts')
	tdmFrame
	}

firstC <- function() {
	vc <- VCorpus( DirSource("data/test", encoding = "UTF-8"), readerControl = list(language = "eng"))
	cleanCorpus(vc)
}

readRandom <- function(fileName) {
	set.seed(1234)
	con <- file(fileName, "r")
	l <- readLines(con, 1)
	randomLines <- c(l)
	while ( length(l) > 0 ) {
		l <- readLines(con, 1)
		if ( runif(1,1,10) < 2.0 ) {
			randomLines <- c( randomLines, l)
		}
	}
	close(con)
	randomLines
}

#Create test file given original file
writeRandom <- function(fileNameIn, fileNameOut = "short.txt") {
	lines <- readRandom(fileNameIn)
	writeLines( lines, paste("data/test/", fileNameOut, sep = '' ) )
}
