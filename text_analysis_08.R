############################################################################### "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 8: Do it KWIC                                                ###############################################################################

### Startup Code

rm(list = ls()) # Clear environment
dev.off(dev.list()["RStudioGD"]) # Clear devices
cat("\014") #Clear console

#setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/text_analysis")
setwd("~/Documents/Git_Repos/text_analysis")

text.v <- scan("data/plainText/melville.txt", what="character", sep="\n")
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
novel.lines.v <-  text.v[start.v:end.v]
novel.lines.v <- unlist(novel.lines.v)
chap.positions.v <- grep("^CHAPTER \\d", novel.lines.v)
last.position.v <-  length(novel.lines.v)
chap.positions.v  <-  c(chap.positions.v , last.position.v)
chapter.freqs.l <- list()
chapter.raws.l <- list()
for(i in 1:length(chap.positions.v)){
  if(i != length(chap.positions.v)){
    chapter.title <- novel.lines.v[chap.positions.v[i]]
    start <- chap.positions.v[i]+1
    end <- chap.positions.v[i+1]-1
    chapter.lines.v <- novel.lines.v[start:end]
    chapter.words.v <- tolower(paste(chapter.lines.v, collapse=" "))
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v[which(chapter.word.v!="")]
    chapter.freqs.t <- table(chapter.word.v)
    chapter.raws.l[[chapter.title]] <-  chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.l[[chapter.title]] <- chapter.freqs.t.rel
  }
}

## 8.1 Introduction
# KWIC or Keyword in Context for determining a word's meaning or sense by looking at the words around it

input.dir <- "data/plainText"
files.v <- dir(input.dir, "\\.txt$")

## 8.2 Custom Functions
# Function to print a vector of file names in user friendly format
show.files <- function(file.name.v){
  for(i in 1:length(file.name.v)){
    cat(i, file.name.v[i], "\n", sep=" ")
  }
}

show.files(files.v)

## 8.3 A Word List Making Function
# Function takes a vector of file names and a directory path and returns a list in which each item in the list is an ordered vector of words from one of the files in the vector of file names
make.file.word.v.l <- function(files.v, input.dir){
  # set up an empty container
  text.word.vector.l <- list()
  # loop over the files
  for(i in 1:length(files.v)){
    # read the file in (notice that it is here that we need to know the input
    # directory)
    text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                   what="character", sep="\n")
    # convert to single string
    text.v <- paste(text.v, collapse=" ")
    # lowercase and split on non-word characters
    text.lower.v <- tolower(text.v)
    text.words.v <- strsplit(text.lower.v, "\\W")
    text.words.v <- unlist(text.words.v)
    # remove the blanks
    text.words.v <- text.words.v[which(text.words.v!="")]
    # use the index id from the files.v vector as the "name" in the list
    text.word.vector.l[[files.v[i]]] <- text.words.v
  }
  return(text.word.vector.l)
}

my.corpus.l <- make.file.word.v.l(files.v, input.dir)

## 8.4 Finding Words and Their Neighbors
my.corpus.l[[1]][1:100]
positions.v <- which(my.corpus.l[[1]][]=="gutenberg")
