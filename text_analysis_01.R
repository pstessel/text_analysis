###################################################################################
# "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
###################################################################################

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/text_analysis")

rm(list=ls())

options(max.print = 1000000)


# store Moby Dick as a character vector
text.v <- scan("data/plainText/melville.txt", what="character", sep="\n")

# separate content from metadata
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
start.v
end.v
length(text.v)

# save Project Gutenberg metadata
start.metadata.v <- text.v[1:start.v -1]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)

# text of the novel
novel.lines.v <- text.v[start.v:end.v]

length(text.v)
length(novel.lines.v)

# remove line breaks
novel.v <- paste(novel.lines.v, collapse=" ")

# convert text to lowercase characters
novel.lower.v <- tolower(novel.v)
novel.lower.v

# extract words into a list
moby.words.l <- strsplit(novel.lower.v, "\\W")
moby.words.l

# convert the list back into a vector
moby.word.v <- unlist(moby.words.l)

# remove blanks caused by punctuation
not.blanks.v <- which(moby.word.v!="")
not.blanks.v

moby.word.v <- moby.word.v[not.blanks.v]
moby.word.v

# shorthand version of above
# moby.word.v <- moby.word.v[which(moby.word.v != "")]

which(moby.word.v=="whale")
moby.word.v[which(moby.word.v=="whale")]

# number of occurances of the word "whale"
length(moby.word.v[which(moby.word.v=="whale")])

# number of words in Moby Dick
length(moby.word.v)

# Put a count of the occurences of whale into whale.hits.v
whale.hits.v <- length(moby.word.v[which(moby.word.v=="whale")])
