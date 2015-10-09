###################################################################################
# "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 4: Token Distribution Analysis                                          #
###################################################################################

rm(list = ls())

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/text_analysis")

text.v <- scan("data/plainText/melville.txt", what="character", sep="\n")
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
novel.lines.v <-  text.v[start.v:end.v]
novel.v <- paste(novel.lines.v, collapse=" ")
novel.lower.v <- tolower(novel.v)
moby.words.l <- strsplit(novel.lower.v, "\\W")
moby.word.v <- unlist(moby.words.l)
not.blanks.v  <-  which(moby.word.v!="")
moby.word.v <-  moby.word.v[not.blanks.v]

# Dispersion Plots

n.time.v <- seq(1:length(moby.word.v))

whales.v <- which(moby.word.v == "whale")
whales.v

w.count.v <- rep(NA, length(n.time.v))
w.count.v
w.count.v[whales.v] <- 1

plot(w.count.v, main="Dispersion Plot of 'whale' in Moby Dick",
     xlab = "Novel Time", ylab = "whale", type = "h", ylim=c(0,1), yaxt='n')

ahabs.v <- which(moby.word.v == "ahab") # find 'ahab'
a.count.v <- rep(NA, length(n.time.v))
# change 'w' to 'a' to keep whales and ahabs in separate variables
a.count.v[ahabs.v] <- 1 # mark the occurrences with a 1
plot(a.count.v, main = "Dispersion Plot of 'ahab' in Moby Dick",
     xlab = "Novel Time", ylab = "ahab", type = "h", ylim=c(0,1), yaxt='n')

### Searching with grep
rm(list=ls(all=TRUE))

text.v <- scan("data/plainText/melville.txt", what="character", sep="\n")
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
novel.lines.v <- text.v[start.v:end.v]

novel.lines.v

chap.positions.v <- grep("^CHAPTER \\d", novel.lines.v)
novel.lines.v[chap.positions.v]

chap.positions.v

novel.lines.v <- c(novel.lines.v, "END")
last.postion.v <- length(novel.lines.v)
chap.positions.v <- c(chap.positions.v, last.postion.v)

chap.positions.v
