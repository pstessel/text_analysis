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

chap.positions.v[1]
chap.positions.v[2]
for(i in 1:length(chap.positions.v)){
  print(chap.positions.v[i])
}

for(i in 1:length(chap.positions.v)){
  print(paste("Chapter",i," begins at position",
              chap.positions.v[i]), sep="")
}

# Create two empty list objects to store the calculated results of each iteration.
chapter.raws.l <- list()
chapter.freqs.l <- list()

for(i in 1:length(chap.positions.v)){
  if(i != length(chap.positions.v)){
    chapter.title <- novel.lines.v[chap.positions.v[i]]
    start <- chap.positions.v[i]+1
    end <- chap.positions.v[i+1]-1
    chapter.lines.v <- novel.lines.v[start:end]
    chapter.words.v <- tolower(paste(chapter.lines.v, collapse = " "))
    chapter.words.l <- strsplit(chapter.words.v, "\\W")
    chapter.word.v <- unlist(chapter.words.l)
    chapter.word.v <- chapter.word.v[which(chapter.word.v != "")]
    chapter.freqs.t <- table(chapter.word.v)
    chapter.raws.l[[chapter.title]] <- chapter.freqs.t
    chapter.freqs.t.rel <- 100*(chapter.freqs.t/sum(chapter.freqs.t))
    chapter.freqs.l[[chapter.title]] <- chapter.freqs.t.rel
  }
}

## Accessing and Processing List Items

chapter.freqs.l[[1]]["whale"]
chapter.freqs.l[[2]]["whale"]
chapter.freqs.l[[135]]["whale"]

whale.l <- lapply(chapter.freqs.l, '[', 'whale')
whale.l
rbind(whale.l[[1]], whale.l[[2]], whale.l[[3]])

## Example of do.call
x <- list(1:3, 4:6, 7:9)
x
# To convert list into a matrix where each row is one of the vectors
# and each column is one of the three integers from each of the list items
# use do.call
do.call(rbind, x)

whales.m <- do.call(rbind, whale.l)
whales.m

# Matrix of chapter-by-chapter values for occurences of 'ahab.'
ahab.l <- lapply(chapter.freqs.l, '[', 'ahab')
ahabs.m <- do.call(rbind, ahab.l)


### cbind
class(whales.m[,1])
whales.m[,1]

x <- c(1,2,3,4,5,6)
y <- c(2,4,5,6,7,8)
z <- c(24,23,34,32,12,10)
test.m <- cbind(x,y,z)
test.m
test.m[2,3]
test.m[2,] # show all the values in the second row
test.m[,"y"]

whales.v <- whales.m[,1]
whales.v
ahabs.v <- ahabs.m[,1]
ahabs.v
whales.ahabs.m <- cbind(whales.v, ahabs.v)
dim(whales.ahabs.m)

# rename the columns
colnames(whales.ahabs.m) <- c("whale", "ahab")
whales.ahabs.m

barplot(whales.ahabs.m, beside=T, col="grey")

### 4.1 Write code that will find the value for another word (e.g., queequeg)
### and then bind those values as a third column in the matrix. Plot the results
### as in the example code.

queequeg.l <- lapply(chapter.freqs.l, '[', 'queequeg')
queequeg.l
queequeg.m <- do.call(rbind, queequeg.l)
queequegs.v <- queequeg.m[,1]
whales.ahabs.queequegs.m <- cbind(whales.v, ahabs.v, queequegs.v)
dim(whales.ahabs.queequegs.m)
colnames(whales.ahabs.queequegs.m) <- c("whale", "ahab", "queequeg")
barplot(whales.ahabs.queequegs.m, beside=T, col="grey")

### 4.2 The above plots were derived from the list of relative frequency data.
### Write a script to plot the raw occurences of "whale" and "ahab" per chapter.

whale.raws.l <- lapply(chapter.raws.l, '[', 'whale')
whales.raw.m <- do.call(rbind, whale.raws.l)
whales.raw.m

ahab.raws.l <- lapply(chapter.raws.l, '[', 'ahab')
ahabs.raw.m <- do.call(rbind, ahab.raws.l)
ahabs.raw.m

whales.raw.v <- whales.raw.m[,1]
ahabs.raw.v <- ahabs.raw.m[,1]
whales.ahabs.raw.m <- cbind(whales.v, ahabs.v)
dim(whales.ahabs.raw.m)

colnames(whales.ahabs.raw.m) <- c("whale", "ahab")
barplot(whales.ahabs.raw.m, beside=T, col="grey")
