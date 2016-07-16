############################################################################### "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 6: Measures of Lexical Variety                                      ###############################################################################

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
whale.l <- lapply(chapter.freqs.l, '[', 'whale')
whales.m <- do.call(rbind, whale.l)
ahab.l <- lapply(chapter.freqs.l, '[', 'ahab')
ahabs.m <- do.call(rbind, ahab.l)
i.l <- lapply(chapter.freqs.l, '[', 'i')
i.m <- do.call(rbind, i.l)
my.l <- lapply(chapter.freqs.l, '[', 'my')
my.m <- do.call(rbind, my.l)
whales.v <- as.vector(whales.m[,1])
ahabs.v <- as.vector(ahabs.m[,1])
i.v <- as.vector(i.m[,1])
my.v <- as.vector(my.m[,1])
whales.ahabs.i.my.m <- cbind(whales.v, ahabs.v, i.v, my.v)
colnames(whales.ahabs.i.my.m) <- c("whale", "ahab", "i", "my")

whales.ahabs.i.my.m[which(is.na(whales.ahabs.i.my.m))] <- 0
whales.ahabs.i.my.m

## 6.2 Mean Word Frequency
length(chapter.raws.l)
names(chapter.raws.l)
class(chapter.raws.l$"CHAPTER 1. Loomings.")
class(chapter.raws.l[[1]])
chapter.raws.l$"CHAPTER 1. Loomings."
chapter.raws.l[[1]]
str(chapter.raws.l)
sum(chapter.raws.l[[1]]) # count of all word tokens in chapter
length(chapter.raws.l[[1]]) # total number of word types

# Calculate word frequency by dividing the total nuber of tokens by the total number of unique word types
sum(chapter.raws.l[[1]])/length(chapter.raws.l[[1]])

# A much simpler way to do this is to use R's built-in mean function
mean(chapter.raws.l[[1]])

## 6.3 Extracting Word Usage Means

# Get the mean word usage for every chapter in Moby Dick
lapply(chapter.raws.l,mean)

# Put the results into a matrix object
mean.word.use.m <- do.call(rbind, lapply(chapter.raws.l,mean))
dim(mean.word.use.m)
mean.word.use.m

# Plot values. Chapters with higher bars are, in one manner of speaking, less rich.
plot(mean.word.use.m, type="h")

# Normalize values across the text. Overall mean for all chapters is calculated and subtracted from the mean of each individual chapter. This subtracts away the expected value (overall mean) and shows only the deviations.
scale(mean.word.use.m)
plot(scale(mean.word.use.m), type="h")

## 6.4 Ranking the values
order(mean.word.use.m)
order(mean.word.use.m, decreasing = T)
mean.word.use.m[order(mean.word.use.m, decreasing = T),]

## 6.5 Calculating the Type-Token Ration (TTR) Inside lapply
length(chapter.raws.l[[1]])/sum(chapter.raws.l[[1]])*100
ttr.l <- lapply(chapter.raws.l, function(x) {length(x)/sum(x)*100})
ttr.l
ttr.m <- do.call(rbind, ttr.l)
ttr.m
plot(ttr.m, type="h")

## 6.6
# Test the assertion that document length is a strong determiner in the rate at which words get recycled. Measure the strength of correlation between chapter length and TTR. Convert TTR values in ttr.m to a vector.
ttr.v <- as.vector(ttr.m)
ttr.v

# Create a vector of chapter lengths.
lapply(chapter.raws.l, sum)
sum.chapter.words.m <- do.call(rbind, lapply(chapter.raws.l, sum))
sum.chapter.words.v <- as.vector(sum.chapter.words.m)
cor(ttr.v, sum.chapter.words.v)

# Try with mean.word.use.m
mean.word.use.v <- as.vector(mean.word.use.m)
cor(mean.word.use.v, sum.chapter.words.v)

plot(mean.word.use.v, sum.chapter.words.v, main="Correlation Chapter Length and Mean Word Use",
     xlab="Mean Word Use", ylab="Number of Chapter Words", pch=19)

# Add fit lines
abline(lm(sum.chapter.words.v~mean.word.use.v), col="red") # regression line (y~x)
lines(lowess(mean.word.use.v,sum.chapter.words.v), col="blue") # lowess line (x,y)

