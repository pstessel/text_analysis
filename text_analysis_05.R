###################################################################################
# "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 5: Correlation                                                          #
###################################################################################

### Startup Code

rm(list = ls())

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/text_analysis")

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
whales.v <- as.vector(whales.m[,1])
ahabs.v <- as.vector(ahabs.m[,1])
whales.ahabs.m <- cbind(whales.v, ahabs.v)
colnames(whales.ahabs.m) <- c("whale", "ahab")

whales.ahabs.m[which(is.na(whales.ahabs.m))] <- 0

cor(whales.ahabs.m)
mycor <- cor(whales.ahabs.m[, "whale"], whales.ahabs.m[, "ahab"])
mycor

### Discursion on Data Frames
x <- matrix(1,3,3)
x[1,2] <- "Sam I am"
x
x <- matrix(1,3,3)
x.df <- as.data.frame(x)
x.df
x.df[1,2] <- "Sam I am"
class(x.df[1,2])
class(x.df[1,3])
x.df[, 2]
x.df[, "V2"]
x.df$V2

### 5.4 Testing Correlation with Randomization

# make life easier and convert to a data frame
cor.data.df <- as.data.frame(whales.ahabs.m)
s