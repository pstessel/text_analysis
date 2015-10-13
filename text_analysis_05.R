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

cor(whales.ahabs.m)
mycor <- cor(whales.ahabs.m[, "whale"], whales.ahabs.m[, "ahab"])
mycor

cor(whales.ahabs.i.my.m)

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

# randomize one of the columns
sample(cor.data.df$whale)

# correlate randomized values against the ordered values in the unshuffled "ahab" column.
cor(sample(cor.data.df$whale), cor.data.df$ahab)

# use a for loop to test the correlation over 10,000 samples
mycors.v <- NULL
for(i in 1:10000){
  mycors.v <- c(mycors.v, cor(sample(cor.data.df$whale), cor.data.df$ahab))
}
min(mycors.v)
max(mycors.v)
range(mycors.v)
mean(mycors.v)
sd(mycors.v)

# plot to show the distribution of all the values in mycors.v
h <- hist(mycors.v, breaks=100, col="grey",
          xlab="Correlation Coefficient",
          main="Histrogram of Random Correlation Coefficients\n
          with Normal Curv",
          plot=T)
xfit <- seq(min(mycors.v), max(mycors.v), length=1000)
yfit <- dnorm(xfit, mean=mean(mycors.v), sd=sd(mycors.v))
yfit <- yfit*diff(h$mids[1:2])*length(mycors.v)
lines(xfit, yfit, col="black", lwd=2)

?cor.test

cor.test(cor.data.df$whale,cor.data.df$ahab, method = "pearson")

## Practice 5.1
# Adding two more columns to the matrix (as per above) with data for the words
# "i" and "my" tells me that those two words are highly correlated (0.7739595).
