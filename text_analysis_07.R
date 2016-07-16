############################################################################### "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 7: Hapax Richness                                            ###############################################################################

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

## 7.3 A Mini-Conditional Function
# A conditional expression that sums all the values in the raw counts table that are equal to one.
chapter.hapax.v <- sapply(chapter.raws.l, function(x) sum(x==1))

# Divide the number of hapax in each chapter by the total number of words in each chapter.
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l,sum))
hapax.percentage <- chapter.hapax.v / chapter.lengths.m
barplot(hapax.percentage,beside = T,col="grey",
        names.arg=seq(1:length(chapter.raws.l)))
