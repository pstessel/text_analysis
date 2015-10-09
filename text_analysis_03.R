###################################################################################
# "Text Analysis with R for Students of Literature", Matthew L. Jockers, (c) 2014 #
# Chapter 3: Accessing and Comparing Word Frequency Data                          #
###################################################################################

rm(list = ls())

text.v <- scan("data/plainText/melville.txt", what="character", sep="\n")
start.v <- which(text.v == "CHAPTER 1. Loomings.")
end.v <- which(text.v == "orphan.")
start.metadata.v <- text.v[1:start.v -1]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)
novel.lines.v <-  text.v[start.v:end.v]
novel.v <- paste(novel.lines.v, collapse=" ")
novel.lower.v <- tolower(novel.v)
moby.words.l <- strsplit(novel.lower.v, "\\W")
moby.word.v <- unlist(moby.words.l)
not.blanks.v  <-  which(moby.word.v!="")
moby.word.v <-  moby.word.v[not.blanks.v]
whale.hits.v <- length(moby.word.v[which(moby.word.v=="whale")])
total.words.v <- length(moby.word.v)
moby.freqs.t <- table(moby.word.v)
sorted.moby.freqs.t <- sort(moby.freqs.t , decreasing=T)

sorted.moby.freqs.t["he"]
sorted.moby.freqs.t["she"]
sorted.moby.freqs.t["him"]
sorted.moby.freqs.t["her"]

moby.word.v[4:6]
sorted.moby.freqs.t[1]
sorted.moby.freqs.t["the"]

sorted.moby.freqs.t["him"]/sorted.moby.freqs.t["her"]
sorted.moby.freqs.t["he"]/sorted.moby.freqs.t["she"]

length(moby.word.v)
sum(sorted.moby.freqs.t)

# Convert raw counts into relative frequency percentages
sorted.moby.rel.freqs.t <- 100*(sorted.moby.freqs.t/sum(sorted.moby.freqs.t))

sorted.moby.rel.freqs.t["the"]

plot(sorted.moby.rel.freqs.t[1:10], type="b",
     xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt="n")
axis(1,1:10, labels=names(sorted.moby.rel.freqs.t[1:10]))

## SENSE AND SENSIBILITY

text.v <- scan("data/plainText/austen.txt", what="character", sep="\n")
start.v <- which(text.v == "CHAPTER 1")
end.v <- which(text.v == "THE END")
start.metadata.v <- text.v[1:start.v -1]
end.metadata.v <- text.v[(end.v+1):length(text.v)]
metadata.v <- c(start.metadata.v, end.metadata.v)
novel.lines.v <-  text.v[start.v:end.v]
novel.v <- paste(novel.lines.v, collapse=" ")
novel.lower.v <- tolower(novel.v)
sense.words.l <- strsplit(novel.lower.v, "\\W")
sense.word.v <- unlist(sense.words.l)
not.blanks.v  <-  which(sense.word.v!="")
sense.word.v <-  sense.word.v[not.blanks.v]
whale.hits.v <- length(sense.word.v[which(sense.word.v=="whale")])
total.words.v <- length(sense.word.v)
sense.freqs.t <- table(sense.word.v)
sorted.sense.freqs.t <- sort(sense.freqs.t , decreasing=T)
sorted.sense.freqs.t[1:10]

# Convert raw counts into relative frequency percentages
sorted.sense.rel.freqs.t <- 100*(sorted.sense.freqs.t/sum(sorted.sense.freqs.t))

sorted.sense.rel.freqs.t["the"]

plot(sorted.sense.rel.freqs.t[1:10], type="b",
     xlab="Top Ten Words", ylab="Percentage of Full Text", xaxt="n")
axis(1,1:10, labels=names(sorted.sense.rel.freqs.t[1:10]))

## Combined Evaluation
combined.top.ten <- c(names(sorted.moby.rel.freqs.t[1:10]), names(sorted.sense.rel.freqs.t[1:10]))
unique.combined.top.ten <- unique(combined.top.ten)
unique.combined.top.ten
names(sorted.sense.rel.freqs.t[which(names(sorted.moby.rel.freqs.t[1:10]) %in% names(sorted.sense.rel.freqs.t[1:10]))])

presentSense <- which(names(sorted.sense.rel.freqs.t[1:10])
                       %in% names(sorted.moby.rel.freqs.t[1:10]))
names(sorted.sense.rel.freqs.t[1:10])[-presentSense]

presentMoby <- which(names(sorted.moby.rel.freqs.t[1:10])
                     %in% names(sorted.sense.rel.freqs.t[1:10]))
names(sorted.moby.rel.freqs.t[1:10])[-presentMoby]
