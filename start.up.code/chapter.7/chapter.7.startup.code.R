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
chapter.lengths.m <- do.call(rbind, lapply(chapter.raws.l,sum))