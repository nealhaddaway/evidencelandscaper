test_that("disk-backed q-gram overlap selection matches original self-join", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  library(data.table)
  signatures <- data.table(idx=c(1L,2L,3L,1L,2L,4L,2L,3L,4L),
                           qgram=c("aaaa","aaaa","aaaa","bbbb","bbbb","bbbb","cccc","cccc","cccc"))
  lengths <- c(100L,100L,80L,110L)
  prior_n <- 1L
  sig <- copy(signatures)
  setkey(sig,qgram)
  old <- sig[sig,allow.cartesian=TRUE,nomatch=0L][idx<i.idx,
    .(shared_rare_qgrams=.N),by=.(i=idx,j=i.idx)]
  old <- old[shared_rare_qgrams>=2L]
  old <- old[is.finite(pmin(lengths[i],lengths[j])/pmax(lengths[i],lengths[j])) &
    pmin(lengths[i],lengths[j])/pmax(lengths[i],lengths[j])>=0.75]
  old <- old[i>prior_n|j>prior_n]
  expected <- unique(paste(old$i,old$j,sep="::"))
  file <- tempfile(fileext=".sqlite")
  db <- DBI::dbConnect(RSQLite::SQLite(),file)
  on.exit({DBI::dbDisconnect(db);unlink(file)},add=TRUE)
  DBI::dbExecute(db,"CREATE TABLE overlaps(i INTEGER,j INTEGER,n INTEGER,PRIMARY KEY(i,j)) WITHOUT ROWID")
  for (v in split(signatures$idx,signatures$qgram)) {
    v <- sort(unique(v))
    if(length(v)<2L)next
    for(i in seq_len(length(v)-1L)) {
      a<-v[[i]]
      for(b in v[seq.int(i+1L,length(v))]) {
        if(a<=prior_n && b<=prior_n)next
        if(min(lengths[a],lengths[b])/max(lengths[a],lengths[b])<0.75)next
        DBI::dbExecute(db,paste0(
          "INSERT INTO overlaps VALUES(",a,",",b,",1) ",
          "ON CONFLICT(i,j) DO UPDATE SET n=MIN(n+1,2)"))
      }
    }
  }
  actual<-DBI::dbGetQuery(db,"SELECT i,j FROM overlaps WHERE n>=2 ORDER BY i,j")
  expect_setequal(paste(actual$i,actual$j,sep="::"),expected)
})
