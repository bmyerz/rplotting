# df= take average of
# todf normalize by average (could be the same df)
# precondition: df only contains rows to be summarized
normalize_all <- function(df, todf, queries= c("Q1", "Q2", "Q3a", "Q3b", "Q3c", "Q4", "Q9", "Q5a", "Q5b")) {
  todf <- todf[!is.na(todf$in_memory_runtime),]
  df <- df[!is.na(df$in_memory_runtime),]
  means <- summarize(df$in_memory_runtime, by=df$query, mean)
  #print(means)
  #print(todf)
  for (queryname in queries) {
    #print(sprintf("q: %s",queryname))
    #print(sprintf("means: %s", means[means$"df$query"==queryname,]))
    runtime <- as.double(todf[todf$query==queryname,]$in_memory_runtime)
    #print("runtime:"); print(typeof(runtime))
    thismean <- means[means$"df$query"==queryname,]$"df$in_memory_runtime"
    #print("thismean:"); print(typeof(thismean[1]))
    todf[todf$query==queryname,]$in_memory_runtime <-  runtime / thismean
  }
  return(todf)
}

query_order <- function(df, queries_ordered) {
  res <- df
  res$query <- factor(res$query, levels=queries_ordered, ordered=TRUE)
  return(res)
}