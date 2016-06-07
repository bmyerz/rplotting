#!/usr/bin/env Rscript
library(sqldf)
library(ggplot2)

makeconn <- function(name) {
  return (function(query) {
    d <- sqldf(query, dbname=name)
    
    return(d)
  })
}

#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}