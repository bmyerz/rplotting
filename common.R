
##suppressPackageStartupMessages(require(plyr))

# suppressPackageStartupMessages(require(RMySQL))
suppressPackageStartupMessages(require(sqldf))
# options(RMySQL.dbname="grappa") # (rest comes from $HOME/.my.cnf)

suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(reshape))
suppressPackageStartupMessages(require(extrafont))
suppressPackageStartupMessages(require(Hmisc))
# loadfonts()

require(scales)

get_theme <- function(size) {
  return (theme(
  panel.background = element_rect(fill="white"),
  panel.border = element_rect(fill=NA, color="grey50"),
  panel.grid.major = element_line(color="grey80", size=0.3),
  panel.grid.minor = element_line(color="grey90", size=0.3),
  strip.background = element_rect(fill="grey90", color="grey50"),
  strip.background = element_rect(fill="grey80", color="grey50"),
  axis.ticks = element_line(colour="black"),
  panel.grid = element_line(colour="black"),
  axis.text.y = element_text(colour="black", size=size),
  axis.text.x = element_text(colour="black", size=size),
  axis.title.y = element_text(colour="black", size=size),
  axis.title.x = element_text(colour="black", size=size),
  legend.text = element_text(colour="black", size=size)
    #text = element_text(size=14, family="Open Sans")
))
}
# theme for 2-column figures
my_theme <- get_theme(10)
# theme for 1-column figures
big_text_theme <- get_theme(18)

prettify <- function(str) gsub('_',' ',gsub('([a-z])([a-z]+)',"\\U\\1\\E\\2",str,perl=TRUE))

regex_match <- function(reg,str) length(grep(reg,str)) > 0

label_pretty <- function(variable, value) {
  vname <- if (regex_match('variable|value',variable)) '' else paste(variable,': ')
  lapply(paste(vname, prettify(as.character(value))), paste, collapse="\n")
}

x <- function(...) { return(paste(..., sep='#')) }
p <- function(...) { return(paste(..., sep='')) }

# options(sqldf.dbname="osdi.sqlite")

db <- function(query, factors=c(), dbname="osdi.sqlite") {
  d <- sqldf(query, dbname=dbname)
  d[factors] <- lapply(d[factors], factor)
  return(d)
}

rsync_data <- function(){
  system(sprintf("rsync -cz pal:~/osdi.sqlite %s",
                 getOption("sqldf.dbname")))
}

as.continuous <- function(var) as.numeric(as.character(var))

LOG <- function(towrite, f=write) {
  fn <- match.fun(f)
  fn(towrite, file="data.out", append=TRUE)
}

save <- function(g, file=sprintf("%s/%s.pdf",FILE_DIR,FILE_BASE), w=3.3, h=3.1) {
  ggsave(plot=g, filename=file, width=w, height=h)
  print(sprintf("saved: %s", file))
}

system_name <- 'RadishX'
var.colors <- c(
  'radish'='#ff0000',
  'radish-iter'='#ff9966',
  'radish-iter-gbp'='#440088',
  #'radish'='#386cb0',
  #'radish-iter'='#beaed4',
  #'impala-ncg'='#7fc97f',
  #'impala-cg'='#f0027f'
  'impala-ncg'='#009999',
  'impala-cg'='#66ffff',
  'impala'='#33bbbb', #midpoint?
  'impala-ncg-cold'='#666666',
  'impala-cg-cold'='#cccccc',
  'radish-gbp'='#9966ff',
  'radish-gbp-noalign'='#996600', # todo replace radish-gbp
  'radish-sym'='#66ff99',
  'radish-sym-gbp'='#ff66aa',
  'radish-osym-gbp'='#ff66dd',
  'radish-iter-gbp-O0'='#66ff99',
  'radish-gbp-O0'='#ff66aa'
)

top_legend <- theme(legend.direction="horizontal", legend.position="top")

my_colors <- list(scale_color_manual(values=var.colors),
                  scale_fill_manual(values=var.colors))

getname <- function(str){ gsub('.*?\\.(.*)\\.exe','\\1', str) }


