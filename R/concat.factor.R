concat.factor <- function(...){
  as.factor(do.call(c, lapply(list(...), as.character)))
}
