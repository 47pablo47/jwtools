#' Quickly paste vector into single string
#'
#' This function does pretty much the same as paste0, though it is much quicker to call
#' @param ... Any type of variables that will be pasted into string
#' @return A single string made of the input vector
#' @examples 
#' var <- "file"
#' output1 <- p("data/",var,".png")
#' output2 <- p("cell_",1,"slice_",2)
#' @export
p <- function(...) {
  a <- c(...)
  b <- paste(a, collapse="")
  if (b=="") {
    message("WARNING: no input")
  }
  return(b)
}

#' Quickly peek the first rows of data frame
#'
#' This function allows for peeking first 10 rows of data frame and is easy to call
#' @param x A data frame
#' @return Ten first rows of input data frame
#' @examples 
#' vd(df)
#' @export

vd <- function(x) {
  View(head(x,10))
}



