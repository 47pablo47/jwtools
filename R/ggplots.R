#' Most basic ggplot
#'
#' The most basic ggplot useful for visualisation of non-complicated data frames
#' @param df Data frame used for plotting
#' @param val Name of the column that will be passed to the aes_string as y
#' @param var Name of the column that will be passed to the aes_string as x
#' @param jit TRUE/FALSE. Choose whether to display jitter points
#' @param tilt TRUE/FALSE. Choose to tilt x axis by 45 degrees
#' @return Nice and simple plot
#' @examples 
#' bplot(mtcars,"cyl","hp")
#' @export
bplot <- function(df,var="treat",val,jit=TRUE, tilt = FALSE) {
  p <- ggplot(df, aes_string(x=var, y=val)) +
      stat_summary(geom="bar", fun="mean", color="black", fill="white", width=0.5) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      theme_bw()
  if (jit == TRUE) {
    p <- p + geom_jitter(width=0.2)
  } 
  if (tilt == TRUE) {
    p <- p +
      theme(axis.text.x = element_text(angle = 45, hjust=1)) 
  }
  return(p)
}


