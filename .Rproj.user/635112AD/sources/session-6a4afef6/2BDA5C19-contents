#' Most basic ggplot
#'
#' The most basic ggplot useful for visualisation of non-complicated data frames
#' @param df Data frame used for plotting
#' @param val Name of the column that will be passed to the aes_string as y
#' @param var Name of the column that will be passed to the aes_string as x
#' @return Nice and simple plot
#' @examples 
#' bplot(mtcars,"cyl","hp")
#' @export
bplot <- function(df,val,var="treat") {
  ggplot(df, aes_string(x=var, y=val)) +
    stat_summary(geom="bar", fun="mean", color="black", fill="white", width=0.5) +
    geom_jitter(width=0.2) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    theme_bw()
}


