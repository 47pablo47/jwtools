#' Check normality
#'
#' Perform quick Shapiro-Wilk test of normality on given data frame
#' @param df A data frame that contains measured values
#' @param y A string containng the named of the column that stores the number values
#' @param x A string conting the name of the grouping variable
#' @return Table with info about normality
#' @examples 
#' shw(iris, "Sepal.Width", "Species")
#' @export
shw <- function(df, y, x="treat") {
  res <- df %>% 
    group_by(!!sym(x)) %>% 
    summarise(pval = shapiro_test(!!sym(y))$p.value) %>% 
    ungroup() %>% 
    mutate(norm = case_when(pval < 0.05 ~ "no",
           TRUE ~ "yes"))
  return(res)
}



