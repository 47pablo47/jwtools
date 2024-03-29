% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplots.R
\name{bplot}
\alias{bplot}
\title{Most basic ggplot}
\usage{
bplot(df, var = "treat", val, jit = TRUE, tilt = FALSE)
}
\arguments{
\item{df}{Data frame used for plotting}

\item{var}{Name of the column that will be passed to the aes_string as x}

\item{val}{Name of the column that will be passed to the aes_string as y}

\item{jit}{TRUE/FALSE. Choose whether to display jitter points}

\item{tilt}{TRUE/FALSE. Choose to tilt x axis by 45 degrees}
}
\value{
Nice and simple plot
}
\description{
The most basic ggplot useful for visualisation of non-complicated data frames
}
\examples{
bplot(mtcars,"cyl","hp")
}
