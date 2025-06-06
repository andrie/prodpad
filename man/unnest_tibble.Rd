% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest_tibble.R
\name{unnest_tibble}
\alias{unnest_tibble}
\title{Unnest a list column of tibbles.}
\usage{
unnest_tibble(.data, .col)
}
\arguments{
\item{.data}{Tibble}

\item{.col}{Column name}
}
\value{
A modified tibble
}
\description{
This is frequently necessary to unnest \code{tags}, \code{products}, \code{feedbacks}, \code{personas}
and other columns embedded in prodpad data.
}
\details{
This function converts the list column into a list of tibbles, then calls
\verb{[tidyr::unnest_longer()]} followed by \verb{[tidyr::unnest_wider()]}.
The effect is to hoist the entire list into the top level data.
}
