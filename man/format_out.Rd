% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customizable_functions.R
\name{format_out}
\alias{format_out}
\title{Format output for custom model to use in eesim}
\usage{
format_out(mod)
}
\arguments{
\item{mod}{A model object from lm, glm, etc.}
}
\value{
Output with the correct values and column names needed for a
   modeling function to pass to \code{\link{eesim}}.
}
\description{
Formats the output within a modeling function to be used in a call to
\code{\link{eesim}} when the model is fit using \code{glm} or something
similar.
}
\examples{
dat <- data.frame(x=rnorm(1000, 0, 1), outcome = rnorm(1000, 5, 1))
lin_mod <- lm(outcome~x, data=dat)
format_out(lin_mod)

}
