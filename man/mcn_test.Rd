% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcn_test.R
\name{mcn_test}
\alias{mcn_test}
\title{Exakter McNemar-Test}
\usage{
mcn_test(data, alternative = c("less", "greater", "two.sided"))
}
\arguments{
\item{data}{Matrix mit den beobachteten Werten}

\item{alternative}{Richtung der Hypothese}
}
\value{
x, n, zweiseitiger p-Wert und Interpreation
}
\description{
Funktion zur Berechnung des exakten McNemar-Tests.
}
\examples{
data <- matrix(c(6,20,10,64), ncol = 2)

# linksseitig gerichteter Test
mcn_test(data, alternative = "less")

# zweiseitiger Test
mcn_test(data, alternative = "two.sided")
}
