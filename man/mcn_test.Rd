% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcn_test.R
\name{mcn_test}
\alias{mcn_test}
\title{McNemar-Test}
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
Funktion zur Berechnung des gerichteten exakten McNemar-Tests
}
