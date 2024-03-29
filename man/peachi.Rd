% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peachi_test.R
\name{peachi}
\alias{peachi}
\title{Pearsons Chi-Quadrat Test}
\usage{
peachi(data)
}
\arguments{
\item{data}{Matrix mit den beobachteten Werten}
}
\value{
n_p2, n_p•, Chi-Quadrat und df, kritischer Bereich, p-Wert und Interpretation
}
\description{
Berechnet Pearsons Chi-Quadrat Test fuer unbestimmte Anzahl Kategorien
}
\examples{
data = matrix(c(8,5,3,2, 2,5,7,8), ncol = 2)
peachi(data)
}
