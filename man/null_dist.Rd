% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/null_dist.R
\name{null_dist}
\alias{null_dist}
\title{Finds the exact null distribution for hazard rate test}
\usage{
null_dist(m, n)
}
\arguments{
\item{m}{the sample size of the first sample}

\item{n}{the sample size of the second sample}
}
\value{
Returns a frequency distribution table for the hazard rate test statistic
}
\description{
Finds the exact null distribution for hazard rate test
}
\examples{
null_dist(3,3)

}
