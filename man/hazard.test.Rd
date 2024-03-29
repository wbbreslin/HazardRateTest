% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hazard.test.R
\name{hazard.test}
\alias{hazard.test}
\title{Compare hazard rates of two samples}
\usage{
hazard.test(x, y, alternative = "two.sided")
}
\arguments{
\item{x}{a vector of data from the first sample}

\item{y}{a vector of data from the second sample}

\item{alternative}{a character string specifying the alternative hypothesis for hazard rate ordering, must be one of "two.sided" (x=y), "greater" (x>y) or "less" (x<y)}
}
\value{
Returns the test statistic and p-value for the hazard rate test
}
\description{
Compare hazard rates of two samples
}
\examples{
#Testing if X > Y in hazard rate ordering
x = rexp(n=10, rate=1/10)
y = rexp(n=10, rate=1/5)
hazard.test(x,y, alternative="greater")

}
