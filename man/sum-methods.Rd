\name{sum-methods}
\docType{methods}
\alias{sum}
\alias{sum-methods}
\alias{sum,acs-method}
\alias{sum,acs,acs-method}
\title{acs Methods for Function \code{sum}}
\description{
Returns the sum of all the estimates present in its arguments,
along with proper treatment of standard errors.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "acs")}}{
When passed an acs object (possibly involving subsetting), \code{sum}
will return a new acs object created by aggregating (adding) all
estimates in the object, and adding the corresponding standard errors in
a statistically appropriate way.  (Aggregate standard errors are
computed by taking the square root of the sum of the squared standard
errors of the terms to be aggregated.)

If the orginal object contains a single row, the geographic metadata and
row name is preserved; if not, the geographic metadata is replaced with
the term "aggregate" (or the contents of the first item of the (vector)
option \code{agg.term}).

If the orginal object contains a single column, the column names and
acs.units data are preserved; if not, the column names are replaced with
the term "aggregate" or the contents of the second item of the (vector)
option \code{agg.term}; note: if \code{agg.term} is one one item in
length, it will be repeated here if needed.

All other acs-class metadata is preserved, except for the \code{modifed}
flag, which is set to TRUE.
}
}}

\examples{

# load ACS data
data(kansas09)

# aggregate the third column, all rows
sum(kansas09[,3])

# aggregate the fifth row, all column
sum(kansas09[5,])

# aggregate all rows, columns 3 through 25, rename rows "Kansas" and columns "Total Males"
sum(kansas09[, 3:25], agg.term=c("Kansas","Males"))
}
\keyword{methods}

