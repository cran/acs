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

\usage{

\S4method{sum}{acs}(x, agg.term=c("aggregate", "aggregate"),
one.zero=FALSE, ..., na.rm=FALSE)

}

\arguments{
  \item{x}{the acs object to be summed}

  \item{agg.term}{a character vector (length 1 or 2) of labels to use
    for the geography or acs.colnames of the new object}

    \item{one.zero}{a logical flag indicating whether to include
    standard errors for only one zero-value estimates or all (the
    default); see details.}

  \item{...}{reserved for other arguments to pass}

  \item{na.rm}{whether to remove \code{NA}s from the values before
  \code{sum}ming; defaults to \code{FALSE}.}

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

If the original object contains a single row, the geographic metadata and
row name is preserved; if not, the geographic metadata is replaced with
the term "aggregate" (or the contents of the first item of the (vector)
option \code{agg.term}).

If the original object contains a single column, the column names and
acs.units data are preserved; if not, the column names are replaced with
the term "aggregate" or the contents of the second item of the (vector)
option \code{agg.term}; note: if \code{agg.term} is only one item in
length, it will be repeated here if needed.

All other acs-class metadata is preserved, except for the \code{modifed}
flag, which is set to TRUE.
}
}}

\details{Note: when aggregating ACS data, users may want to sum many
fields with "0" values for estimates, especially when working with small
geographies or detailed tables that split the population into many
categories.  In these cases, some analysts have suggested that the
traditional summation procedure for standard errors (taking the
square-root of the sum of the squares of the errors) may over-inflate
the associated margins of error; instead, they recommend an alternative
method, which ignores all but the single largest of the standard errors
for any "zero-estimate" fields.  Although this is somewhat
unconventional, it is provided as an additional user-specified option
here, through the "one.zero" argument.}

\examples{

# load ACS data
data(kansas09)

# aggregate the third column, all rows
sum(kansas09[,3])

# aggregate the fifth row, all column
sum(kansas09[5,])

# aggregate all rows, columns 3 through 25, rename rows "Kansas" and columns "Total Males"
sum(kansas09[, 3:25], agg.term=c("Kansas","Total Males"))
}
\keyword{methods}

