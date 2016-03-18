\name{divide.acs}
\alias{divide.acs}
\title{Divide one acs object or variable by another.
}

\description{The \code{acs} package provides a new S4 method for
  standard division operations using "/" notation.  However, due to the
  nature of estimates and errors, there are actually two types of
  division, with slightly different meanings: depending on which
  variables are being divided, the process may be either a
  "proportion"-type division (in which the numerator is a subset of the
  denominator) or a "ratio"-type division (in which this is not the
  case).  When dividing with standard "a/b" notation, the package will
  always use the more conservative ratio-type procedure.  

  When appropriate, "proportion"-type division may be desirable, as it
  results in lower standarard errors.  To allow users to specify which
  type of division to use for acs objects, the package includes a new
  \code{"divide.acs"} function.  (See details.)}

\usage{
divide.acs(numerator, denominator, method="ratio", verbose=T, output="result")
}

\arguments{
  \item{numerator}{ an acs object to divide }
  \item{denominator}{ an acs object to divide by }
  \item{method}{ either "ratio" (the default) or "proportion", to
  indicate what kind of division is desired }
  \item{verbose}{ whether to provide additional warnings or just shut up }
  \item{output}{either "result" (the default), "div.method", or "both"}
}

\value{
  Returns a new acs object with the results of the division (the
  default), or (when result="div.method") a martix with diagnostic
  information, or (when result="both"), a list with both of these
  objects (the first name $result and the second $div.method).
}

\details{In certain cases, "proportion-style" division will fail, due
to the creation of a negative number under a square root when
calculating new standard errors.  To address this problem and prevent
unnecessary NaN values in the standard.errors, the package implements
the recommended Census practice of simply using "ratio-style" division in
those cases.

If method="proportion" (not the default) and verbose=T (the default),
\code{division.acs} will provide a warning to indicate when
"ratio-style" division has been used, including the number of standard
error cells so affected.  Users wishing to examine a detailed,
cell-by-cell report may run \code{divide.acs} with the
output="div.method" of output="both" to get additional diagnostic
information.

See
"A Compass for Understanding and Using American Community Survey Data"
below for details on when this substitution is recommended.}

\seealso{
      \code{\link{acs-class}}
  }

  \references{
  \enumerate{
  
\item{"A Compass for Understanding and Using American Community Survey
Data: What State and Local Governments Need to Know." Washington, DC:
U.S. Census Bureau.
2009. \url{http://www.census.gov/library/publications/2009/acs/state-and-local.html}.}
}}
  \author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

