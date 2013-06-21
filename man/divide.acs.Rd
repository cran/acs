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

  To allow users to specify which type of division to use for acs
  objects, the package includes a new \code{"divide.acs"} function.  
   }

\usage{
divide.acs(numerator, denominator, method="ratio", verbose=T)
}

\arguments{
  \item{numerator}{ an acs object to divide }
  \item{denominator}{ an acs object to divide by }
  \item{method}{ either "ratio" (the default) or "proportion", to
  indicate what kind of division is desired }
  \item{verbose}{ whether to provide additional warnings or just shut up }
}

\value{
  Returns a new acs object with the results of the division.
}

\seealso{
      \code{\link{acs-class}}
  }
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

