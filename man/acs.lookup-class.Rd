\name{acs.lookup-class}
\Rdversion{1.1}
\docType{class}
\alias{acs.lookup-class}
\alias{is.acs.lookup}
\alias{results}
\alias{+,acs.lookup,acs.lookup-method}
\alias{c,acs.lookup-method}
\alias{endyear,acs.lookup-method}
\alias{show,acs.lookup-method}
\alias{[,acs.lookup-method}
\alias{results,acs.lookup-method}
\alias{span,acs.lookup-method}

\title{Class \code{"acs.lookup"}}

\description{ A new class to hold the results of calls to
\code{acs.lookup}, typically for possible modification and then passing
to calls to \code{acs.fetch} (using the "variable=" argument).  }

\section{Objects from the Class}{ Objects can be created by calls of the
form \code{new("acs.lookup", ...)}, but more typically will be created
as output from calls to \code{acs.lookup}.  }

\section{Slots}{
  \describe{
    \item{\code{endyear}:}{Object of class \code{"numeric"} indicating
  the year of the census dataset; e.g., for data from the 2005-2009 ACS,
  endyear=2009}
    \item{\code{span}:}{Object of class \code{"numeric"} indicating
  the span in years of the census dataset; e.g., for data from the 2005-2009 ACS,
  span=5}
    \item{\code{args}:}{Object of class \code{"list"} containing the
  search terms used in the call to \code{acs.lookup}, including some or
  all of: keyword, table.name, endyear, dataset, table.number, and case.sensitive.}
    \item{\code{results}:}{Object of class \code{"data.frame"}
  containing character strings in four columns: variable.code,
  table.number, table.name, and variable.name.}
  }
}
\section{Methods}{
  \describe{
    \item{+}{\code{signature(e1 = "acs.lookup", e2 = "acs.lookup")}:
  used for combining two acs.lookup objects into one }
    \item{c}{\code{signature(x = "acs.lookup")}: used for combining two
  acs.lookup objects into one }
    \item{endyear}{\code{signature(object = "acs.lookup")}: returns
  endyear from acs.lookup object }
    \item{[}{\code{signature(object = "acs.lookup")}: used for
  subsetting an acs.lookup object }
    \item{results}{\code{signature(object = "acs.lookup")}: returns
  results (as dataframe) from acs.lookup object }
    \item{span}{\code{signature(object = "acs.lookup")}: returns
  span from acs.lookup object }
	 }
}

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{acs.lookup}}
}
\examples{
showClass("acs.lookup")
}
\keyword{classes}
