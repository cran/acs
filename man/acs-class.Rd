\name{acs-class}
\Rdversion{1.1}
\docType{class}
\alias{acs-class}
\alias{acs.colnames,acs-method}
\alias{acs.units,acs-method}
\alias{-,acs,acs-method}
\alias{-,acs,numeric-method}
\alias{-,numeric,acs-method}
\alias{/,acs,acs-method}
\alias{/,acs,numeric-method}
\alias{/,numeric,acs-method}
\alias{[<-,acs-method}
\alias{[,acs-method}
\alias{*,acs,acs-method}
\alias{*,acs,numeric-method}
\alias{*,numeric,acs-method}
\alias{+,acs,acs-method}
\alias{+,acs,numeric-method}
\alias{+,numeric,acs-method}
\alias{currency.year,acs-method}
\alias{endyear,acs-method}
\alias{estimate,acs-method}
\alias{geography,acs-method}
\alias{modified,acs-method}
\alias{show,acs-method}
\alias{span,acs-method}
\alias{standard.error,acs-method}
\alias{summary,acs-method}
\alias{currency.year}
\alias{endyear}
\alias{estimate}
\alias{geography}
\alias{modified}
\alias{span}
\alias{standard.error}
\alias{acs.colnames}
\alias{acs.units}
\alias{cbind.acs}
\alias{rbind.acs}
\alias{dim.acs}
\alias{length.acs}

\title{Class \code{"acs"}}
\description{
The acs class provides a convenient wrapper for data from the
U.S. Census American Community Survey.  Estimates and standard errors
are kept together, along with metadata necessary to manipulate and
analyze data in this form.
}
\section{Objects from the Class}{

Objects can be created by calls of the form \code{new("acs", ...)}, or
  through helper functions provided by the package (currently only
  read.acs).  Once created, acs objects can be manipulated through new
  methods to deal appropriately with common analytical tasks such as
  combining subgroups or geographies, mathematical operations on
  estimates, tests of significance, and computing (and eventually
  plotting) confidence intervals.

}
\section{Slots}{
  \describe{

    \item{\code{endyear}:}{Object of class \code{"integer"} indicating
  the last year included in the dataset (e.g., 2009 for data from the
  2005--2009 ACS)}

\item{\code{span}:}{Object of class \code{"integer"} representing the
  number of years the dataset spans (e.g., 3 for data from the
  2007--2009 ACS) }

\item{\code{geography}:}{Object of class \code{"data.frame"} containing
  four variables extracted from the data's geographic header: the
  geographic place names, the summary level, and two numeric
  identifiers}

\item{\code{acs.colnames}:}{Object of class \code{"character"} giving
the variable names for each column }

\item{\code{modified}:}{Object of class \code{"logical"} to indicate
  whether the object has been modified since construction }

\item{\code{acs.units}:}{Object of class \code{"factor"} designating the
  type of units in each column (e.g., count or percentage or dollars)}

\item{\code{currency.year}:}{Object of class \code{"integer"} indicating
  the year that all currency values have been adjusted to (by default
  the same as endyear, but able to be modified by the user for
  comparisons)}

\item{\code{estimate}:}{Object of class \code{"matrix"}  holding
  the reported ACS estimates}

\item{\code{standard.error}:}{Object of class \code{"matrix"} holding
  the calculated values of the standard errors for each estimate,
  derived from the reported 90\% confidence intervals }


  }
}
\section{Methods}{
  \describe{
    \item{acs.colnames}{\code{signature(object = "acs")}:   Standard
      accessor function; returns character vector }
    \item{acs.units}{\code{signature(object = "acs")}:   Standard
      accessor function; returns factor vector }
    \item{currency.year}{\code{signature(object = "acs")}: Standard accessor function; returns integer }
    \item{endyear}{\code{signature(object = "acs")}: Standard accessor function; returns integer }
    \item{estimate}{\code{signature(object = "acs")}: Standard accessor
      function; returns matrix }
    \item{geography}{\code{signature(object = "acs")}: Standard accessor
      function; returns data.frame }
    \item{modified}{\code{signature(object = "acs")}: Standard accessor
      function; return logical }
    \item{span}{\code{signature(object = "acs")}: Standard accessor function; returns integer }
    \item{standard.error}{\code{signature(object = "acs")}: Standard
      accessor function; returns matrix }
    \item{summary}{\code{signature(object = "acs")}: Prints standard summary data on both estimates
      and standard errors}
    \item{confint}{\code{signature(object = "acs")}: Prints estimates
      with 90\% confidence intervals}
%     \item{-}{\code{signature(e1 = "acs", e2 = "acs")}: Subtract one
%       estimate from another; deal appropriately with standard error
%       (\code{sqrt(standard.error(e1)^2+standard.error(e2)^2} }
%     \item{-}{\code{signature(e1 = "acs", e2 = "numeric")}: Subtract a
%       standard number from an estimate; deal appropriately with standard error
%       (\code{sqrt(standard.error(e1)} }
%     \item{-}{\code{signature(e1 = "numeric", e2 = "acs")}: Subtract an estimate
% from a standard number; deal appropriately with standard error
%       (\code{sqrt(standard.error(e2)} }
%     \item{/}{\code{signature(e1 = "acs", e2 = "acs")}: Divide one
%       estimate by another; deal appropriately with standard error.  (Assumes that numerator
%       is NOT a subset of denominator.) }
%     \item{/}{\code{signature(e1 = "acs", e2 = "numeric")}: ... }
%     \item{/}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
%     \item{[<-}{\code{signature(x = "acs")}: ... }
%     \item{[}{\code{signature(x = "acs")}: ... }
%     \item{*}{\code{signature(e1 = "acs", e2 = "acs")}: ... }
%     \item{*}{\code{signature(e1 = "acs", e2 = "numeric")}: ... }
%     \item{*}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
%     \item{+}{\code{signature(e1 = "acs", e2 = "acs")}: ... }
%     \item{+}{\code{signature(e1 = "acs", e2 = "numeric")}: ... }
%     \item{+}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
	 }
In addition to these methods, R subset and replacement methods are
provided for acs class objects, and new methods for basic arithmetic functions (+, -, *,
/) have been included to deal appropriately with combining estimates
and standard errors. 
       }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\examples{
showClass("acs")
# load some data from the ACS
data(kansas09)
str(kansas09)

# access slots
endyear(kansas09)
span(kansas09)
estimate(kansas09)[1:5,1:5]
standard.error(kansas09[1:5,1:5])


# subset
kansas09[1:4,6:9]

# more complicated subsets
kansas09[c("Linn County, Kansas", "Wilson County, Kansas") , grep(pattern="21.years", x=acs.colnames(kansas09))]

# addition on estimates and errors
kansas09[1:4,25]+kansas09[1:4,49]

# can even multiply and divided
# males per female, by county
kansas09[1:4,2]/kansas09[1:4,26]

# (males<5 plus females<5) * 12
(kansas09[7,3]+kansas09[7,27]) * 15

}
\keyword{classes}
