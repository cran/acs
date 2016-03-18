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
\alias{estimate,acs-method}
\alias{modified,acs-method}
\alias{show,acs-method}
\alias{span,acs-method}
\alias{standard.error,acs-method}
\alias{summary,acs-method}
\alias{estimate}
\alias{modified}
\alias{span}
\alias{standard.error}
\alias{acs.colnames}
\alias{acs.units}
\alias{dim.acs}
\alias{length.acs}
\alias{acs.colnames<-}
\alias{acs.units<-}
\alias{is.acs}
\alias{span<-}
\alias{apply}
\alias{acs.colnames<-,acs-method}
\alias{acs.units<-,acs-method}
\alias{is.acs}
\alias{span<-,acs-method}
\alias{apply,acs-method}

\title{Class \code{"acs"}}
\description{

The acs class provides a convenient wrapper for demographic data from
the U.S. Census, especially the American Community Survey.  Estimates
and standard errors are kept together, along with geographic information
and metadata necessary to manipulate and analyze data in this form.

}
\section{Objects from the Class}{

acs objects can be created by calls of the form \code{new("acs", ...)},
  or through helper functions provided by the package (currently
  \code{read.acs} and \code{acs.fetch}), or from the output of
  subsetting or other calls on existing acs objects.  Once created, acs
  objects can be manipulated through new methods to deal appropriately
  with common analytical tasks such as combining subgroups or
  geographies, mathematical operations on estimates, and computing (and
  plotting) confidence intervals.

}
\section{Slots}{
  \describe{

    \item{\code{endyear}:}{Object of class \code{"integer"} indicating
  the last year included in the dataset (e.g., 2012 for data from the
  2008--2012 ACS)}

\item{\code{span}:}{Object of class \code{"integer"} representing the
  number of years the dataset spans (e.g., 3 for data from the
  2011--2013 ACS); for decennial census datasets (SF1 and SF3), span =
  0.}

\item{\code{geography}:}{Object of class \code{"data.frame"} containing
   columns extracted from the data's geographic header: typically
   includes geographic place names, census summary level values, and
   unique numeric identifiers, but can contain any geographic names or
   labels desired.  When acs objects are created or modified, the first
   geography column will be used to label estimates and standard
   errors.}

\item{\code{acs.colnames}:}{Object of class \code{"character"} giving
the variable names for each column }

\item{\code{modified}:}{Object of class \code{"logical"} to indicate
  whether the object has been modified since construction }

\item{\code{acs.units}:}{Object of class \code{"factor"} designating the
  type of units in each column (e.g., count or percentage or dollars);
  only used minimally, to check appropriateness of some operations;
  mostly reserved for future functionality}

\item{\code{currency.year}:}{Object of class \code{"integer"} indicating
  the year that all currency values have been adjusted to (by default
  the same as endyear, but able to be modified by the user for
  comparisons with other years; see \code{\link{currency.convert}}.)}

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

    \item{sum}{\code{signature(object = "acs")}: Aggregates (adds) all
      estimates in the object, and adds the corresponding standard
      errors in a statistically appropriate way; returns new acs object}

    \item{summary}{\code{signature(object = "acs")}: Prints standard summary data on both estimates
      and standard errors}
    \item{confint}{\code{signature(object = "acs")}: Prints estimates
      with confidence intervals}
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
% \item{/}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
\item{[}{\code{signature(x = "acs")}: subsetting works for acs objects
using standard \code{[i,j]} square bracket notation, similar to
two-dimensional matrices; returns a new acs object with estimates,
standard errors, and associated metadata for "i" rows (geographies) and
"j" columns (variable columns); essentially, subsetting for this class is
structured to mirror standard operations on matrix objects}

   \item{[<-}{\code{signature(x = "acs")}: new values may be
     replaced/assigned to an existing acs object using standard
     \code{[i,j]} bracket notation.  The assignment can accept a number
     of different forms: a valid acs object (including a subsetted one),
     a list of two matrices (ideally named "estimate" and "error" or
     "standard.error"), or a numeric object which may be coerced into a
     matrix (to be used as estimates, with zero-values assigned to
     corresponding standard errors). }
%     \item{*}{\code{signature(e1 = "acs", e2 = "acs")}: ... }
%     \item{*}{\code{signature(e1 = "acs", e2 = "numeric")}: ... }
%     \item{*}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
%     \item{+}{\code{signature(e1 = "acs", e2 = "acs")}: ... }
%     \item{+}{\code{signature(e1 = "acs", e2 = "numeric")}: ... }
%     \item{+}{\code{signature(e1 = "numeric", e2 = "acs")}: ... }
	 }

	 In addition to these methods, new methods for basic arithmetic
	 functions (+, -, *, /) have been provided to deal appropriately
	 with combining estimates and standard errors.  }

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
kansas09[c("Linn County, Kansas", "Wilson County, Kansas") ,
   grep(pattern="21.years", x=acs.colnames(kansas09))]

# addition on estimates and errors
kansas09[1:4,25]+kansas09[1:4,49]

# can even multiply and divide
# males per female, by county
kansas09[1:4,2]/kansas09[1:4,26]

# (males<5 plus females<5) * 12
(kansas09[7,3]+kansas09[7,27]) * 12

# some replacement: males<5 as a percentage of all males
kansas09[,3]=kansas09[,3]/kansas09[,2]
}

\keyword{classes}
