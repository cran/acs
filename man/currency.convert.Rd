\name{currency.convert}
\alias{currency.convert}
\alias{currency.convert,acs-method}
\title{Convert dollar values of acs object to a new base year.
}
\description{
  \code{currency.convert} provides a helper function to create a new
  copy of an acs-class object with a modified currency.year and
  converted dollar values without altering the original object.  
}
\usage{

currency.convert(object, rate="auto", newyear=NA_integer_, verbose=F)

}

\arguments{
  \item{object}{ an acs object}
  \item{rate}{an optional rate to apply; "auto" (the default) will look
    up values from the cpi dataset.}
  \item{newyear}{an integer specifying the new value of currency.year to
    convert into}
  \item{verbose}{whether to print additional information about the conversion}
}

\details{
  
  \code{currency.convert} provides a helper function to create a new
  copy of an acs-class object with a modified currency.year and
  converted dollar values without altering the original object.  When
  \code{rate="auto"} (the default), \code{currency.convert} will look up
  values from the \code{cpi} database to use in conversion.  When a
  numeric rate is provided through this option, actual \code{cpi} values
  are ignored.  When \code{verbose=T}, currency.convert will provide
  additional information about the rates of conversion and the
  acs.colnames converted.

  As of version 2.0 the package includes CPI data from 1913 through
  2015, allowing conversion of dollar values for any years in this
  range.}


\value{ Returns a new acs object with updated dollar values and
  \code{currency.year} metadata.

  Unlike \code{currency.year<-}, \code{currency.convert} does not alter
  the original object.}

\seealso{

  \code{\link{currency.year}}

  \code{\link{cpi}}

}
\examples{
lawrence10                                   # median income data, endyear = 2010
currency.convert(lawrence10, newyear=2014)   # convert $$ to 2014 dollars 
currency.convert(lawrence10, newyear=1929)   # convert $$ to 1929 dollars
}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

