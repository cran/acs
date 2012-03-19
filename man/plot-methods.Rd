\name{plot-methods}
\docType{methods}
\alias{plot}
\alias{plot-methods}
\alias{plot,acs-method}
\alias{plot,acs,acs-method}
\title{acs Methods for Function \code{plot}}
\description{
Plot acs objects, with both estimates and confidence intervals.}
\usage{

\S4method{plot}{acs}(x, conf.level=.95, err.col="red", err.lwd=1,
err.pch="-", err.cex=2, err.lty=2, x.res=300, labels="auto",
by="geography", ...)

}

\arguments{
  \item{x}{the acs object to be plotted}

  \item{conf.level}{the desired confidence interval to use for error
  bars; numeric between 0<1} 

\item{err.col}{the color to use for the error bars; analogous to
  graphic parameter \code{col}}

\item{err.lwd}{the line weight to use for the error bars; analogous to
  graphic parameter \code{lwd}}

\item{err.pch}{the point character to use for the error bars; analogous to
  graphic parameter \code{pch}}

\item{err.cex}{the scaling factor to use for the error bars; analogous to
  graphic parameter \code{cex}}

\item{err.lty}{the line type to use for the error bars; analogous to
  graphic parameter \code{lty}}

\item{x.res}{when plot called with a single acs value (see below), x.res
  determines the resolution of the resulting density plot; integer
  (defaults to 300, i.e., the curve is drawn with 300 points)}

\item{labels}{the labels to use for the x axis; defaults to either
  geography names or acs.colnames based on dimensions of object plotted; vector of proper length required}

\item{by}{in cases where multiple rows and columns are plotted,
  whether to provide a different plot for each value of \code{geography} (the
  default) or  \code{acs.colnames}; accepts either "geography" or "acs.colnames"} 

\item{...}{provided to allow for passing of additional arguments to
  plot functions}

}
\section{Methods}{

  \describe{

    \item{\code{signature(object = "acs")}}{

	When passed an acs object (possibly involving subsetting), \code{plot}
	will call a plot showing both estimates and confidence intervals for
	the data contained in the object.

	If the object contains only one row or only one column, \code{plot}
	will use this dimension as the y-axis and will plot each observation
	along the x-axis, as three points (an estimate bracketed by upper and
	lower confidence bounds).  If the object contains multiple rows and
	columns, \code{plot} will return a 1-by-y "plot of plots": by default
	there will be one plot per row showing all the data for each
	geography, although this can be changed by specifying
	\code{by="acs.colnames"}, to plot each variable as its own plot, with
	all of the geographies along the x-axis.

	In the special case where the dimensions of the object are
	exactly c(1,1) (i.e., a single geography and column),
	\code{plot} will return a density plot of the estimate.  In this
	case, \code{conf.level}, \code{err.col}, \code{err.lty}, and
	\code{err.lwd} will be used to determine the properties of the
	margins of error lines.  (For none, use \code{conf.level=F}.
	For these density plots, users may also wish to set \code{xlim}
	and \code{x.res}, which specify the horizontal extent and
	resolution of the plot.)
	
	\code{plot} accepts many of the standard graphical arguments to
	\code{plot}, such as \code{main}, \code{sub}, \code{xlab}, \code{pch},
	and \code{col}, as well new ones listed above.
	}
    }
  }
  
\examples{

# load ACS data
data(kansas07)

# plot a single value
plot(kansas07[4,4])

# plot by geography
plot(kansas07[,10])

# plot by columns
plot(kansas07[4,3:10])

# a density plot for a single variable
plot(kansas07[7,10])

# same, using some graphical parameters
plot(kansas07[7,10], col="blue", err.col="purple", err.lty=3)

plot(kansas07[7,49], col="lightblue", type="h", x.res=3000,
err.col="purple", err.lty=3, err.lwd=4, conf.level=.99,
main=(paste("Distribution of Females>85 Years in ",
geography(kansas07)[7,1], sep="")),
sub="(99-percent margin of error shown in purple)")

# something more complicated...

plot(kansas07[c(1,3,4),3:25], err.col="purple", pch=16, err.pch="x", err.cex=1, ylim=c(0,5000), col=rainbow(23), conf.level=.99, labels=paste("grp. ",1:23))
}
\keyword{methods}

