\name{geo.set-class}
\Rdversion{1.1}
\docType{class}
\alias{geo-class}
\alias{geo.set-class}
\alias{api.for}
\alias{api.in}
\alias{sumlev}
\alias{is.geo}
\alias{geo.list}
\alias{is.geo.set}
\alias{[,geo.set-method}
\alias{[[,geo.set-method}
\alias{[<-,geo.set-method}
\alias{[[<-,geo.set-method}
\alias{+,geo,geo.set-method}
\alias{+,geo.set,geo-method}
\alias{+,geo,geo-method}
\alias{+,geo.set,geo.set-method}
\alias{c,geo.set-method}
\alias{combine}
\alias{combine.term}
\alias{combine<-,geo.set-method}
\alias{combine,geo.set-method}
\alias{combine.term<-,geo.set-method}
\alias{combine.term,geo.set-method}
\alias{geo.list,geo.set-method}
\alias{length,geo.set-method}
\alias{show,geo-method}
\alias{name,geo-method}
\alias{sumlev,geo-method}
\alias{api.for,geo-method}
\alias{api.in,geo-method}
\alias{combine<-}
\alias{combine.term<-}
\alias{name}

\title{Class \code{"geo.set"}}

\description{The geo.set class provides a convenient wrapper for
user-defined geographies, used for downloading data from the U.S. Census
American Community Survey.  A geo.set may hold the designation of a
single geography (say, a census tract, a county, or a state), or may
bundle together multiple geographies of various levels, which may or may
not be "combined" when downloaded.  Note that geo.sets may even contain
nested geo.sets.

Note: even a single geographic unit --- one specific tract or county ---
must be wrapped up as a geo.set.  Technically, each individual element
in the set is known as a "geo", but users will rarely (if ever) interact
will individual elements such as this; wrapping all groups of
geographies --- even groups consisting of just one element --- in
geo.sets like this will help make them easier to deal with as the
geographies get more complex.

geo.set objects may be combined with the simple addition operator
(\code{+}).  By default, this will always return "flat" geo.sets with
all the geographies in a single list.  The combination operator
(\code{c}), on the other hand, will generally return nested hierarchies,
embedding sets within sets.  When working with nested sets like this,
the "combine" flag can be set at each level to aggregate subsets within
the structure (although be careful --- if a higher level of set includes
"combine=T" you'll never actually see the unaggregated subsets deeper
down).

Using these different techniques, users are able to create whatever sort
of new geographies they need --- aggregating some geographies, keeping
others distinct (but still bundled as a set for convenience), mixing and
matching different levels of Census geography, and so on.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("geo.set", ...)},
or more frequently through the \code{geo.make()} helper function.
}
\section{Slots}{
  \describe{
    \item{\code{geo.list}:}{Object of class \code{"list"} containing individual census geographies (as geo class
      object) and/or geo.sets.}
    \item{\code{combine}:}{Object of class \code{"logical"} indicating
      whether or not data from the constituent geographies should be
      combined when downloaded.  Set with \code{combine<-} or specified
      when using \code{geo.make}. }
    \item{\code{combine.term}:}{Object of class \code{"character"}
      indicating a new label to use when data is combined; ignored when
      combine set to F.  Set with \code{combine.term<-} or specified
      when using \code{geo.make}. }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "geo.set")}: subset geo.set, similar to
      single-bracket list subsetting in R}
    \item{[[}{\code{signature(x = "geo.set")}: subset geo.set, similar to
      double-bracket list subsetting in R }
    \item{+}{\code{signature(e1 = "geo", e2 = "geo")}: combine two geo
      objects; returns a geo.set (generally reserved for internal use) }
    \item{+}{\code{signature(e1 = "geo", e2 = "geo.set")}: combine a geo object
    onto an existing geo.set; returns a geo.set (generally reserved for internal use)}
    \item{+}{\code{signature(e1 = "geo.set", e2 = "geo")}: combine an
      existing geo.set object with a geo object; returns a geo.set (generally reserved for internal use) }
    \item{+}{\code{signature(e1 = "geo.set", e2 = "geo.set")}: combine
      two geo.set objects; always flattens each set -- no nesting }
    \item{c}{\code{signature(x = "geo.set")}: combine two or more
      geo.set objects, preserving the structure of each -- allows nesting }
    \item{combine<-}{\code{signature(object = "geo.set")}: used to
      set or change value of combine}
    \item{combine}{\code{signature(object = "geo.set")}: returns logical
      value of combine }
    \item{combine.term<-}{\code{signature(object = "geo.set")}: used to
      set or change combine.term }
    \item{combine.term}{\code{signature(object = "geo.set")}: returns combine.term }
    \item{geo.list}{\code{signature(object = "geo.set")}: returns the
      geo.list of the geo.set (used internally)}
    \item{length}{\code{signature(x = "geo.set")}: returns an integer
      indicating how many geographies it contains; non-recursive. }

    \item{name}{\code{signature(object = "geo")}: returns the text name
      of an individual geo object.}

    \item{sumlev}{\code{signature(object = "geo")}: returns the summary
	 level of an individual geo object.}  } }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\references{
http://eglenn.scripts.mit.edu/citystate/category/code/
}

\seealso{
\code{\link{geo.make}}
}
\examples{
showClass("geo.set")
}
\keyword{classes}
