\name{acs.lookup}
\alias{acs.lookup}

\title{
Search for relevant variables and tables from the US Census American Community Survey.
}

\description{ The \code{acs.fetch} function is used to download data
  from the US Census American Community Survey.  The \code{acs.lookup}
  function provides a convenience function to use in advance to locate
  tables and variables that may be of interest.

  \code{acs.lookup} takes arguments similar to \code{acs.fetch} --- in
  particular, "table.number", "table.name", and "keyword", as well as
  "endyear" and "span" --- and searches for matches in the meta-data of
  the Census tables.  When multiple search terms are passed to a given
  argument (e.g., \code{acs.lookup(keyword=c("Female", "GED"))}), the
  tool returns matches where ALL of the terms are found; similarly, when
  more than one lookup argument is used (e.g.,
  \code{acs.lookup(table.number="B01001", keyword="Female")}), the tool
  searches for matches that include all of the terms (i.e., terms are
  combined with a logical "AND", not a logical "OR").

  Results from acs.lookup --- which are acs.lookup class objects --- can
  then be inspected, subsetted (with [square brackets]), and combined
  (with \code{c} or \code{+}) to create custom acs.lookup objects to
  store and later pass to \code{acs.fetch}.
  }

\usage{
acs.lookup(endyear = 2011, span = 5, dataset = "acs", keyword,
     table.name, table.number, case.sensitive = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{endyear}{an integer (defaults to 2011) indicating the latest
    year of the data in the survey (e.g., for data from the 2007-2011
    5-year ACS data, endyear would be 2011; limited by acceptable values
    currently provided by the Census API)}

  \item{span}{an integer indicating the span (in years) of the desired
    ACS data (should be 1, 3, or 5); defaults to 5}

  \item{dataset}{either "acs" (the default) or "sf1", indicating whether
    to look for tables and variables in the American Community Survey or
    the SF1 dataset.}

    \item{keyword}{ a string or vector of strings giving the search
      term(s) to find in the name of the ACS census variable (for
      example, "Male" or "Haiti"); accepts multiple words, which must
      all be found in the returned variable names.  }

    \item{table.name}{a string giving the search term(s) to find in the
      name of the ACS census table (for example, "Sex" or "Age" or "Age
      by Sex"); accepts multiple words, which must all be found in the
      returned table names.  }

    \item{table.number}{a string (not a number) indicating the desired
      table from the Census to fetch; examples: "B01003" or "B23013";
      always case-sensitive. Used to identify all variables for a given
      table number. }

    \item{case.sensitive}{a logical flag indicating whether searching is
      case-sensitive (the default) or not.  Note that the Census is not
      entirely consistent in capitalization in table and variable names,
      so setting \code{case.sensitive=F} may be useful in finding all
      possible matches.}
  }

\value{ Returns an acs.lookup class object with the results of the
  query.  acs.lookup objects can be subsetted and combined, and passed
  to the "variable" argument of \code{acs.fetch}.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}

\seealso{
\code{\link{acs.lookup-class}}
}

\examples{
acs.lookup(table.number="B01001")
acs.lookup(table.number="B01001", keyword="Female")
acs.lookup(keyword=c("Female", "GED"))
acs.lookup(keyword="Urdu")
age.by.sex=acs.lookup(table.name="Age by Sex")
age.by.sex
workers.age.by.sex=age.by.sex[4:6]
workers.age.by.sex
}

