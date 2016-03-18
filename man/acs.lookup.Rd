\name{acs.lookup}
\alias{acs.lookup}

\title{ Search for relevant demographic variables and tables from the US
Census.  }

\description{ The \code{acs.fetch} function is used to download data
  from the US Census American Community Survey.  The \code{acs.lookup}
  function provides a convenience function to use in advance to locate
  tables and variables that may be of interest.

  \code{acs.lookup} takes arguments similar to \code{acs.fetch} --- in
  particular, "table.number", "table.name", and "keyword", as well as
  "endyear","span", and "dataset" --- and searches for matches in the
  meta-data of the Census tables.  When multiple search terms are passed
  to a given argument (e.g., \code{keyword=c("Female", "GED")}), the
  tool returns matches where ALL of the terms are found; similarly, when
  more than one lookup argument is used (e.g.,
  \code{table.number="B01001", keyword="Female"}), the tool searches for
  matches that include all of the terms (i.e., terms are combined with a
  logical "AND", not a logical "OR").

  Results from acs.lookup --- which are acs.lookup class objects --- can
  then be inspected, subsetted (with [square brackets]), and combined
  (with \code{c} or \code{+}) to create custom acs.lookup objects to
  store and later pass to \code{acs.fetch}.

}

\usage{
acs.lookup(endyear, span = 5, dataset = "acs", keyword,
     table.name, table.number, case.sensitive = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{endyear}{an integer indicating the latest year of the data in
    the survey (e.g., for data from the 2007-2011 5-year ACS data,
    endyear would be 2011; limited by acceptable values currently
    provided by the Census API)}

  \item{span}{an integer indicating the span (in years) of the desired
    ACS data (should be 1, 3, or 5); defaults to 5.  Ignored and reset
    to 0 if dataset="sf1" or "sf3".}

  \item{dataset}{either "acs" (the default), "sf1", or "sf3", indicating
    whether to look for tables and variables in the American Community
    Survey, the SF1 dataset (decennial/"short-form"), or the SF3 dataset
    (decennial/"long-form").}

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

\details{

In many cases, \code{acs.lookup} is called internally by
\code{acs.fetch}, to determine the variable codes to use for a given
table.name or table.number.  Since each lookup involves a search of
static XML tables (provided by the census for each endyear/span
combination, and included by the acs package in /extdata), searches
involving more recent years (e.g., for version 2.0, endyears > 2014) may
fail.  In such situations, users may wish to call \code{acs.fetch} with
the "variable=" option, perhaps reusing variables from a saved
\code{acs.lookup} search for a previous year.

For example, once the 2011-2015 5-year ACS data is available via the
API, users can attempt the following to access Table B01003, even before
the new version of the package is installed with the correct variable
lookup tables: \code{acs.fetch(endyear=2015, span=5,
variable=acs.lookup(endyear=2014, span=5, table.number="B01003"))}. }

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
acs.lookup(endyear=2014, span=5, table.number="B01001")
acs.lookup(endyear=2012, span=1, table.number="B01001", keyword="Female")
acs.lookup(endyear=2012, span=1, keyword=c("Female", "GED"))
acs.lookup(endyear=2000, dataset="sf3", table.number="P56")
acs.lookup(endyear=1990, dataset="sf3", table.number="H058")
age.by.sex=acs.lookup(endyear=2014, span=5, table.name="Age by Sex")
age.by.sex
workers.age.by.sex=age.by.sex[4:6]
workers.age.by.sex
}

