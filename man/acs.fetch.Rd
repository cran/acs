\name{acs.fetch}
\alias{acs.fetch}
\alias{api.url.maker}



\title{ Downloads ACS data via the Census API and converts to a proper
acs object with estimates, standard errors, and associated metadata.}

\description{ When passed a valid geo.set object and either lookup terms
(table.number, table.name, keyword) or a valid acs.lookup object,
queries the Census ACS API and downloads data to create a new acs-class
object.  Geographical information is preserved, metadata in bundled
together with the acs object, and margins of errors are converted to
standard errors to accompany estimates (see \code{help(acs)}).}

\usage{
acs.fetch(endyear = 2011, span = 5, geography, table.name,
      table.number, variable, keyword, key, col.names = "auto", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{endyear}{an integer (defaults to 2011) indicating the latest year of the
data in the survey (e.g., for data from the 2007-2011 5-year ACS data,
endyear would be 2011; limited by acceptable values currently provided
by the Census API)}

\item{span}{an integer indicating the span (in years) of the desired
    ACS data (should be 1, 3, or 5); defaults to 5}

  \item{geography}{a valid geo.set object specifying the census
    geography or geographies to be fetched; can be created "on the fly"
    with a call to \code{geo.make()} }
  
  \item{table.name}{a string giving the search term(s) to find in the
    name of the ACS census table (for example, "Sex" or "Age"); accepts
    multiple words, which must all be found in the returned table names;
    always case-sensitive.  (Note: when set, this variable is passed to
    an internal call to acs.lookup---see
    \code{\link{acs.lookup}}).}
  
  \item{table.number}{a string (not a number) indicating the table from
      the Census to fetch; examples: "B01003" or "B23013"; always
      case-sensitive. Used to fetch all variables for a given table
      number; if "table.number" is provided, other lookup variables
      ("table.name" or "keyword") will be ignored.}
    
    \item{variable}{an object of acs.lookup class, or a string (not a
      number) or vector of strings indicating the exact variable
      number(s) the Census to fetch.  See details for more.
      Non-acs.lookup examples include "B01003_001" or "B23013_003" or
      c("B01003_001", "B23013_003").  Used to fetch specific variables,
      as opposed to all variables for a given table number; if
      "variable" is provided, all other lookup variables ("table.name",
      "table.number", and "keyword") will be ignored. }

    \item{keyword}{a string or vector of strings giving the search
      term(s) to find in the name of the ACS census variable (for
      example, "Male" or "Haiti"); accepts multiple words, which must
      all be found in the returned variable names; always
      case-sensitive.  (Note: when set, this variable is passed to an
      internal call to acs.lookup---see \code{\link{acs.lookup}}).}

    \item{key}{a string providing the Census API key to use for when
      fetching data.  Typically saved once via \code{api.key.install}
      and passed automatically with each call; see
      \code{\link{api.key.install}}}

    \item{col.names}{either "auto","pretty", or a vector of strings of
      the same length as the number of variables to be fetched.  When
      "auto" (the default), census variable codes will be used as column
      names for the fetched data; when "pretty", descriptive variables
      names will be used; otherwise col.names will be used.}
    
    \item{\dots}{ Not used interactively (reserved for recursive calls).
    }}

\details{

Assuming you have created some geography with geo.make and you have
already installed an API key, the call is quite simple: for example,
acs.fetch(geography=my.geo, table.number="B01003".  (For more on API
keys, see \code{\link{api.key.install}}; if you haven't installed one,
you can always add a "key=YOUR.KEY.HERE" argument to \code{acs.fetch}
each time.)

By default, acs.fetch will download the ACS data from 2007--2011 (the
Five-Year ACS), but the functions includes options for "endyear" and
"span".  At present, the API only provides the five-year data for
2006--2010, 2007--2011, and 2008--2012, but as more data is added the
function will be able to download other years and products.

Downloading based on a table number is probably the most fool-proof way
to get the data you want, but acs.fetch will also accept a number of
other arguments instead of "table.number".  Users can provide strings to
search for in table names (e.g., table.name="Age by Sex" or
table.name="First Ancestry Reported") or keywords to find in the names
of variables (e.g., keyword="Male" or keyword="Haiti")---but be warned:
given how many tables there are, you may get more matches than you
expected and suffer from the "download overload" of fast-scrolling
screens full of data. (But don't lose hope: see the acs.lookup tool,
which can help with this problem.)

On the other hand, if you know you want a specific variable or two (not
a whole table, just a few columns of it, such as variable="B05001_006"
or variable=c("B16001_058", "B16001_059")), you can ask for that with
\code{acs.fetch(variable="these.variable.codes", ...)}.

Note: when "combine=T" for the fetched geo.set, data will be aggregated
in the resulting acs abject.  Some users may therefore wish to specify
"one.zero=T" as an additional argument to \code{acs.fetch}; see
\code{\link{sum-methods}}. }

\value{Returns a new acs-class object with estimates, standard errors
(derived from the census 90\% margins of error), and metadata of the
fetched data from the Census API.  }

\references{
http://www.census.gov/developers/
}
\author{
Ezra Haber Glenn \email{eglenn@mit.edu}
}


\seealso{
\code{\link{acs.lookup}}.
}

