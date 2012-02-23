\name{confint.acs}
\alias{confint.acs}
\alias{confint}
\title{
Return upper and lower bounds of given confidence intervals for acs objects.
}
\description{
When passed a valid acs object, confint will return a list of two-column
dataframes (one for each variable specified in \code{parm}) including
lower and upper bounds for given confidence intervals.  Intervals can be one- or two-sided.
}
\usage{
\method{confint}{acs}(object, parm = "all", level = 0.95, alternative = "two.sided", \ldots)
}


\arguments{
  \item{object}{
a valid acs object (or subset).
}
 \item{parm}{which variables/columns to return confidence intervals for;
  defaults to "all", which computes confidence intervals for all
  estimates in the acs object.}
  \item{level}{
the confidence level required -- e.g., .95 = 95\% confidence.
}
  \item{alternative}{
whether the interval should be one-sided (i.e., one-tailed -- "greater" or
"less" -- extending to Inf (or -Inf) on one side) or "two-sided".
}
 \item{...}{additional argument(s) for methods.}
}
\value{ Returns a list of dataframes (one for each variable specified in
\code{parm}) of the lower and upper bounds of the confidence interval
for each row of the data.  }

\author{
Ezra Haber Glenn \email{eglenn@mit.edu}.
}


\examples{

# load ACS data
data(kansas09)

# confidence intervals for select columns
confint(kansas09[20:25,], parm=c(4,5,10))

# another way to accomplish this
confint(kansas09[20:25,c(4,5,10)])

# store data and extract at will
my_data <- confint(kansas09)
str(my_data)
my_data[32]
my_data$Universe...TOTAL.POPULATION.IN.THE.UNITED.STATES..U.S..citizen.by.naturalization  

# try a different value for level
confint(kansas09[1:10,6], level=.75)

# ... or a one-sided confidence interval
confint(kansas09[1:10,6], level=.75, alternative="greater")
confint(kansas09[1:10,29], level=.75, alternative="less")
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
