% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{XSDNARedunFilter}
\alias{XSDNARedunFilter}
\title{XSDNARedunFilter}
\usage{
XSDNARedunFilter(xsnuc)
}
\arguments{
\item{xsnuc}{}
}
\value{
Filtered StringSet i.e. discards one of the duplicates in no particular order.
}
\description{
For nucleic sequences, remove identical duplicates of StringSet objects.
N.B - this checks revcomp as well.
for DNA
}
\examples{
XSDNARedunFilter
}
\seealso{
Other BString_functions: 
\code{\link{AAcoor2NAcoor_dFAST}()},
\code{\link{AAcoor2NAcoor_df}()},
\code{\link{AAcoor2NAcoor}()},
\code{\link{DF2XString}()},
\code{\link{Trim2Core2}()},
\code{\link{XString2DF}()},
\code{\link{XString2KmerDT}()}
}
\concept{BString_functions}
