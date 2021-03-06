% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{CompareDFDT}
\alias{CompareDFDT}
\title{CompareDFDT}
\usage{
CompareDFDT(dfdt1, dfdt2)
}
\arguments{
\item{dfdt1}{Input table to work on (data.frame or data.table)}

\item{dfdt2}{Input table to work on (data.frame or data.table)}
}
\value{

}
\description{
base '==' doesn't work for DFs with nested lists?
DO NOT USE (needs reworking)
}
\examples{
CompareDFDT
}
\seealso{
Other Table_functions: 
\code{\link{CalcPcoverage}()},
\code{\link{CalcQcoverage}()},
\code{\link{HeaderBreakerCb}()},
\code{\link{HeaderBreaker}()},
\code{\link{MergeXtYf}()},
\code{\link{ReadXlsx}()},
\code{\link{RemoveEmptyStrCols}()},
\code{\link{RemoveEmptyStrRows}()},
\code{\link{RemoveNACols}()},
\code{\link{RemovesSparseols}()},
\code{\link{Rename1Col}()},
\code{\link{SharedCols}()},
\code{\link{Trimifo}()},
\code{\link{Trimito}()},
\code{\link{UnSharedCols}()},
\code{\link{WriteWolfTbl}()},
\code{\link{WriteXlsx}()},
\code{\link{dropcols}()},
\code{\link{droprows}()}
}
\concept{Table_functions}
