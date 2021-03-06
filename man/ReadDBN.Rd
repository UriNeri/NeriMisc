% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{ReadDBN}
\alias{ReadDBN}
\title{ReadDBN}
\usage{
ReadDBN(DBNfile, includes_MFE = T, return_type = "DF")
}
\arguments{
\item{DBNfile}{Input path.}

\item{includes_MFE}{For outputs of certain RNA secondary structre folding predctions, should the MFE value be included?}

\item{return_type}{Should the output be a data.frame or a BStringSet}
}
\value{

}
\description{
Read a dot bracket notation (DBN) file.
}
\examples{
ReadDBN
}
