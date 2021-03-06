% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{GenericRunnerInOut}
\alias{GenericRunnerInOut}
\title{GenericRunnerInOut}
\usage{
GenericRunnerInOut(
  xs,
  infile = "infile.faa",
  inflag = "i",
  outflag = "o",
  outfile = "outfile.afa",
  keepinout = F,
  command = "echo",
  param = list(threads = 4, mem = 120),
  RunInTmp = F,
  FlgType = "-",
  AsType = " ",
  ...
)
}
\arguments{
\item{...}{}
}
\value{

}
\description{
Like GenericRunner but directs the output and input through files
i.e. writes something, calls some command on it, then reads in the output.
}
\examples{
GenericRunnerInOut
}
\seealso{
\code{\link{GenericRunner}}
}
