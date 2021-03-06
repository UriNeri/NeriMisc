% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{GenericHitsParser}
\alias{GenericHitsParser}
\title{GenericHitsParser}
\usage{
GenericHitsParser(
  inpt = "hit.tsv",
  breakhdrs = T,
  input_was = "Scaf_ID_frame",
  Cull_hits = F,
  search_tool = "psiblast",
  reducecols = T,
  calc_pcoverage = T,
  colsnms = "provide",
  CullCol = "score",
  hedsep = ".",
  hedclbs = c("id", "frame")
)
}
\arguments{
\item{inpt}{Input path.}

\item{hedclbs}{}
}
\value{

}
\description{
Reads as input search results and parses them consistently.
}
\examples{
GenericHitsParser
}
