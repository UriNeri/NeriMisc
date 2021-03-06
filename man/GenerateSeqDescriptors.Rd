% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicf.r
\name{GenerateSeqDescriptors}
\alias{GenerateSeqDescriptors}
\title{GenerateSeqDescriptors}
\usage{
GenerateSeqDescriptors(
  dfdt = allmots,
  PrecentRand = 10,
  aacol = "AA_seq",
  HEC_Prob = F,
  AA_Prob = F,
  K.HEC = 3,
  K.aa = 2,
  casecol = "case",
  Exmp_src_faa = rdrps_faa,
  SEED = 123,
  valuecol = "motif_type",
  RemovesSparseCols = T
)
}
\arguments{
\item{dfdt}{Input table (data.frame)}

\item{PrecentRand}{% of shuffled sequences to generate.}

\item{aacol}{Colomn name of field with the sequence.}

\item{casecol}{Column name for the field splitting the input table to train/test sets.}

\item{valuecol}{Target column (try to predict this value)}

\item{RemovesSparseCols}{}
}
\value{

}
\description{
TODO: Need some refactoring but should work.
}
\examples{
GenerateSeqDescriptors
}
