# Information  --------------------------------------------------------------------
## Script name: basicf.r
## Purpose of script: basic functions (hopefully) stable across shenanigans
## Author: Uri Neri
## Created: 24-06-2020
## 1st Major Revision: 28-10-2020
## 2nd Major Revision: 12-10-2021
## 3rd Major Revision: 13-04-2022

## Email: uri.neri@gmail.com

# set glob options  --------------------------------------------------------------------
# options(scipen = 6, digits = 14) # I prefer to view outputs in non-scientific notation
# options(stringsAsFactors = FALSE)

# on.exit(options(opts), add = TRUE);

#' InstallDependenciesCran
#' @description
#' Installs and load packages from CRAN (or tries to)
#' @return
#' @export
#'
#' @examples
#' InstallDependenciesCran()
InstallDependenciesCran <- function() {
  LibListx <- c(
    "Rcpp",
    "castor",
    "devtools",
    "methods",
    "plyr",
    "dplyr",
    "data.table",
    "stringr",
    "ggrepel",
    "ggplot2",
    "phangorn",
    "BBmisc"
  ) # "phytools"
  for (lib in LibListx) {
    x <- try(library(package = as.character(lib), character.only = T))
    if (class(x) == "try-error") {
      utils::install.packages(pkgs = lib)
    }
  }
}

#' InstallDependenciesGH
#' @description
#' Installs and load packages from GitHub (or tries to)
#' @return
#' @export
#'
#' @examples
#' InstallDependenciesGH()
InstallDependenciesGH <- function() {
  devtools::install_github("jimhester/knitrBootstrap")
  devtools::install_github("barkasn/fastSave")
  devtools::install_github("urineri/biofiles")
}

#' InstallDependenciesBC
#' @description
#' Install and load packages from BioConducter (or tries to)
#' @return
#' @export
#'
#' @examples
#' InstallDependenciesBC
InstallDependenciesBC <- function() {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    utils::install.packages(pkgs = "BiocManager")
  }
  LibListx <- c(
    "Biostrings",
    "DECIPHER",
    "ggtree",
    "GenomicRanges",
    "plyranges",
    "BBmisc",
    "tidytree"
  )
  for (lib in LibListx) {
    x <- try(library(package = as.character(lib), character.only = T))
    if (class(x) == "try-error") {
      BiocManager::install(pkgs = lib)
    }
  }
}

# Load packages --------------------------------------------------------------------
# library(BBmisc)
# library(Rcpp)
# library(devtools)
# library(methods)
# library(readr)
# library(Biostrings)
# library(plyr); library(dplyr)
# library(data.table)
# library(stringr)
# library(stringi)
# library(DECIPHER)
# library(ggplot2)
# library(phangorn)
# library(castor)
# library(ggtree)
# library(plyranges)
# library(writexl)
# library(fastSave)
# library(readxl)
# library(rtracklayer)
# library(XVector)
# library(universalmotif)
# library(GenomicRanges)
# library(tidytree)
# library(roperators)

# Data  --------------------------------------------------------------------
StopLess_GENETIC_CODE <- gsub(pattern = "*", replacement = "X", fixed = T, x = Biostrings::GENETIC_CODE)
THREADS <- data.table::getDTthreads()

###### Misc. functions ######
#' p0
#' @description
#' Short call to paste0 (sep = "").
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' p0
p0 <- function(...) {
  paste0(...)
}

#' len
#' @description
#' Short call to length.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' len
len <- function(...) {
  length(...)
}

#' ClearFunct
#' @description
#' Remove all function from the current environment (only?).
#' @return
#' @examples
#'
#' @export
ClearFunct <- function() {
  list1 <- setdiff(ls(), lsf.str())
  list2 <- setdiff(ls.str(), list1)
  rm(list = list2)
}


#' ClearTmps
#' @description
#' Remove temp-ish objects variables from the current environment.
#' @return
#' @export
#'
#' @examples
#' ClearTmps
ClearTmps <- function() {
  rm(list = intersect(ls(), c("TMP2", "TMP3", "TMP4", "TMP5", "TMP_gr", "x", "testttt", "tmmp", "TMP1", "ij", "cntr", "j", "Nlb", "Nyd", "Rmems1", "RTsNodes", "tmplabs", "tmpkids", "tmpppp", "wx", "i", "w01", "w00", "gbtrees", "iz", "ix", "iy", "dfdt", "btrees", "b", "textast", "textastDF", "WNewNodTaxDF_master", "tmpclsts2", "tmpclsts", "tmppp", "tmpsubtree", "tmpdf_dist", "tmpcount", "x", "lib", "LibListx", "cx", "cx1", "xcx", "lsfiles", "faa_df", "IDFT4print", "MtmpNewNodTaxDF_toc", "tmpNewNodTaxDF_blnk", "tmpNewNodTaxDF_toc", "hits_df1", "dupdf", "aaa", "ALL_nuc.faa", "ALL_nuc.faa.dfdt", "ALL_nuc_3007.dfdt", "ALL_nuc_3007.fasta", "ALL_nuc.faa_trimmed", "workies", "Wnrws", "ToCorr", "study", "subject_was", "susrows", "Ntax", "NewTips", "mts_in_this_study", "min_len", "max_len", "www", "www1", "www2", "WWWusage", "tmptmpdf", "tmptmptmpdf", "tmptmpdf", "tmptmp3", "tmpranks", "tmplsit", "Rmems1", "Rmems2", "Rcls90", "RNC90", "Rnuc.cls9595", "wx", "w0", "w00", "w1", "ir", "lambda", "mems1", "NewNodTaxDF_template", "ir", "maxlier", "brnm", "LCARnkCntr", "AllwRank", "colstst", "AllTrees", "db2c.2", "colstst1", "distss", "exncol", "ncolneeded", "nrowsneeded", "w3", "WantedKids", "UnwantedKids", "i", "M_c", "M_t", "ntax", "l", "drows11", "drows4", "workdf", "workDF", "workdf_certain", "workdf1", "workDF1", "workdf2", "workDF2", "TaxMotCount", "TaxMotCount3", "TaxMotCount3.2", "RsomeDF1", "rdrp_v301.c210209.id.tab", "Parsed_RBS_motifs_gen11", "Parsed_RBS_motifs_gen4", "p3", "p4", "p5", "p6", "Orig_tmp.210112a.phylo", "fsomeDF1", "someDF", "someDF1", "tmpfasta", "tmp_4sureenaes", "w00", "w0000", "w0001", "NewNode", "tmpfilter", "testmmmm", "tmpTaxDF", "tmpmp", "tmpmat", "tmp_allnaes", "tmpa", "tmpclstmems4count", "y", "WnPrDf", "WnTxDf", "W_NNTFM", "tmperdf", "tmpdf", "tmp_notsure", "tmpNodetbl_test2", "tmprank", "tmptpmpmpmpm", "tmptreeque", "tmptreeque", "temp_file", "rdrp_v301.05.hit.tab", "RdRps3007.seqsdf", "olog", "temp2", "test13", "test23", "test1", "xcl", "tempdt", "tmpali", "tmptree", "tmptest133", "w", "AAA", "cc2", "asdsad", "adgtmpali", "mems", "mem", "cgtables", "outdf", "Nucs", "nwolf", "newtree", "rv20_v2", "ggNeoTree", "c210112a.seqsdf", "nucls", "RdRps", "bspkgs.nbu", "bspkgs.nb", "command", "kids", "Comscaf", "cpath", "reps1", "FlgType", "InrNodes2eval", "Tips2eval", "tmpvar2", "wd", "origdir", "param", "outfile", "clsts", "AsType", "Comsdf", "ya20_JAAOEH010001231_1", "omit.internal", "infile", "Taxa", "ALL_nuc.fasta", "clsts.nuc.newids.9595", "compdf", "AllIDs_vr0520", "AllIDs_set2n0_ALL_nuc", "ALL_nuc.fasta_df", "AllLCAKids", "AllIDs", "clsts.nuc.newids.9595", "AllIDs_vr0520_test", "ALL_nuc.fasta_df1", "cls9590", "info", "ModifiedW0", "rdrp_v301_162507", "list2", "states", "taxid2lineage", "taxid2lineageTrim", "cntr", "ccls", "logvar", "trimifo", "trimito", "wp", "InrNodes2Rem", "stated", "TreeQue", "maxclade", "wy", "wx", "ix", "iy", "i", "j", "cc", "a", "clade", "clades", "Yith", "izh", "label", "tmpNotReady", "tmptest1", "logvarmVizTree", "w1", "X", "x", "x1", "x2", "x3", "w0", "tree", "node", "GenDF", "cols2test", "subtaxtree", "splitedtax", "AllanoY", "Allano", "Allanotips", "Allanox", "tmplst", "tmpsum", "test1021", "test1", "test2", "test3", "test1121", "maxval", "maxX", "maxF", "LCA", "nodesdone", "nodesleft", "Nodes4Pies", "pb", "NODES2DROP", "NID", "problomatic", "thiscladelca", "tmpanc", "listerrs", "maxcladelca", "Max_Rank", "include.self", "Tips", "Tips2Rem", "cols", "w2", "w8", "mll", "nnid", "gcode", "cpath2", "cpath", "TreeQue_NoTips", "ProtectedLabs")))
  gc()
}

#' pad
#' @description
#' Short call to stringi::stri_pad.
#' @param str
#' String to pad (character)
#' @param pad
#' String to use as prefix/suffix when padding
#' @param width
#' Maxiaml Length (number of characters) of pad copies to add to str.
#' @param side
#' On which end to append pad
#' @param use_length
#' See stringi::stri_pad
#' @return
#' @export
#'
#' @examples
#' pad
pad <- function(str,
                pad = " ",
                width = floor(0.9 * getOption("width")),
                side = c("left", "right", "both"),
                use_length = FALSE) {
  stringi::stri_pad(str, width, side, pad, use_length)
}


#' Odd
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Odd <- function(x){
  x%%2 == 1
}

#' Even
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
Even <- function(x){
  x%%2 != 1
}

#' swap
#'
#' @param x
#' @param swapin
#' @param swapout
#'
#' @return
#' @export
#'
#' @examples
swap <- function(x,swapin,swapout){
  x[x == swapout] <- swapin
  x
}


#' QuickLoad
#' @description
#' Uses the multithearded fastSave to save the current word session as an ".RDataFS" file to some location.
#' @param THREADS
#' Number of cores.
#' @param SessionCatalogPath
#' Path to folder on your machine you want to save the RDataFS to.
#' @return
#' @export
#' @seealso \code{\link{QuickSave}}
#' @examples
#' QuickLoad
QuickLoad <- function(THREADS = THREADS, SessionCatalogPath = "/media/HDD1/uri/RNA_Vir_MTs/V3/Rdatacatalog/") {
  SessionCatalogPath <- "/media/HDD1/uri/RNA_Vir_MTs/V3/Rdatacatalog/"
  lsfiles <- file.info(
    dir(SessionCatalogPath,
      full.names = T,
      pattern = "*.RDataFS"
    )
  )
  fastSave::load.lbzip2(file = rownames(lsfiles)[which.max(lsfiles$mtime)], n.cores = THREADS)
}

#' QuickSave
#' @description
#' Uses the multithearded fastSave to load the most recently modified ".RDataFS" session from a predefined location.
#' @param THREADS
#' Number of cores.
#' @param SessionCatalogPath
#' Path to folder on your machine to read the RDataFS files from.
#' @param name
#' File name of output.
#' @returns
#' @export
#' @seealso \code{\link{QuickSave}}
#' @examples
#' QuickSave
QuickSave <- function(THREADS = THREADS,
                      SessionCatalogPath = "/media/HDD1/uri/RNA_Vir_MTs/V3/Rdatacatalog/",
                      name = "KvPhylo") {
  name <- "KvMeta"
  SessionCatalogPath <- p0(
    "/media/HDD1/uri/RNA_Vir_MTs/V3/Rdatacatalog/",
    name,
    ".",
    Sys.Date(),
    ".RDataFS"
  )
  fastSave::save.image.lbzip2(SessionCatalogPath, n.cores = THREADS)
}

#' as.char
#' @description
#' Short call to as.character
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' as.char
as.char <- function(...) {
  as.character(...)
}

#' as.num
#' @description
#' Short call to as.numeric
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' as.num
as.num <- function(...) {
  as.numeric(...)
}

#' AllTrue
#' @description
#' Are all items of a logical vector == TRUE?
#' @param x
#' Input logical array/vector
#' @return
#' Logical
#' @export
#' @family AnyWhich
#' @examples
#' AllTrue
AllTrue <- function(x) {
  if (length(which(x == F)) == 0) {
    return(T)
  }
  return(F)
}

#' AllFalse
#' @description
#' Are all items of a logical vector == False?
#' @param x
#' Input logical array/vector
#' @return
#' Logical
#' @export
#' @family AnyWhich
#' @examples
#' AllFalse
AllFalse <- function(x) {
  if (length(which(x != F)) == 0) {
    return(T)
  }
  return(F)
}

#' AnyFalse
#' @description
#' Does the input contain any False values?
#' @param x
#' Logical vector
#' @return
#' Logical
#' @export
#' @family AnyWhich
#' @examples
#' AnyFalse
AnyFalse <- function(x) {
  #
  if (length(which(x == F)) > 0) {
    return(T)
  }
  return(F)
}


#' show_me_the_contig
#' @description
#' Open up the contig (scaffold) viewer on IMG/MER site.
#' @param full_name
#' A single string of the format "IMG Taxon ID"_"Scaffold ID"
#' @return
#' URL + opens up the web page.
#' @export
#'
#' @examples
#' show_me_the_contig
show_me_the_contig <- function(full_name) {
  brkt <- (strsplit(
    as.character(full_name),
    split = "_",
    fixed = T
  ))
  mt <- brkt[[1]][1]
  scf <- paste0(as.character(brkt[[1]][2]), "_", as.character(brkt[[1]][3]))
  bu_wd <- paste0(
    "https://img.jgi.doe.gov/cgi-bin/mer/main.cgi?section = MetaScaffoldDetail&page = metaScaffoldDetail&taxon_oid = ",
    mt,
    "&scaffold_oid = ",
    scf,
    "&data_type = assembled"
  )
  browseURL(bu_wd)
  invisible(bu_wd)
}

#' fread0
#' @description
#' Shortened fread
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' fread0
#' @seealso \code{\link{data.table::fread}}
fread0 <- function(...) {
  data.table::fread(
    quote = F,
    header = T,
    sep = "\t",
    ...
  )
}

#' which.na
#' @description
#' Indices of the input that are NAs
#' @param x
#' Vector/Array (usually).
#' @return
#' Indices (numeric vector)
#' @export
#'
#' @examples
#' which.na
#' @family AnyWhich
which.na <- function(x) {
  which(is.na(x))
}

#' which.Not.na
#' @description
#' Indices of the input that are NOT NAs
#' @param x
#' Vector/Array (usually).
#' @return
#' Indices (numeric vector)
#' @export
#'
#' @family AnyWhich
#' @examples
#' which.Not.na
which.Not.na <- function(x) {
  which(!is.na(x))
}

#' which.duplicated
#' @description
#' Indices values of duplicated entries in x
#' @param x
#' Whatever.
#' @param returnValue
#' values of duplicated entries
#' @param returnalldups
#' Output all  instances/values of duplicated entries?
#' @return
#' If returnValue is True, output is the values of duplicated entries, else output is the indices (numeric vector)
#' @export
#'
#' @family AnyWhich
#' @examples
#' which.duplicated
which.duplicated <- function(x,
                             returnValue = F,
                             returnalldups = T) {
  w1 <- which(duplicated(x))
  if (!returnValue) {
    if (returnalldups) {
      return(which(x %in% x[w1]))
    }
    return(w1)
  }
  if (returnValue) {
    return(unique(x[which(x %in% x[w1])]))
  }
}

#' whd
#' @description
#' Short call to which.duplicated
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @family AnyWhich
#' @examples
#' whd
whd <- function(x, ...) {
  which.duplicated(x, ...)
}

#' wna
#' @description
#' Short call to which.na
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @family AnyWhich
#' @examples
#' wna
wna <- function(x, ...) {
  which.na(x, ...)
}

#' wana
#' @description
#' Short call to which.Not.na
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @family AnyWhich
#' @examples
#' wana
wana <- function(x, ...) {
  which.Not.na(x, ...)
}


#' wh
#' @description
#' shortend to which
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @family AnyWhich
#' @examples
#' wh
wh <- function(x, ...) {
  which(x, ...)
}


#' zstdR
#'
#' @param files
#' @param zstdout
#' @param compression_level
#'
#' @return
#' @export
#'
#' @examples
zstdR <- function(files = files, zstdout="tmp.zstd",compression_level=22){
  system2(p0("zstd -",compression_level," -o ", zstdout," ",collapse(files)))
}


###### DF/DT functions ######

#' SharedCols
#' @description
#' Column names that both input tables have in common.
#' @param df1
#' Table (data.frame)
#' @param df2
#' Table (data.frame)
#' @return
#' Character
#' @export
#' @family Table_functions
#' @examples
#' SharedCols
SharedCols <- function(df1, df2) {
  intersect(colnames(data.frame(df1)), colnames(data.frame(df2)))
}

#' UnSharedCols
#' @description
#' Column names that  input tables don't have in common.
#' @param df1
#' Table (data.frame)
#' @param df2
#' Table (data.frame)
#' @return
#' Character
#' @export
#' @family Table_functions
#' @examples
#' UnSharedCols
UnSharedCols <- function(df1, df2) {
  unique(setdiff(colnames(data.frame(df2)), colnames(data.frame(df1))), setdiff(colnames(data.frame(df1)), colnames(data.frame(df2))))
}

#' TabUnroll
#' @description
#' Inspired by Yuri Wolf' tab_unroll.pl
#' "Reads the table, unrolls the specified field into separate lines"
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param sep
#' Delimiter to break the rolled field's cells with.
#' @param colnym
#' Field to unroll
#' @param NewColNym
#' A name for the field that will contain the unrolled values
#' @return
#' A data.frame.
#' @seealso \code{\link{stringi::stri_pad}}
#' @export
#'
#' @examples
#' TabUnroll
TabUnroll <- function(dfdt, sep = ",", colnym = "mems", NewColNym = "values") {
  dfdt$TmpLstCol <- apply(X = dfdt, MARGIN = 1, FUN = function(x) BiocGenerics::unlist(S4Vectors::unname(stringr::str_split(string = x[colnym], pattern = coll(sep, ignore_case = FALSE, locale = "en"), n = Inf,simplify=T))))
  dfdt3 <- with(dfdt, {
    data.frame(lapply(`dropcols<-`(dfdt, "TmpLstCol"), rep, times = lengths(TmpLstCol)), TmpCol1 = unlist(TmpLstCol))
  })
  dfdt3 <- `dropcols<-`(dfdt3, "TmpLstCol")
  dfdt3 <- Rename1Col(dfdt3, "TmpCol1", NewColNym)
  return(dfdt3)
}



#' Change2Date
#'
#' @param z
#'
#' @return
#' @export
#'
#' @examples
Change2Date<-function(z) { # From Leah, use like this: timevec<-as.numeric(difftime(date1,date2),units="weeks")
  x=grep("date",colnames(z),ignore.case =T)
  for ( i in (1:length(x))) {
    z[ ,x[i]]=as.Date(z[ ,x[i]],"%d/%m/%Y")
  }
  z
}

#' CalcPcoverage
#' @description
#' Calculate profile (subject) coverage, based on alignment coordinates and profile length.
#' @param hit_df
#' Input table (data.frame). Usually tabular search results of profile search e.g. hmmsearch
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' CalcPcoverage
CalcPcoverage <- function(hit_df) {
  #
  # Oldname: calc_pCoverage
  if (AnyFalse(hit_df$p2 > hit_df$p1)) {
    tmplsit <- hit_df$p1 > hit_df$p2
    hit_df$pCoverage <- NA
    hit_df$pCoverage[tmplsit] <- (hit_df$p1[tmplsit] - hit_df$p2[tmplsit] +
      1) / hit_df$pL[tmplsit]
    hit_df$pCoverage[!tmplsit] <- (hit_df$p2[!tmplsit] - hit_df$p1[!tmplsit] +
      1) / hit_df$pL[!tmplsit]
    return(hit_df)
  }
  hit_df$pCoverage <- (hit_df$p2 - hit_df$p1 + 1) / hit_df$pL
  return(hit_df)
}


#' CalcQcoverage
#' @description
#' Calculate query coverage, based on alignment coordinates.
#' @param hit_df
#' Input table (data.frame). Usually tabular search results of profile search e.g. hmmsearch
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' CalcQcoverage
CalcQcoverage <- function(hit_df) { # Calculate Query coverage, based on alignment coordinates and profile length.
  if (AnyFalse(hit_df$q2 > hit_df$q1)) {
    tmplsit <- hit_df$q1 > hit_df$q2
    hit_df$qCoverage <- NA
    hit_df$qCoverage[tmplsit] <- (hit_df$q1[tmplsit] - hit_df$q2[tmplsit] + 1) / hit_df$qL[tmplsit]
    hit_df$qCoverage[!tmplsit] <- (hit_df$q2[!tmplsit] - hit_df$q1[!tmplsit] + 1) / hit_df$qL[!tmplsit]
    return(hit_df)
  }
  hit_df$qCoverage <- (hit_df$q2 - hit_df$q1 + 1) / hit_df$qL
  return(hit_df)
}

#' `dropcols`
#' @description
#' Replacement function to remove columns.
#' @param df
#' Input table (data.frame).
#' @param dcols
#' Column to remove.
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' `dropcols`
`dropcols` <- function(df, dcols) {
  df[, dcols] <- NULL
  df
}

#' `droprows`
#' @description
# Replacement function to remove rows.
#' @param df
#' Input table (data.frame).
#' @param drows
#' Rows to remove.
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' `droprows`
`droprows` <- function(df, drows) {
  rwnms <- rownames(df)
  if (!is.numeric(drows)) {
    w <- which(rwnms %in% drows)
    return(`droprows`(df, w))
  }
  if (length(drows) > 0) {
    df <- df[-drows, ]
    rownames(df) <- rwnms[-drows]
    return(df)
  }
  df
}

#' CompareDFDT
#' @description
#' base '==' doesn't work for DFs with nested lists?
#' DO NOT USE (needs reworking)
#' @param dfdt1
#' Input table to work on (data.frame or data.table)
#' @param dfdt2
#' Input table to work on (data.frame or data.table)
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' CompareDFDT
CompareDFDT <- function(dfdt1, dfdt2) {
  if (nrow(dfdt1) != nrow(dfdt2) || ncol(dfdt1) != ncol(dfdt2)) {
    return(F)
  }
  logdf <- data.frame(nrow = nrow(dfdt1), ncol = ncol(dfdt2))
  for (Ir in 1:nrow(dfdt1)) {
    for (Ic in 1:ncol(dfdt2)) {
      logdf[Ir, Ic] <- (unlist(dfdt1[Ir, Ic]) == unlist(dfdt2[Ir, Ic]))
    }
  }
  logvar <- apply(
    logdf,
    MARGIN = 2,
    FUN = function(x) {
      unique(x)
    }
  )
  if (length(which(unlist(logvar) == F)) > 0) {
    return(F)
  }
  return(T)
}

#' RemoveEmptyStrRows
#' @description
#' Drops rows that contain only empty strings ("").
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' RemoveEmptyStrRow
RemoveEmptyStrRows <- function(dfdt) {
  w1 <- unlist(apply(
    dfdt,
    MARGIN = 1,
    FUN = function(x) {
      AllTrue(x == "")
    }
  ))
  if (length(w1) != 0) {
    if (length(which(w1)) != 0) {
      dfdt <- `droprows`(dfdt, drows = which(w1))
    }
  }
  dfdt
}

#' RemoveEmptyStrCols
#' @description
#' Drops columns that contain only empty strings ("").
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @return
#' data.frame
#' @export
#'
#' @examples
#' RemoveEmptyStrCol
#' @family Table_functions
RemoveEmptyStrCols <- function(dfdt) {
  w1 <- unlist(apply(
    dfdt,
    MARGIN = 2,
    FUN = function(x) {
      AllTrue(x == "")
    }
  ))
  if (length(w1) != 0) {
    if (length(which(w1)) != 0) {
      dfdt <- `dropcols`(dfdt, dcols = which(w1))
    }
  }
  dfdt
}

#' RemoveNACols
#' @description
#' Drops columns that contain only NAs.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @return
#' data.frame
#' @export
#'
#' @examples
#' RemoveNACols
#' @family Table_functions
RemoveNACols <- function(dfdt) {
  w1 <- unlist(apply(
    dfdt,
    MARGIN = 2,
    FUN = function(x) {
      AllTrue(is.na(x))
    }
  ))
  if (length(w1) != 0) {
    if (length(which(w1)) != 0) {
      dfdt <- `dropcols`(dfdt, dcols = which(w1))
    }
  }
  dfdt
}

#' RemoveNARows
#'
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @return
#' data.frame
#' @export
#'
#' @examples
RemoveNARows <- function(dfdt){
  w1 <- BiocGenerics::unlist(apply(dfdt, MARGIN = 1, FUN = function(x) AllTrue(is.na(x))))
  if (length(w1) != 0) {
    if (length(which(w1)) != 0) {
      dfdt <- `droprows<-`(dfdt,which(w1))
    }
  }
  dfdt
}


#' Rename1Col
#' @description
#' Change the name of a single column.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param colnm
#' Existing column name
#' @param newcolnm
#' New column name
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' Rename1Col
Rename1Col <- function(dfdt, colnm, newcolnm) {
  if (is.matrix(dfdt)) {
    dfdt <- data.frame(dfdt, stringsAsFactors = F)
  }
  if (is.numeric(colnm)) {
    names(dfdt)[colnm] <- newcolnm
    return(dfdt)
  }
  names(dfdt)[names(dfdt) == colnm] <- newcolnm
  dfdt
}

#' RemovesSparseols
#' @description
#' Remove columns that have only a single value.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' RemovesSparseols
RemovesSparseols <- function(dfdt) {
  dfdt <- data.frame(dfdt)
  rembols <- apply(dfdt, 2, unique)
  nurembols <- simplify2array(lapply(rembols, FUN = length))
  return(dfdt[, which(nurembols != 1)])
}

#' Trimifo
#' @description
#' Misc functions for extraction of core domain region based on profile matches.
#' @param x
#'
#' @param y
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' Trimifo
Trimifo <- function(x, y) {
  return(max(1, as.numeric(x["q1"]) - (y * (as.numeric(
    x["p1"]
  ) - 1))))
}

#' Trimito
#' @description
#' Misc functions for extraction of core domain region based on profile matches.
#' @param x
#' @param y
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' Trimito
Trimito <- function(x, y) {
  return(min(as.numeric(x["qL"]), as.numeric(x["q2"]) + (y * (
    as.numeric(x["pL"]) - as.numeric(x["p2"])
  ))))
}

#' HeaderBreaker
#' @description
#' Splits a column into multiple columns/nested table.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param sep
#' Delimiter to split the column by
#' @param clb
#' Column (name) to split
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' HeaderBreaker
HeaderBreaker <- function(dfdt, sep = ".", clb = "id") {
  breaked <- within(dfdt, splitted = data.frame(do.call(
    "rbind", strsplit(as.character(as.data.frame(dfdt)[, c(clb)]), sep, fixed = T)
  ), stringsAsFactors = F))
  return(breaked)
}

#' HeaderBreakerCb
#' @description
#' Splits a column into multiple columns/nested table.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param sep
#' Delimiter to split the column by
#' @param clb
#' Column (name) to split
#' @param nclb
#' Names for the new columns added by splitting.
#' @return
#' data.frame
#' @export
#' @family Table_functions
#' @examples
#' HeaderBreakerCb
HeaderBreakerCb <- function(dfdt,
                            sep = ".",
                            clb = "id",
                            nclb = hedclbs) {
  i <- colnames(dfdt)
  dfdt <- cbind(dfdt, data.frame(do.call(
    "rbind", strsplit(as.character(as.data.frame(dfdt)[, c(clb)]), sep, fixed = T)
  ), stringsAsFactors = F))
  colnames(dfdt) <- c(i, nclb)
  return(dfdt)
}

#' WriteWolfTbl
#' @description
#' Save a tab delimited file with heading but no quotes and no rownames.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param filepath
#' @param ...
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' WriteWolfTbl
WriteWolfTbl <- function(dfdt, filepath, ...) {
  write.table(
    dfdt,
    filepath,
    quote = F,
    row.names = F,
    col.names = T,
    sep = "\t",
    ...
  )
}

#' WriteXlsx
#' @description
#' Shortend call to writexl::write_xlsx
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param filepath
#' @param ...
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' WriteXlsx
WriteXlsx <- function(dfdt, filepath = "tmpout.xlsx", ...) {
  writexl::write_xlsx(
    x = dfdt,
    path = filepath,
    col_names = T,
    format_headers = T,
    ...
  ) #
}

#' ReadXlsx
#' @description
#' Shortend call to readxl::read_xlsx
#' @param filepath
#' @param ...
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' ReadXlsx
ReadXlsx <- function(filepath = "tmpout.xlsx", ...) {
  distinct(readxl::read_xlsx(filepath, na = "NA", col_types = "text")) #
}

#' MergeXtYf
#' @description
#' Shortend call to merge with fixed flags (all.x = T, all.y = F)
#' @param Xdf
#' @param Ydf
#' @param ...
#'
#' @return
#' @export
#' @family Table_functions
#' @examples
#' MergeXtYf
MergeXtYf <- function(Xdf, Ydf, ...) {
  merge(
    x = Xdf,
    y = Ydf,
    all.x = T,
    all.y = F,
    ...
  )
}

###### BioFormats / Genomic Ranges functions ######
#' ReadGFF
#' @description
#' Shortend call to rtracklayer::readGFF(...)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' ReadGFF
ReadGFF <- function(...) {
  rtracklayer::readGFF(...)
}

#' CreateCSVFromGBFF
#' @description
#' Forked from https://github.com/federiva/Monkeys-Working.
#' The .sh script must be saved in the working directory
#' @param input.gbff
#'
#' @return
#' @export
#'
#' @examples
#' CreateCSVFromGBFF
CreateCSVFromGBFF <- function(input.gbff) {
  path.to.script <- "/home/neri/Documents/GitHub/Monkeys-Working/mitochondrial_DNA/r_project_mitochondrial_DNA/bash_scripts/CreateCSVFromGBFF.sh"
  arguments.to.bash <- paste(path.to.script, input.gbff, sep = " ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  print("The resulting .csv file is saved in the working
        directory as indexes_summary.csv")
}

#' GetNCBITaxonomyChildNodes
#' @description
#' Forked from https://github.com/federiva/Monkeys-Working
#' nodes.dmp.path is a STRING which is the path to the nodes.dmp
#' file downloaded from NCBI:
#'  ftp://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdmp.zip
#' target.taxid is a  STRING which is the upper NCBI taxonomy
#'  taxid from which we want to get the child's taxids
#' The output of the function is a .csv file ("|" delimited)
#'  saved in the current working directory named child_nodes.csv
#' The function assumes that the .sh script
#'  GetNCBITaxonomyChildNodes.sh is located in the
#'  working directory.
#' @param nodes.dmp.path
#' @param target.taxid
#'
#' @return
#' @export
#'
#' @examples
#' GetNCBITaxonomyChildNodes
GetNCBITaxonomyChildNodes <- function(nodes.dmp.path, target.taxid) {
  path.to.script <- "/home/neri/Documents/GitHub/Monkeys-Working/mitochondrial_DNA/r_project_mitochondrial_DNA/bash_scripts/GetNCBITaxonomyChildNodes.sh"
  arguments.to.bash <- paste(path.to.script, nodes.dmp.path, target.taxid, sep = " ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  cat("The resulting .csv file is saved in the working directory as child_nodes.csv")
}

#' ExtractFastaSeqsFromGBFF
#' @description
#' Forked from https://github.com/federiva/Monkeys-Working
#' The .sh script must be located in the working directory
#' @param path.to.folder.with.gbff.files
#'
#' @return
#' @export
#'
#' @examples
#' ExtractFastaSeqsFromGBFF
ExtractFastaSeqsFromGBFF <- function(path.to.folder.with.gbff.files) {
  path.to.script <- "/home/neri/Documents/GitHub/Monkeys-Working/mitochondrial_DNA/r_project_mitochondrial_DNA/bash_scripts/ExtractFastaSeqsFromGBFF.sh"
  arguments.to.bash <- paste(path.to.script, path.to.folder.with.gbff.files, sep = " ")
  system2(command = "/bin/bash", args = arguments.to.bash)
  cat("The individual .fasta sequences were saved in ./gbff_filtered_files/fasta_seqs")
}


#' Grange2gbRecord
#' @description
#' Attempt to write out a grange object as an NCBI-like Genebank (".gbk") file.
#' @param Grng
#' @param sequnces
#' @param contig_name
#' @param contig_length
#' @param source_name
#' @param circ
#' @param DRNA
#' @param organism
#' @param keywords
#' @param datee
#' @param type
#' @param workdir
#' @param outputF
#' @param inputF
#'
#' @return
#' @export
#'
#' @examples
#' Grange2gbRecord
Grange2gbRecord <- function(Grng,
                            sequnces,
                            contig_name = "Tuliv",
                            contig_length = 24864,
                            source_name = "Tuliv",
                            circ = "circular",
                            DRNA = "DNA",
                            organism = "Tuliv",
                            keywords = "Virus",
                            datee = "10-JAN-2021",
                            type = "PHG",
                            workdir = "/home/neri/scratch/X2gbk/",
                            outputF = "/home/neri/scratch/X2gbk/testing.gbk",
                            inputF = "/media/HDD1/uri/projects/Isra/Tuli/New/TuliV.fasta") {
  #### Fake the header (TODO:change to sprintf)
  Fheader <- p0(
    "LOCUS       ",
    contig_name,
    "              ",
    contig_length,
    " bp    ",
    DRNA,
    "     ",
    circ,
    " ",
    type,
    " ",
    datee,
    "
DEFINITION  ",
    source_name,
    "
ACCESSION   ",
    contig_name,
    "
VERSION     ",
    contig_name,
    ".1
KEYWORDS    ",
    keywords,
    "
SOURCE      ",
    source_name,
    "
  ORGANISM  ",
    source_name,
    "
REFERENCE   1  (bases 1 to ",
    contig_length,
    ")
  AUTHORS   Lorem,I., Dolor,S., A,C.E., Sed,D.E., Tempor,I.
  TITLE     Lorem ipsum dolor sit amet, consectetur adipiscing elit
            sed do eiusmod tempor incididunt ut labore et dolore
  JOURNAL   Unpublished
FEATURES             Location/Qualifiers
     source          1..",
    contig_length,
    "
                     /organism = \"",
    source_name,
    "\"
                     /mol_type = \"genomic ",
    DRNA,
    "\" "
  )
  write(Fheader, outputF, )

  #### Write the features (TODO:change to sprintf)
  for (i in 1:length(Grng@ranges)) {
    print(i)
    tmptype <- Grng@elementMetadata@listData[["type"]][i]
    tmpstrt <- Grng@ranges@start[i]
    tmpend <- (Grng@ranges@start[i] + Grng@ranges@width[i])
    print((tmpend - tmpstrt) %% 3)
    if (as.character(Grng@strand)[i] == "-") {
      tmpstrand <- -1L
      loci <- p0("complement(", tmpstrt, "..", tmpend - 1, ")")
    } else {
      tmpstrand <- +1L
      loci <- p0(tmpstrt, "..", tmpend)
    }
    if (tmptype == "gene") {
      cat((p0("     ", tmptype, "            ", loci)),
        file = outputF,
        sep = "\n",
        append = T
      )
      cat(
        p0(
          gsub(
            toString(rep(" ", 11)),
            pattern = ",",
            replacement = ""
          ),
          "/locus_tag = \"",
          Grng[i]$ID,
          "\""
        ),
        file = outputF,
        sep = "\n",
        append = T
      )
    }
    if (tmptype == "CDS") {
      cat((p0("     ", tmptype, "             ", loci)),
        file = outputF,
        sep = "\n",
        append = T
      )
      cat(
        p0(
          gsub(
            toString(rep(" ", 11)),
            pattern = ",",
            replacement = ""
          ),
          "/locus_tag = \"",
          Grng[i]$Parent,
          "\""
        ),
        file = outputF,
        sep = "\n",
        append = T
      )
      cat(
        p0(
          gsub(
            toString(rep(" ", 11)),
            pattern = ",",
            replacement = ""
          ),
          "/product = \"",
          Grng[i]$Name,
          "\""
        ),
        file = outputF,
        sep = "\n",
        append = T
      )
      cat(
        p0(
          gsub(
            toString(rep(" ", 11)),
            pattern = ",",
            replacement = ""
          ),
          "/protein_id = \"",
          Grng[i]$ID,
          "\""
        ),
        file = outputF,
        sep = "\n",
        append = T
      )
    }
  }
  #### Write the sequence (Adapted from biofiles).
  if (length(seq = sequences) > 0L) {
    lineno <- seq(
      from = 1,
      to = seq@ranges@width,
      by = 60
    )
    lines <- seq_along(lineno)
    n_lines <- length(lines)
    s <- character(n_lines)
    for (i in lines) {
      seqw <- ifelse(i < n_lines, i * 60, seq@ranges@width)
      seqs <- XVector::toString(XVector::subseq(seq, 1 + (i - 1) * 60, seqw))
      nnn <- seq(1, nnncc = nchar(seqs), by = 10)
      s[i] <- paste0(substring(seqs, nnn, c(nnn[-1] - 1, nnncc)), collapse = " ")
    }
    s <- sprintf("%+9s %s", lineno, s)
    cat("\nORIGIN",
      file = outputF,
      sep = "\n",
      append = T
    )
    cat(s,
      file = outputF,
      sep = "\n",
      append = T
    )
    cat("//", file = outputF, append = T)
  } else {
    cat("\n//", file = outputF, append = T)
  }
}

###### Tree functions ######
#' Tree2Edgetbl
#' @description
#' tabulates a phylo object.
#' Consider replacnig with tidytree::as_tibble(tree)
#' @param tree
#' phylo
#' @return
#' data.frame
#' @export
#'
#' @examples
#' Tree2Edgetbl
Tree2Edgetbl <- function(tree) {
  data.frame(
    "parent_node" = tree$edge[, 1],
    "parent_node_name" = sapply(tree$edge[, 1], select.tip.or.node, tree = tree),
    "child_node" = tree$edge[, 2],
    "child_node_name" = sapply(tree$edge[, 2], select.tip.or.node, tree = tree)
  )
}


#' QuickTreePDF
#'
#' @param name
#' name for output PDF file.
#' @return
#' @export
#'
#' @examples
QuickTreePDF <- function(name="QuickTree.pdf"){
  Name <- gsub(pattern = ", ",replacement = ".",x = toString.default(gsub(pattern = ":",replacement = "-",x = str_split_fixed(string =  Sys.time(),pattern = " ",n = 3)[,1:2])))
  pdf(
    file = p0("TmpTree.",Name,".",name),
    width = 50,
    height = 50,
    title = "ArgMax with CRISPR and Lysis matches tips",
    useDingbats = T,
  )
}

#' QuickTreePDF2
#'
#' @param name
#' name for output PDF file.
#' @return
#' @export
#'
#' @examples
QuickTreePDF2 <- function(name="QuickTree.pdf"){
  Name <- gsub(pattern = ", ",replacement = ".",x = toString.default(gsub(pattern = ":",replacement = "-",x = str_split_fixed(string =  Sys.time(),pattern = " ",n = 3)[,1:2])))
  cairo_pdf(antialias = "none",
            filename = p0("TmpTree.",Name,".",name),
            width = 50,
            height = 50)
}

#' RepositionClade
#' @description
#' prune a clade from a phylogentic tree and re-graft it at given position
#' @param tree
#' phylo
#' @param clade
#' Internal node id
#' @param where
#' Internal node id (to graft to)
#' @param position
#'
#' @return
#' phylo
#' @export
#'
#' @examples
#' RepositionClade
RepositionClade <- function(tree, clade, where, position = NULL) {
  cladetree <- ape::extract.clade(
    phy = tree,
    node = clade,
    collapse.singles = F
  )
  tree <- treeio::drop.tip(
    object = tree,
    tip = phangorn::Descendants(tree, clade, type = "tips")[[1]]
  )
  tree <- ape::bind.tree(tree, cladetree, where = where, position = position)
  return(tree)
}

#' isTip
#' @description
#' Shortend call to treeio::isTip
#' @param tree
#' phylo
#' @param node
#' node id
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' isTip
isTip <- function(tree, node, ...) {
  treeio::isTip(.data = tree, .node = node, ...)
}

#' GetKids
#' @description
#' Shortend call to phangorn::Descendants
#' @param tree
#' phylo
#' @param node
#' @param Return_Labels
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' GetKids
GetKids <- function(tree, node, Return_Labels = F, ...) {
  if (!Return_Labels) {
    return(phangorn::Descendants(x = tree, node = node, ...))
  }
  return(lapply(phangorn::Descendants(x = tree, node = node, ...), function(x) {
    NID2NLabel(tree, x)
  }))
}

#' GetParents
#' @description
#' Shortend call to phangorn::Ancestors
#' @param tree
#' phylo
#' @param node
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
GetParents <- function(tree, node, ...) {
  phangorn::Ancestors(x = tree, node = node, ...)
}

#' DropNode
#' @description
#' TreeTools::DropTip doesn't work, and ape's drop.tip require labels (doesn't work with internal nodes)
#' @param tree
#' phylo
#' @param node
#' @param include.self
#'
#' @return
#' @export
#'
#' @examples
#' DropNode
DropNode <- function(tree, node, include.self = F) {
  Tips2Rem <- tree$tip.label[unique(unlist(GetKids(tree, node, type = "tips")))]
  InrNodes2Rem <- tree$node.label[setdiff(unique(unlist(GetKids(tree, node, type = "all"))), 1:Ntip(tree)) -
    Ntip(tree)]
  ProtectedLabs <- tree$node.label[node - Ntip(tree)]
  if (include.self == F) {
    InrNodes2Rem <- setdiff(InrNodes2Rem, ProtectedLabs)
  }
  Tips2Rem <- union(Tips2Rem, InrNodes2Rem)
  while (length(Tips2Rem) != 0) {
    tree <- ape::drop.tip(
      tree,
      tip = Tips2Rem,
      trim.internal = F,
      rooted = T,
      collapse.singles = F,
      interactive = F
    )
    Tips2Rem <- intersect(Tips2Rem, tree$tip.label)
  }
  return(tree)
}

#' DropNodeKids
#'
#' @param tree
#' phylo
#' @param node
#'
#' @return
#' @export
#'
#' @examples
DropNodeKids <- function(tree = WorkTree, node) { # 2022 Version.2
  if(class(node) == "character"){
    Nodelabel <- node
    nodeid <- NLabel2NID(tree,node)
  }
  if(class(node) == "numeric"){
    nodeid <- node
    Nodelabel <- NID2NLabel(tree,node)
  }
  w1 <- wh(tree$edge[,2] == nodeid)
  Len2Parent <- tree$edge.length[w1]
  ParentLabel <- NID2NLabel(tree,tree$edge[w1,1])
  Tips2Rem <- unlist(GetKids(tree, node = nodeid, type = "all",Return_Labels =T))
  tree <- ape::drop.tip(phy = tree, tip = Tips2Rem, trim.internal = T,collapse.singles = F)
  tree <- phangorn::add.tips(tree = tree, tips = Nodelabel, where = NLabel2NID(tree,ParentLabel), edge.length = Len2Parent)
  return(tree)
}



#' GetUnwantedKids
#' @description
#' @param tree
#' phylo
#' @param node
#' @param omit.internal
#' @param asids
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' GetUnwantedKids
GetUnwantedKids <- function(tree,
                            node,
                            omit.internal = T,
                            asids = F,
                            ...) {
  node <- as.integer(na.omit(node))
  Tips2eval <- tree$tip.label[unique(unlist(GetKids(tree, node, type = "tips")))]
  InrNodes2eval <- tree$node.label[setdiff(unique(unlist(GetKids(tree, node, type = "all"))), 1:Ntip(tree)) -
    Ntip(tree)]
  # ProtectedLabs = tree$node.label[node-Ntip(tree)]
  # if (include.self == F){
  #   InrNodes2eval = setdiff(InrNodes2eval,ProtectedLabs)
  # }
  Tips2eval <- unique(union(Tips2eval, InrNodes2eval))
  LCA <- castor::get_mrca_of_set(tree = tree, descendants = Tips2eval)
  if (omit.internal) {
    AllLCAKids <- tidytree::nodelab(tree = tree, GetKids(tree, LCA, type = "tips")[[1]])
  }
  if (!omit.internal) {
    AllLCAKids <- tidytree::nodelab(tree = tree, GetKids(tree, LCA, type = "all")[[1]])
  }
  UnwantedKids <- setdiff(AllLCAKids, Tips2eval)
  if (asids) {
    UnwantedKids <- NLabel2NID(tree, UnwantedKids)
  }
  return(UnwantedKids)
}

#' SplitTree2MonoPhyloByTips
#' @description
#' @param tree
#' phylo
#' @param node
#'
#' @return
#' @export
#'
#' @examples
#' SplitTree2MonoPhyloByTips
SplitTree2MonoPhyloByTips <- function(tree, node) {
  # EDIT DESCRIPTION!
  # TreeTools:: input tree searched for LCA of given nodes/tips, then, if the input tips define a monophyletic group returns a subtreee
  # with that LCA as root, ommitting all tips and nodes not in the monophyletic group. If the group of tips doesn't define a monophyletic group, split it into N subgroups that do define monophyletic groups with those tips.
  # Note! tree needs to have labels (tips and nodes) because I can't be bothered right now.
  LCA <- get_mrca_of_set(tree, node)
  tmpsubtree <- get_subtree_at_node(tree = tree, node = (LCA - Ntip(tree)))$subtree
  NewNode <- NLabel2NID(tmpsubtree, NID2NLabel(tree, node))

  UnwantedKids <- unlist(GetUnwantedKids(
    tree = tmpsubtree,
    node = NewNode,
    asids = T
  ))
  WantedKids <- unique(unlist(GetKids(
    tree = tmpsubtree,
    node = NewNode,
    type = "tips"
  )))
  tmptreeque <- get_tree_traversal_root_to_tips(tmpsubtree, include_tips = T)$queue

  tmpdf <- data.frame(
    "Node" = c(1:(tmpsubtree$Nnode + Ntip(tmpsubtree))),
    "Node_Label" = c(tmpsubtree$tip.label, tmpsubtree$node.label),
    "Kids" = NA,
    "Has_unwanted" = T,
    "muster" = F,
    "IsTip" = F
  )
  tmpdf$Kids <- GetKids(
    tree = tmpsubtree,
    node = tmpdf$Node,
    type = "all"
  )
  tmpdf$Nunwanted <- unlist(
    mclapply(
      tmpdf$Node,
      FUN = function(x) {
        unlist(length(setdiff(
          unlist(tmpdf[x, "Kids"]), unlist(WantedKids)
        )))
      },
      mc.preschedule = T,
      mc.set.seed = T,
      mc.silent = F,
      mc.cores = THREADS,
      mc.cleanup = F,
      mc.allow.recursive = T,
      affinity.list = NULL
    )
  )
  tmpdf$Has_unwanted <- unlist(
    mclapply(
      tmpdf$Node,
      FUN = function(x) {
        (if (tmpdf[x, "Nunwanted"] == 0) {
          F
        } else {
          T
        })
      },
      mc.preschedule = T,
      mc.set.seed = T,
      mc.silent = F,
      mc.cores = THREADS,
      mc.cleanup = F,
      mc.allow.recursive = T,
      affinity.list = NULL
    )
  )
  tmpdf$Nwanted <- unlist(
    mclapply(
      tmpdf$Node,
      FUN = function(x) {
        (length(intersect(
          unlist(tmpdf[x, "Kids"]), unlist(WantedKids)
        )))
      },
      mc.preschedule = T,
      mc.set.seed = T,
      mc.silent = F,
      mc.cores = THREADS,
      mc.cleanup = F,
      mc.allow.recursive = T,
      affinity.list = NULL
    )
  )
  tmpdf$Has_wanted <- unlist(
    mclapply(
      tmpdf$Node,
      FUN = function(x) {
        (if (tmpdf[x, "Nwanted"] == 0) {
          F
        } else {
          T
        })
      },
      mc.preschedule = T,
      mc.set.seed = T,
      mc.silent = F,
      mc.cores = THREADS,
      mc.cleanup = F,
      mc.allow.recursive = T,
      affinity.list = NULL
    )
  )
  tmpdf <- filter(tmpdf, Has_wanted == T)
  tmpdf <- filter(tmpdf, Has_unwanted == F)

  tmpdf$IsTip[which(tmpdf$Node <= Ntip(tmpsubtree))] <- T
  kidsleft <- as.integer(WantedKids)
  kidsdone <- c()
  tmptreeque_rel <- intersect(tmptreeque, tmpdf$Node)
  for (w in (tmptreeque_rel)) {
    print(length(kidsleft))
    # print(w)
    j <- which(tmpdf$Node == w)
    # if(tmpdf$Has_unwanted[j]){
    #   next
    # }
    leny <- length(intersect(unlist(tmpdf$Kids[j]), kidsleft))
    if (leny > 0) {
      kidsdone <- unique(union(kidsdone, as.integer(unlist(tmpdf$Kids[j]))))
      tmptreeque_rel <- setdiff(tmptreeque_rel, kidsdone)
      kidsleft <- setdiff(kidsleft, kidsdone)
      tmpdf$muster[j] <- T
    }
    if (leny == 0) {
      tmpdf$muster[j] <- F
    }
    if (length(kidsleft) == 0) {
      break
    }
  }
  return(filter(tmpdf, muster))
}

#' NLabel2NID
#' @description
#' Shortend call to tidytree::nodeid
#' @param tree
#' phylo
#' @param label
#'
#' @return
#' @export
#'
#' @examples
#' NLabel2NID
NLabel2NID <- function(tree, label) {
  tidytree::nodeid(tree, label)
}

#' NID2NLabel
#' @description
#' For a phylo object, convert node ID (including tips) to node.label
#' @param tree
#' phylo
#' @param tip
#'
#' @return
#' @export
#'
#' @examples
#' NID2NLabel
NID2NLabel <- function(tree, tip) {
  if (len(tip) == 1) {
    if (tip <= Ntip(tree)) {
      return(tree$tip.label[tip])
    }
    return(tree$node.label[tip - Ntip(tree)])
  }
  if (len(tip) > 1) {
    sapply(X = tip, FUN = function(x) NID2NLabel(WorkTree, x))
  }
}


###### B/X string functions ######
#' XString2DF
#' @description
#' Convert BString/like objects into a data.frame
#' @param faa
#' input xstring/bstring/dnaStringSet...
#' @param input_was
#' Column name for the IDs
#' @param trimwhite
#' Should whitespaces be removd from sequence names?
#' @param seqcolname
#' Column name for the sequence.
#' @param addlength
#' Add a "Length" column with the sequences' width?
#' @return
#' data.frame
#' @export
#'
#' @examples
#' XString2DF
#' @family BString_functions
XString2DF <- function(faa,
                       input_was = "new_name",
                       trimwhite = F,
                       seqcolname = "seq",
                       addlength = T) {
  o1 <- as.character(faa)
  outdf <- data.frame(
    xyz = names(o1),
    "seq" = unname(o1),
    Length = faa@ranges@width
  )
  if (trimwhite == T) {
    outdf$xyz <- trimws(outdf$xyz)
  }
  colnames(outdf) <- c(toString(input_was), seqcolname, "Length")
  if (addlength == F) {
    outdf$Length <- NULL
  }
  return(outdf)
}

#' DF2XString
#' @description
#' Convert data.frame into a BString object.
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param seqcol
#' Column name for the sequence.
#' @param nmcol
#' Column name for the IDs
#' @return
#' BStringSet
#' @export
#'
#' @examples
#' DF2XString
#' @family BString_functions
DF2XString <- function(dfdt,
                       seqcol = "seq",
                       nmcol = "id") {
  dfdt <- as.data.frame(dfdt)
  outxstring <- Biostrings::BStringSet(x = dfdt[, seqcol])
  names(outxstring) <- dfdt[, nmcol]
  return(outxstring)
}

#' XString2KmerDT
#' @description
#' Compute kmer frequency/count per input StringSet.
#' TODO: replace with Biostrings::oligonucleotideFrequency
#' @param infaa
#' input xstring/bstring/dnaStringSet...
#' @param abcde
#' Alpha-Beit of input strings
#' @param k
#' kmer to use (numeric)
#' @param out_probablity
#' Should output be normalized as per-sequence probality?
#' @param input_was
#' Column name for the IDs
#' @return
#' data.frame
#' @export
#' @family BString_functions
#' @examples
#' XString2KmerDT
XString2KmerDT <- function(infaa = BS_all_mot_sssp,
                           abcde = c("H", "E", "C"),
                           k = 3,
                           out_probablity = F,
                           input_was = "name_mot") {
  # out_probablity == return output as absolute number (count) or as probablity
  allKmers <- Biostrings::BStringSet(mkAllStrings(abcde, k, fast.moving.side = "right"))
  names(allKmers) <- allKmers
  Kmer_counts <- data.table(t(
    vcountPDict(
      allKmers,
      infaa[],
      max.mismatch = 0,
      min.mismatch = 0,
      with.indels = F,
      fixed = T,
      algorithm = "auto",
      verbose = T
    )
  ))
  Kmer_counts <- cbind(c(names(infaa)), Kmer_counts)
  colnames(Kmer_counts) <- c(input_was, names(allKmers))
  Kmer_counts <- Kmer_counts[, lapply(.SD, as.numeric), by = input_was]
  mertypes <- length(allKmers)
  if (out_probablity == F) {
    return(Kmer_counts)
  } else {
    # Kmer_counts_1 = Kmer_counts
    Kmer_counts$sumr <- as.numeric(apply(
      Kmer_counts[, -1],
      1,
      FUN = function(x) {
        sum(x)
      }
    ))
    for (mer in names(allKmers)) {
      # Figure out a better way to do this
      Kmer_counts[, mer] <- apply(
        Kmer_counts,
        1,
        FUN = function(x) {
          (as.numeric(x[mer])) / as.numeric(x["sumr"])
        }
      )
    }
    return(`dropcols`(Kmer_counts, "sumr"))
  }
}

#' AAcoor2NAcoor
#' @description
#' Convert amino acid coordinates to their coorsponding nucleic coordinates (i.e. from region on an ORF to region on a gene)
#' Transform alignment coordinates from protein locations to ~locationn on the (coding?) nucleic acid sequence.
#' @param frm
#' Frame
#' @param AAc1
#' Coordinate 1
#' @param AAc2
#' Coordinate 2
#'
#' @return
#'
#' @export
#' @family BString_functions
#' @examples
#' AAcoor2NAcoor
AAcoor2NAcoor <- function(frm, AAc1, AAc2) {
  frm <- as.numeric(unlist(unname(trimws(frm))))
  AAc1 <- as.numeric(unlist(unname(trimws(AAc1))))
  AAc2 <- as.numeric(unlist(unname(trimws(AAc2))))
  if (frm > 0) {
    NAc1 <- ((AAc1 - 1) * 3) + frm
    NAc2 <- (AAc2 * 3) + frm
  }
  if (frm < 0) {
    NAc1 <- AAc2 * 3 - frm
    NAc2 <- ((AAc1 - 1) * 3) - frm
  }
  return(c(NAc1, NAc2))
}

#' AAcoor2NAcoor_df
#' @description
#' Like AAcoor2NAcoor, but takes a data.frame as input
#' @param dfdt
#' Input table to work on (data.frame or data.table)
#' @param frmcol
#' Column name of the field with the frame information.
#' @param AAc1col
#' Column name of the field with the start coordinate information.
#' @param AAc2col
#' Column name of the field with the end coordinate information.
#' @param outdf
#' Should output be a data.frame?
#'
#' @return
#' @export
#' @family BString_functions
#' @examples
#' AAcoor2NAcoor_df
AAcoor2NAcoor_df <- function(dfdt, frmcol, AAc1col, AAc2col, outdf = T) {
  data.table::setDT(dfdt)
  rv <- which(dfdt[, ..frmcol] < 0)
  fr <- which(dfdt[, ..frmcol] > 0)
  dfdt$NAcoor <- ""
  dfdt$NAc1 <- -1
  dfdt$NAc2 <- -1
  dfdt$NAc1[rv] <- as.integer(unlist(unname(dfdt[rv, ..AAc2col] * 3))) -
    as.integer(unlist(unname(dfdt[rv, ..frmcol])))
  dfdt$NAc2[rv] <- as.integer(unlist(unname((dfdt[rv, ..AAc1col] - 1) * 3))) -
    as.integer(unlist(unname(dfdt[rv, ..frmcol])))
  dfdt$NAc1[fr] <- as.integer(unlist(unname((dfdt[fr, ..AAc1col] - 1) * 3))) +
    as.integer(unlist(unname(dfdt[fr, ..frmcol])))
  dfdt$NAc2[fr] <- as.integer(unlist(unname(dfdt[fr, ..AAc2col] * 3))) +
    as.integer(unlist(unname(dfdt[fr, ..frmcol])))
  dfdt$NAc2[which(dfdt$NAc2 > dfdt$Length)] <- dfdt$Length[which(dfdt$NAc2 > dfdt$Length)]
  dfdt$NAc1[which(dfdt$NAc1 > dfdt$Length)] <- dfdt$Length[which(dfdt$NAc1 > dfdt$Length)]
  # dfdt$NAc1[which(dfdt$NAc1<0)] = dfdt$Length[which(dfdt$NAc1 > dfdt$Length)]
  dfdt[, "NAcoor"] <- p0(dfdt$NAc1, "-", dfdt$NAc2)
  if (outdf == T) {
    return(dfdt)
  }
  return(dfdt$NAcoor)
}

#' AAcoor2NAcoor_dFAST
#' @description
#' Like AAcoor2NAcoor, but takes a data.frame as input AND assumes specific colnames.
#' @param dfdt
#' Input table to work on (data.frame or data.table)rw
#'
#' @return
#' data.frame
#' @export
#' @family BString_functions
#' @examples
#' AAcoor2NAcoor_dFAST
AAcoor2NAcoor_dFAST <- function(dfdtrw) {
  dfdtrw <- data.frame(dfdtrw)
  dfdtrw$frame <- as.numeric(trimws(dfdtrw[, "frame"]))
  # AAc1 = as.numeric(dfdtrw[,"q1"])
  # AAc2 = as.numeric(dfdtrw[,"q2"])

  return(apply(
    dfdtrw,
    MARGIN = 1,
    FUN = function(x) {
      (AAcoor2NAcoor(
        frm = x["frame"],
        AAc1 = x["q1"],
        AAc2 = x["q2"]
      ))
    }
  ))
}

#' Prodigal_AA2NA
#'
#' @param ORFlen
#' @param ORFstart_nuc
#' @param ORFend_nuc
#' @param AA1
#' @param AA2
#' @param Strand
#'
#' @return
#' @export
#'
#' @examples
Prodigal_AA2NA <- function(ORFlen, ORFstart_nuc, ORFend_nuc, AA1, AA2,Strand) { # Transform alignment coordinates from a prodgial or similar predicted region. Not suitable for 6frxs!.
  AA1 <- as.num(AA1)
  AA2 <- as.num(AA2)
  ORFlen <- as.num(ORFlen)
  ORFstart_nuc <- as.num(ORFstart_nuc)
  ORFend_nuc <- as.num(ORFend_nuc)
  if(Strand == "-"){
    DeltaQ <- ORFlen + 1 - (AA2 + 1)
    NA2 <- (DeltaQ * 3) + ORFstart_nuc
    NA1 <- NA2 + (3 * (AA2 - AA1 -1))
  }
  if(Strand == "+"){
    NA1 <- (((AA1) - 1) * 3) + (ORFstart_nuc)
    NA2 <- ORFstart_nuc + ((AA2 - 1) * 3)
  }
  return(p0(NA1,"..",NA2))
}

#' XSDNARedunFilter
#' @description
#' For nucleic sequences, remove identical duplicates of StringSet objects.
#' N.B - this checks revcomp as well.
#' for DNA
#' @param xsnuc
#'
#' @return
#' Filtered StringSet i.e. discards one of the duplicates in no particular order.
#' @export
#' @family BString_functions
#' @examples
#' XSDNARedunFilter
XSDNARedunFilter <- function(xsnuc) {
  xsnuc_rev <- reverseComplement(xsnuc)
  rev_reg_xsnuc <- unique(union(xsnuc, xsnuc_rev))
  uniquers <- unique(names(rev_reg_xsnuc))
  return(xsnuc[uniquers])
}

#' Trim2Core2
#' @description
#' Misc functions for extraction of core domain region based on profile matches.
#' @param hits_df
#' Input table (data.frame). Usually tabular search results of profile search e.g. hmmsearch
#' @param faa
#' Input XString/BString/DNAStringSet...
#' @param add_genetic_code
#' If canonical stops are identified within the core region, should this be noted in the output?
#' @param Out_faa
#' Should the output include the trimmed StringSet?
#' @param out_df
#' Should the output include the potentially modified hit table?
#' @param input_was
#' Column name for the input (usually seq IDs).
#' @param subject_was
#' Column name for the profile (usually profile IDs).
#' @param Expand_projectionX
#' Should the core region be expanded slightly based to cover the entire *projected* region of the alignment?
#' @return
#' @export
#' @family BString_functions
#' @examples
#' Trim2Core2
Trim2Core2 <- function(hits_df = Motif_df,
                       faa = FL_longest_members,
                       add_genetic_code = F,
                       Out_faa = T,
                       out_df = F,
                       input_was = "new_name",
                       subject_was = "profile",
                       Expand_projectionX = 0) {
  faa_df <- XString2DF(faa, input_was = input_was, seqcolname = "seq")
  workies <- intersect(hits_df[, input_was], names(faa))
  hits_df <- hits_df[which(hits_df[, input_was] %in% workies), ]
  work_faa <- faa[hits_df[, input_was]] # This'll keep duplicates AND the order.
  hits_df <- merge(
    hits_df,
    faa_df[, c(input_was, "seq")],
    by = c(input_was),
    all.x = T,
    all.y = F
  )
  hits_df$QnS <- p0(hits_df[, input_was], ".", hits_df[, subject_was])
  if (Expand_projectionX != 0) {
    hits_df$extract_from <- apply(hits_df, 1, trimifo, Expand_projectionX)
    hits_df$extract_to <- apply(hits_df, 1, trimito, Expand_projectionX)
    out_faa <- BStringSet(hits_df$seq,
      start = hits_df$extract_from,
      end = hits_df$extract_to
    )
  } else {
    out_faa <- BStringSet(hits_df$seq, start = hits_df$q1, end = hits_df$q2)
  }
  names(out_faa) <- hits_df$QnS
  if (add_genetic_code == T) {
    hits_df$Genetic_Code <- "Standard"
    hits_df$Genetic_Code[unique(grep("X", x = out_faa[]))] <- "Non-Standard"
  }
  output <- list()
  if (Out_faa == T) {
    output <- append(output, out_faa)
  }
  if (out_df == T) {
    hits_df <- data.frame(hits_df, stringsAsFactors = F)
    output <- list(output, hits_df[, setdiff(colnames(hits_df), c("seq"))])
  }
  return(output)
}

###### Parsers and Runners ######
MMseq2_outfmt6_cols <- c("subject_name", "evalue", "gapopen", "pident", "nident", "q1", "q2", "qL", "p1", "p2", "pL", "ali_len", "raw", "score", "frame", "mismatch", "qcov", "tcov")
hmmsearh_cols <- c("r1", "qL", "subject_name", "r2", "pL", "evalue", "score", "bias", "#", "of", "c-Evalue", "i-Evalue", "score2", "bias", "p1", "p2", "q1", "q2", "env_from", "env_to", "acc", "r3", "r4")
psiblast_cols <- c("pident", "q1", "q2", "p1", "p2", "qL", "pL", "ali_len", "evalue", "score", "subject_name")
hhsearch_cols <- c("subject_name", "pCoverage", "ali_len", "pL", "mismatch", "gapOpen", "q1", "q2", "p1", "p2", "Probab", "evalue", "score")
DaimondP_cols <- c("subject_name", "evalue", "gapopen", "pident", "ali_len", "p1", "p2", "q1", "q2", "pL", "qL", "mismatch", "score")
DaimondP_rev <- c("subject_name", "pident", "ali_len", "q1", "q2", "p1", "p2", "qL", "pL", "mismatch", "gapopen", "evalue", "score")
Blastn_cols <- c("query_name", "subject_name", "pident", "qL", "pL", "q1", "q2", "p1", "p2", "ali_len", "evalue", "score")
unicols <- c("profile, qL, pL, p1, p2, q1, q2, score, evalue, ali_len, pCoverage")
MMseq2_Long_fmt6_cols <- c("query_name", "subject_name", "evalue", "gapopen", "pident", "nident", "q1", "q2", "qL", "p1", "p2", "pL", "ali_len", "raw", "score", "qframe", "mismatch", "qcov", "tcov")

colnyms <- data.frame(
  "psiblast" = toString(psiblast_cols),
  "mmseqs" = toString(MMseq2_outfmt6_cols),
  "hhsearch" = toString(hhsearch_cols),
  "hmmsearch" = toString(hmmsearh_cols),
  "diamondp" = toString(DaimondP_cols),
  "uni" = unicols,
  stringsAsFactors = F
)

#' GenericHitsParser
#' @description
#' Reads as input search results and parses them consistently.
#' @param inpt
#' Input path.
#' @param breakhdrs
#' @param input_was
#' @param Cull_hits
#' @param search_tool
#' @param reducecols
#' @param calc_pcoverage
#' @param colsnms
#' @param CullCol
#' @param hedsep
#' @param hedclbs
#'
#' @return
#' @export
#'
#' @examples
#' GenericHitsParser
GenericHitsParser <- function(inpt = "hit.tsv",
                              breakhdrs = T,
                              input_was = "Scaf_ID_frame",
                              Cull_hits = F,
                              search_tool = "psiblast",
                              reducecols = T,
                              calc_pcoverage = T,
                              colsnms = "provide",
                              CullCol = "score",
                              hedsep = ".",
                              hedclbs = c("id", "frame")) {
  if (!(search_tool %in% c("psiblast", "mmseqs", "hmmsearch", "hhsearch", "diamondp"))) {
    print(
      "If search tool isn't psiblast, mmseqs, diamondp, hhmsearch or hmmsearch, provide the raw input table colnames in the arg colsnms"
    )
    # return()
  } else {
    colsnms <- c(input_was, stringr::str_split(colnyms[1, which(colnames(colnyms) == search_tool)], ", ", simplify = T))
  }
  hits_tbl <- try(data.table::fread(
    inpt,
    stringsAsFactors = F,
    col.names = colsnms,
    sep = "\t"
  ))
  if ((search_tool == "mmseqs")) {
    hits_tbl <- subset(hits_tbl, select = -c(6, 4, 16))
  }
  if ((search_tool == "hmmsearch")) {
    hits_tbl <- subset(hits_tbl, select = -c(2, 5, 23, 24))
    hits_tbl$ali_len <- hits_tbl$q2 - hits_tbl$q1
  }
  if (Cull_hits == T) {
    dt <- data.table(hits_tbl)
    min_dt <- data.frame(dt[, max(score), by = input_was])
    colnames(min_dt) <- c(input_was, CullCol)
    hits_tbl <- merge(
      min_dt,
      hits_tbl,
      by = c(input_was, CullCol),
      all.x = T,
      all.y = F,
      stringsAsFactors = F
    ) # Hard culling
  }
  hits_tbl$profile <- hits_tbl$subject_name
  if (breakhdrs == T) {
    hits_tbl <- HeaderBreakerCb(
      input_df = hits_tbl,
      sep = hedsep,
      clb = input_was,
      nclb = hedclbs
    )
    if (reducecols == T) {
      hits_tbl <- subset(
        hits_tbl,
        select = c(input_was, hedclbs, intersect(
          str_split(colnyms[1, "uni"], ", ")[[1]], colnames(hits_tbl)
        )),
        stringsAsFactors = F
      )
    }
  } else {
    if (reducecols == T) {
      hits_tbl <- subset(
        hits_tbl,
        select = c(input_was, intersect(
          str_split(colnyms[1, "uni"], ", ")[[1]], colnames(hits_tbl)
        )),
        stringsAsFactors = F
      )
    }
  }
  if (calc_pcoverage == T) {
    hits_tbl <- CalcPcoverage(hits_tbl)
  }
  gc()
  return(hits_tbl)
}

#' ReadMcl
#' @description
#' @param mclfile
#' Input path.
#' @param col1prefix
#'
#' @return
#' data.frame
#' @export
#'
#' @examples
#' ReadMcl
ReadMcl <- function(mclfile, col1prefix = "motif") {
  cx <- scan(mclfile, what = "", sep = "\n", )
  xcx <- strsplit(cx, "[[:space:]]+") # Separate elements by one or more whitespaces...
  cluster_df <- data.frame(
    "cls" = paste0(col1prefix, ".", 1:length(xcx)),
    "reps" = "",
    stringsAsFactors = F
  )
  for (i in 1:nrow(cluster_df)) {
    cluster_df$reps[i] <- (xcx[i])
  }
  return(cluster_df)
}

#' ReadABC
#' @description
#' @param ABCfile
#' Input path.
#' @param clsnms
#'
#' @return
#' data.frame
#' @export
#'
#' @examples
#' ReadABC
ReadABC <- function(ABCfile, clsnms = c("reprs", "mems")) {
  cx <- fread(
    input = ABCfile,
    data.table = F,
    skip = 0,
    header = F,
    sep = "\n"
  )
  cx <- distinct(cx)
  cx <- as.data.table(stringi::stri_split_fixed(
    as.character(cx[, "reprs"]),
    pattern  = " ",
    n = 2,
    simplify = T
  ))
  cx[, lmems := list(stringi::stri_split_fixed(as.character(.SD[]), pattern = " ")), by = "V1", .SDcols = "V2"]
  colnames(cx) <- c(clsnms, "lmems")
  return(cx)
}

#' ReadABC2
#' @description
#' @param ABCfile
#' @param clsnms
#'
#' @return
#' @export
#'
#' @examples
#' ReadABC2
ReadABC2 <- function(ABCfile, clsnms = c("reprs", "mems")) {
  cx <- fread(
    input = ABCfile,
    data.table = F,
    skip = 0,
    header = F,
    sep = "\t"
  )
  cx <- distinct(cx)
  xcx <- strsplit(cx$V2, "[[:space:]]+") # Separate elements by one or more whitespaces...
  cx <- as.data.table(cx)[, lmems := xcx]
  colnames(cx) <- c(clsnms, "lmems")
  return(cx)
}

#' ReadDBN
#' @description
#' Read a dot bracket notation (DBN) file.
#' @param DBNfile
#' Input path.
#' @param includes_MFE
#' For outputs of certain RNA secondary structre folding predctions, should the MFE value be included?
#' @param return_type
#' Should the output be a data.frame or a BStringSet
#' @return
#' @export
#'
#' @examples
#' ReadDBN
ReadDBN <- function(DBNfile,
                    includes_MFE = T,
                    return_type = "DF") {
  # Read DBN file
  cx <- scan(DBNfile, what = "", sep = "\n")
  Ncx <- gsub(
    pattern = " > ",
    replacement = "",
    x = cx[seq(1, length(cx), by = 3)],
    fixed = T
  )
  Scx <- cx[seq(2, length(cx), by = 3)]
  Dcx <- cx[seq(3, length(cx), by = 3)]
  if (includes_MFE) {
    Mcx <- unlist(strsplit(Dcx, "[[:space:]]+"))
    Dcx <- Mcx[seq(1, length(Mcx), by = 2)]
    Mcx <- Mcx[seq(2, length(Mcx), by = 2)]
  }
  if (return_type == "DF") {
    return(data.frame(
      "ID" = Ncx,
      "Seq" = Scx,
      "DBN" = Dcx,
      "MFE" = Mcx
    ))
  }
  if (return_type == "BS") {
    OutBS <- Biostrings::BStringSet(x = Scx)
    names(OutBS) <- Ncx
    OutBS@elementMetadata <- DataFrame("DBN" = Dcx, "MFE" = Mcx) # OutBS["NC_001653.2 Hepatitis delta virus, complete genome"]@elementMetadata$DBN
    return(OutBS)
  }
}

#' PopForna
#' @description
#' Given a DBN enrty, generates (and opens) a URL for forna (RNA secondary structre visulaizer).
#' @param DBN
#' @param pop
#' @param returl
#'
#' @return
#' @export
#'
#' @examples
#' PopForna
PopForna <- function(DBN, pop = F, returl = T) {
  # To view on IMG/MER site.
  bu <- p0("http://nibiru.tbi.univie.ac.at/forna/forna.html?id = url/name&sequence = ")
  bu_wd <- p0(bu, (unname(as.char(DBN))), "&structure = ", DBN@elementMetadata$DBN)
  if (pop) {
    browseURL(bu_wd)
  }
  if (returl) {
    return(bu_wd)
  }
}

#' MMseqsClstsReader
#' @description
#' TODO: Rewrite in a sensical manner and write proper documentation.
#' @param inpt
#' Input path.
#' @param clsnm
#'
#' @return
#' data.frame
#' @export
#'
#' @examples
#' MMseqsClstsReader
MMseqsClstsReader <- function(inpt = "resultsDB_clu.tsv", clsnm = "rdrp_id") {
  clsts <- fread(
    input = inpt,
    col.names = c("reprs", clsnm),
    data.table = F,
    skip = 0,
    header = F
  )
  singlts <- clsts[which(clsts$reprs == clsts[, clsnm]), ]
  clsts <- clsts[-which(clsts$reprs == clsts[, clsnm]), ]
  singlts <- singlts[-which(singlts$reprs %in% unique(clsts$reprs)), ]
  memtbl <- data.table::as.data.table(clsts)[, p0((.SD)), by = .(reprs)]
  memtbl$V1 <- gsub(
    x = memtbl$V1,
    pattern = 'c("',
    fixed = T,
    replacement = ""
  )
  memtbl$V1 <- gsub(
    x = memtbl$V1,
    pattern = '")',
    fixed = T,
    replacement = ""
  )
  memtbl$V1 <- gsub(
    x = memtbl$V1,
    pattern = '"',
    fixed = T,
    replacement = ""
  )
  memtbl$V1 <- gsub(
    x = memtbl$V1,
    pattern = ")",
    fixed = T,
    replacement = ""
  )
  memtbl$V1 <- gsub("\n", "", memtbl$V1, fixed = T)
  memtbl$V1 <- p0(memtbl$reprs, ", ", memtbl$V1)
  memtbl <- Rename1Col(memtbl, "V1", clsnm)
  singlts <- Rename1Col(singlts, "rdrp_id", clsnm)
  memtbl <- as.data.frame(rbind(memtbl, singlts))
  tstdf <- data.frame(do.call("rbind", strsplit(as.character(memtbl[, clsnm]), ", ", fixed = T)))
  memtbl$memlist <- apply(tstdf, 1, unique)
  memtbl[, clsnm] <- NULL
  memtbl$Nseq <- as.integer(lapply((memtbl$memlist), length))
  memtbl <- Rename1Col(memtbl, "memlist", clsnm)
  return(distinct(memtbl))
}

#' ScreenLastz
#' @description
#' Forked from Jerome Ambroise script: https://github.com/JeromeAmbroise/Pathogenomics2/blob/master/R/screenfunction.R
#' @param reference
#' @param querry
#'
#' @return
#' @export
#'
#' @examples
#' ScreenLastz
ScreenLastz <- function(reference, querry) {
  rando <- floor(stats::runif(1, min = 1, max = 10000000))
  try(unlink(p0("temp.", rando), recursive = TRUE))
  dir.create(p0("temp.", rando), showWarnings = F)
  myarg <- paste0(
    reference,
    "[multiple] ",
    querry,
    " --ambiguous = iupac --notransition --strand = both --step = 100 --nogapped ‑‑format = rdotplot > ",
    p0("temp.", rando),
    "/result.maf"
  )
  system2(command = "lastz", args = myarg)

  last <- try(utils::read.table(p0("temp.", rando, "/", "result.maf")), silent = T)
  if (class(last) == "data.frame") {
    start.stop <- as.numeric(na.omit(suppressWarnings(as.numeric(
      as.character(last$V1)
    ))))
    start <- start.stop[seq(1, length(start.stop), by = 2)]
    stop <- start.stop[seq(2, length(start.stop), by = 2)]
    GR <- GenomicRanges::GRanges(
      seqnames = "seq",
      ranges = IRanges(start = start, end = stop)
    )
    GR.disjoin <- GenomicRanges::disjoin(GR)

    hitlength <- sum(width(GR.disjoin))
    seqlength <- sum(width(Biostrings::readDNAStringSet(reference)))

    percentage <- 100 * round(hitlength / seqlength, 3)
  } else {
    (percentage <- 0)
  }
  try(unlink(p0("temp.", rando), recursive = TRUE))
  return(percentage)
}

#' Kalign3
#' @description
#' Shortend caller for kalign3
#' @param xs
#' StringSet
#' @param param
#' Flags?
#' @return
#' @export
#'
#' @examples
#' Kalign3
Kalign3 <- function(xs, param = NULL) {
  wd <- tempdir()
  dir <- getwd()
  temp_file <- basename(tempfile(tmpdir = wd))
  on.exit({
    file.remove(Sys.glob(paste(temp_file, ".*", sep = "")))
    setwd(dir)
  })
  setwd(wd)
  infile <- p0(temp_file, ".in")
  outfile <- p0(temp_file, ".aln")
  Biostrings::writeXStringSet(xs, infile, append = FALSE, format = "fasta")
  system(p0("kalign3 -in ", infile, " -out ", outfile, " -f fasta"))
  if (is(xs, "DNAStringSet")) {
    r <- Biostrings::readDNAMultipleAlignment(outfile, format = "fasta")
  }
  if (is(xs, "RNAStringSet")) {
    r <- Biostrings::readRNAMultipleAlignment(outfile, format = "fasta")
  }
  if (is(xs, "AAStringSet")) {
    r <- Biostrings::readAAMultipleAlignment(outfile, format = "fasta")
  }
  return(r)
}


#' GenericRunner
#' @description
#' A simplified way to call executable / shell commands from within R code.
#' @param command
#' Command to call (if notin path, provide the full path to the binary).
#' @param param
#' Flags/parameters to pass to the command.
#' Formatted as a named list, e.g. "flag" = "value".
#' @param RunInTmp
#' Should the command be called from a temporary folder made inside the current working directory?
#' @param FlgType
#' How should the flags (from "params") be passed to the command? character, e.g. "-", "--"
#' @param AsType
#' How should the values of flags (from "params") be joined with the flags? character, e.g. "=", " "
#' @return
#' @export
#'
#' @examples
#' GenericRunner
GenericRunner <- function(command = "echo",
                          param = list("threads" = 4, "mem" = 120),
                          RunInTmp = F,
                          FlgType = "-",
                          AsType = " ",
                          ...) {
  if (RunInTmp) {
    origdir <- getwd()
    wd <- tempdir()
  }
  if (!RunInTmp) {
    origdir <- getwd()
    wd <- origdir
  }
  setwd(wd)
  temp_file <- basename(tempfile(tmpdir = wd))
  on.exit({
    file.remove(Sys.glob(paste(temp_file, ".*", sep = "")))
    setwd(origdir)
  })
  Comsdf <- data.frame("argname" = names((param)), "argval" = unname(unlist(param)))
  Comsdf$astr <- paste0(FlgType, Comsdf$argname)
  Comsdf$astr <- paste0(Comsdf$astr, AsType, Comsdf$argval)
  Comscaf <- paste(sep = " ", command, paste(Comsdf$astr, collapse = " "))
  system(Comscaf, ...)
}

#' GenericRunnerInOut
#' @description
#' Like GenericRunner but directs the output and input through files
#' i.e. writes something, calls some command on it, then reads in the output.
#' @param xs
#' @param infile
#' @param inflag
#' @param outflag
#' @param outfile
#' @param keepinout
#' @param command
#' @param param
#' @param RunInTmp
#' @param FlgType
#' @param AsType
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' GenericRunnerInOut
#' @seealso \code{\link{GenericRunner}}
GenericRunnerInOut <- function(xs,
                               infile = "infile.faa",
                               inflag = "i",
                               outflag = "o",
                               outfile = "outfile.afa",
                               keepinout = F,
                               command = "echo",
                               param = list("threads" = 4, "mem" = 120),
                               RunInTmp = F,
                               FlgType = "-",
                               AsType = " ",
                               ...) {
  # For Biostrings
  Biostrings::writeXStringSet(xs, infile, append = FALSE, format = "fasta")
  iol <- list(infile, outfile)
  names(iol) <- c(inflag, outflag)
  param <- append(param, iol)
  GenericRunner(param = param, ...)
  if (is(xs, "DNAStringSet")) {
    r <- Biostrings::readDNAMultipleAlignment(outfile, format = "fasta")
  }
  if (is(xs, "RNAStringSet")) {
    r <- Biostrings::readRNAMultipleAlignment(outfile, format = "fasta")
  }
  if (is(xs, "AAStringSet")) {
    r <- Biostrings::readAAMultipleAlignment(outfile, format = "fasta")
  }
  return(r)
}

###### ML/lm related  ######
#' GenerateSeqDescriptors
#' @description
#' TODO: Need some refactoring but should work.
#' @param dfdt
#' Input table (data.frame)
#' @param PrecentRand
#' % of shuffled sequences to generate.
#' @param aacol
#' Colomn name of field with the sequence.
#' @param HEC_Prob
#' @param AA_Prob
#' @param K.HEC
#' @param K.aa
#' @param casecol
#' Column name for the field splitting the input table to train/test sets.
#' @param Exmp_src_faa
#' @param SEED
#' @param valuecol
#' Target column (try to predict this value)
#' @param RemovesSparseCols
#'
#' @return
#' @export
#'
#' @examples
#' GenerateSeqDescriptors
GenerateSeqDescriptors <- function(dfdt = allmots,
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
                                   RemovesSparseCols = T) {
  set.seed(SEED)
  # indf =
  setDT(dfdt)
  trainR <- which(dfdt[, ..casecol] == "Train")
  testR <- setdiff(c(1:nrow(dfdt)), trainR)
  if (PrecentRand != 0) {
    Nfakes <- (PrecentRand / 100) * length(trainR)
    fake_seqs <- universalmotif::shuffle_sequences(sequences = Exmp_src_faa[(sample(x = length(Exmp_src_faa), size = Nfakes))])
    fakemotifs <- narrow(fake_seqs, start = 1, end = (sample(x = dfdt$mL[trainR], size = Nfakes)))
    names(fakemotifs) <- c(p0("fake.", (1:Nfakes)))
    fakemotifs <- XString2DF(fakemotifs, input_was = "new_name")
    fakemotifs[, valuecol] <- "666"
    fakemotifs$name_seq <- p0(fakemotifs$new_name, ".", unlist(fakemotifs[, valuecol]))
    fakemotifs <- Rename1Col(fakemotifs, "seq", aacol)
    fakemotifs <- rename1col(fakemotifs, "Length", "mL")
    fakemotifs$case <- "Train"
    dfdt <- rbind(dfdt, fakemotifs)
    trainR <- union(trainR, grep("fake", dfdt$new_name, fixed = T))
  }
  dfdt$pssp <- DECIPHER::PredictHEC(AAStringSet(unlist(dfdt[, ..aacol])))
  pssp <- Biostrings::BStringSet(dfdt$pssp)
  names(pssp) <- dfdt$name_seq
  psspdfdt <- XString2KmerDT(
    infaa = pssp,
    abcde = c("H", "E", "C"),
    k = K.HEC,
    out_probablity = HEC_Prob,
    input_was = "name_seq"
  )
  colnames(psspdfdt) <- tolower(colnames(psspdfdt))
  AAset <- Biostrings::BStringSet(AAStringSet(unlist(dfdt[, ..aacol])))
  names(AAset) <- dfdt$name_seq
  AAdfdt <- XString2KmerDT(
    infaa = AAset,
    abcde = setdiff(Biostrings::AA_ALPHABET, c(".", "+", "-", "*")),
    k = K.aa,
    out_probablity = AA_Prob,
    input_was = "name_seq"
  )
  dfdt <- distinct(merge(
    merge(
      dfdt,
      AAdfdt,
      by = "name_seq",
      all.x = T,
      all.y = F
    ),
    psspdfdt,
    by = "name_seq",
    all.x = T,
    all.y = F
  ))
  names(dfdt) <- trimws(names(dfdt)) # Legacy
  if (RemovesSparseCols == T) {
    dfdt <- setDT(RemovesSparseCols(as.data.frame(dfdt)))
  }
  vals <- unique(unlist(dfdt[, ..valuecol]))
  dfdt[, as.character(c(vals))] <- 0
  for (val in vals) {
    set(dfdt,
      i = which((dfdt[, ..valuecol] == val)),
      j = val,
      value = 1
    )
  }
  gc()
  return(dfdt)
}
