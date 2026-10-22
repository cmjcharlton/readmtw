#' Read .mtw format files
#'
#' \pkg{readMTW} is an R command to allow reading data from the historic .mtw file format
#'
#' This package reads constants, matrices and columns from files saved in the .mtw
#' format that was used in Minitab prior to release 20. 
#'
#' Formats corresponding to release 5 to 19 should be handled, with the exception of 
#' string formats for release 11 where values were first stored in double-precision.
#' 
#' Subsequent versions have switched to the .mwx format, which is not supported by 
#' this package. This consists of JSON formatted files stored in a zip container.
#' 
#' Support for structured storage is provided by the POLE library (BSD license)
#' Support for decompressing the ARJ format is based on code from the pyembroidery 
#' project (MIT license)
#' Support for conversion of UTF-16 strings to UTF-8 is provided by the utfcpp
#' library (Boost Software License 1.0)
#' 
#' Disclaimer:
#' The details of the format were mostly determined through the examination of
#' sample worksheets freely available on the Internet. This is therefore possible
#' that variants exist that have not been examined and therefore are not correctly
#' handled. Please take care to check any data after loading it.
#'
#'
#' @importFrom Rcpp evalCpp
#' @importFrom utils download.file
#' @useDynLib readmtw, .registration = TRUE
#'
#' @examples
#' \dontrun{
#' library(readmtw)
#' 
#' archiveurl <- "https://web.archive.org/web/20140913043122/"
#' sampleurl <- "https://support.minitab.com/en-us/datasets/"
#' paint <- readMTW(paste0(archiveurl, sampleurl, "PaintHardness.MTW"))
#' 
#' summary(paint)
#' paintdf <- as.data.frame(paint)
#' summary(paintdf)
#' 
#' download.file(
#'   paste0(archiveurl, sampleurl, "PercentMoisture.MTW"), 
#'   "PercentMoisture.MTW", 
#'   mode = "wb"
#' )
#' moisture <- readMTW("PercentMoisture.MTW")
#' summary(moisture)
#' moisturedf <- as.data.frame(moisture)
#' summary(moisturedf)
#' }
#' 
#' @section Maintainer:
#' Chris Charlton \email{c.charlton@@bristol.ac.uk}
#'
#' @author Charlton, C.M.J., (2026) Centre for Multilevel Modelling, University of Bristol.
#' 
#'
"_PACKAGE"