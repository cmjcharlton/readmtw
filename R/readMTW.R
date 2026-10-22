#' Reads data from the historic .mtw file format.
#' @param filename path or url of file to open
#' @param convert_names logical - indicates whether to name columns by their label
#' if defined. Defaults to TRUE
#' @return an object of class "MTWorksheet". This can be converted 
#' into a data.frame with the \code{\link{as.data.frame}} function, provided that 
#' all the columns are the same length.
#' @export
readMTW <- function(filename, convert_names = TRUE) {
  if (isFALSE(file.exists(filename))) {
    # If the file is not local then it may be a URL, attempt to download it if so
    tmpname <- tempfile(fileext=".mtw")
    download.file(filename, tmpname, quiet = TRUE, mode = "wb")
    if (file.exists(tmpname)) {
      # Check that the file was downloaded, and if so point to this
      filename <- tmpname
      on.exit(unlink(tmpname))
    }
  }

  result <- rcpp_readmtw(filename)
  
  if (isTRUE(convert_names)) {
    constnames <- names(result$constants)
    for (i in seq_along(constnames)) {
      id <- constnames[i]
      name <- attr(result$constants[[id]], "name")
      if (!is.null(name)) {
        constnames[i] <- name
      }
    }
    names(result$constants) <- constnames
    
    columnnames <- names(result$columns)
    for (i in seq_along(columnnames)) {
      id <- columnnames[i]
      name <- attr(result$columns[[id]], "name")
      if (!is.null(name)) {
        columnnames[i] <- name
      }
    }
    names(result$columns) <- columnnames
    
    matrixnames <- names(result$matrices)
    for (i in seq_along(matrixnames)) {
      id <- matrixnames[i]
      name <- attr(result$matrices[[id]], "name")
      if (!is.null(name)) {
        matrixnames[i] <- name
      }
    }
    names(result$matrices) <- matrixnames
  }
  invisible(result)
}