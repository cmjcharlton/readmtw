#' Print "MTWorksheet" objects
#' @param x an "MTWorksheet" object
#' @param ... other parameters
#' @seealso \code{\link[base]{print}}
#' @method print MTWorksheet
#' @export
print.MTWorksheet <- function(x, ...) {
  wstitle <- attr(x, "title")
  wsauthor <- attr(x, "author")
  wsdate <- attr(x, "date")
  wsdescription <- attr(x, "description")
  
  cat("\n")
  if (!is.null(wstitle)) {
    cat(paste0("Title: ", wstitle, "\n"))
  }
  if (!is.null(wsauthor)) {
    cat(paste0("Author: ", wsauthor, "\n"))
  }
  if (!is.null(wsdate)) {
    cat(paste0("Date: ", wsdate, "\n"))
  }
  if (!is.null(wsdescription)) {
    cat(wsdescription)
  }
  cat("\n")
  if (length(x$constants) > 0) {
    cat("Constants:\n")
    print(data.frame(x$constants))
    cat("\n")
  }
  if (length(x$matrices) > 0) {
    cat("Matrices:\n")
    for (mat in names(x$matrices)) {
      label <- attr(x$matrices[[mat]], "label")
      cat(mat)
      if (!is.null(label)) {
        cat(paste0(" (", label, ")"))
      }
      cat(":\n")
      print(x$matrices[[mat]])
    }
    cat("\n")
  }
  if (length(x$columns) > 0) {
    cat("Columns:\n")
    for (col in names(x$columns)) {
      label <- attr(x$columns[[col]], "label")
      cat(col)
      if (!is.null(label)) {
        cat(paste0(" (", label, ")"))
      }
      cat(":\n")
      print(x$columns[[col]])
    }
    cat("\n")
  }
}

#' Summarize "MTWorksheet" objects
#' @param object an "MTWorksheet" object
#' @param ... other parameters
#' @method summary MTWorksheet
#' @return an object of class "summary.MTWorksheet", which is a list containing
#' summary statistics for each of the constants, matrices and columns present.
#' @export
summary.MTWorksheet <- function(object, ...) {
  result <- list(constants = object$constants,
                 columns = sapply(object$columns, summary, simplify = FALSE),
                 matrices = object$matrices)
  
  attr(result, "title") <- attr(object, "title")
  attr(result, "author") <- attr(object, "author")
  attr(result, "date") <- attr(object, "date")
  attr(result, "description") <- attr(object, "description")

  return (structure(result, class = "summary.MTWorksheet"))
}

#' Print "summary.MTWorksheet" objects
#' @param x a "summary.MTWorksheet" object
#' @param ... other parameters
#' @seealso \code{\link[base]{print}}
#' @method print summary.MTWorksheet
#' @export
print.summary.MTWorksheet <- function(x, ...) {
  wstitle <- attr(x, "title")
  wsauthor <- attr(x, "author")
  wsdate <- attr(x, "date")
  wsdescription <- attr(x, "description")
  
  cat("\n")
  if (!is.null(wstitle)) {
    cat(paste0("Title: ", wstitle, "\n"))
  }
  if (!is.null(wsauthor)) {
    cat(paste0("Author: ", wsauthor, "\n"))
  }
  if (!is.null(wsdate)) {
    cat(paste0("Date: ", wsdate, "\n"))
  }
  if (!is.null(wsdescription)) {
    cat(wsdescription)
  }
  cat("\n")
  if (length(x$constants) > 0) {
    cat("Constants:\n")
    print(data.frame(x$constants))
    cat("\n")
  }
  if (length(x$matrices) > 0) {
    cat("Matrices:\n")
    for (mat in names(x$matrices)) {
      label <- attr(x$matrices[[mat]], "label")
      cat(mat)
      if (!is.null(label)) {
        cat(paste0(" (", label, ")"))
      }
      cat(":\n")
      print(x$matrices[[mat]])
    }
    cat("\n")
  }
  if (length(x$columns) > 0) {
    cat("Columns:\n")
    for (col in names(x$columns)) {
      label <- attr(x$columns[[col]], "label")
      cat(col)
      if (!is.null(label)) {
        cat(paste0(" (", label, ")"))
      }
      cat(":\n")
      print(x$columns[[col]])
    }
    cat("\n")
  }
}

#' Convert the columns held in an "MTWorksheet" object into a data.frame
#' @param x the "MTWorksheet" object to be converted
#' @param  ... parameters to be passed to as.data.frame() function
#' @return a \code{\link{data.frame}} object representing the columns stored
#' in the "MTWorksheet" object passed in.
#' @export
as.data.frame.MTWorksheet <- function(x, ...) {
  lens <- sapply(x$columns, length)
  if (length(lens) > 0) {
    if (all(lens == lens[1])) {
      return(as.data.frame(x$columns, ...))
    }
  } else {
    warning("Cannot convert to data.frame, columns have unequal lengths")
    return(NULL)
  }
}
