#' Print available local search methods
#'
#' Prints the local search methods available in the MOEADr package
#'
#' This routine prints the names of the local search methods available in
#' the MOEADr package, to be used as the \code{aggfun$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{variation$localsearch$type}) and instructions for More Info about
#' each method.
#'
#' @export

get_localsearch_methods <- function(){

  # Get only functions with "scalarization_" in the name
  ls.list <- ls("package:MOEADr")
  ls.list <- ls.list[grep(pattern = "ls_", ls.list)]
  ls.list.name <- gsub("ls_", "", ls.list)
  output <- data.frame(type = ls.list.name,
                       `More Info` = paste0("'?", ls.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
