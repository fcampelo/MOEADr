#' Print available scalarization methods
#'
#' Prints the scalarization methods available in the MOEADr package
#'
#' This routine prints the names of the scalarization methods available in
#' the MOEADr package, to be used as the \code{aggfun$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{aggfun$name}) and instructions for More Info about each method.
#'
#' @export

get_scalarization_methods <- function(){

  # Get only functions with "scalarization_" in the name
  scal.list <- ls("package:MOEADr")
  scal.list <- scal.list[grep(pattern = "scalarization_", scal.list)]
  scal.list <- scal.list[-which(scal.list == "get_scalarization_methods")]
  scal.list.name <- gsub("scalarization_", "", scal.list)
  output <- data.frame(name = scal.list.name,
                       `More Info` = paste0("'?", scal.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
