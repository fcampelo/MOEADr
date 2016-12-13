#' Print available decomposition methods
#'
#' Prints the decomposition methods available in the MOEADr package
#'
#' This routine prints the names of the decomposition methods available in
#' the MOEADr package, to be used as the \code{decomp$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{decomp$name}) and instructions for More Info about each method.
#'
#' @export

get_decomposition_methods <- function(){

  # Get only functions with "decomposition_" in the name
  decomp.list <- ls("package:MOEADr")
  decomp.list <- decomp.list[grep(pattern = "decomposition_", decomp.list)]
  decomp.list <- decomp.list[-which(decomp.list == "get_decomposition_methods")]
  decomp.list.name <- gsub("decomposition_", "", decomp.list)
  output <- data.frame(name = decomp.list.name,
                       `More Info` = paste0("'?", decomp.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
