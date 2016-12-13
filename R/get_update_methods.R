#' Print available update methods
#'
#' Prints the update methods available in the MOEADr package
#'
#' This routine prints the names of the update methods available in
#' the MOEADr package, to be used as the \code{update$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{update$name}) and instructions for More Info about each method.
#'
#' @export

get_update_methods <- function(){

  # Get only functions with "updt_" in the name
  updt.list <- ls("package:MOEADr")
  updt.list <- updt.list[grep(pattern = "updt_", updt.list)]
  updt.list.name <- gsub("updt_", "", updt.list)
  output <- data.frame(name = updt.list.name,
                       `More Info` = paste0("'?", updt.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
