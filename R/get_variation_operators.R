#' Print available variation operators
#'
#' Prints the variation operators available in the MOEADr package
#'
#' This routine prints the names of the variation operators available in
#' the MOEADr package, to be used as the \code{variation$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{variation$name}) and instructions for More Info about each operator.
#'
#' @export

get_variation_operators <- function(){

  # Get only functions with "variation_" in the name (except for
  # "get_variation_operators"
  var.list <- ls("package:MOEADr")
  var.list <- var.list[grep(pattern = "variation_", var.list)]
  var.list <- var.list[-which(var.list == "get_variation_operators")]
  var.list.name <- gsub("variation_", "", var.list)
  output <- data.frame(name = var.list.name,
                       `More Info` = paste0("'?", var.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
