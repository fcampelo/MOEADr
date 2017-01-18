#' Print available repair operators
#'
#' Prints the repair operators available in the MOEADr package
#'
#' This routine prints the names of the repair operators available in
#' the MOEADr package, to be used as the \code{repair$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{repair$name}) and instructions for More Info about each operator.
#'
#' @export

get_repair_operators <- function(){

  # Get only functions with "repair_" in the name (except for
  # "get_repair_operators"
  var.list <- ls("package:MOEADr")
  var.list <- var.list[grep(pattern = "repair_", var.list)]
  var.list <- var.list[-which(var.list == "get_repair_operators")]
  var.list.name <- gsub("repair_", "", var.list)
  output <- data.frame(name = var.list.name,
                       `More Info` = paste0("'?", var.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
