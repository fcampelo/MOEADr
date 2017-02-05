#' Print available constraint methods
#'
#' Prints the constraint handling methods available in the MOEADr package
#'
#' This routine prints the names of the constraint handling methods available in
#' the MOEADr package, to be used as the \code{constraint$name} parameter in the
#' \code{moead(...)} call. Instructions for obtaining more info on each
#' operator are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{constraint$name}) and instructions for More Info about each method.
#'
#' @export

get_constraint_methods <- function(){

  # Get only functions with "constraint_" in the name
  con.list <- ls("package:MOEADr")
  con.list <- con.list[grep(pattern = "constraint_", con.list)]
  con.list <- con.list[-which(con.list == "get_constraint_methods")]
  con.list.name <- gsub("constraint_", "", con.list)
  output <- data.frame(name = con.list.name,
                       `More Info` = paste0("'?", con.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
