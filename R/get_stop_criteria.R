#' Print available stop criteria
#'
#' Prints the stop criteria available in the MOEADr package
#'
#' This routine prints the names of the stop criteria available in
#' the MOEADr package, to be used as the \code{stopcrit[[i]]$name} parameter
#' in the \code{moead(...)} call. Instructions for obtaining more info on each
#' criterion are also returned.
#'
#' @return Formatted data frame containing reference name (for
#' \code{stopcrit[[i]]$name}) and instructions for More Info about each
#' criterion.
#'
#' @export

get_stop_criteria <- function(){

  # Get only functions with "stop_" in the name (except for
  # "get_stop_criteria"
  stop.list <- ls("package:MOEADr")
  stop.list <- stop.list[grep(pattern = "stop_", stop.list)]
  stop.list <- stop.list[-which(stop.list == "get_stop_criteria")]
  stop.list <- stop.list[-which(stop.list == "check_stop_criteria")]
  stop.list.name <- gsub("stop_", "", stop.list)
  output <- data.frame(name = stop.list.name,
                       `More Info` = paste0("'?", stop.list, "'"))
  name.width    <- max(sapply(as.character(output$More.Info), nchar))
  names(output) <- format(names(output),
                          width = name.width,
                          justify = "centre")
  format(output, width = name.width, justify = "centre")
}
