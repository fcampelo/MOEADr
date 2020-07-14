#' savePlotData
#'
#' savePlotData
#'
#' save moead Data
#'
#' @param moead moead class output
#' @param name filename to save
#' @param wd which direction to save
#'
#' @return nothing
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export

savePlotData <- function (moea, name, j,wd = '~/dataExp/') {
  if (is.null(moea$Archive)) {
    write_feather(data.frame(moea$X), paste0(wd, name, j, '_X'))
    write_feather(data.frame(moea$Y), paste0(wd, name, j, '_Y'))
  }
  else{
    write_feather(data.frame(moea$Archive$X),
                  paste0(wd, name, j, '_X'))
    write_feather(data.frame(moea$Archive$Y),
                  paste0(wd, name, j, '_Y'))
  }


  # temp <- moea$plot.paretofront[-1,]
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_plot.paretofront'))
  #
  # temp <- as.data.frame(moea$plot.resources[-1,])
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_plot.resources'))

  temp <- as.data.frame(moea$plot.paretoset[-1, ])
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/dataExp/', name, j, '_plot.paretoset'))

  # temp <- as.data.frame(moea$W)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_W'))
  #
  # temp <- as.data.frame(moea$usage)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_usage'))

  temp <- as.data.frame(moea$nfe)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_nfe'))

  temp <- as.data.frame(moea$n.iter)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_iter'))

  # temp <- as.data.frame(moea$time)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_time'))

}

