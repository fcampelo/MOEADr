evaluate_population <- function(X,
                                probpars)
{
  # ========== Error catching and default value definitions

  # ==========

  # Denormalize population
  Pop <- denormalize_population(probpars, X)

  # Prepare arguments for function call
  fun.args <- as.list(formals(probpars$name))

  my.args  <- sapply(names(fun.args),
                     function(argname, pars, args){
                       if(argname %in% names(pars)) {
                         args[argname] <- pars[argname]
                         }
                       return(args[[argname]])},
                     probpars,
                     fun.args,
                     simplify = FALSE)

  my.args[[grep("[x|X]",
                names(my.args))]] <- X

  Y <- do.call(probpars$name,
               args = my.args)

  return(Y)
}
