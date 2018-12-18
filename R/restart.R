restart.moead <-
  function(algo_conf,
           num.iter.run,
           seed,
           number.instances,
           num.iter.bet,
           algo,
           randomPar = FALSE) {

    bets.stopcrit  <- list(list(name    = "maxiter",
                                maxiter = num.iter.bet))
    best.hv <- -Inf
    best.seed <- NULL
    run.population <- NULL
    set.seed(seed)
    bets.data <- list()
    my.seed <- sample(1:10000, number.instances)
    for (i in 1:number.instances) {
      if (isTRUE(randomPar)) {
        algo_conf <- randomParSelection(algo_conf, algo)
      }
      algo_conf$stopcrit <- bets.stopcrit
      algo_conf$seed <- my.seed[[i]]
      bet <- do.call(moead,  algo_conf)

      bets.data[[length(bets.data)+1]] <- bet
      num.iter.run <- num.iter.run - bet$n.iter
    }
    rf.app.all <- bets.data[[1]]$Y
    for (bet in bets.data[2:length(bets.data)]){
      rf.app.all <- rbind(rf.app.all, bet$Y)
    }
    temp.max<- apply(rf.app.all, 2, max)
    temp.max <- unname(temp.max)
    temp.min <- apply(rf.app.all, 2, min)
    temp.min <- unname(temp.min)

    ref.points <- rep(round(1 + 1 / algo_conf$decomp$H, 3), algo_conf$problem$m)
    for (bet in bets.data){
      for (i in length(bet$n.problems)){
        bet$Y[,i]<-scale_vector(Y = bet$Y[,i],temp.max[i],temp.min[i])
      }
      hv <-
        emoa::dominated_hypervolume(points = t(bet$Y),
                                    ref = ref.points)

      if (hv > best.hv) {
        best.hv <- hv
        best.seed <- my.seed[[i]]
        run.population <- bet$X
      }
    }
    stopcrit  <- list(list(name    = "maxiter",
                           maxiter = num.iter.run))
    algo_conf$stopcrit <- stopcrit
    algo_conf$seed <- best.seed
    algo_conf$population <- run.population
    run <- do.call(moead,  algo_conf)
    return(run)
  }

restart.moead.diff.pop <-
  function(algo_conf,
           num.iter.run,
           seed,
           number.instances,
           num.iter.bet,
           algo,
           randomPar = FALSE) {
    bets.stopcrit  <-
      list(list(name    = "maxiter", maxiter = num.iter.bet))

    best.hv <- -Inf
    best.seed <- NULL
    run.population2 <- NULL
    set.seed(seed)
    bets.data <- list()
    algo_conf$stopcrit <- bets.stopcrit
    nfe <- 0
    temp <- algo_conf$preset$neighbors$T

    if (algo_conf$problem$m == 2){
      pop_size <- 200
      algo_conf$decomp$H <- length(algo_conf$problem$xmax)
      algo_conf$preset$neighbors$T <- sample(2:length(algo_conf$problem$xmax), 1)
    }
    else{
      algo_conf$decomp$H <- 13
      pop_size <- 210
    }
    my.seed <- sample(1:10000, number.instances)
    for (i in 1:number.instances) {
      if (isTRUE(randomPar)) {
        algo_conf <- randomParSelection(algo_conf, algo)
      }
      algo_conf$seed <- my.seed[[i]]
      bet <- do.call(moead,  algo_conf)
      bets.data[[length(bets.data)+1]] <- bet
      nfe <- nfe - bet$nfe
    }
    rf.app.all <- bets.data[[1]]$Y
    for (bet in bets.data[2:length(bets.data)]){
      rf.app.all <- rbind(rf.app.all, bet$Y)
    }
    pos.solutions <- bets.data[[1]]$X
    for (bet in bets.data[2:length(bets.data)]){
      pos.solutions <- rbind(pos.solutions, bet$X)
    }
    temp.max<- apply(rf.app.all, 2, max)
    temp.max <- unname(temp.max)
    temp.min <- apply(rf.app.all, 2, min)
    temp.min <- unname(temp.min)

    ref.points <- rep(round(1 + 1 / algo_conf$decomp$H, 3), algo_conf$problem$m)

    for (bet in bets.data){
      for (i in length(bet$n.problems)){
        bet$Y[,i]<-scale_vector(Y = bet$Y[,i],temp.max[i],temp.min[i])
      }
      hv <-
        emoa::dominated_hypervolume(points = t(bet$Y),
                                    ref = ref.points)
      if (hv > best.hv) {
        best.hv <- hv
        best.seed <- my.seed[[i]]
      }
    }
    if (algo_conf$problem$m == 2) {
      nfe <- 10200 + nfe
      num.iter.run <- ceiling((nfe - 200) / 200)
    }
    else{
      nfe <- 10710 + nfe
      num.iter.run <- ceiling((nfe - 210) / 210)
    }
    pos.solutions <- t(emoa::nondominated_points(t(pos.solutions)))
    if (nrow(bet$X)-nrow(pos.solutions)<0){
      aux <- abs(nrow(bet$X)-nrow(pos.solutions))
      pos.solutions <- pos.solutions[-sample(1:nrow(pos.solutions), aux), ]
    }
    run.population <-
      apply(
        X = pos.solutions,
        MARGIN = 2,
        FUN = function(x) {
          approx(x, n = pop_size, method = "constant")$y
        }
      )
    if (algo_conf$problem$m == 2)
      algo_conf$decomp$H <- 199
    if (algo_conf$problem$m == 3)
      algo_conf$decomp$H <- 19
    if (algo_conf$problem$m == 4) algo_conf$decomp$H <- 9

    algo_conf$preset$neighbors$T <- temp
    stopcrit  <- list(list(name    = "maxiter",
                           maxiter = num.iter.run))
    algo_conf$stopcrit <- stopcrit
    algo_conf$seed <- best.seed
    algo_conf$population <- run.population
    run <- do.call(moead,  algo_conf)
    return(run)
  }


randomParSelection <- function(algo_conf, algo) {
  if (algo == "original") {
    if (is.null(algo_conf$neighbors)) {
      algo_conf$neighbors <- algo_conf$preset$neighbors
      algo_conf$neighbors$T = sample(10:20, 1)
      algo_conf$neighbors$delta.p = runif(1)
    }
    else{
      algo_conf$neighbors$T = sample(10:20, 1)
      algo_conf$neighbors$delta.p = runif(1)
    }
    if (is.null(algo_conf$variation)) {
      algo_conf$variation <- algo_conf$preset$variation
      algo_conf$variation[[1]]$etax <- sample(1:100, 1)
      algo_conf$variation[[1]]$pc <- runif(1)

      algo_conf$variation[[2]]$etam <- sample(1:100, 1)
      algo_conf$variation[[2]]$pm <- runif(1)
    }
    else{
      algo_conf$variation <- algo_conf$preset$variation
      algo_conf$variation[[1]]$etax <- sample(1:100, 1)
      algo_conf$variation[[1]]$pc <- runif(1)

      algo_conf$variation[[2]]$etam <- sample(1:100, 1)
      algo_conf$variation[[2]]$pm <- runif(1)
    }
    algo_conf$UseArchive <- sample(0:1, 1)
    if (is.null(algo_conf$update)) {
      algo_conf$update <- algo_conf$preset$update
      algo_conf$update$nr <- sample(1:10, 1)
    }
    else{
      algo_conf$update$nr <- sample(1:10, 1)
    }
  }
  else{
    if (is.null(algo_conf$neighbors)) {
      algo_conf$neighbors <- algo_conf$preset$neighbors
      algo_conf$neighbors$T = sample(10:20, 1)
      algo_conf$neighbors$delta.p = runif(1)
    }
    else{
      algo_conf$neighbors$T = sample(10:20, 1)
      algo_conf$neighbors$delta.p = runif(1)
    }
    if (is.null(algo_conf$variation)) {
      algo_conf$variation <- algo_conf$preset$variation
      algo_conf$variation[[1]]$phi <- runif(1)

      algo_conf$variation[[2]]$etam <- sample(1:100, 1)
      algo_conf$variation[[2]]$pm <- runif(1)
    }
    else{
      algo_conf$variation <- algo_conf$preset$variation
      algo_conf$variation[[1]]$phi <- runif(1)

      algo_conf$variation[[2]]$etam <- sample(1:100, 1)
      algo_conf$variation[[2]]$pm <- runif(1)
    }
    algo_conf$UseArchive <- sample(0:1, 1)
    if (is.null(algo_conf$update)) {
      algo_conf$update <- algo_conf$preset$update
      algo_conf$update$nr <- sample(1:10, 1)
    }
    else{
      algo_conf$update$nr <- sample(1:10, 1)
    }
  }
  return (algo_conf)
}
