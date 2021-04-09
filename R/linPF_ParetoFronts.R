library(plotly)

gerate_graph <- function(pos) {
  aux <- as.data.frame(pos)
  
  p <-
    plot_ly(
      aux,
      x = aux$f1,
      y = aux$f2,
      z = aux$f3,
      color = aux$f1,
      colors =  c('#4AC6B7', '#1972A4'),
      marker = list(size = 4, sizeref = 2)
    ) %>%
    add_markers() %>%
    layout(scene = list(
      xaxis = list(title = 'X'),
      yaxis = list(title = 'Y'),
      zaxis = list(title = 'Z')
    ))
  p
}


linPF1 <- function() {
  x <-
    y <-
    t(matrix(rep(seq(
      from = 0, to = 1, by = 0.01
    ), 101), nrow = 101))
  
  y <- t(2 * pi * y)
  
  f3 <- (0.3333333 + x * cos(y) - x * sin(y)) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}

linPF2 <- function() {
  x <-
    t(matrix(rep(seq(
      from = 0.8,
      to = 1,
      length.out = 101
    ), 101), nrow = 101))
  y <-
    (matrix(rep(seq(
      from = 0,
      to = 1,
      length.out = 101
    ), 101), nrow = 101))
  
  y <- 2 * pi * y
  
  f3 <- 0.3333333 + x * cos(y) - x * sin(y) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}

linPF3 <- function() {
  x <-
    y <-
    (matrix(rep(seq(
      from = 0,
      to = 1,
      length.out = 22
    ), 22), nrow = 22))
  
  y <- y*pi/2+2*pi
  x <- t(0.5*(x+1))
  
  f3 <- (0.3333333 + x * cos(y) - x * sin(y)) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}

linPF4 <- function() {
  x <-
    y <-
    (matrix(rep(seq(
      from = 0,
      to = 1,
      length.out = 101
    ), 101), nrow = 101))
  
  y=y*pi/2+pi/4
  x=0.2*x+0.8
  
  f3 <- (0.3333333 + x * cos(y) - x * sin(y^2)) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y^2) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}

linPF5 <- function() {
  x <- t(matrix(rep(c(seq(
    from = 0,
    to = 0.25,
    by = 0.01
  ),(seq(
    from = 0.75,
    to = 1,
    by = 0.01
  ))),101), ncol = 101))
  
  y <-
    (matrix(rep(seq(
      from = 0,
      to = 1,
      length.out = 101
    ), 52), nrow = 101))
  
  
  y <- 2*y*pi
  
  f3 <- (0.3333333 + x * cos(y) - x * sin(y)) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}

linPF6 <- function() {
  x <- t(matrix(t(rep(c(seq(
    from = 0,
    to = 0.5,
    by = 0.01
  ),1), 51)), nrow = 52))
  
  y <- matrix(rep(seq(
    from = 0,
    to = 1,
    by = 0.02
  ), 52), ncol = 52)
  
  
  y <- 2*y*pi
  
  f3 <- (0.3333333 + x * cos(y) - x * sin(y)) + 3
  f2 <- 0.3333333 + x * cos(y) + x * sin(y) + 2
  f1 <- 0.3333333 - x * cos(y) + 1
  Y <- matrix(c(f1, f2, f3), ncol = 3)
  colnames(Y) <- c("f1", "f2", "f3")
  
  gerate_graph(Y)
  
}



linPF1()
linPF2()
linPF3()
linPF4()
linPF5()
linPF6()

