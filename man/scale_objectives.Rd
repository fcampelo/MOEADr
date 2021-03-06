% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_objectives.R
\name{scale_objectives}
\alias{scale_objectives}
\title{Scaling of the objective function values}
\usage{
scale_objectives(Y, Yt, scaling, eps = 1e-16, ...)
}
\arguments{
\item{Y}{matrix of objective function values for the incumbent solutions}

\item{Yt}{matrix of objective function values for the candidate solutions}

\item{scaling}{list containing the scaling parameters (see \code{\link[=moead]{moead()}} for
details).}

\item{eps}{tolerance value for avoiding divisions by zero.}

\item{...}{other parameters (included for compatibility with generic call)}
}
\value{
List object containing scaled objective function value matrices
\code{Y} and \code{Yt}, as well as estimates of the "ideal" point \verb{minP`` and "nadir" point }maxP`.
}
\description{
Performs scaling of the objective function values for the MOEADr package
}
\details{
This routine scales the matrices of objective function values for the
current (\code{Yt}) and candidate (\code{Y}) solutions. The
following methods are currently available:

\itemize{
\item \code{scaling$name = "none"}: no scaling
\item \code{scaling$name = "simple"}: simple linear scaling between
estimated ideal and nadir points, calculated from the available points in
\code{Y} and \code{Yt} at each iteration.
}
}
\section{References}{

F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
}

