% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulator.R
\name{simulator}
\alias{simulator}
\title{Robustness Simulation Container}
\usage{
simulator(
  graph,
  .name,
  nchunks = 5,
  diam = NULL,
  unconn_dist = NULL,
  seed_rand = 1,
  vrb = 1,
  from_scratch = FALSE,
  trim_to_largest_component = TRUE
)
}
\arguments{
\item{.name}{\code{(character)} : a unique identifier (string) for \code{rb_state} methods.}

\item{diam}{\code{(integer)}   : diameter of \code{g} if pre-calculated.  Pre-calculation is highly recommended, as \code{rb_graph} objects are instantiated fairly often in this package, and diameter calculation for connected graphs is \code{O(E)}.}

\item{unconn_dist}{\code{(integer)}   : if two vertices are unconnected, \code{igraph::distances()} will return \code{INF} by default.  To help with averages (instead of ignoring \code{INF} values or having an \code{INF} average), \code{unconn_dist} can be assigned a value to replace \code{INF}. The default value is the graph's \code{diameter + 1}.}

\item{seed_rand}{\code{(integer)}  : random number generator seed passed to \code{base::set.seed()}.}

\item{vrb}{\code{(integer)}  : level of verbosity for printing progress bars, etc.}

\item{trim_to_largest_component}{}
}
\description{
A container for performing robustness analysis on a graph while preserving the original.
}
