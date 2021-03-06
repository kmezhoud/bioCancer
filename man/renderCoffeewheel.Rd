% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coffeewheel.R
\name{renderCoffeewheel}
\alias{renderCoffeewheel}
\title{Widget render function for use in Shiny}
\usage{
renderCoffeewheel(expr,env =  parent.frame(),  quoted = FALSE)
}
\arguments{
\item{expr}{id}

\item{env}{parent.frame()}

\item{quoted}{FALSE}
}
\value{
A circular layout with genetic profile in Shiny App.
}
\description{
Widget render function for use in Shiny
}
\examples{
How <- "runManually"
\dontrun{
coffeewheel(treeData = sampleWheelData)
}
}
