\name{motifMatrix-class}
\Rdversion{1.1}
\docType{class}
\alias{motifMatrix-class}
\alias{alphabet,motifMatrix-method}
\alias{background,motifMatrix-method}
\alias{bg,motifMatrix-method}
\alias{data,motifMatrix-method}
\alias{initialize,motifMatrix-method}
\alias{pseudocount,motifMatrix-method}

\title{Class "motifMatrix"}
\description{
A virtual class describing a generic motif matrix. Actual motif classes pfm, ppm and pwm inherit from this one.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{alphabet}:}{Object of class \code{"character"} ~~ }
    \item{\code{background}:}{Object of class \code{"numeric"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{alphabet}{\code{signature(.Object = "motifMatrix")}: ... }
    \item{background}{\code{signature(.Object = "motifMatrix")}: ... }
    \item{bg}{\code{signature(.Object = "motifMatrix")}: ... }
    \item{data}{\code{signature(.Object = "motifMatrix")}: ... }
    \item{initialize}{\code{signature(.Object = "motifMatrix")}: ... }
    \item{pseudocount}{\code{signature(.Object = "motifMatrix")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
Caroline Johnston
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("motifMatrix")
}
\keyword{classes}
