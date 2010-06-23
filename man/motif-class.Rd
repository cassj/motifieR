\name{motif-class}
\Rdversion{1.1}
\docType{class}
\alias{motif-class}
\alias{alphabet,motif-method}
\alias{.ic<-,motif,numeric-method}
\alias{ic,motif-method}
\alias{initialize,motif-method}
\alias{max.ic,motif-method}
\alias{pfm,motif-method}
\alias{.ppm<-,motif,ppm-method}
\alias{ppm,motif-method}
\alias{.pwm<-,motif,pwm-method}
\alias{pwm,motif-method}

\title{Class "motif"}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("motif", pfm, ppm)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{pfm}:}{Object of class \code{"pfmOrNULL"} ~~ }
    \item{\code{ppm}:}{Object of class \code{"ppmOrNULL"} ~~ }
    \item{\code{pwm}:}{Object of class \code{"pwmOrNULL"} ~~ }
    \item{\code{ic}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{alphabet}:}{Object of class \code{"characterOrNULL"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{alphabet}{\code{signature(.Object = "motif")}: ... }
    \item{.ic<-}{\code{signature(.Object = "motif", value = "numeric")}: ... }
    \item{ic}{\code{signature(.Object = "motif")}: ... }
    \item{initialize}{\code{signature(.Object = "motif")}: ... }
    \item{max.ic}{\code{signature(.Object = "motif")}: ... }
    \item{pfm}{\code{signature(.Object = "motif")}: ... }
    \item{.ppm<-}{\code{signature(.Object = "motif", value = "ppm")}: ... }
    \item{ppm}{\code{signature(.Object = "motif")}: ... }
    \item{.pwm<-}{\code{signature(.Object = "motif", value = "pwm")}: ... }
    \item{pwm}{\code{signature(.Object = "motif")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
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
showClass("motif")
}
\keyword{classes}
