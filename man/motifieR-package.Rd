\name{motifieR-package}
\alias{motifieR-package}
\docType{package}
\title{motifieR}
\description{
Classes and functions for creating, manipulating and using position specific scoring matrix models of sequence motifs.
}
\details{
my.motif<-read.jaspar("MA0171.1")
}
\author{
Caroline Johnston
}
\references{
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
my.motif <- read.jaspar("MA0171.1")
pfm <- pfm(my.motif)
ppm <- ppm(my.motif)
pwm <- pwm(my.motif)
ic <- ic(my.motif)
plot(my.motif, ic.scale=T)
}
