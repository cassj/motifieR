require(methods)
require(RUnit)
require(Biostrings)

#if not installed, load the packages
if(!require(motifieR)){
  source("R/generics.R")
  source("R/seqGenerator.R")
  source("R/motifMatrix.R")
  source("R/pfm.R")
  source("R/ppm.R")
  source("R/pwm.R")
  source("R/motif.R")
  source("R/JASPAR.R")
}

testsuite.motifieR<-defineTestSuite("motifieR", 
                                    dirs=file.path("./tests"),
                                    testFileRegexp="^runit.+\\.R$",
                                    testFuncRegexp="^test.+",
                                    rngKind = "Marsaglia-Multicarry",
                                    rngNormalKind = "Kinderman-Ramage"
                            )
testResult <- runTestSuite(testsuite.motifieR)
printTextProtocol(testResult)

