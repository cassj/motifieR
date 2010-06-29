library(RUnit)
testsuite.motifieR<-defineTestSuite("motifieR", 
                                    dirs=file.path("./tests"),
                                    testFileRegexp="^runit.+\\.R$",
                                    testFuncRegexp="^test.+",
                                    rngKind = "Marsaglia-Multicarry",
                                    rngNormalKind = "Kinderman-Ramage"
                            )
testResult <- runTestSuite(testsuite.motifieR)
printTextProtocol(testResult)

