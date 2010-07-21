checkTrue(require(methods))

#test the virtual class motifMatrix 
test.JASPAR<-function(){
  className="JASPAR"
  this <- new(className)
  checkTrue(is(this, className))

  #get by ID
  mo <- getMatrixById(this,"MA0138.2")
  checkTrue(is(this, className))

  #check it's setting stuff ok:
  checkEquals(motif.identifier(mo),c(ID="MA0138.2"))
  checkEquals(motif.name(mo),c(Name="REST"))

  mo <- getMatrixByName(this,"REST")
  checkTrue(is(this, className))
  checkEquals(motif.identifier(mo),c(ID="MA0138.2"))

}
