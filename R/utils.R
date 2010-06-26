####
# Utility Functions
####



#this should generate a motif
#read transfac formatted pfms (jaspar are same format)
read.transfac <- read.jaspar <- function(file, pseudocount=0){
  pfm <- as.matrix(read.table(file))
  rownames(pfm) <- c('A', 'C', 'G', 'T')
  pfm <- new("pfm",data=pfm, pseudocount=pseudocount)
  return(new("motif",pfm=pfm))
}

  

#calculate background frequencies from an XStringSet
make.bg <- function(sequences){

  ab <- c("A","C","G","T")

  tot <- sum(nchar(sequences))
  B <- lapply(ab, function(x){
    sum(vcountPattern(x,sequences))/tot
  })
  names(B) <- ab
  return(B)
}



