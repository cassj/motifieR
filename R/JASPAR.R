#this should probably use the JASPAR SOAP API, but I can't get it to work


#Class to search, fetch, parse JASPAR motifs
setClass("JASPAR",
         representation=representation(
           .url = "character",
           .metadata.url="character",
           .metadata="data.frame"
         ))
         

setMethod("initialize","JASPAR", function(.Object){
            .Object@.url <-"http://jaspar.genereg.net/html/DOWNLOAD/all_data/FlatFileDir" 
            .Object@.metadata.url<-"http://jaspar.genereg.net/html/DOWNLOAD/all_data/FlatFileDir/matrix_list.txt"
            metadata<-read.table(.Object@.metadata.url, as.is=T, sep="\t")
             
            message("Loading JASPAR data, please wait a moment...")            

            #this is horrifically slow, fix.
            colnames(metadata)<-c("ID", "value","Name", "Family", "More")
            more<-strsplit(metadata[,"More"], ";")          
            acc<-collection<-comment<-family<-medline<-pazar_tf_id<-species<-tax_group<-type<-character(length(more))
            metadata<-metadata[,-1*(which(colnames(metadata)=="More"))]
            metadata<-data.frame(metadata, acc, collection, comment, family, medline, pazar_tf_id, species, tax_group, type, stringsAsFactors=F)            
            for(i in 1:length(more)){
              these <- more[[i]]
              metadata[i,"acc"]<-sub("acc ","",these[2])
              metadata[i,"collection"]<-sub("collection","",these[3])
              metadata[i,"comment"]<-sub("comment ","",these[4])
              metadata[i,"family"]<-sub("family ","",these[5])
              metadata[i,"medline"]<-sub("medline ","",these[6])
              metadata[i,"pazar_tf_id"]<-sub("pazar_tf_id ","",these[7])
              metadata[i,"species"]<-sub("species ","",these[8])
              metadata[i,"tax_group"]<-sub("tax_group ","",these[9])
              metadata[i,"type"]<-sub("type ","",these[10])
            }
            rownames(metadata)<-metadata[,"ID"]
            .Object@.metadata<-metadata
            .Object
 })

#note to self - method defs can have default expressions for args,
#but the generic has to have some default for the same ard
#in order for the default to be used.

#getMatrixByName, getAllMatrices, searchByTag


prepare.meta <- function(meta){
     nms <- colnames(meta)
     meta <- as.character(meta)
     names(meta) <- gsub('\\s*',"",nms)
     meta <- gsub('\\s*',"",meta)
     meta <- c(meta, date=date())
     meta
}

setGeneric("getMatrixById", function(.Object, ID, pseudocount=0) standardGeneric("getMatrixById"))
setMethod("getMatrixById",
          signature=signature("JASPAR"),
          function(.Object, ID, pseudocount=0)  {
             file <- paste(paste(.Object@.url, ID, sep="/", collapse=""), ".pfm", sep="", collapse="")
             pfm <- as.matrix(read.table(file))
   	     rownames(pfm) <- c('A', 'C', 'G', 'T')
             pfm <- new("pfm",motif.data=pfm, pseudocount=pseudocount)
             meta =.Object@.metadata[which(.Object@.metadata$ID==ID),]
             meta <- prepare.meta(meta)             
             mo <- new("motif",pfm=pfm, motif.identifier=meta["ID"], motif.name=meta["Name"], motif.source=paste("JASPAR"), motif.notes=meta)
          })


setGeneric("getMatrixByName", function(.Object, Name, pseudocount=0) standardGeneric("getMatrixByName"))
setMethod("getMatrixByName",
          signature=signature("JASPAR"),
          function(.Object, Name, pseudocount=0) {
             ID<-.Object@.metadata[which(.Object@.metadata[,"Name"]==name),"ID"]
             getMatrixById(.Object, ID=ID, pseudocount=pseudocount)
})

setGeneric("getAllMatrices", function(.Object, Database='CORE', pseudocount=0) standardGeneric("getAllMatrices"))
setMethod("getAllMatrices", 
          signature=signature("JASPAR"),
          function(.Object, Database, pseudocount=0){
             databases<-c('CORE', 'FAM', 'CNE', 'SPLICE')
             if(! Database %in% databases){
                 stop('Database should be one of "CORE", "FAM", "CNE", "SPLICE"')
             }
             inds <- grep(Database, .Object@.metadata[,"collection"])
             ids<-.Object@.metadata[inds, "ID"]
             matrices <- lapply(ids, function(x){getMatrixById(.Object, ID=x, pseudocount=pseudocount)})
             names(matrices)<-ids
             
})

setGeneric("getMetadataFromID", function(.Object, ID) standardGeneric("getMetadataFromID"))
setMethod("getMetadataFromID",
          signature=signature("JASPAR"),
          function(.Object, ID){
             .Object@.metadata[ID,] 
})


