redoPlateMap <- function(sample.data, n.orig.plates=24,
                         batch.size=48, batches.per.plate=2,
                         duplicates=duplicates, ...) {

    stopifnot(all(c("SampleID", "Orig.Plate") %in% names(sample.data)))

    dat <- sample.data
    pm.list <- list()
    i <- 1

    ## loop to assign samples with max n.orig.plates per new plate
    stuck <- 0
    while (nrow(dat) > 0) {
        
        ## select a group of n.orig.plates plates at random
        plates <- unique(dat$Orig.Plate)
        plates.sub <- sample(plates, min(n.orig.plates, length(plates)))
        
        message("Next plate: ", i)
        message("Samples left: ", nrow(dat))
        message("Original plates left: ", length(plates))
        message("Samples per original plate: ")
        print(table(table(dat$Orig.Plate)))
        
        ## run plate map code
        sample.strata <- dat[dat$Orig.Plate %in% plates.sub,]
        sample.strata$Orig.Plate <- NULL
        message("Trying to plate ", nrow(sample.strata), " samples...")

        ## if we don't have enough samples for a batch, try again
        if (stuck < 10) {
            if (nrow(dat) > batch.size & nrow(sample.strata) < batch.size) {
                stuck <- stuck+1; next
            }
        }
        stuck <- 0  

        nb <- batch.size # number of samples in batch
        np <- ceiling(nrow(sample.strata) / nb)
        plate.manifest <- data.frame(Plate=paste("Batch", rep(1:np, each=nb), sep=""),
                                     Well=paste("Well", rep(1:nb, np), sep=""),
                                     SampleID=rep("", np*nb), stringsAsFactors=FALSE)

        duplicates <- duplicates[dups[,1] %in% sample.strata$SampleID &
                                 dups[,2] %in% sample.strata$SampleID,]
        pm.tmp <- plateMap(sample.strata, plate.manifest, ...)

        ## keep the first two batches, assign to one plate
        ## unless we're stuck, then just keep one batch
        plate.size <- batch.size * batches.per.plate
        maxn <- ifelse(sum(pm.tmp$SampleID != "") < plate.size, batch.size, plate.size)
        pm.tmp <- pm.tmp[1:maxn,]
        names(pm.tmp)[1] <- "Batch"
        pm.tmp$Plate <- paste0("Plate", i)
        stopifnot(max(table(pm.tmp$Batch)) <= batch.size)
        stopifnot(max(table(pm.tmp$Plate)) <= plate.size)
        
        ## repeat until all samples are assigned
        dat <- dat[!(dat$SampleID %in% pm.tmp$SampleID),]
        pm.list[[i]] <- pm.tmp
        i <- i+1
        
    }
    pm <- do.call(rbind, pm.list)
    
    ## merge original data
    pm.fin <- merge(sample.data, pm)
    tbl <- table(pm.fin$Orig.Plate, pm.fin$Plate)
    message("Original plates per new plate: ")
    print(colSums(tbl > 0))

    message("Samples per new plate: ")
    print(table(pm.fin$Plate, pm.fin$Batch))

    return(pm.fin)
}
