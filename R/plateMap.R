 
##
# function to return possible plates for maximum plate balance
# possTypes - possible plate types
# plates - vector of plates to consider
# type - find plates with the minimum ratio of this type
# current - current map with columns "Plate," "type"
.platesForType <- function(possTypes, plates, type, current) {
  currentTypes <- intersect(c(possTypes, 0), unique(current$type))
  if (type %in% currentTypes) {
    platebalance <- table(current$Plate, current$type)[plates, currentTypes]
    typeratio <- vector()
    for (pl in rownames(platebalance)) {
      typeratio[pl] <- platebalance[pl, type] /
        sum(platebalance[pl, colnames(platebalance) != type])
    }
    names(typeratio)[typeratio == min(typeratio)]
  } else {
    plates
  }
}
  
plateMap <- function(sample.data, plate.data, duplicates=NULL,
                      empty.wells.at.end=TRUE, debug=FALSE) {

  # redefine sample with debug parameter
  # AND so that sample(x) returns x if length(x) == 1, rather than sample(1:x)
  .sample <- function(x, ...) {
    if (length(x) == 1) return(x)
    if(debug) { t <- 422; set.seed(t) }
    base::sample(x, ...)
  }

  stopifnot(is.element("SampleID",names(sample.data)))
  stopifnot(all(is.element(c("Plate", "Well", "SampleID"), names(plate.data))))
  if (!is.null(duplicates)) {
    stopifnot(all(is.element(unlist(duplicates[,1:2]), sample.data$SampleID)))
  }
 
  # load sample.data data (randomize rows)
  strata <- sample.data[.sample(1:nrow(sample.data)),]       
  
  ##
  # define the types
  # first, find out how many strata exist
  ns <- names(strata)[!(names(strata) %in% c("SampleID", "Reserve", "Family"))]
  for(r in 1:nrow(strata)) {
    strata$type[r] <-  paste(strata[r,ns], collapse=".")
  }
  possTypes <- unique(strata$type)
  
  ##
  # split strata table by dup status
  if (!is.null(duplicates)) {
    strata.dups <- strata[strata$SampleID %in% duplicates[,2],]
    strata <- strata[!(strata$SampleID %in% duplicates[,2]),]
  
    names(duplicates) <- c("dup.ID", "SampleID")
    dups <- merge(duplicates, strata.dups)
    dups <- dups[.sample(1:nrow(dups)),] # randomize rows
  }
  
  ##
  # split strata table by reserve status
  if ("Reserve" %in% names(strata)) {
    stopifnot(is.logical(strata$Reserve))
    reserve <- strata[strata$Reserve,]
    strata <- strata[!strata$Reserve,]
  }
  
  ##
  ids <- plate.data
  # ids has the "Plate Id" and "Well" (within plate)
  pnames <- unique(ids$Plate)
  lastplate <- ids$Plate[nrow(ids)]
  
  ##
  # reserve empty wells at end of last plate
  if (empty.wells.at.end) {
    nctrl <- sum(ids$SampleID != "")
    nempty <- nrow(ids) - (nrow(sample.data) + nctrl)
    if (nempty > 0) {
      availwells <- ids$Well[ids$Plate == lastplate & ids$SampleID == ""]
      emptywells <- availwells[(length(availwells) - nempty + 1):length(availwells)]
      ids$SampleID[ids$Plate == lastplate & ids$Well %in% emptywells] <- "empty"
    }
    stopifnot(sum(ids$SampleID == "empty") == nempty)
  }
  
  ##
  # randomly assign wells for duplicates
  # should be evenly distributed across plates until we run out of dups
  if (!is.null(duplicates)) {
    randplates <- .sample(pnames)
    # last plate at end
    randplates <- append(setdiff(randplates, lastplate), lastplate)
    dupsleft <- nrow(dups)
    while(dupsleft > 0) {
      for (plate in randplates) {
        availwells <- ids$Well[ids$Plate == plate & ids$SampleID == ""]
        well <- .sample(availwells, 1)
        ids$SampleID[ids$Plate == plate & ids$Well == well] <- "dup"
        dupsleft <- dupsleft - 1
        if (dupsleft == 0) break
      }
    }
    stopifnot(sum(ids$SampleID == "dup") == nrow(dups))
  }
  
  ##
  # randomly assign wells for reserved samples
  # should be evenly distributed across plates
  if ("Reserve" %in% names(strata)) {
    randplates <- .sample(pnames)
    # last plate at end
    randplates <- append(setdiff(randplates, lastplate), lastplate)
    resleft <- nrow(reserve)
    while(resleft > 0) {
      for (plate in randplates) {
        availwells <- ids$Well[ids$Plate == plate & ids$SampleID == ""]
        well <- .sample(availwells, 1)
        ids$SampleID[ids$Plate == plate & ids$Well == well] <- "reserve"
        resleft <- resleft - 1
        if (resleft == 0) break
      }
    }
    stopifnot(sum(ids$SampleID == "reserve") == nrow(reserve))
  }
  
  strata$plate <- 0
  strata$used <- 0 # indicator for assigning samples to well positions
  
  ids$type <- 0
  ids$type[which(ids$SampleID != "")] <- "ctrl"
  ids$type[which(ids$SampleID == "dup")] <- "dup"
  ids$type[which(ids$SampleID == "reserve")] <- "reserve"
  ids$type[which(ids$SampleID == "empty")] <- "empty"
  
  ##
  # if there are families, they should be plated together
  if ("Family" %in% names(strata)) {
    # divide strata into families and singles
    famsize <- table(strata$Family)
    single <- names(famsize)[famsize == 1]
  
    # order families by size
    famsize <- famsize[!(names(famsize) %in% single)]
    famsize <- sort(famsize, decreasing=TRUE)

    # randomize families within size groups
    sizegroups <- rle(as.vector(famsize))
    families <- list()
    for (i in 1:length(sizegroups$values)) {
      nfam <- sizegroups$length[i]
      size <- sizegroups$values[i]
      fams <- names(famsize)[famsize == size]
      stopifnot(length(fams) == nfam)
      families[[as.character(size)]] <- .sample(fams)      
    }
    famnames <- unlist(families, use.names=FALSE)
  
    # assign families to plates, starting with largest first
    for (f in 1:length(famnames)) {
      thisfam <- strata$Family == famnames[f]
      size <- sum(thisfam)
      # get number of available wells per plate
      nwells <- vapply(pnames, function(x) {sum(ids$Plate == x & ids$SampleID == "")}, 1L)
      # find plates with enough wells for family
      #possibleplates <- names(nwells)[nwells >= size]
      # find plates with max number of empty wells
      # this way each plate will get a family before we start re-using plates
      possibleplates <- names(nwells)[nwells == max(nwells)]
      # find best plates for this type
      #possibleplates <- .platesForType(possTypes, possibleplates, thistype, ids)
      # select one of the possible plates
      thisplate <- .sample(possibleplates, 1)
      # select wells from this plate
      possiblewells <- which(ids$Plate %in% thisplate & ids$SampleID=="")
      stopifnot(length(possiblewells) >= size)
      inds <- .sample(possiblewells, size)
      # assign family to wells
      ids$SampleID[inds] <- strata$SampleID[thisfam]
      ids$type[inds] <- strata$type[thisfam]
      strata$plate[thisfam] <- thisplate
      strata$used[thisfam] <- 1
    }
  }
  
  ##
  # for singletons, we can determine number of samples
  # of each type for each plate
  # find the approx number of wells per type per plate-batch
  # want approx = number of each type per plate
  samppertype <- table(strata$type[which(strata$used == 0)])
  wellsperpl <- table(ids$Plate[ids$SampleID == ""])
  totalwells <- sum(wellsperpl)
  typeperpl <- matrix(0, nrow=length(wellsperpl), ncol=length(samppertype),
                      dimnames=list(names(wellsperpl), names(samppertype)))
  for (i in 1:nrow(typeperpl)) {
    typeperpl[i,] <- floor(samppertype * (wellsperpl[i] / totalwells))
  }
  
  # randomly assign residual types to plates
  residtypes <- samppertype - colSums(typeperpl)
  residwells <- wellsperpl - rowSums(typeperpl)
  types <- names(residtypes)[order(residtypes, decreasing=TRUE)]
  # reset plate names in case families filled up some plates
  pnames <- names(wellsperpl)
  for (ty in types) {
    while (residtypes[ty] > 0) {
      randplates <- .sample(pnames)
      for (pl in randplates) {
        if (residwells[pl] > 0) {
          typeperpl[pl,ty] <- typeperpl[pl,ty] + 1
          residtypes[ty] <- residtypes[ty] - 1
          residwells[pl] <- residwells[pl] - 1
        }
        if (residtypes[ty] == 0) break
      }
    }
  }
  stopifnot(all(colSums(typeperpl) == samppertype))
  if (empty.wells.at.end) stopifnot(sum(typeperpl) == sum(ids$SampleID == ""))
  
  ##
  # plate samples by type
  for(pl in pnames) { # loop through plates
    for(ty in colnames(typeperpl)) { # loop through types
      ss <- typeperpl[pl,ty]
      if(ss > 0) {
        # randomly select which SampleIDs of this type to plate
        possibleids <- strata$SampleID[which(strata$used==0 & strata$type==ty)]
        stopifnot(length(possibleids) > 0)
        if (length(possibleids) > 1) {
          sids <- .sample(possibleids, ss)
        } else {
          sids <- possibleids
        }
        # randomly select which positions to store them
        possiblewells <- which(ids$Plate==pl & ids$SampleID=="")
        stopifnot(ss <= length(possiblewells))
        if (length(possiblewells) > 1) {
          inds <- .sample(possiblewells, ss)
        } else {
          inds <- possiblewells
        }
        stopifnot(length(inds)==length(sids))
        # assign the selected SampleIDs to the selected positions
        ids$SampleID[inds] <- sids
        ids$type[inds] <- ty
        strata$plate[is.element(strata$SampleID, sids)] <- pl
        strata$used[is.element(strata$SampleID, sids)] <- 1
      }
    }
  }
  if (empty.wells.at.end) stopifnot(sum(ids$SampleID == "") == 0)
  
  ##
  # plate reserved samples
  if ("Reserve" %in% names(strata)) {
    for (r in 1:nrow(reserve)) { 
      restype <- reserve$type[r]
      possiblewells <- which(ids$SampleID == "reserve")
      # select empty well, trying to keep plates balanced by type  
      if (length(possiblewells) > 1) {
        possibleplates <- unique(ids$Plate[possiblewells])
        if (length(possibleplates) > 1) {
          possibleplates <- .platesForType(possTypes, possibleplates, restype, ids)
          possiblewells <- which(ids$SampleID == "reserve" & ids$Plate %in% possibleplates)
        } 
        ind <- .sample(possiblewells, 1)
      } else {
        ind <- possiblewells
      }
      stopifnot(length(ind) == 1)
      ids$SampleID[ind] <- reserve$SampleID[r]
      ids$type[ind] <- restype
    }
  }
  stopifnot(sum(ids$SampleID == "reserve") == 0)
  
  ##
  # plate duplicates
  if (!is.null(duplicates)) {
    for (r in 1:nrow(dups)) { 
      duptype <- dups$type[r]
      # make sure other member of pair is not on same plate
      dupplate <- ids$Plate[ids$SampleID == dups$dup.ID[r]]
      stopifnot(length(dupplate) == 1)
      possiblewells <- which(ids$SampleID == "dup" & ids$Plate != dupplate)
      # select empty well, trying to keep plates balanced by type  
      if (length(possiblewells) > 1) {
        possibleplates <- unique(ids$Plate[possiblewells])
        if (length(possibleplates) > 1) {
          possibleplates <- .platesForType(possTypes, possibleplates, duptype, ids)
          possiblewells <- which(ids$SampleID == "dup" & ids$Plate %in% possibleplates)
        } 
        ind <- .sample(possiblewells, 1)
      } else {
        ind <- possiblewells
      }
      stopifnot(length(ind) == 1)
      ids$SampleID[ind] <- dups$SampleID[r]
      ids$type[ind] <- duptype
    }
  }
  stopifnot(sum(ids$SampleID == "dup") == 0)
  if (empty.wells.at.end) {
    stopifnot(setequal(ids$SampleID[!(ids$type %in% c("ctrl", "empty"))], sample.data$SampleID))
  }
  
  ids$SampleID[ids$SampleID == "empty"] <- ""
  stopifnot(setequal(setdiff(ids$SampleID[!(ids$type %in% "ctrl")], ""), sample.data$SampleID))
  idfinal <- ids[,-which(names(ids)=="type")]
  
  return(idfinal)
  
}
  
