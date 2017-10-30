test_platesForType <- function() {
  possTypes <- c("A", "B", "C", "D")
  plates <- c("1", "2", "3")
  type <- "A"
  current <- data.frame("Plate"=rep(plates, each=5),
                        "type"=c(rep("A",2), rep("B",2), "0",
                          rep("A",2), "C", rep("0",2),
                          rep("A",2), rep("C",2), "0"),
                        stringsAsFactors=FALSE)
  checkIdentical(c("1","2","3"), PlateMap:::.platesForType(possTypes, plates, "A", current))
  checkIdentical(c("2","3"), PlateMap:::.platesForType(possTypes, plates, "B", current))
  checkIdentical(c("1"), PlateMap:::.platesForType(possTypes, plates, "C", current))
  checkIdentical(c("1","2","3"), PlateMap:::.platesForType(possTypes, plates, "D", current))
}

test_plateMapSimple <- function() {
  # simple case - one sample per well,
  # number of groups equals number of wells per plate
  nplates <- 5
  nwells <- 4
  plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                           "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                           "SampleID"=rep("", nplates*nwells),
                           stringsAsFactors=FALSE)
  sample.data <- data.frame("SampleID"=1:(nplates*nwells),
                            "Group"=rep(1:4, nplates),
                            stringsAsFactors=FALSE)
  map <- plateMap(sample.data, plate.data, debug=TRUE)
  tmp <- merge(map, sample.data)
  checkTrue(all(table(tmp$Plate, tmp$Group) == 1))
}

test_plateMapEmpty <- function() {
  # check that empty.wells.at.end behaves as expected
  nplates <- 2
  nwells <- 10
  plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                           "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                           "SampleID"=rep("", nplates*nwells),
                           stringsAsFactors=FALSE)
  sample.data <- data.frame("SampleID"=1:12,
                            "Group"=rep(1:2, 6),
                            stringsAsFactors=FALSE)
  map <- plateMap(sample.data, plate.data, empty.wells.at.end=TRUE)
  checkTrue(all(map$SampleID[13:20] == ""))
  tmp <- merge(map, sample.data)
  checkEquals(matrix(c(5,1,5,1), nrow=2), matrix(table(tmp$Plate, tmp$Group), nrow=2))
  
  map <- plateMap(sample.data, plate.data, empty.wells.at.end=FALSE, debug=TRUE)
  tmp <- merge(map, sample.data)
  checkTrue(all(table(tmp$Plate, tmp$Group) == 3))
}

test_plateMapReserve <- function() {
  nplates <- 5
  nwells <- 4
  plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                           "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                           "SampleID"=rep("", nplates*nwells),
                           stringsAsFactors=FALSE)
  sample.data <- data.frame("SampleID"=1:(nplates*nwells),
                            "Group"=rep(1:2, (nplates*nwells)/2),
                            "Reserve"=rep(FALSE, nplates*nwells),
                            stringsAsFactors=FALSE)
  res.ind <- c(1,6,11,17,18)
  sample.data$Reserve[res.ind] <- TRUE

  # should be one Reserve sample per plate
  map <- plateMap(sample.data, plate.data, debug=TRUE)
  reserve.id <- sample.data$SampleID[sample.data$Reserve]
  checkEquals(nplates, length(unique(map$Plate[map$SampleID %in% reserve.id])))
  
  # deal with case where "reserve" samples have type that is not present in rest of samples
  sample.data$Group[res.ind] <- 3
  map <- plateMap(sample.data, plate.data, debug=TRUE)
  reserve.id <- sample.data$SampleID[sample.data$Reserve]
  checkEquals(nplates, length(unique(map$Plate[map$SampleID %in% reserve.id])))
}

test_plateMapFamily <- function() {
  nplates <- 5
  nwells <- 4
  plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                           "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                           "SampleID"=rep("", nplates*nwells),
                           stringsAsFactors=FALSE)
  sample.data <- data.frame("Family"=c(1,1,1,1, 2,2,2, 3,3, 4,4, 5,5,
                                       6,7,8,9,10,11,12),
                            "SampleID"=1:(nplates*nwells),
                            "Group"=rep(1:2, (nplates*nwells)/2),
                            stringsAsFactors=FALSE)
  map <- plateMap(sample.data, plate.data, debug=TRUE)
  tmp <- merge(map, sample.data)
  checkTrue(all(colSums(table(tmp$Plate, tmp$Family) > 0) == 1))
}

test_plateMapRandom <- function() {
    # completely random plate map
    nplates <- 5
    nwells <- 4
    plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                             "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                             "SampleID"=rep("", nplates*nwells),
                             stringsAsFactors=FALSE)
    sample.data <- data.frame("SampleID"=1:(nplates*nwells),
                              stringsAsFactors=FALSE)
    map <- plateMap(sample.data, plate.data, debug=TRUE)
    checkTrue(all(sample.data$SampleID %in% map$SampleID))
}

test_plateMapFamilyRandom <- function() {
    nplates <- 5
    nwells <- 4
    plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                             "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                             "SampleID"=rep("", nplates*nwells),
                             stringsAsFactors=FALSE)
    sample.data <- data.frame("Family"=c(1,1,1,1, 2,2,2, 3,3, 4,4, 5,5,
                                         6,7,8,9,10,11,12),
                              "SampleID"=1:(nplates*nwells),
                              stringsAsFactors=FALSE)
    map <- plateMap(sample.data, plate.data, debug=TRUE)
    tmp <- merge(map, sample.data)
    checkTrue(all(colSums(table(tmp$Plate, tmp$Family) > 0) == 1))
}

test_plateMapDups <- function() {
    # check that duplicates are plated appropriately
    nplates <- 5
    nwells <- 4
    plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                             "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                             "SampleID"=rep("", nplates*nwells),
                             stringsAsFactors=FALSE)
    sample.data <- data.frame("SampleID"=1:(nplates*nwells),
                              "Group"=rep(1:4, nplates),
                              stringsAsFactors=FALSE)
    duplicates <- data.frame(SampleID1=c(1,2), SampleID2=c(3,4), stringsAsFactors=FALSE)
    map <- plateMap(sample.data, plate.data, duplicates, debug=TRUE)
    checkTrue(map$Plate[map$SampleID == 1] != map$Plate[map$SampleID == 3])
    checkTrue(map$Plate[map$SampleID == 2] != map$Plate[map$SampleID == 4])
}
