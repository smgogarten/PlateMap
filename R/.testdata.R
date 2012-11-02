# plate data - 10 96-well plates
nplates <- 10
nwells <- 96
plate.data <- data.frame("Plate"=paste("Plate", rep(1:nplates, each=nwells), sep=""),
                       "Well"=paste("Well", rep(1:nwells, nplates), sep=""),
                       "SampleID"=rep("", nplates*nwells),
                       stringsAsFactors=FALSE)

# reserve some wells for controls
plate.data$SampleID[plate.data$Well %in% c("Well95","Well96")] <- "control"

save(plate.data, file="../data/plates.RData")


# sample data
navail <- sum(plate.data$SampleID == "")
ndup <- 20
nempty <- 20
nsamp <- navail - ndup - nempty
ids <- paste("Sample", 1:nsamp, sep="")
sex <- rep("M", nsamp)
sex[sample(1:nsamp, round(0.4*nsamp))] <- "F"
group <- rep("A", nsamp)
group[sample(1:nsamp, round(0.1*nsamp))] <- "B"
group[sample(1:nsamp, round(0.3*nsamp))] <- "C"
group[sample(1:nsamp, round(0.2*nsamp))] <- "D"
reserve <- rep(FALSE, nsamp)
reserve[sample(1:nsamp, nplates*1.5)] <- TRUE

sample.data <- data.frame("SampleID"=ids, "Sex"=sex,
                          "Group"=group, "Reserve"=reserve,
                          stringsAsFactors=FALSE)

# duplicate list
duplicates <- data.frame("SampleID.1"=ids[1:ndup],
                   "SampleID.2"=paste("Dup", 1:ndup, sep=""),
                   stringsAsFactors=FALSE)

save(duplicates, file="../data/duplicates.RData")

# add duplicates to sample.data
dup.data <- merge(duplicates, sample.data, by.x="SampleID.1", by.y="SampleID",
                  sort=FALSE)
dup.data <- dup.data[,2:ncol(dup.data)]
names(dup.data)[1] <- "SampleID"
sample.data <- rbind(sample.data, dup.data)

save(sample.data, file="../data/samples.RData")
