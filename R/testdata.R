# plate data - 9 96-well plates
nplates <- 9
nwells <- 96
plate.data <- data.frame("Plate"=paste0("Plate", rep(1:nplates, each=nwells)),
                        "Well"=paste0("Well", rep(1:nwells, nplates)),
                        "SampleID"=rep("", nplates*nwells),
                        stringsAsFactors=FALSE)

# reserve some wells for controls
plate.data$SampleID[plate.data$Well %in% c("Well95","Well96")] <- "control"


# sample data
navail <- sum(plate.data$SampleID == "")
ndup <- nplates*2
nempty <- 10
nsamp <- navail - ndup - nempty
ids <- paste("Sample", 1:nsamp, sep="")
sex <- rep("M", nsamp)
sex[sample(1:nsamp, round(0.4*nsamp))] <- "F"
group <- rep("A", nsamp)
group[sample(1:nsamp, round(0.3*nsamp))] <- "B"
group[sample(1:nsamp, round(0.1*nsamp))] <- "C"
reserve <- rep(FALSE, nsamp)
reserve[sample(1:nsamp, nplates*1.5)] <- TRUE
family.size <- c(rep(10, round(0.01*nsamp)), 
                 rep(5, round(0.05*nsamp)),
                 rep(2, round(0.1*nsamp)))
family.size <- c(family.size, rep(1, nsamp - sum(family.size)))
family <- rep(seq_along(family.size), times=family.size)

sample.data <- data.frame("SampleID"=ids, "Family"=family, "Sex"=sex,
                          "Group"=group, "Reserve"=reserve,
                          stringsAsFactors=FALSE)

# duplicate list
dup.ids <- sort(sample(1:nsamp, ndup))
duplicate.data <- data.frame("SampleID.1"=ids[dup.ids],
                   "SampleID.2"=paste("Dup", dup.ids, sep=""),
                   stringsAsFactors=FALSE)

# add duplicates to sample.data
dup.data <- merge(duplicate.data, sample.data, by.x="SampleID.1", by.y="SampleID",
                  sort=FALSE)
dup.data <- dup.data[,2:ncol(dup.data)]
names(dup.data)[1] <- "SampleID"
sample.data <- rbind(sample.data, dup.data)

#save(plate.data, sample.data, duplicate.data, file="../data/testdata.RData")
