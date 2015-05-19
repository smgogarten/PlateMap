\name{redoPlateMap}
\alias{redoPlateMap}
\title{Plate samples for redos}
\description{Plate sampels for redos with a constraint on the number of original plates from which each original plate is drawn.}
\usage{
redoPlateMap(sample.data, n.orig.plates=24,
             batch.size=48, batches.per.plate=2, ...)
}

\arguments{
  \item{sample.data}{A data.frame with sample data, including "SampleID,"
    "Orig.Plate",
    and as many other columns as there are categories on which to
    stratify the plate maps.
    An optional column called "Reserve" is a logical variable for samples that
    should be distributed evenly across plates outside of stratification
    (such as cross-study duplicates or WGA samples).
    If families should be plated together, give family ID in column
    "Family."
  }
  \item{n.orig.plates}{Maximum number of original plates from which samples on a new plate can be drawn.}
  \item{batch.size}{Size of each batch of samples on the new plate. Can be useful to make this half a plate if there are many original plates.}
  \item{batches.per.plate}{Number of batches on each new plate.}
  \item{\dots}{Other arguments passed to \code{\link{plateMap}}.}
}

\details{
  Randomly selects \code{n.orig.plates} original plates and runs \code{\link{plateMap}} with the given \code{batch.size}. Repeats until all samples have been assigned to new plates.
}

\value{
  A data frame with the contents of \code{sample.data} as well as new columns "Batch", "Well", and "Plate".
}

\author{Stephanie M. Gogarten}

\seealso{\code{\link{plateMap}}}

\examples{
## make original plate map
head(plate.data)
head(sample.data)
head(duplicate.data)
map <- plateMap(sample.data, plate.data, duplicate.data, debug=TRUE)
tmp <- merge(map, sample.data)
table(tmp$Plate, paste(tmp$Sex, tmp$Group, sep="."))

## select some samples as redos
redo <- tmp[sample(1:nrow(tmp), 100), c("SampleID", "Plate", "Group")]
names(redo)[2] <- "Orig.Plate"
map2 <- redoPlateMap(redo)
}

\keyword{manip}
