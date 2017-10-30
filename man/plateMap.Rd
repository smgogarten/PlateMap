\name{plateMap}
\alias{plateMap}
\title{Generate a set of plate maps for genotyping}
\description{Assigns samples to plates for genotyping, balancing
  important variables across plates.}
\usage{
plateMap(sample.data, plate.data, duplicates=NULL,
         empty.wells.at.end=TRUE,
         families.at.random=TRUE,
         debug=FALSE)
}

\arguments{
  \item{sample.data}{A data.frame with sample data, including "SampleID,"
    and as many other columns as there are categories on which to
    stratify the plate maps. If there are no other columns, the plate map
    will be completly random.
    An optional column called "Reserve" is a logical variable for samples that
    should be distributed evenly across plates outside of stratification
    (such as cross-study duplicates or WGA samples).
    If families should be plated together, give family ID in column
    "Family."
  }
  \item{plate.data}{A data frame with columns "Plate," "Well,"
    and "SampleID."  If any wells are to be reserved for controls, they
    should have a non-empty value in the "SampleID" column.  Available
    wells should have an empty string ("") in the "SampleID" column.
  }
  \item{duplicates}{A data.frame with two columns, where the SampleID in
    column two is the duplicate of the SampleID in column one.  If this
    argument is provided, duplicates will not be placed on the same
    plate and the ids from column two will be evenly balanced across
    plates.  SampleIDs in both columns should be present in \code{sample.data}.
  }
  \item{empty.wells.at.end}{A logical.  If \code{TRUE}, all empty wells
    will be adjacent on the last plate.  If \code{FALSE}, empty wells will
    be randomly distributed across plates.
  }
  \item{families.at.random}{A logical.  If \code{TRUE}, families will be
    plated at random, with the constraint that all members of the family
    will fit on the plate.  If \code{FALSE}, plates for families will be
    selected in order of the number of empty wells, such that all plates
    will have at least one family before any plate has more than one
    family.
  }
  \item{debug}{A logical.  If \code{TRUE}, the the "random" sampling of
    sample ids & well positions will be taken from a previously set
    seed, so repeated runs of the function will generate the same results.
  }
}

\details{
  The plates will be balanced so that the
  fraction of samples in each category given in \code{sample.data}
  on each plate mirrors the total
  fraction in the sample set as closely as possible.
}

\value{
  The data.frame in \code{plate.data}, with the "SampleID" column
  filled in with values from \code{sample.data}.
}

\author{Stephanie M. Gogarten, Caitlin McHugh, David Levine}

%\seealso{}

\examples{
head(plate.data)
head(sample.data)
head(duplicate.data)
map <- plateMap(sample.data, plate.data, duplicate.data, debug=TRUE)
tmp <- merge(map, sample.data)
table(tmp$Plate, paste(tmp$Sex, tmp$Group, sep="."))
}

\keyword{manip}
