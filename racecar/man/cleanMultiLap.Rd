\name{cleanMultiLap}
\alias{cleanMultiLap}

\title{Clean and generate dataset with multiple laps
}
\description{
Returns a dataframe containing data collected from multiple laps
}
\usage{
cleanMultiLap(c("laponedata.csv", "laptwodata.csv"))
}

\arguments{
  \item{file_names}{a character vector containing paths to all files of individual lap data to be combined
  }
}
\details{
The file names included in the file_names vector should be listed in sequential order by lap number. The lap number column will correspond to the position of the .csv file in the file_names vector. For example, in cleanMultiLap(c(laponedata.csv, laptwodata.csv)), the data in laponedata.csv will have Lap = 1 for all observations. The data from laptwodata.csv will have Lap = 2
}
\value{
A tibble with data from racestudio2 that has been cleaned. A column for lap number has also been included.
}

\references{
}

\author{
Jess Kaminsky, Blain Morin, Fuyu Zou
}
\note{
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
## The function is currently defined as

cleanMultiLap <- function(file_names) {
  all_laps <- tibble()
  lap_numbers <- 1:length(file_names)
  for (i in 1:length(file_names)) {
    temp_lap<- cleanSingleLap(file_names[i], lap_numbers[i])
    all_laps <- bind_rows(all_laps, temp_lap)
  }
  return(all_laps)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{clean}
\keyword{multiple}
\keyword{laps}
\keyword{data}
