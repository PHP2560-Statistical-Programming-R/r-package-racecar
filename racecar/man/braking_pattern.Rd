\name{braking_pattern}
\alias{braking_pattern}

\title{
Plot the braking position of the car around the racetrack by lap
}
\description{
This function will create a heat style map of the track based on the braking position throughout the lap. If the lap vector length is greater than 1, there will be one plot generated for each lap.
}
\usage{
braking_pattern(lap_data, 1, 1, 4)
braking_pattern(lap_data, 2)
}

\arguments{
  \item{data}{a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{a vector containing all lap numbers that you would like to look at plots for}
  \item{startdist}{the inital position of the track that you would like to look at   plots for, the default is the beginning of the track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
}

\details{

}
\value{
A plot that is a map of the track is generated.
Multiple plots will be generated if you are looking at more than more lap
}
\references{

}
\author{
Jess Kaminsky
}
\note{
}

\seealso{
}

\examples{
## The function is currently defined as
braking_pattern <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = BPS_Front))
  }
}

\keyword{braking}
\keyword{position}
\keyword{graph}
\keyword{map}