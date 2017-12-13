\name{oilpressure}
\alias{oilpressure}

\title{
oilpressure
}
\description{
Creates a plot of the oil pressure around the track
}
\usage{
oilpressure(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{the number of laps that you would like to look at}
  \item{startdist}{the inital position of the track that you would like to look at   plots for, the default is the beginning of the track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
}


\value{
Returns a plot of the track map
}


\author{
Blain Morin
}


\examples{
oilpressure <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)){
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    rename(oilpress = "Oil Pressure_Cal") %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) + geom_point(aes(color = oilpress))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
