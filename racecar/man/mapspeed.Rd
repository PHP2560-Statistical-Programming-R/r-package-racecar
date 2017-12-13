\name{mapspeed}
\alias{mapspeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mapspeed

}
\description{
Plots the speed of the racecar around the track map.
}
\usage{
mapspeed(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{the number of laps that you would like to look at}
  \item{startdist}{the inital position of the track that you would like to look at   plots for, the default is the beginning of the track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot that is a map of the track is generated.
Multiple plots will be generated if you are looking at more than more lap
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Blain Morin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

mapspeed <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = GPS_Speed))
  }
}

\keyword{ speed }
\keyword{ race }
\keyword{ lap }