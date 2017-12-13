\name{airfuel}
\alias{airfuel}

\title{airfuel

}
\description{
Creates a scatter plot with air fuel ratio vs rpm. Also includes a linnear regression line. 
}
\usage{
airfuel(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance))
}

\arguments{
  \item{data}{a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{the number of laps that you would like to look at}
  \item{startdist}{the inital position of the track that you would like to look at   plots for, the default is the beginning of the          track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
}

}
\value{
A scatter plot is created.
}

\author{
Blain Morin
}


}
\examples{
airfuel <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = PE3_RPM , y = PE3_LAMBDA)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ air }
\keyword{ fuel }
\keyword{ rpm }
