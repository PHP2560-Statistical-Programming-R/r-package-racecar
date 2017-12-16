\name{maprpm}
\alias{maprpm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{maprpm
%%  ~~function to do ... ~~
}
\description{ Plots the rpm of the racecar around the track map.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{maprpm(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{the number of laps that you would like to look at}
  \item{startdist}{the inital position of the track that you would like to look at   plots for, the default is the beginning of the track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
%%     ~~Describe \code{x} here~~
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot that is a map of the track is generated.
Multiple plots will be generated if you are looking at more than more lap
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Fuyu Zou
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
maprpm <- function(data, laps = 1, startdist = min(data$Distance) , enddist = max(data$Distance)) {
  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist) %>%
    filter(Distance <= enddist) %>%
    ggplot(aes( x = GPS_Latitude, y = GPS_Longitude)) +
    geom_point(aes(color = PE3_RPM), size = 3) + facet_wrap(~Lap)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ race }
\keyword{ lap }
\keyword{ rpm }