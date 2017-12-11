\name{lapspeed}
\alias{lapspeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plot the speed of each lap and compare speed of multiple laps if there is moer than one laps.
%%  ~~function to do ... ~~
}
\description{ This function will create a heat style line based on the speed throughout the lap. If the lap argument length is greater than 1, there will be one line generated for each lap in one graph.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lapspeed(data, laps, startdist, enddist)
lapspeed(data, 1, 0, 3.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a dataframe that has been output from cleanSingleLap or cleanMultiLap}
  \item{laps}{the number of laps that you would like to look at}
  \item{startdist}{the inital position of the track that you would like to look at plots for, the default is the beginning of the track}
  \item{enddist}{the ending position of the track the you would like to look at plots for, the default is the ending of the track}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot that shows speed change in one or more laps is drawed.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Fuyu Zou
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
function (data, laps, startdist, enddist) 
{  data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance, y = Lap)) +
    geom_point(aes(color = GPS_Speed), size = 3, pch = 15) +
    scale_colour_gradientn(colours=rainbow(4)) +
    scale_y_continuous(breaks= c(1, seq(1,length(laps),1)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ speed }
\keyword{ lap }
\keyword{ distance }
\keyword{ graph }