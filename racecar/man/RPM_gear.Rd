\name{RPM_gear}
\alias{RPM_gear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ This function draws a plot that shows what speeds different gear ratios allow for.
%%  ~~function to do ... ~~
}
\description{The plots present this information in a X-Y plot of RPM versus speed. Different colors represent varying amounts of throttle position and color vary from red at 0 precent to purple at 100 percent. And one plot is generated for each gear.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RPM_gear(data, laps, startdist, enddist)
RPM_gear(data, 1, 0, 3.5)
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
A plot that shows what speeds different gear ratios allow for is generated.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Fuyu Zou
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
{data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist)%>%
    mutate(gear_floor = floor(Calculated_Gea)) %>%
    group_by(gear_floor) %>%
    ggplot(aes(colour = PE3_TPS_)) +
    scale_colour_gradientn(colours=rainbow(4))+
    geom_point(aes(x = GPS_Speed, y = PE3_RPM), size = 0.1)+
    facet_wrap(~gear_floor)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ RPM }
\keyword{ speed }% __ONLY ONE__ keyword per line
\keyword{ gear }