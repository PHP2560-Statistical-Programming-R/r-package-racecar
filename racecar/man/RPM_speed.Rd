\name{RPM_speed}
\alias{RPM_speed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The plot presents speed change and RPM change of one lap in one graph.
%%  ~~function to do ... ~~
}
\description{ The plots present this information in a X-Y plot of speed and RPM versus distance while speed is showed by left y-axis and RPM is showed by right y-axis.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RPM_speed(data, lap, startdist, enddist)
RPM_speed(data,1,0,3.5)
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
A plot that discribe speed and RPM along distence in one lap is generated.
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
function (data, lap, startdist, enddist) 
{p1 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance_km >= startdist & Distance_km <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = GPS_Speed), color = "#0033FF", size = 0.5) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x =element_blank())+
    theme(panel.background = element_blank())+
    scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
    scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1)))+
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "npc"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank())+
    ggtitle("GPS_Speed\n") +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = - 0.2, vjust = 2.12, colour = "#0033FF", size = 12))
  
  p2 <- data %>%
    filter(Lap == laps) %>%
    filter(Distance >= startdist & Distance <= enddist) %>%
    ggplot(aes(x = Distance)) +
    geom_line(aes(y = PE3_RPM), color = "#FF3333", size = 0.5) +
    ggtitle("RPM\n") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.major.x =element_blank()) +
    theme(panel.background = element_blank()) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 16000)) +
    scale_x_continuous(breaks = c(0, seq(startdist, enddist, 1))) +
    theme(axis.text.y = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.ticks = element_line(colour = 'gray50', size = 0.5),
          axis.ticks.length = unit(.25, "npc"),
          axis.ticks.x = element_line(colour = "black"),
          axis.ticks.y = element_blank()) +
    labs(x = NULL, y = NULL) +
    theme(plot.title = element_text(hjust = 0.85, vjust = 2.12, colour = "#FF3333", size = 12))
  
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  print(pp)
  
  grob_p2 <- as.list(g2$grobs[[which(g2$layout$name == "panel")]])
  
  g <- gtable_add_grob(g1, grob_p2, pp$t, pp$l, pp$b, pp$l)
  print(g)

  ia <- which(g2$layout$name == "axis-l")
  
  ga <- g2$grobs[[ia]]
  ax <- as.list(ga$children)[[2]]
  
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  g$grobs[[16]]$children$GRID.text.3853$label <- c("GPS_Speed\n", "RPM\n")
  
  g$grobs[[16]]$children$GRID.text.3853$gp$col <- c("#0033FF","#FF3333")

  g$grobs[[16]]$children$GRID.text.3853$x <- unit(c(-0.155, 0.9), "npc")
  
  return(grid.draw(g))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ RPM }
\keyword{ speed }% __ONLY ONE__ keyword per line
\keyword{ distence }