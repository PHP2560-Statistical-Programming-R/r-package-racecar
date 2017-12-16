USE OF THE PACKAGE: This package is designed to be used by those interested in perfomance analytics from race car data. The data is measured from the car while driving around a race track. The car sends the data to racestudio2 software. The data can be exported from racestudio2 into a csv. This package provides functions that take the output from racestudio2 and cleans + maninpulates it into a more useable form. This package also provides functions for generating the most common plots used when analyzing racecar data.

CREATING THE PACKAGE: In creating this package, our group first wrote a set of functions that manipulated and plotted data from racestudio2 output. We used a textbook "Making Sense of Squiggly Lines: The Basic Analysis of Race Car Data Acquisition" to help determine which graphs would be the most useful for individuals working with racecar data. After determining the most useful functions, we organized all of our functions into one R file and used RStudio to help design the file structure for our package. We decided to use a single file format because it seemed simpler to have all of our functions stored in one file.

Package Installing instruction: 
1. Download our repository https://github.com/PHP2560-Statistical-Programming-R/r-package-racecar/tree/master/racecar%20files
2. run the code: 
"install.package("racecar", type = "source", repos = NULL) "


INDIVIDUAL ROLES:

BLAIN: 
- provided the software, data, and idea for project 
- helped write the clean data functions
- wrote oilpressure, airfuel, and mapseed functions
- wrote documentation files for oilpressure, airfuel, and mapspeed functions

FUYU: 
- helped write the clean data functions
- added aesthetics to all functions
- wrote RPM map, RPM_speed, and RPM_gear function
- wrote documentation files for RPM map, RPM_speed, and RPM_gear functions

JESS: 
- helped write the clean data functions 
- wrote throttle position and braking pattern functions
- set up file layout for package
- wrote documentation file for cleaning data, throttle, and braking functions

--------------------
R Package Proposal: 
For this project we will create an R package that manipulates the csv output from Racestudio2. These csvs contain car race data from the Brown Formula Racing endurance laps (speed, throttle, acceleration, GPS position, oil pressure, braking, RPM, gear, etc). 

We will create functions that easily complete the following tasks:

Blain:

Cleans/Merges csv files among laps

Graph that looks at speed across GPS coordinates

Fuyu:

Graph of RPM vs Speed

Graph that compares lap speed

Graph that compares speed among driver

Jess:

Histograms with engine RPM

Graphs of throttle position

Graphs of braking pattern

------------------------------------------------------------------------------------------------------------------------
