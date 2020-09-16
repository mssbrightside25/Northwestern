# run choropleth map jump-start using R package googleViz

# program prepared by Tom Miller

# alternative to building a map from scratch using ggplot2 

library(googleVis)  # for working with Google Chart Tools APIA
# see CRAN documentation at http://cran.r-project.org/web/packages/googleVis/index.html
 
# note that googleVis depends upon RJSONIO... think JavaScript and JavaScript Object Notation

# user-defined function to recode states as state abbreviations
# this assumes that both input and output are character strings
# note how we are putting the state abbreviations in alphabetical order
# to ensure that things do not get mixed up if we later convert to factors
# state abbreviations will be used as input to the googleVis package
make.state.abbreviation <- function(x) {
  switch(x, 
    "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR", 
    "Arizona" = "AZ",  "California" = "CA",
    "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC", 
    "Delaware" = "DE", "Florida" = "FL",
    "Georgia" =  "GA", "Hawaii" = "HI", "Iowa" = "IA", 
    "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN",
    "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", 
    "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME",
    "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO",  
    "Mississippi" = "MS", "Montana" = "MT", 
    "North Carolina" = "NC", "North Dakota" = "ND",
    "Nebraska" = "NE", "New Hampshire" = "NH", "New Jersey" = "NJ",
    "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY", 
    "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", 
    "Pennsylvania" = "PA", 
    "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
    "Tennessee" = "TN", "Texas" = "TX",
    "Utah" = "UT",  "Virginia" = "VA", "Vermont" = "VT", 
    "Washington" = "WA", "Wisconsin" = "WI", 
    "West Virginia" = "WV", "Wyoming" = "WY", "")    
    }

# variables in the data frame named my.data.frame
# Rank: rank of the state based upon the combined SAT score
# State: state name 
# Participation_Rate: percentage of high school students participating in SAT
# Reading: mean critical reading score for students taking the SAT
# Math: mean math aptitude score for students taking the SAT
# Writing: mean writing score for students taking the SAT
# SAT: mean combined SAT score for students taking the SAT

my.data.frame <- read.csv("data_sat_scores_2013.csv", header = TRUE)
# character string for state... not factor
my.data.frame$State <- as.character(my.data.frame$State)  

my.data.frame$state <- rep("", length = nrow(my.data.frame))
for(index.for.state in seq(along = my.data.frame$State)) 
  my.data.frame$state[index.for.state] <- make.state.abbreviation(my.data.frame$State[index.for.state])
  
# check the coding of states
print(my.data.frame[,c("State", "state")])

# set up color gradient color codes and values to be used in the JSON
# as required for the Google API: 
#   colorAxis = "{values: [my.value.gradient], colors: [my.color.gradient]}",
# here is the color gradient array for use in the ggplot2 choropleth map
# my.color.gradient <- c(hex(HLS(12,0.5,0.9)), hex("gray90"), hex(HLS(253,0.5,0.9)))
# we will will use three corresponding Web colors: coral, lightgray, and blue
# which we will enter into JSON format for colors  
# these colors in the gradient are set to correspond to the min, median, and max
# values of the state data to be displayed in the choropleth map
my.value.gradient <- c(min(my.data.frame$SAT), 
                       median(my.data.frame$SAT),
                       max(my.data.frame$SAT)) 
print(my.value.gradient)  # 1351 1551 1807 to be entered into JSON format                       

# create the map object by using googleVis function to 
# generate a javascript program for running the choropleth map
javascript.us.map.object <-  gvisGeoChart(my.data.frame, "state", "SAT",
  options=list(region="US", 
  displayMode="regions",
  resolution="provinces", 
  colorAxis = "{values: [1351, 1551, 1807], colors: [\'coral', \'lightgray', \'blue']}", 
  width=700, height=500))

# what kind of class is this javascript program in R?
#   class(javascript.us.map.object)
#   [1] "gvis" "list"

# using plot on a gvis list object sends the program to the default browser application
plot(javascript.us.map.object)  # plots in a new browser window

