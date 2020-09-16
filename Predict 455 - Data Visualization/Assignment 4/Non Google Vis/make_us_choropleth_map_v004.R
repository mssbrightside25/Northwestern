# R code for making choropleth map of the United States

# state input date flows into object called my.data.frame
# state names in my.data.frame from the input data must match 
# state names in gadm.data.frame for the state topology

library(sp)  # spatial analysis in R
library(maptools)  # utilities for working with shapefiles and map projection
library(ggplot2)  # plotting utilities
library(colorspace)  # color functions like HLS() for hue, luminance, and saturation/chroma
library(maps)
library(mapproj)

library(Cairo)  # many output options for data visualization
# Cairo reference manual provides the following description of the package
# Cairo initializes a new graphics device that uses the cairo graphics library 
# for rendering. The current implementation produces high-quality PNG, 
# JPEG, TIFF bitmap files, high resolution PDF files with embedded fonts, 
# SVG graphics and PostScript files. It also provides X11 and Windows 
# interactive graphics devices. Unlike other devices it supports all graphics 
# features including alpha blending, anti-aliasing etc.

gpclibPermit()  # permit use of non-commercial package gpclib

# Chang (2013) R Graphics Cookbook is a good reference for ggplot2 work
# here we enter the theme_clean function from Chang (2013) pages 317-318
# this will provide a map with no grid lines for longitude and latitude
theme_clean <- function(base_size = 12) {
    require(grid)
    theme_grey(base_size) %+replace%
    theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.ticks.margin = unit(0, "cm"),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0,0,0,0), "lines"),
    complete = TRUE)
    } # end of theme_clean function
    
# To prepare the geographical data for the United States map, 
# we download ESRI shapefiles from <www.gadm.org>. These provide 
# information for administrative/political areas worldwide.  
# We download shapefiles for the United States in RData format 
# for use in R. These define an R SpatialPolygonsDataFrame with a 
# longitude/latitude reference system. This R object will be used 
# in conjunction with the sp package. There are three levels 
# of detail available with these files, identified as levels 
# 0, 1, and 2. The corresponding data files are
# 
# USA_adm0.RData  # outline of the US
# USA_adm1.RData  # US with state boundaries
# USA_adm2.RData  # US with county boundaries

load("USA_adm1.RData")  # US with states shapefile from http://www.gadm.org/

# print(ls())  # shows one object named gadm
# print(str(gadm))  # class 'SpatialPolygonsDataFrame'... polygons needed for maps

# fortify() from ggplot2 converts to a data frame for use in ggplot2 maps 
# it is easier to work with the data frame object for bounding box selection
# and for merging with new data, as needed for the choropleth maps
gadm.data.frame <- fortify(gadm)
# set id as factor with the state name from the gadm object
gadm.data.frame$state <- factor(gadm.data.frame$id, 
  levels = 1:length(gadm$NAME_1), labels = gadm$NAME_1) 
# character string for state... not factor
gadm.data.frame$state <- as.character(gadm.data.frame$state)
    
# for the choropleth map... find a variable to plot for the states... 
# my choice for this sample program is SAT scores by state, with the data from
# <http://www.commonwealthfoundation.org/policyblog/detail/sat-scores-by-state-2013>
# note that participation rates vary greatly from state to state
# so the interpretation of mean scores is affected by the fact that students
# in the midwest who take the SAT are those who plan to apply to schools
# outside of the midwest (where the ACT is not accepted in place of SAT)
# these data were entered into a comma-delimited text file
my.data.frame <- read.csv("data_sat_scores_2013.csv", header = TRUE)
# character string for state... not factor
my.data.frame$State <- as.character(my.data.frame$State)  

# variables in the my.data.frame
# Rank: rank of the state based upon the combined SAT score
# State: state name 
# Participation_Rate: percentage of high school students participating in SAT
# Reading: mean critical reading score for students taking the SAT
# Math: mean math aptitude score for students taking the SAT
# Writing: mean writing score for students taking the SAT
# SAT: mean combined SAT score for students taking the SAT

# check that the state names match up between the two data frames
if(!setequal(unique(my.data.frame$State), unique(gadm.data.frame$state))) {
   cat("\n\nState names from input data:\n\n")
   print(sort(unique(my.data.frame$State)))
   cat("\n\nState names from GIS database:\n\n")
   print(sort(unique(gadm.data.frame$state)))
   cat("\n\n")
   stop("\n\nExecution terminated")  
   }

# merge the SAT data with the map data
combined.data.frame <- merge(gadm.data.frame, my.data.frame, 
  by.x = "state",  by.y = "State")

# define a bounding box for the continental United States
# Bounding Box: -124.7625, 24.5210, -66.9326, 49.3845
# select polygons within the continental United States
us.data.frame <- subset(combined.data.frame,
  subset = ((long >= -124.7625) & (long <= -66.9326) &
            (lat >= 24.5210) & (lat <= 49.3845)))
            
# let's also drop the polygons for the holes... lakes...
selected.us.data.frame <- subset(us.data.frame, subset = (!hole))
                     
# coord_map function offers alternative projections, see help(mapproject)  
# mercator is the default
# for US map with mercator projection use coord_map("mercator")
# for us map with conic projection use coord_map("polyconic")
# albers(lat0,lat1) is another commonly used projection (requires latitude values)
# for US map with albers projection across the continental US 
#   use coord_map("albers", lat0 = 24.5210, lat1 = 49.3845)
# choosing a projection is often a matter of personal choice... I like albers

# here we show albers with grid lines for longitude and latitude
# gradient scale values were selected to provide a convenient
# and visually appealing red-to-gray-to-blue gradient for the map
us.map.object <- ggplot(data = selected.us.data.frame, 
    aes(map_id = id, x = long, y = lat, fill = SAT)) + 
  geom_map(map = selected.us.data.frame, colour = "black") + 
  coord_map("albers", lat0 = 24.5210, lat1 = 49.3845) + 
  scale_fill_gradient2(low = hex(HLS(12,0.5,0.9)), 
                       mid = "gray90", 
                       high = hex(HLS(253,0.5,0.9)), 
                       midpoint = median(my.data.frame$SAT))
print(us.map.object)  # print the map to the screen (long run time... be patient)

# here we show the same map with no grid lines using theme_clean function
us.map.object <- ggplot(data = selected.us.data.frame, 
    aes(map_id = id, x = long, y = lat, fill = SAT)) + 
  geom_map(map = selected.us.data.frame, colour = "black") + 
  coord_map("albers", lat0 = 24.5210, lat1 = 49.3845) + 
  scale_fill_gradient2(low = hex(HLS(12,0.5,0.9)), 
                       mid = "gray90", 
                       high = hex(HLS(253,0.5,0.9)), 
                       midpoint = median(my.data.frame$SAT)) +
    theme_clean()
print(us.map.object)  # print the map to the screen (long run time... be patient)

# put map with no grid lines into a pdf file
pdf(file = "plot_map_sat_scores_by_state.pdf", width = 11, height = 8.5)
print(us.map.object)
dev.off()

# put map with no grid lines into an svg file
svg(file = "plot_map_sat_scores_by_state.svg", width = 11, height = 8.5)
print(us.map.object)
dev.off()

# for faster display of the map consider using a bitmap/pixel output
# size must be provided in points/pixel units 
# one point is 1/72 inches So if the image on a screen is 72ppi 
# (pixels per inch), then one point will equal exactly one pixel
# display of this image then will depend upon screen resolution
# 8.5 inches = 612 points   11 inches = 792 points
CairoPNG(file = "plot_map_sat_scores_by_state.png", width = 792, height = 612)
print(us.map.object)
dev.off()

# Notes.

# would like to know what the problem is with lake Michigan
# thought we would have taken care of that when dropping the holes
# otherwise this is looking pretty good... slow in plotting but good

# the original shapefiles contain information 
# for Alaska and Hawaii... by changing the longitude we could bring these
# states into the frame of the image along with the continental US
# this programming work is left for others

# the location of the legend in the standard image is far to the right
# of the map... this is something that could be improved upon prior
# to formal publication of the map















