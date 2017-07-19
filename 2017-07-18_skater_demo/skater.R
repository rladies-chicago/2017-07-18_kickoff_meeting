library(foreign)
library(ggplot2)
library(spdep)
library(maptools)



# Load a shapefile of the Census tracts in Chicago
# The data include tract-level disadvantage characteristics
chicago <- readShapePoly('HomRates_Disadvantage.shp')
summary(chicago)
plot(chicago,border=gray(0.5))



# Define a list of the variable names
vars <- c('PctMinPop','PctPoorHH','MedIndvInc','MedHHInc','PctNoBac','PctNoHSD','PctFHH','PctHisp','PctMNoJob','PctUnemp')



# Pull the variables out into a dataframe
dat <- data.frame(chicago@data[,vars])
summary(dat)



# Center and scale the variables
sdat <- scale(dat)
summary(sdat)



# Create neighbor list from tract map based on first-order Queen contiguity
chicago.nb <- poly2nb(chicago)



# Plot the connectivity diagram
plot(chicago.nb,coordinates(chicago),col='dark turquoise',add=TRUE)



# Calculate the cost of each edge (multi-dimensional distance between the nodes)
lcosts <- nbcosts(chicago.nb,sdat)



# Calculate the neighbor weights based on the costs
chicago.w <- nb2listw(chicago.nb,lcosts,style='B')



# Create the minimum spanning tree based on the neighbor weights
chicago.mst <- mstree(chicago.w)



# Plot the minimum spanning tree
plot(chicago.mst,coordinates(chicago),col='dark turquoise',cex.lab=0.001)
plot(chicago,border='deep pink',add=TRUE)



# Use SKATER to cluster Chicago Census tracts into 12 regions based on social-disadvantage variables
clusters12 <- skater(chicago.mst[,1:2],sdat,11)
groups12 <- clusters12$groups
table(groups12)

# Fun with colors!  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
plot(chicago,col=c('green yellow','orangered','dark gray','cyan','gold','pink','deep pink','dark cyan','purple 3','spring green 3','royal blue 2','dark orchid 4')[clusters12$groups])



# Create a new dataframe from the groups vector and add the GeoID
SKATER12 <- as.data.frame(cbind(chicago$geoid10, chicago@data[,vars],
                                chicago$HOMRATE, groups12))



# Write the dataframe to a CSV
write.csv(SKATER12,'SKATER12.csv', row.names = FALSE)