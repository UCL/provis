library(osmar)
library(prettymapr)
library(geosphere)

# Define the spatial extend of the OSM data we want to retrieve
cambridge.box <- center_bbox(center_lon = 0.048047, center_lat = 52.159829,
                         width =  2000, height = 2000)
#cambridge.box <- center_bbox(center_lon = 0.109870, center_lat = 52.142139,
#                             width =  2000, height = 2000)

# Download all osm data inside this area
api <- osmsource_api()
cambridge <- get_osm(cambridge.box, source = api)

# General plot
plot(cambridge)

# Find highways
ways <- find(cambridge, way(tags(k == "highway")))
ways <- find_down(cambridge, way(ways))
ways <- subset(cambridge, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  

# Plot
spplot(hw_lines, zcol = "uid")

# Interactive view
mapview::mapview(hw_lines) 

# Make a random points dataset (like GPS)
gpsPoints <- spsample(x = hw_lines, n = 100, type = "random")

# Plot points
plot(hw_lines, xlab = "Lon", ylab = "Lat")
plot(gpsPoints, add = TRUE, pch = 19, col = "red")
box()

# Distances between Higways and random points
distances <- dist2Line(p = gpsPoints, line = hw_lines)
