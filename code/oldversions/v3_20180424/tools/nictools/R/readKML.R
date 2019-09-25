read.kml <- function(file, layers) {
  require(sp)
  require(rgdal)
  read.layer <- function (layer_name) {
    spobj <- rgdal::readOGR(dsn=file, layer=layer_name)
    coords <- coordinates(spobj)
    colnames(coords) <- c('x', 'y', 'z')[1:ncol(coords)]
    df <- data.frame(coords, spobj@data)
    transform(df, layer=layer_name)
  }
  Reduce(rbind, lapply(layers, read.layer))
}