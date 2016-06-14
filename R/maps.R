
x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap", "leaflet", "htmltools")
#install.packages(x) # warning: this may take a number of minutes
lapply(x, library, character.only = TRUE) # load the required packages

# load shape files ----
eng <- readOGR(dsn = "data", layer = "LAD_DEC_2015_GB_BFE")
#' these objects have multiple 'slots', for example this is the data slot:
head(eng@data)

## first issue is that we're in the wrong projection type. Lots of docs on this
## on the web.
proj4string(eng) # current projection
eng_orig <- eng # preserve the original projection in case we need it
eng <- spTransform(eng, CRS("+init=epsg:4326")) # WGS84 projection (what we need for leaflet)

# simplify the shapefile (it's kind of massive otherwise) ----
eng_sim <- gSimplify(eng, tol = 0.01, topologyPreserve = TRUE)
## A downside of this is that it unbinds your data... :(
eng_data <- eng@data
eng_df <- SpatialPolygonsDataFrame(eng_sim, eng_data) # rebinds the data

# let's make a quick plot that shows where Hartlepool is ----
sel <- eng_df$LAD15NM == "Hartlepool"

plot(eng_df, col = "grey")
plot(eng_df[sel, ], col = "red", add = T) # add = T adds the selection to the plot
# dev.off()

# add some census data ----
bed <- read.csv("./data/bed.csv", stringsAsFactors = F)

sum(!(eng_df$LAD15CD %in% bed$GeographyCode)) 
#' this tells you how many of the LAD15CDs aren't in the bed 'Geography code' 
#' list. The difference seems to be scotland and a few areas that have changed
#' between 2011 and 2015. This shows the lines of code:

eng_df@data[!(eng_df$LAD15CD %in% bed$GeographyCode),]
bed[!(bed$GeographyCode %in% eng_df$LAD15CD),]

bed <- bed %>% rename(LAD15CD = GeographyCode)

## merge on the census data
eng_df@data <- left_join(eng_df@data, bed, by = "LAD15CD")
## make a pct of people who are in 1 bedders column
eng_df$pctbed1 <- round(eng_df$bed1 / eng_df$all * 100, 1)

# qtm plot
qtm(eng_df, "pctbed1", title = "Percent of people in 1 bedroom properties") # makes a nice static plot

# leaflet
## Woo - we're going totally interactive!
pal <- colorBin("Reds", domain = eng84_df$pctbed1, n = 5) # make a little 'palette' function which will plot our colours

leaflet() %>%
  addTiles() %>%
  addPolygons(data = eng84,
              fillColor = ~pal(pctbed1),
              color = "black",
              fillOpacity = 0.6,
              weight = 1,
              popup = ~htmlEscape(LAD15NM)) %>%
  addLegend(position = "bottomright", pal = pal, values = eng84$pctbed1, title = "% single bed hhs")
