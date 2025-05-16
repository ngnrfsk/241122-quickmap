# Working alpha codebase for quickmap
# current verion 0.1
# Includes:
#   importing NO2 and schools data from CSV file into a simple format table
#   plot points on a leaflet map with selector by year (time slice)
#   offers more than one colour and labelling scheme, currently WHO NO2 and 
#   small value changes
# Objectives:
#   modify to use create and data in OpenAir format (that is wide rather than long)
#   make into a set of standalone functions to be integrated with OAPL library
#   improve code efficiency and fault tolerance
#   replace redundant code with functions (e.g. use assign_color in later map
#   layers, and add scope to keep labels in a vector)


# Install and load required packages
packages <- c("leaflet", "sf", "dplyr", "leaflegend")

# Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)


prepare_data <- function(file_path, required_cols = c("Easting", "Northing")) {
  # Read the CSV file
  data <- read.csv(file_path, stringsAsFactors = FALSE, 
                   check.names = FALSE, na.strings = c("", "NA", "NaN"))
  
  # Clean the data frame column names to remove the X prefix if present
  names(data) <- gsub("^X", "", names(data))
  
  # Check if the required columns are present after cleaning
  if (!all(required_cols %in% names(data))) {
    stop(paste("Missing required columns:", paste(setdiff(required_cols, names(data)), collapse = ", ")))
  }
  
  # Filter out rows with NA in any of the required columns
  data <- data[complete.cases(data[, required_cols]), ]
  
  # Identify value columns (excluding coordinate columns)
  value_columns <- names(data)[!names(data) %in% c("Easting", "Northing")]
  
  # Ensure that there is at least one value column
  if (length(value_columns) == 0) {
    stop("No value columns found in data")
  }
  
  # Return a list containing the cleaned data and the value columns
  return(list(data = data, value_columns = value_columns))
}


result <- prepare_data('your_data_Merton.csv', required_cols = c("Easting", "Northing"))
data <- result$data
value_columns <- result$value_columns

result <- prepare_data('your_schools_Merton.csv', required_cols = c("Easting", "Northing", "School", "Level"))
schools <- result$data
value_columns_schools <- result$value_columns

# Create sf objects of the data that includes the coordinates in the required
# coordinate system (CRS) with error handling
tryCatch({
    sf_data <- st_as_sf(data, coords = c("Easting", "Northing"), crs = 27700)
    sf_data_wgs84 <- st_transform(sf_data, crs = 4326)
    sf_data_wgs84$Longitude <- st_coordinates(sf_data_wgs84)[,1]
    sf_data_wgs84$Latitude <- st_coordinates(sf_data_wgs84)[,2]
}, error = function(e) {
    stop("Error in coordinate transformation: ", e$message)
})

tryCatch({
  sf_schools <- st_as_sf(schools, coords = c("Easting", "Northing"), crs = 27700)
  sf_schools_wgs84 <- st_transform(sf_schools, crs = 4326)
  sf_schools_wgs84$Longitude <- st_coordinates(sf_schools_wgs84)[,1]
  sf_schools_wgs84$Latitude <- st_coordinates(sf_schools_wgs84)[,2]
}, error = function(e) {
  stop("Error in coordinate transformation: ", e$message)
})


# Function to assign color based on value with NA handling
assign_color <- function(value, scale = 'who_no2') {
  # Return white if the value is NA or not numeric
  if (is.na(value)) return("white")
  if (!is.numeric(value)) return("white")
  
  # Define named vectors for each scale, using thresholds as names
  # the delta colour scheme is intended for changes between years or trends
  delta_colors <- c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#FEE391", "#FEB24C", "#FB6A4A", "#DE2D26", "#A50F15")
  delta_thresholds <- c(8, 6, 4, 2, 0, -2, -4, -6, -8, -Inf)
  # the who colours are an absolute scale, based on the WHO guidelines for NO2
  who_colors <- c("blue", "green", "yellow", "orange", "red", "darkred", "#8E388E", "purple", "#4D4D4D", "black")
  who_thresholds <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, Inf)
  
  # Use findInterval to get the correct index based on the value
  if (scale == 'delta') {
    index <- findInterval(value, delta_thresholds, left.open = TRUE)
    return(delta_colors[index])
  } else if (scale == 'who_no2') {
    index <- findInterval(value, who_thresholds, left.open = TRUE)
    return(who_colors[index])
  } else {
    stop("Invalid scale specified. Choose 'delta' or 'who'.")
  }
}



# Create a proper color palette function using colorFactor
pal <- colorFactor(
  palette = c("#1E90FF", "#32CD32"),  # Colors for Primary, Secondary, Other
  domain = unique(sf_schools_wgs84$Level)
)

icons <- makeSymbolsSize(
   values = rep(2,length(sf_schools_wgs84$Level)),
   shape = 'cross',
   color = pal(sf_schools_wgs84$Level),
   fillColor = pal(sf_schools_wgs84$Level),
   baseSize = 10,
   fillOpacity = 0.7
  )


# The wardBoundaries.Rdata file contains boundaries for all the London councils
# and their wards. You can subfilter them using their name either using a single
# name or a set of names.
# The valid names are: "Kingston upon Thames", "Croydon", "Bromley", "Hounslow",
# "Ealing", "Havering", "Hillingdon", "Harrow", "Brent", "Barnet", "Lambeth",
# "Southwark", "Lewisham", "Greenwich", "Bexley", "Enfield", "Waltham Forest",
# "Redbridge", "Sutton", "Richmond upon Thames", "Merton", "Wandsworth",
# "Hammersmith and Fulham", "Kensington and Chelsea", "City of Westminster",
# "Camden", "Tower Hamlets", "Islington", "Hackney", "Haringey", "Newham",
# "Barking and Dagenham", "City and County of the City of London"
load("wardBoundaries.Rdata")
borough <- c("Merton")

# Filter borough boundaries with error checking
if (!borough %in% wardBoundaries$DISTRICT) {
    stop("Borough not found in wardBoundaries")
}
borough_sf <- wardBoundaries %>%
    filter(DISTRICT %in% borough) %>%
    st_transform(crs = 4326)

# Create extended bounding box with error checking
tryCatch({
    original_bbox <- st_bbox(borough_sf)
    width <- original_bbox["xmax"] - original_bbox["xmin"]
    height <- original_bbox["ymax"] - original_bbox["ymin"]
    extended_bbox <- c(
        original_bbox["xmin"] - 0.5 * width,
        original_bbox["ymin"] - 0.5 * height,
        original_bbox["xmax"] + 0.5 * width,
        original_bbox["ymax"] + 0.5 * height
    )
    extended_bbox <- st_bbox(extended_bbox, crs = st_crs(borough_sf))
    bbox_polygon <- st_as_sfc(extended_bbox)
    inverse_polygon <- st_difference(bbox_polygon, st_union(borough_sf))
}, error = function(e) {
    stop("Error in bounding box creation: ", e$message)
})

# Create map with dynamic layers
map <- leaflet(sf_data_wgs84, options = leafletOptions(zoomSnap = 0, zoomDelta = 0.25)) %>%
     addTiles() 

# Add layers dynamically for each value column
for(col in value_columns) {
    if (!col %in% names(sf_data_wgs84)) {
        warning(paste("Column", col, "not found in data, skipping"))
        next
    }

    # Create the markers without using formula notation for color
    map <- map %>%
        addCircleMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            color = sapply(sf_data_wgs84[[col]], assign_color),
            stroke = TRUE,
            radius = 10,
            fillOpacity = 0.7,
            label = lapply(sf_data_wgs84[[col]], function(x) paste("NO2 ug/m3 =", x)),
            group = col
        )
}

# Add remaining map elements
map <- map %>%
    addPolygons(
        data = borough_sf,
        color = "#078141",
        weight = 2.5,
        dashArray = "5, 10",
        opacity = 0.75,
        fillColor = "transparent",
        fillOpacity = 0.1,
        label = ~NAME,
        labelOptions = labelOptions(
            style = list(
                "font-weight" = "bold",
                padding = "3px 8px",
                "background-color" = "rgba(255,255,255,0.7)",
                "border-color" = "rgba(0,0,0,0.1)",
                "border-radius" = "4px"
            ),
            textsize = "12px",
            direction = "auto",
            permanent = TRUE,
            sticky = TRUE
        )
    ) %>%
    addLegend(
    position = "bottomright",
    colors = c("blue", "green", "yellow", "orange", "red", 
               "#8B0000", "#8E388E", "purple", "#4D4D4D", "#000000", "white"),
    labels = c("< 10: WHO safe level", 
               "10-19: WHO Interim 3", 
               "20-29: WHO Interim 2",
               "30-39: WHO Interim 1/UK Limit", 
               "40-49: Over UK limit",
               "50-60: 5x WHO safe level", 
               "60-70: 6x WHO safe level",
               "70-80: 7x WHO safe level", 
               "80-90: 8x WHO safe level",
               "90-100: 9x WHO safe level", 
               "Site not in use that year"),
    title = "NO2 levels"
    ) %>%
    addPolygons(
        data = inverse_polygon,
        fillColor = "grey",
        fillOpacity = 0.4,
        color = "transparent",
        weight = 0
    ) %>%
  fitBounds(
    lng1 = unname(original_bbox["xmin"]),
    lat1 = unname(original_bbox["ymin"]),
    lng2 = unname(original_bbox["xmax"]),
    lat2 = unname(original_bbox["ymax"]),
    options = list(padding = c(30, 30))
  ) %>%
    addLayersControl(
        baseGroups = value_columns,
        options = layersControlOptions(collapsed = FALSE)
    ) %>% addMarkers(
      data = sf_schools_wgs84,
      lng = ~Longitude,
      lat = ~Latitude,
      icon = icons,
      label = ~School
    )

# Display the map
map

# Save the map with error handling
tryCatch({
    library(htmlwidgets)
    saveWidget(map, file = "Wandsworth_Merton_Schools_NO2.html")
}, error = function(e) {
    warning("Could not save map to file: ", e$message)
})

