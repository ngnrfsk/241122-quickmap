# Working alpha codebase for quickmap
# Version 0.3
# Objectives:
#   done: modify to use create and data in OpenAir format (eg wide rather than long) 
#   done: make into a set of standalone functions to be integrated with OAPL library
#   done: replace redundant code with functions (e.g. use assign_colour in later map
#         layers, and add scope to keep labels in a vector)
# History
#   Original scripts:
#     imports NO2 and schools data from CSV file into a simple format table
#     plot points on a leaflet map with selector by year (time slice)
#     offers more than one colour and labelling scheme, currently WHO NO2 and 
#     small value changes
#   250516: 0.1
#     code adjusted to work with long data, OA format.
#     code works in this current alpha. need to add back in the
#     schools locations on next task.
#   250519: 0.2
#     these steps completed. Checkpoint uploaded to GitHub.
#     fault tolerance for filenames and borough names improved
#   250520: 0.3
#     fixed issue with the assign_color returning one interval too high
#     added code to export each years map as an image
#     added a year label to each map
#     added a label scheme for "lbrut_no2" showing appropriate limits
#     many small adjustments to improve appearance
#   250521: v0.4 now included data from the Breathe London nodes overlaid as
#   squares, and optional Labels in the CSV/DT input file. Code now needs a good
#   bit of simplification as there's a lot that could be reused. 
# To do
#     add a colour scale for PM2.5
#     part done: improve code efficiency
#     move year & title to middle top and add banner text as an input instead of hard coded
#     move controls to one place on the map

# Install and load required packages
packages <- c("leaflet", "sf", "dplyr", "leaflegend", "webshot2","htmlwidgets")

# Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# function definitions ####

# import and prepares the data from csv files in wide format ####
# import_csv_data <- function(file_path, required_cols = c("Easting", "Northing")) {
#   # Read the CSV file
#   data <- read.csv(file_path, stringsAsFactors = FALSE, 
#                    check.names = FALSE, na.strings = c("", "NA", "NaN"))
#   
#   # Clean the data frame column names to remove the X prefix if present
#   names(data) <- gsub("^X", "", names(data))
#   
#   # Check if the required columns are present after cleaning
#   if (!all(required_cols %in% names(data))) {
#     stop(paste("Missing required columns:", paste(setdiff(required_cols, names(data)), collapse = ", ")))
#   }
#   
#   # Filter out rows with NA in any of the required columns
#   data <- data[complete.cases(data[, required_cols]), ]
#   
#   # Identify value columns (excluding coordinate columns)
#   value_columns <- names(data)[!names(data) %in% c("Easting", "Northing")]
#   
#   # Ensure that there is at least one value column
#   if (length(value_columns) == 0) {
#     stop("No value columns found in data")
#   }
#   
#   # Return a list containing the cleaned data and the value columns
#   return(list(data = data, value_columns = value_columns))
# }

import_csv_data <- function(file_path, required_cols = c("Easting", "Northing")) {
  data <- read.csv(file_path, stringsAsFactors = FALSE, 
                   check.names = FALSE, na.strings = c("", "NA", "NaN"))
  names(data) <- gsub("^X", "", names(data))
  if (!all(required_cols %in% names(data))) {
    stop(paste("Missing required columns:", paste(setdiff(required_cols, names(data)), collapse = ", ")))
  }
  if ("Label" %in% names(data)) {
    required_cols <- unique(c(required_cols, "Label"))
  }
  data <- data[complete.cases(data[, required_cols]), ]
  value_columns <- setdiff(names(data), required_cols)
  if (length(value_columns) == 0) {
    stop("No value columns found in data")
  }
  list(data = data, value_columns = value_columns)
}


# Helper function to retrieve borough boundaries (case-insensitive)
get_borough_sf <- function(borough, crs = 4326) {
  # Load the wardBoundaries.Rdata, which has boundaries for London councils
  # and their wards. You can subfilter them using their name either using a single
  # name or a set of names. Lots of error checking due to the scope for typos. 
  # Note the original file only lists one ward for the City
  # Correct known GLA SHP name differences (original 2018 file here: 
  # https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london)
  # Improvement option - update to include the MSOAs and LSOAs, though this would
  # intermix 2014 L/MSOA files with ward boundaries from 2018, which may not
  # be aligned. Check for national source of current data?
  # Returns the result in latitude-longitude coordinates (CRS 4326).
  
  load(file.path(Sys.getenv("DATA_PATH"), "ward_boundaries.Rdata"))
  
  # Standardise input
  borough_input <- tools::toTitleCase(tolower(borough))
  
  # Correct known GLA SHP naming differences
  corrections <- c(
    "The City" = "City and County of the City of London",
    "Westminster" = "City of Westminster",
    "Kingston" = "Kingston upon Thames",
    "Richmond" = "Richmond upon Thames"
  )
  borough_input <- ifelse(borough_input %in% names(corrections),
                          corrections[borough_input],
                          borough_input)
  
  # Return all boroughs if "all" is specified (case-insensitive)
  if (length(borough_input) == 1 && tolower(borough_input) == "all") {
    return(st_transform(wardBoundaries, crs = crs))
  }
  
  # Check if all boroughs are valid
  # Validate borough input with feedback on invalid entries
  valid_districts <- unique(tolower(wardBoundaries$DISTRICT))
  input_check <- tolower(borough_input) %in% valid_districts
  
  # careful error checking here as lots of scope for typos
  if (!all(input_check)) {
    missing <- borough_input[!input_check]
    stop(paste(
      "Error: Borough(s) not found:", paste(missing, collapse = ", "), "\n\n",
      "Accepted borough names are:\n",
      "All, Barking and Dagenham, Barnet, Bexley, Brent,\n",
      "Bromley, Camden, Croydon, Ealing, Enfield,\n",
      "Greenwich, Hackney, Hammersmith and Fulham, Haringey,\n",
      "Harrow, Havering, Hillingdon, Hounslow, Islington,\n",
      "Kensington and Chelsea, Kingston, Lambeth, Lewisham,\n",
      "Merton, Newham, Redbridge, Richmond, Southwark,\n",
      "Sutton, The City, Tower Hamlets, Waltham Forest,\n",
      "Wandsworth, Westminster.\n",
      "Note: input is case-insensitive and City of London wards are incomplete."
    ))
  }
  
  # Filter and return selected boroughs
  wardBoundaries %>%
    filter(tolower(DISTRICT) %in% tolower(borough_input)) %>%
    st_transform(crs = crs)
}

# helper function to convert from OS northings and eastings (CRS 27700) to standard
# latitude-longitude coordinates (CRS 4326) optionally adding year_str (probably
# not needed as moved to OA long data tables)
transform_to_wgs84 <- function(df, 
                               easting = "Easting", 
                               northing = "Northing", 
                               crs_from = 27700) {
  tryCatch({
    sf_obj <- sf::st_as_sf(df, coords = c(easting, northing), crs = crs_from) |>
      sf::st_transform(crs = 4326)
    coords <- sf::st_coordinates(sf_obj)
    sf_obj$Longitude <- coords[, 1]
    sf_obj$Latitude <- coords[, 2]
    
    if ("year" %in% names(df)) {
      sf_obj$year_str <- format(sf_obj$year, "%Y")
    }
    
    sf_obj
  }, error = function(e) {
    stop("Coordinate transformation failed: ", e$message)
  })
}

# Create vignette overlay from extended bounding box ####
create_vignette_overlay <- function(spatial_feature) {
  tryCatch({
    original_bbox <- st_bbox(spatial_feature)
    width <- original_bbox["xmax"] - original_bbox["xmin"]
    height <- original_bbox["ymax"] - original_bbox["ymin"]
    extended_bbox <- c(
      original_bbox["xmin"] - 0.5 * width,
      original_bbox["ymin"] - 0.5 * height,
      original_bbox["xmax"] + 0.5 * width,
      original_bbox["ymax"] + 0.5 * height
    )
    extended_bbox <- st_bbox(extended_bbox, crs = st_crs(spatial_feature))
    bbox_polygon <- st_as_sfc(extended_bbox)
    vignette_overlay <- st_difference(bbox_polygon, st_union(spatial_feature))
    vignette_overlay
  }, error = function(e) {
    stop("Error in bounding box creation: ", e$message)
  })
}

# Unified colour scale definitions ####
colour_scales <- list(
  who_no2 = list(
    colours = c("blue", "green", "yellow", "orange", "#FF4500",
               "#8B0000", "#DA70D6", "#4B0082", "#696969", "black", "white"),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c("< 10: WHO safe level", "10-19: WHO Interim 3", "20-29: WHO Interim 2",
               "30-39: WHO Interim 1/UK Limit", "40-49: Over UK limit",
               "50-60: 5x WHO safe level", "60-70: 6x WHO safe level",
               "70-80: 7x WHO safe level", "80-90: 8x WHO safe level",
               "90-100: 9x WHO safe level", "Site not in use that year"),
    title = "NO2 levels",
    shape = "circle"
  ),
  lbrut_no2 = list(
    colours = c("blue", "green", "yellow", "orange", "#FF4500",
                "#8B0000", "#DA70D6", "#4B0082", "#696969", "black", "white"),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c("< 10: WHO safe level", "10-19: under Richmond limit", "20-29: WHO Interim 2",
               "30-39: under UK limit", "40-49: Over UK limit",
               "50-60: 5x WHO safe level", "60-70: 6x WHO safe level",
               "70-80: 7x WHO safe level", "80-90: 8x WHO safe level",
               "90-100: 9x WHO safe level", "Site not in use that year"),
    title = "NO2 levels"
  ),
  bl_no2 = list(
    colours = c("blue", "green", "yellow", "orange", "#FF4500",
                "#8B0000", "#DA70D6", "#4B0082", "#696969", "black", "white"),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c("< 10: WHO safe level", "10-19: under Richmond limit", "20-29: WHO Interim 2",
               "30-39: under UK limit", "40-49: Over UK limit",
               "50-60: 5x WHO safe level", "60-70: 6x WHO safe level",
               "70-80: 7x WHO safe level", "80-90: 8x WHO safe level",
               "90-100: 9x WHO safe level", "Site not in use that year"),
    title = "NO2 levels"
  ),
  deltas = list(
    colours = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1",
               "#FEE391", "#FEB24C", "#FB6A4A", "#DE2D26", "#A50F15", "white"),
    thresholds = c(Inf, 8, 6, 4, 2, 0, -2, -4, -6, -8, -Inf),
    labels = c(">8", "6-8", "4-6", "2-4", "0-2",
               "Increase 0-2", "increase of 2-4", "increase of 4-6",
               "increase of 6-8", "increase over 8", "Site not in use"),
    title = "Fall in NO2 levels, µg/m3, year on year"
  ),
  schools = list(
    colours = c("#1E90FF", "#32CD32"),
    domain = c("Primary", "Secondary"),
    labels = c("Primary", "Secondary"),
    title = "School Level"
  )
)

# Get colour legend info from unified scale
get_colour_legend <- function(scale = "lbrut_no2") {
  if (!scale %in% names(colour_scales)) {
    stop("Unknown scale: ", scale)
  }
  with(colour_scales[[scale]], list(colors = colours, labels = labels, title = title))
}

# Assign colour to a value based on scale
assign_colour <- function(value, scale = "lbrut_no2") {
  if (is.na(value) || !is.numeric(value)) return("white")
  if (!scale %in% names(colour_scales)) stop("Invalid scale specified.")
  
  thresholds <- colour_scales[[scale]]$thresholds
  colours <- colour_scales[[scale]]$colours
  index <- findInterval(value, thresholds, left.open = FALSE)
  return(colours[index])
}

# Create colour-coded icon set for schools
create_school_icons <- function(school_sf) {
  pal <- colorFactor( # do not spell using English spelling, as no such fn
    palette = c("#1E90FF", "#32CD32"),
    domain = unique(school_sf$Level)
  )
  icons <- makeSymbolsSize(
    values = rep(2, length(school_sf$Level)),
    shape = 'cross',
    color = pal(school_sf$Level),
    fillColor = pal(school_sf$Level),
    baseSize = 10,
    fillOpacity = 0.75
  )
  return(icons)
}

# MAIN FUNCTION create_pollution_map: map data with optional school/points overlay ####
create_pollution_map <- function(
    csv_data_file,
    oa_data_file = "none",
    boroughs,
    school_file = "none",
    output_file = "pollution_map.html",
    image_export = FALSE,
    label_scheme = "who_no2"
) {
  # data loading section
  # first the CSV file of DT 
  result <- import_csv_data(csv_data_file, required_cols = c("Easting", "Northing"))
  
  # Dynamically determine non-value columns
  non_value_cols <- c("Easting", "Northing")
  if ("Label" %in% names(result$data)) {
    non_value_cols <- c(non_value_cols, "Label")
  }
  
  data_long <- result$data |>
    tidyr::pivot_longer(
      cols = -all_of(non_value_cols),
      names_to = "year",
      values_to = "NO2"
    ) |>
    dplyr::mutate(
      year = lubridate::as_datetime(paste0(
        stringr::str_extract(year, "\\d{4}"), "-01-01"))
    )
  
  sf_data_wgs84 <- transform_to_wgs84(data_long)
  
  # then the location of the schools
  if (school_file != "none") {
    tryCatch({
      sf_schools_wgs84 <- transform_to_wgs84(
        import_csv_data(school_file, 
                     required_cols = c("Easting", "Northing", "School", "Level"))$data
      )
      icons <- create_school_icons(sf_schools_wgs84)
    }, error = function(e) {
      warning("Schools file not found: ",school_file," School locations will not be plotted.")
      school_file <- "none"
    })
  }
  
  # then the Breath London data in OpenAir (long) format 
  if (oa_data_file != "none") {
    tryCatch({
      load(file.path(Sys.getenv("DATA_PATH"), oa_data_file), verbose = TRUE)
      bl_annual_means_sf <- dataOAformat |>
             group_by(siteCode, year) |>
             summarise(
                   annual_no2 = mean(no2, na.rm = TRUE),
                   annual_pm25 = mean(pm25, na.rm = TRUE),
                   lat = first(lat),
                   lon = first(lon),
                   yr = as.character(first(year)),
                   .groups = "drop"
               ) |> 
        st_as_sf(coords = c("lon","lat"), crs = 4326) |>
        mutate(year_str = as.character(year))
      coords <- st_coordinates(bl_annual_means_sf)
      bl_annual_means_sf$Longitude <- coords[, 1]
      bl_annual_means_sf$Latitude <- coords[, 2]
    }, error = function(e) {
      warning("Breathe London file not found: ",oa_data_file," School locations will not be plotted.")
      oa_data_file <- "none"
    })
  }
  borough_sf <- tryCatch(
    get_borough_sf(boroughs),
    error = function(e) {
      message(e$message)
      return(NULL)
    }
  )
  if (is.null(borough_sf)) return()
  vignette_overlay <- create_vignette_overlay(borough_sf)
  bbox <- st_bbox(borough_sf)
  legend_info <- get_colour_legend(label_scheme)
  
  # create the main map object
  map <- leaflet(sf_data_wgs84, options = leafletOptions(zoomSnap = 0, zoomDelta = 0.25)) %>%
    addTiles()
  
  # add the dynamic layers
  for (yr in unique(sf_data_wgs84$year_str)) {
    # if required add breathe london data as a bottom layer 
    if (oa_data_file != "none") {
      # Subset and filter OA data for this year
      oa_subset <- bl_annual_means_sf[bl_annual_means_sf$year_str == yr, ]
      

      # v3 kinda vectorised
      if (nrow(oa_subset) > 0) {
        dx <- 0.001
        dy <- dx * cos(oa_subset$Latitude * pi / 180)
        rects <- mapply(
          function(lon, lat, x, y) {
            st_polygon(list(matrix(c(
              lon - x, lat - y,
              lon - x, lat + y,
              lon + x, lat + y,
              lon + x, lat - y,
              lon - x, lat - y
            ), ncol = 2, byrow = TRUE)))
          },
          oa_subset$Longitude, oa_subset$Latitude, dx, dy,
          SIMPLIFY = FALSE
        )
        oa_rect_sf <- st_sf(
          geometry = st_sfc(rects, crs = 4326),
          fillColor = sapply(oa_subset$annual_no2, 
                             assign_colour, scale = label_scheme),
          label = paste("BL NO2: ",round(oa_subset$annual_no2, 1),"ug/m3")
        )
        map <- map %>%
          addPolygons(
            data = oa_rect_sf,
            fillColor = ~fillColor,
            fillOpacity = 0.7,
            color = "black",
            weight = 1,
            label = ~label,
            group = yr
          )
      }
    }
    
    subset_data <- sf_data_wgs84[sf_data_wgs84$year_str == yr, ]
    
    # Create labels based on presence of "Label" column
    labels <- if ("Label" %in% names(subset_data)) {
#      paste(subset_data$Label,":", subset_data$NO2, "ug/m3")
      paste(subset_data$Label)
    } else {
      paste("NO2:", subset_data$NO2, "ug/m3")
    }
    
    map <- map %>%
      addCircleMarkers(
        data = subset_data,
        lng = ~Longitude,
        lat = ~Latitude,
        color = sapply(subset_data$NO2, assign_colour, scale = label_scheme),
        stroke = TRUE,
        radius = 10,
        fillOpacity = 0.7,
        group = yr,
        label = labels,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-size" = "11px",
            "background-color" = "rgba(255,255,255,0.7)",
            "padding" = "1px"
          )
        )
      ) %>% addLabelOnlyMarkers( # this overlays the year on the map
        data = data.frame(
          Longitude = bbox["xmin"] + 0.005,
          Latitude = bbox["ymin"] + 0.005
        ),
        lng = ~Longitude,
        lat = ~Latitude,
        label = yr,
        group = yr,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list(
            "font-weight" = "bold",
            "font-size" = "24px",
            "background-color" = "rgba(255,255,255,0.8)",
            "padding" = "4px"
          )
        )
      ) 
  }
  
  # add borough and wards boundaries, vignette overlay, legend, and
  # a control for which dynamic map layer is displayed
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
        textsize = "11px",
        direction = "auto",
        noHide = FALSE,
        sticky = TRUE
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = legend_info$colors,
      labels = legend_info$labels,
      title = "NO2 levels"
    ) %>%
    addPolygons(
      data = vignette_overlay,
      fillColor = "grey",
      fillOpacity = 0.4,
      color = "transparent",
      weight = 0
    ) %>%
    fitBounds(
      lng1 = unname(bbox["xmin"]),
      lat1 = unname(bbox["ymin"]),
      lng2 = unname(bbox["xmax"]),
      lat2 = unname(bbox["ymax"]),
      options = list(padding = c(30, 30))
    ) %>%
    addLayersControl(
      baseGroups = unique(sf_data_wgs84$year_str),
      options = layersControlOptions(collapsed = FALSE)
    ) 
  
  # add schools markers if they are indicated
  if (school_file != "none") {
    map <- map %>% addMarkers(
      data = sf_schools_wgs84,
      lng = ~Longitude,
      lat = ~Latitude,
      icon = icons,
      label = ~School
    )
  }
  
  tryCatch({
    htmlwidgets::saveWidget(map, file = output_file)
  }, error = function(e) {
    warning("Could not save map to file: ", e$message)
  })
  
  # If image export is enabled, create one image per year
  if (isTRUE(image_export)) {
    dir.create("map_images", showWarnings = FALSE)
    for (yr in unique(sf_data_wgs84$year_str)) {
      subset_data <- sf_data_wgs84[sf_data_wgs84$year_str == yr, ]
      yearly_map <- leaflet(subset_data, 
                            options = 
                              leafletOptions(
                              zoomControl = FALSE,
                              zoomSnap = 0, 
                              zoomDelta = 0.25)) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          color = sapply(subset_data$NO2, assign_colour, scale = label_scheme),
          stroke = TRUE,
          radius = 10,
          fillOpacity = 0.7,
          label = lapply(subset_data$NO2, function(x) paste("NO2 ug/m3 =", x))
        ) %>%
        addPolygons(
          data = vignette_overlay,
          fillColor = "grey",
          fillOpacity = 0.4,
          color = "transparent",
          weight = 0
        ) %>%
        addLabelOnlyMarkers(
          data = data.frame(
            Longitude = bbox["xmin"] + 0.005,
            Latitude = bbox["ymin"] + 0.005
          ),
          lng = ~Longitude,
          lat = ~Latitude,
          label = yr,
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            style = list(
              "font-weight" = "bold",
              "font-size" = "24px",
              "background-color" = "rgba(255,255,255,0.8)",
              "padding" = "4px"
            )
          )
        ) %>%
        addPolygons(data = borough_sf, 
                    color = "#078141", 
                    weight = 2.5, 
                    fillColor = "transparent"
                    ) %>%
        addLegend(
          position = "bottomright",
          colors = legend_info$colors,
          labels = legend_info$labels,
          title = "NO2 levels"
        ) %>%
        fitBounds(
          lng1 = unname(bbox["xmin"]),
          lat1 = unname(bbox["ymin"]),
          lng2 = unname(bbox["xmax"]),
          lat2 = unname(bbox["ymax"])
        )
      
      # Decompose the base file name for use with year suffixes
      file_parts <- tools::file_path_sans_ext(basename(output_file))
      file_ext <- tools::file_ext(output_file)
      
      html_file <- file.path("map_images", paste0(file_parts, "_", yr, ".html"))
      img_file <- file.path("map_images", paste0(file_parts, "_", yr, ".jpg"))
      
      saveWidget(yearly_map, file = html_file, selfcontained = TRUE)
      webshot2::webshot(url = html_file, file = img_file, vwidth = 900, vheight = 700)
    }
    return(invisible(map))
  }
  return(invisible(map))
}


# Main script ####

map_to_display <- create_pollution_map(
  csv_data_file = "your_data_Richmond.csv",
  oa_data_file = "complete_breathe_london_210122-250422.Rdata",
  boroughs = c("Richmond"),
  output_file = "richmond_no2_map_asr.html",
  image_export = F,
  label_scheme = "lbrut_no2"
)

map_to_display
