# Working, stable production codebase for quickmap ####
# Version 0.8 - Complete Architectural Refactor (Phase 3C Final)

# MAJOR REFACTOR COMPLETED - All objectives achieved:
# ✅ Unified marker-based architecture (circles=DT, diamonds=BL, crosses=schools)
# ✅ Single loop processing for HTML and image generation
# ✅ Configuration-driven layer system for easy extensibility
# ✅ Generic icon system replacing all hardcoded layer creation
# ✅ Integrated image export with unified layer processing
# ✅ Performance optimization: eliminated slow polygon rendering
# ✅ Modular, reusable mapping code for both HTML and static export
# ✅ Separated data loading from layer creation with generic interfaces

# Current Architecture:
#   get_measurement_layers() → generate_map_layers() → unified HTML + image output
#   All layers use: prepare_generic_layer_data() → create_generic_icons() → addMarkers()

# Future Enhancement Opportunities (Phase 4):
#   4A: File management optimization (eliminate _files folders, temp file cleanup)
#   4B: Configuration system enhancement (external config files, custom icons)
#   4C: Performance and scalability (lazy loading, batch processing)
#   4D: Add database import using duckdb
#   4E: User experience enhancements (clustering, custom popups, export formats)
#   4E: Error handling and robustness (validation, logging, graceful failures)

# Benefits Achieved:
#   - Easy layer addition: New pollutants/data sources via configuration only
#   - Unified output: HTML and images use identical layer processing
#   - Maintainable: Single codebase for all layer types
#   - Extensible: Clean foundation for future requirements
#   - Performant: Optimized marker rendering throughout

# History:
#   Original scripts (pre-0.1):
#     Basic NO2 and schools data import from CSV
#     Simple leaflet map with year selector
#     Basic colour schemes (WHO NO2, small value changes)
#   250516: v0.1 - Adjusted for long data, OA format compatibility
#   250519: v0.2 - Added schools locations, improved error handling
#   250520: v0.3 - Fixed assign_color interval issue, added image export, year labels
#   250521: v0.4 - Added Breathe London nodes as squares, optional CSV labels
#   250522: v0.5 - Added year selector functionality
#   250620: v0.6 - Removed white circles for NA values, OpenAir format support
#   250620: v1.0 - COMPLETE REFACTOR: Unified architecture with single loop processing

# Core Functions:
#   create_pollution_map() - Main function with unified single-loop processing
#   generate_map_layers() - Unified layer generation for HTML and images
#   create_generic_icons() - Universal icon system for all layer types
#   get_measurement_layers() - Configuration-driven layer definitions
#   prepare_generic_layer_data() - Unified data preparation
#   add_generic_layer() - Universal layer addition

# Install and load required packages
packages <- c(
  "leaflet",
  "sf",
  "dplyr",
  "leaflegend",
  "tidyr",
  "lubridate",
  "stringr",
  "webshot2",
  "htmlwidgets"
)

# Install missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)

# function definitions ####

# import and prepares the data from csv files in wide format ####
import_csv_data <- function(
  file_path,
  required_cols = c("Easting", "Northing")
) {
  data <- read.csv(
    file_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA", "NaN")
  )
  names(data) <- gsub("^X", "", names(data))
  if (!all(required_cols %in% names(data))) {
    stop(paste(
      "Missing required columns:",
      paste(setdiff(required_cols, names(data)), collapse = ", ")
    ))
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
  borough_input <- ifelse(
    borough_input %in% names(corrections),
    corrections[borough_input],
    borough_input
  )

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
      "Error: Borough(s) not found:",
      paste(missing, collapse = ", "),
      "\n\n",
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
transform_to_wgs84 <- function(
  df,
  easting = "Easting",
  northing = "Northing",
  crs_from = 27700
) {
  tryCatch(
    {
      sf_obj <- sf::st_as_sf(
        df,
        coords = c(easting, northing),
        crs = crs_from
      ) |>
        sf::st_transform(crs = 4326)
      coords <- sf::st_coordinates(sf_obj)
      sf_obj$Longitude <- coords[, 1]
      sf_obj$Latitude <- coords[, 2]

      if ("year" %in% names(df)) {
        sf_obj$year_str <- format(sf_obj$year, "%Y")
      }

      sf_obj
    },
    error = function(e) {
      stop("Coordinate transformation failed: ", e$message)
    }
  )
}

# Create vignette overlay from extended bounding box ####
create_vignette_overlay <- function(spatial_feature) {
  tryCatch(
    {
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
    },
    error = function(e) {
      stop("Error in bounding box creation: ", e$message)
    }
  )
}

# Unified colour scale definitions ####
colour_scales <- list(
  who_no2 = list(
    colours = c(
      "blue",
      "green",
      "yellow",
      "orange",
      "#FF4500",
      "#8B0000",
      "#DA70D6",
      "#4B0082",
      "#696969",
      "black",
      "white"
    ),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c(
      "< 10: WHO safe level",
      "10-19: WHO Interim 3",
      "20-29: WHO Interim 2",
      "30-39: WHO Interim 1/UK Limit",
      "40-49: Over UK limit",
      "50-60: 5x WHO safe level",
      "60-70: 6x WHO safe level",
      "70-80: 7x WHO safe level",
      "80-90: 8x WHO safe level",
      "90-100: 9x WHO safe level",
      "Site not in use that year"
    ),
    title = "NO2 levels",
    shape = "circle"
  ),
  lbrut_no2 = list(
    colours = c(
      "blue",
      "green",
      "yellow",
      "orange",
      "#FF4500",
      "#8B0000",
      "#DA70D6",
      "#4B0082",
      "#696969",
      "black",
      "white"
    ),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c(
      "< 10: WHO safe level",
      "10-19: Under Richmond target",
      "20-29: WHO Interim 2",
      "30-39: Under UK target",
      "40-49: Over UK target",
      "50-60: 5x WHO safe level",
      "60-70: 6x WHO safe level",
      "70-80: 7x WHO safe level",
      "80-90: 8x WHO safe level",
      "90-100: 9x WHO safe level",
      "Site not in use that year"
    ),
    title = "NO2 levels"
  ),
  lbw_no2 = list(
    colours = c(
      "blue",
      "green",
      "yellow",
      "orange",
      "#FF4500",
      "#8B0000",
      "#DA70D6",
      "#4B0082",
      "#696969",
      "black",
      "white"
    ),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c(
      "< 10: WHO safe level",
      "10-19: WHO Interim 3",
      "20-29: Under Wandsworth target",
      "30-39: Under UK target",
      "40-49: Over UK target",
      "50-60: 5x WHO safe level",
      "60-70: 6x WHO safe level",
      "70-80: 7x WHO safe level",
      "80-90: 8x WHO safe level",
      "90-100: 9x WHO safe level",
      "Site not in use that year"
    ),
    title = "NO2 levels"
  ),
  lbm_no2 = list(
    colours = c(
      "blue",
      "green",
      "yellow",
      "orange",
      "#FF4500",
      "#8B0000",
      "#DA70D6",
      "#4B0082",
      "#696969",
      "black",
      "white"
    ),
    thresholds = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
    labels = c(
      "< 10: WHO safe level",
      "10-19: WHO Interim 3",
      "20-29: WHO Interim 2",
      "30-39: UK/WHO Interim 1 target",
      "40-49: Over UK target",
      "50-60: 5x WHO safe level",
      "60-70: 6x WHO safe level",
      "70-80: 7x WHO safe level",
      "80-90: 8x WHO safe level",
      "90-100: 9x WHO safe level",
      "Site not in use that year"
    ),
    title = "NO2 levels"
  ),
  gla_pm25 = list(
    colours = c(
      "darkblue", # 0–5
      "blue", # 5–7.5
      "lightgreen", # 7.5–10
      "yellow", # 10–12.5
      "orange", # 12.5–15
      "darkorange", # 15–20
      "red", # 20–25
      "darkred", # 25–Inf
      "white" # NA
    ),
    thresholds = c(0, 5, 7.5, 10, 12.5, 15, 20, 25, Inf),
    labels = c(
      "< 5: WHO safe level",
      "5-7.5",
      "7.5-10 Under GLA/WHO Interim 1 target",
      "10-12.5",
      "12.5-15: Under WHO Interim 2 target",
      "15-20: Under UK target",
      "20-25: Under WHO Interim 2 target",
      "> 25",
      "Site not in use that year"
    ),
    title = "PM2.5 annual ug/m3"
  ),
  deltas = list(
    colours = c(
      "#084594",
      "#2171B5",
      "#4292C6",
      "#6BAED6",
      "#9ECAE1",
      "#FEE391",
      "#FEB24C",
      "#FB6A4A",
      "#DE2D26",
      "#A50F15",
      "white"
    ),
    thresholds = c(Inf, 8, 6, 4, 2, 0, -2, -4, -6, -8, -Inf),
    labels = c(
      ">8",
      "6-8",
      "4-6",
      "2-4",
      "0-2",
      "Increase 0-2",
      "increase of 2-4",
      "increase of 4-6",
      "increase of 6-8",
      "increase over 8",
      "Site not in use"
    ),
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
  with(
    colour_scales[[scale]],
    list(colors = colours, labels = labels, title = title)
  )
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

create_generic_icons <- function(
  data,
  layer_type,
  pollutant = NULL,
  scale_to_use = NULL
) {
  # Determine shape and size
  shape_config <- switch(
    layer_type,
    "schools" = list(shape = 'cross', size = 10), # Keep current working size
    "dt_sites" = list(shape = 'circle', size = 20), # Keep current working size
    "bl_nodes" = list(shape = 'diamond', size = 20), # Keep current working size
    stop("Unknown layer type: ", layer_type)
  )

  # Determine colors based on layer type
  colors <- switch(
    layer_type,
    "schools" = {
      # Use colorFactor for categorical school data (same logic as create_school_icons)
      pal <- colorFactor(
        palette = c("#1E90FF", "#32CD32"),
        domain = unique(data$Level)
      )
      pal(data$Level)
    },
    "dt_sites" = {
      # Use assign_colour for continuous pollution data
      sapply(data[[pollutant]], assign_colour, scale = scale_to_use)
    },
    "bl_nodes" = {
      # Use assign_colour for continuous pollution data
      sapply(data[[pollutant]], assign_colour, scale = scale_to_use)
    }
  )

  # Create icons with unified approach
  makeSymbolsSize(
    values = rep(1, length(colors)),
    shape = shape_config$shape,
    color = colors, # Use data colors (your proven approach)
    fillColor = colors,
    baseSize = shape_config$size,
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1
  )
}

# STEP 1: Create layer configuration system
get_measurement_layers <- function(
  csv_data_file,
  oa_data_file,
  school_file,
  use_data_labels
) {
  list(
    bl_nodes = list(
      enabled = (oa_data_file != "none"),
      data_source = "bl_annual_means_sf",
      layer_type = "bl_nodes",
      temporal = TRUE,
      prepare_function = "prepare_bl_layer_data"
    ),
    dt_sites = list(
      enabled = (csv_data_file != "none"),
      data_source = "sf_data_wgs84",
      layer_type = "dt_sites",
      temporal = TRUE,
      prepare_function = "prepare_dt_layer_data",
      options = list(use_data_labels = use_data_labels)
    ),
    schools = list(
      enabled = (school_file != "none"),
      data_source = "sf_schools_wgs84",
      layer_type = "schools",
      temporal = FALSE,
      prepare_function = "prepare_static_layer_data"
    )
  )
}

# STEP 2: Create data preparation function
prepare_generic_layer_data <- function(
  layer_config,
  year_data,
  pollutant = NULL,
  scale_to_use = NULL
) {
  # Route to appropriate preparation function based on layer type
  switch(
    layer_config$layer_type,
    "bl_nodes" = {
      prepare_bl_layer_data(year_data, pollutant, scale_to_use)
    },
    "dt_sites" = {
      use_labels <- if (!is.null(layer_config$options))
        layer_config$options$use_data_labels else FALSE
      prepare_dt_layer_data(year_data, pollutant, scale_to_use, use_labels)
    },
    "schools" = {
      prepare_static_layer_data(year_data)
    },
    stop("Unknown layer type: ", layer_config$layer_type)
  )
}

# STEP 3: Create generic layer addition function
add_generic_layer <- function(
  map,
  layer_data,
  layer_config,
  year = NULL,
  pollutant = NULL,
  scale_to_use = NULL
) {
  # Route to appropriate addition function based on layer type
  switch(
    layer_config$layer_type,
    "bl_nodes" = {
      add_bl_layer(map, layer_data, year, pollutant, scale_to_use)
    },
    "dt_sites" = {
      add_dt_layer(map, layer_data, year, pollutant, scale_to_use)
    },
    "schools" = {
      add_static_layer(map, layer_data)
    },
    stop("Unknown layer type: ", layer_config$layer_type)
  )
}

# Gneralise static layer preparation code
prepare_static_layer_data <- function(static_sf) {
  # Pre-compute labels for schools
  labels <- static_sf$School

  # Return structured data ready for generic processing (same pattern as other layers)
  list(
    data = static_sf,
    labels = labels,
    layer_type = "schools" # Add layer type for generic processing
  )
}

# Generalised static layer addition code
add_static_layer <- function(map, static_layer_data) {
  # Use generic icon system instead of create_school_icons
  icons <- create_generic_icons(static_layer_data$data, "schools")

  map %>%
    addMarkers(
      data = static_layer_data$data,
      lng = ~Longitude,
      lat = ~Latitude,
      icon = icons, # Now using generic icons
      label = static_layer_data$labels
      # Note: Schools don't use group parameter (they're static)
    )
}

# STEP 4: Create data subset function for temporal layers
get_layer_year_data <- function(data_source_name, year, data_environment) {
  # Get the actual data object from the calling environment
  data_source <- get(data_source_name, envir = data_environment)

  # Filter by year if it's temporal data
  if (year != "static") {
    data_source[data_source$year_str == year, ]
  } else {
    data_source
  }
}

# NEW FUNCTION: Add HTML controls (multi-year interactive map)
add_html_controls <- function(
  map,
  legend_info,
  title_prefix,
  years_to_plot,
  borough_sf,
  vignette_overlay,
  vignette_overlay_on,
  bbox
) {
  map <- map %>%
    # Ward and borough boundaries
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
    # Legend
    addLegend(
      position = "bottomright",
      colors = legend_info$colors,
      labels = legend_info$labels,
      title = legend_info$title
    ) %>%
    # Fit bounds
    fitBounds(
      lng1 = unname(bbox["xmin"]),
      lat1 = unname(bbox["ymin"]),
      lng2 = unname(bbox["xmax"]),
      lat2 = unname(bbox["ymax"]),
      options = list(padding = c(30, 30))
    ) %>%
    # Multi-year layer control
    addLayersControl(
      baseGroups = years_to_plot,
      options = layersControlOptions(collapsed = FALSE, position = 'topleft')
    ) %>%
    # Title control
    addControl(
      html = htmltools::HTML(
        sprintf(
          '<div style="
        background-color: rgba(255,255,255,0.8);
        padding: 2px 2px;
        border-radius: 3px;
        text-align: center;
        margin-top: 4px;
        line-height: 1.5;
        width: 50vw;
        margin-left: auto;
        margin-right: auto;
      ">%s</div>',
          title_prefix
        )
      ),
      position = "topright"
    )

  # Add vignette overlay if enabled
  if (vignette_overlay_on) {
    map <- map %>%
      addPolygons(
        data = vignette_overlay,
        fillColor = "grey",
        fillOpacity = 0.4,
        color = "transparent",
        weight = 0
      )
  }

  return(map)
}

# NEW FUNCTION: Add yearly controls (single year static map)
add_yearly_controls <- function(
  map,
  legend_info,
  title_prefix,
  yr,
  borough_sf,
  vignette_overlay,
  vignette_overlay_on,
  bbox
) {
  map <- map %>%
    # Ward and borough boundaries (same as HTML)
    addPolygons(
      data = borough_sf,
      color = "#078141",
      weight = 2.5,
      fillColor = "transparent"
    ) %>%
    # Legend (same as HTML)
    addLegend(
      position = "bottomright",
      colors = legend_info$colors,
      labels = legend_info$labels,
      title = legend_info$title
    ) %>%
    # Fit bounds (same as HTML)
    fitBounds(
      lng1 = unname(bbox["xmin"]),
      lat1 = unname(bbox["ymin"]),
      lng2 = unname(bbox["xmax"]),
      lat2 = unname(bbox["ymax"])
    ) %>%
    # Year-specific title
    addControl(
      html = htmltools::HTML(
        paste0(
          '<div style="
        background-color: rgba(255,255,255,0.8);
        padding: 2px 2px;
        border-radius: 3px;
        text-align: center;
        margin-top: 4px;
        width: 95vw;
        margin-left: auto;
        margin-right: auto;
      ">',
          title_prefix,
          " ",
          yr,
          '</div>'
        )
      ),
      position = "topright"
    )

  # Add vignette overlay if enabled
  if (vignette_overlay_on) {
    map <- map %>%
      addPolygons(
        data = vignette_overlay,
        fillColor = "grey",
        fillOpacity = 0.4,
        color = "transparent",
        weight = 0
      )
  }

  return(map)
}

# NEW FUNCTION: Unified layer generation for both HTML and image export
generate_map_layers <- function(
  base_map,
  measurement_layers,
  target_year,
  pollutant,
  scale_to_use,
  data_env
) {
  for (layer_name in names(measurement_layers)) {
    layer_config <- measurement_layers[[layer_name]]
    if (!layer_config$enabled) next

    # Handle temporal vs static layers
    if (layer_config$temporal) {
      # For temporal layers, only process if we have a specific year
      if (target_year != "static_only") {
        year_data <- get_layer_year_data(
          layer_config$data_source,
          target_year,
          data_env
        )
        if (nrow(year_data) == 0) next

        # Filter pollution data for DT and BL layers
        if (layer_config$layer_type %in% c("dt_sites", "bl_nodes")) {
          year_data <- dplyr::filter(year_data, !is.na(.data[[pollutant]]))
          if (nrow(year_data) == 0) next
        }

        layer_data <- prepare_generic_layer_data(
          layer_config,
          year_data,
          pollutant,
          scale_to_use
        )
        if (!is.null(layer_data)) {
          base_map <- add_generic_layer(
            base_map,
            layer_data,
            layer_config,
            target_year,
            pollutant,
            scale_to_use
          )
        }
      }
    } else {
      # Static layers (schools, hospitals, etc.) - always process
      static_data <- get(layer_config$data_source, envir = data_env)
      layer_data <- prepare_generic_layer_data(layer_config, static_data)
      base_map <- add_generic_layer(base_map, layer_data, layer_config)
    }
  }
  return(base_map)
}

# MAIN FUNCTION create_pollution_map: map data with optional school/points overlay ####
create_pollution_map <- function(
  csv_data_file = "none",
  oa_data_file = "none",
  boroughs,
  school_file = "none",
  output_file = "pollution_map.html",
  image_export = FALSE,
  scale_to_use = "who_no2",
  title_prefix = "",
  vignette_overlay_on = TRUE,
  legend_title = "Annual NO2, ug/m3",
  years_to_plot = NULL,
  map_width_px = 1200,
  use_data_labels = FALSE,
  pollutant = "no2",
  map_title = "Air pollution map"
) {
  # data loading section

  # Load the diffusion tube data from CSV files
  if (csv_data_file != "none") {
    # first the CSV file of DT
    result <- import_csv_data(
      csv_data_file,
      required_cols = c("Easting", "Northing")
    )

    # Dynamically determine non-value columns
    non_value_cols <- c("Easting", "Northing")
    if ("Label" %in% names(result$data)) {
      non_value_cols <- c(non_value_cols, "Label")
    }

    # convert to long format to suit OpenAir
    data_long <- result$data |>
      tidyr::pivot_longer(
        cols = -all_of(non_value_cols),
        names_to = "year",
        values_to = "no2"
      ) |>
      dplyr::mutate(
        year = lubridate::as_datetime(paste0(
          stringr::str_extract(year, "\\d{4}"),
          "-01-01"
        ))
      )

    sf_data_wgs84 <- transform_to_wgs84(data_long)
  }
  # create directory to hold the output files
  if (!dir.exists("aq_maps")) dir.create("aq_maps", showWarnings = TRUE)

  # then the location of the schools
  if (school_file != "none") {
    tryCatch(
      {
        # Step 1: Import CSV data
        school_import <- import_csv_data(
          school_file,
          required_cols = c("Easting", "Northing")
        )

        # Step 2: Transform to WGS84 (e.g. lat-lon coordinates)
        sf_schools_wgs84 <- school_import$data |>
          transform_to_wgs84()

        # Step 3: Icons are now created via generic system - no need for create_school_icons()
      },
      error = function(e) {
        warning(
          "Schools file not found or error in processing: ",
          school_file,
          " School locations will not be plotted.\n",
          "Error message: ",
          e$message
        )
        school_file <- "none"
      }
    )
  }

  if (oa_data_file != "none") {
    tryCatch(
      {
        load(paste0(
          "~/Coding/R projects/RSPcoding/Library/data/",
          oa_data_file
        ))
        bl_annual_means_sf <- dataOAformat |>
          group_by(siteCode, year) |>
          summarise(
            no2 = mean(no2, na.rm = TRUE),
            pm25 = mean(pm25, na.rm = TRUE),
            lat = first(lat),
            lon = first(lon),
            yr = as.character(first(year)),
            .groups = "drop"
          ) |>
          st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
          mutate(year_str = as.character(year))
        coords <- st_coordinates(bl_annual_means_sf)
        bl_annual_means_sf$Longitude <- coords[, 1]
        bl_annual_means_sf$Latitude <- coords[, 2]
      },
      error = function(e) {
        warning(
          "Breathe London file not found: ",
          oa_data_file
        )
        oa_data_file <- "none"
      }
    )
    if (!pollutant %in% colnames(dataOAformat)) {
      warning(
        paste0(
          "Required pollutant ('",
          pollutant,
          "') not in data file: ",
          oa_data_file
        )
      )
      oa_data_file <- "none"
    }
  }

  # setup the bounding box and overlay
  borough_sf <- tryCatch(
    get_borough_sf(boroughs),
    error = function(e) {
      message(e$message)
      return(NULL)
    }
  )
  if (is.null(borough_sf)) return()
  if (vignette_overlay_on)
    vignette_overlay <- create_vignette_overlay(borough_sf)
  bbox <- st_bbox(borough_sf)
  legend_info <- get_colour_legend(scale_to_use)

  # if the Diffusion Tube data isn't to be used, use the BL data for the map
  if (csv_data_file == "none") sf_data_wgs84 <- bl_annual_means_sf

  # create the main HTML map object
  map <- leaflet(
    sf_data_wgs84,
    options = leafletOptions(zoomSnap = 0, zoomDelta = 0.25)
  ) %>%
    addTiles()

  # subselect for the years to be plotted
  if (is.null(years_to_plot)) {
    years_to_plot <- unique(sf_data_wgs84$year_str)
  } else {
    years_to_plot <- intersect(years_to_plot, unique(sf_data_wgs84$year_str))
  }

  # add the dynamic layers to the HMTL map
  # MODIFIED: Sections 2 & 3 in create_pollution_map function
  # Replace both sections with this unified approach:

  # Get layer configuration
  measurement_layers <- get_measurement_layers(
    csv_data_file,
    oa_data_file,
    school_file,
    use_data_labels
  )

  # If image export is enabled, create one image per year as well as the live HTML map

  # Initialize HTML map
  html_map <- leaflet(
    sf_data_wgs84,
    options = leafletOptions(zoomSnap = 0, zoomDelta = 0.25)
  ) %>%
    addTiles()

  # Get layer configuration
  measurement_layers <- get_measurement_layers(
    csv_data_file,
    oa_data_file,
    school_file,
    use_data_labels
  )

  # SINGLE LOOP: Process both HTML accumulation and yearly image export
  for (yr in unique(years_to_plot)) {
    # Add layers to HTML map (accumulative - builds multi-year interactive map)
    html_map <- generate_map_layers(
      html_map,
      measurement_layers,
      yr,
      pollutant,
      scale_to_use,
      environment()
    )

    # Generate yearly image if export enabled
    if (image_export) {
      # Create fresh yearly map
      yearly_map <- leaflet(
        options = leafletOptions(
          zoomControl = FALSE,
          zoomSnap = 0,
          zoomDelta = 0.25
        )
      ) %>%
        addTiles()

      # Add layers for this specific year
      yearly_map <- generate_map_layers(
        yearly_map,
        measurement_layers,
        yr,
        pollutant,
        scale_to_use,
        environment()
      )

      # Add static layers (schools, etc.) to yearly map
      yearly_map <- generate_map_layers(
        yearly_map,
        measurement_layers,
        "static_only",
        pollutant,
        scale_to_use,
        environment()
      )

      # Add yearly controls and styling
      yearly_map <- add_yearly_controls(
        yearly_map,
        legend_info,
        title_prefix,
        yr,
        borough_sf,
        vignette_overlay,
        vignette_overlay_on,
        bbox
      )

      # Save yearly image
      file_parts <- tools::file_path_sans_ext(basename(output_file))
      html_file <- file.path("aq_maps", paste0(file_parts, "_", yr, ".html"))
      img_file <- file.path("aq_maps", paste0(file_parts, "_", yr, ".jpg"))

      saveWidget(
        yearly_map,
        file = html_file,
        selfcontained = TRUE,
        title = map_title
      )
      webshot2::webshot(
        url = html_file,
        file = img_file,
        vwidth = map_width_px,
        vheight = map_width_px
      )
    }
  }

  # Finalize HTML map (after all years processed)
  # Add static layers to HTML map
  html_map <- generate_map_layers(
    html_map,
    measurement_layers,
    "static_only",
    pollutant,
    scale_to_use,
    environment()
  )

  # Add HTML controls and styling
  html_map <- add_html_controls(
    html_map,
    legend_info,
    title_prefix,
    years_to_plot,
    borough_sf,
    vignette_overlay,
    vignette_overlay_on,
    bbox
  )

  # Save HTML map
  html_file <- file.path("aq_maps", output_file)
  htmlwidgets::saveWidget(
    html_map,
    file = html_file,
    selfcontained = TRUE,
    title = map_title
  )

  # Force cleanup of _files folder as there seems to be a bug
  files_folder <- paste0(tools::file_path_sans_ext(html_file), "_files")
  if (dir.exists(files_folder)) {
    unlink(files_folder, recursive = TRUE)
  }

  # Return the map (remove the image export conditional return)
  return(invisible(html_map))
}
