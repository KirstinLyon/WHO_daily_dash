# PROJECT: WHO Daily Dash
# DESCRIPTION: This script fetches the latest data from the WHO API for the
# AUTHOR: Kirstin Lyon
# LICENSE: MIT
# CREATED: 2025-03-22



# LOAD LIBRARIES -------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(janitor)
library(tidyr)
library(purrr)
library(glue)
library(readr)


# GLOBAL VARIABLES -----------------------------------------------------------
URL_BASE <-  "https://ghoapi.azureedge.net/api/"


# FUNCTIONS -----------------------------------------------------------------
#' convert JSON to tibble
#'
#' @param url URL of the JSON file
#'
#' @returns a tibble
#' @export
#'
#' @examples
#'  \dontrun{
#'    convert_JSON_to_tbl(url)
#' }
convert_JSON_to_tbl <- function(url){
    data <- GET(url)
    data_df <- fromJSON(content(data, as = "text", encoding = "utf-8"))
    data_tbl <-  map_if(data_df, is.data.frame, list) |> 
        as_tibble() |> 
        unnest(cols = c(value)) |> 
        select(-'@odata.context')
    return(data_tbl)
}


#' Find an indicator at random that has data
#'
#' @param all_indicators a tibble of all indicators
#' @param URL_BASE the base URL for the WHO API
#'
#' @returns a tibble of the indicator data
#' @export
#'
#' @examples
#'  \dontrun{
#'    find_indicator(all_indicators, URL_BASE)
#' }

find_indicator <- function(all_indicators, URL_BASE) {
    indicator_data_tbl <- tibble()  # Initialize as an empty tibble
    
    #keep looking for a new indicator if you find one that is empty
    while (nrow(indicator_data_tbl) == 0) {
        # Select a random indicator
        random_indicator <- all_indicators |>
            sample_n(1) |>
            select(IndicatorCode) |>
            pull()
        
        # Fetch data for the selected indicator
        response <- GET(paste0(URL_BASE, random_indicator))
        indicator_data <- content(response, "text")
        
        # Process the fetched data
        indicator_data_tbl <- fromJSON(indicator_data, simplifyVector = TRUE) |>
            as_tibble() |>
            unnest(cols = everything())
    }
    
    return(indicator_data_tbl)
}


# GET DATA ------------------------------------------------------------------

all_dimension <-  convert_JSON_to_tbl("https://ghoapi.azureedge.net/api/Dimension") 
all_spatial <-  convert_JSON_to_tbl(paste0(URL_BASE,"Dimension/COUNTRY/DimensionValues")) 
all_indicators <-  convert_JSON_to_tbl(paste0(URL_BASE,"Indicator")) 


# Find an indicator that has data and at COUNTRY level
data <- tibble() 

while(nrow(data) == 0) {
    data <- find_indicator(all_indicators, URL_BASE) |> 
        clean_names() |> 
        filter(!is.na(numeric_value)) |> 
        filter(spatial_dim_type == "COUNTRY") |> 
        filter(time_dim_type == "YEAR") 
    
}

# CLEAN DATA ----------------------------------------------------------------

#all countries
all_countries <- all_spatial |> 
    select(Code, Title) |> 
    rename(spatial_dim = Code, country = Title)

#create final dataset
data_final <- data |> 
    left_join(all_countries, by = "spatial_dim") |> 
    left_join(all_indicators, by = c("indicator_code" = "IndicatorCode") ) |> 
    left_join(all_dimension, by = c("dim1type" = "Code")) |> 
    clean_names() |> 
    rename(
           year = time_dim, 
           iso_code = spatial_dim,
           region_code = parent_location_code, 
           region = parent_location
           ) 

# SAVE DATA  ------------------------------------------------------
write_csv(data_final, "Dataout/who_data.csv")

