library(tidyverse)
library(jsonlite)
library(quantmod)

# WebRobots data -------------------------------------------------------------
# Extract useful data from WebRobots JSON data from December 2015 to May 2017.
# Duplicate entries across months are removed, keeping only the latest entry.

# The actual data entries are data.frames within the "data" column of the
# overarching data.frame so extract them and keep only columns of interest
cull_json <- function(json, i) {
    json[i, "data"] %>%
        select(id, name, goal, state, pledged, currency, deadline, created_at,
               launched_at, usd_pledged, backers_count, static_usd_rate,
               blurb) %>%
        mutate(usd_pledged = as.double(usd_pledged)) %>%
        as_tibble
}

json_to_tibble <- function(json) {
    do.call(rbind, lapply(1:nrow(json), function(x) cull_json(json, x)))
}

json_tbl <- function(filename) {
    json_to_tibble(stream_in(file(filename)))
}

json_files <- list.files("data", pattern = ".json", full.names = TRUE)
wr_data_list <- lapply(json_files, json_tbl) # Takes a while to complete

wr_list_indexed <- lapply(1:length(wr_data_list), function(x)
    mutate(wr_data_list[[x]], index = x))

# Now keep only the latest entry and exlude campaigns without end-state data
wr_successful <- do.call(rbind, wr_list_indexed) %>%
    group_by(id) %>%
    top_n(1, index) %>%
    ungroup() %>%
    filter(state != "live") %>%
    select(-index)

write_csv(wr_successful, "data/webrobots_aggregated_successful_blurb.csv")

# Ben Rugg data --------------------------------------------------------------
# Compiled by Ben Rugg as part of kickscraper project on GitHub and
# distributed on Dropbox. Includes campaigns that did not fund, as well as
# some whose successful ending states were not captured by WebRobots data.

br_data <- read_delim("data/benrugg.csv", delim = ",",
                      escape_backslash = TRUE, escape_double = FALSE) %>%
    filter(sub_category == "Tabletop Games")

# This data set does not include the USD conversion rate, nor the pledged
# amount in USD, so we will use quantmod to get historical exchange rate
# data. The closing price from Yahoo! Finance data on the date of the
# project's launch, or the next available date, will be used to set the
# exchange rate.
exchange_USD <- function(currency, time) {
    if(currency == "USD") {
        return(1)
    } else {
        launch_date <- as.character(as.Date(as.POSIXct(
            time, origin = "1970-01-01", tz = "UTC")))
        xrate <- tryCatch({
            getSymbols(
            paste0(currency, "USD=X"), from = launch_date,
            auto.assign = FALSE)[[1,4]]
        }, error = function(e) {
            # Some dates cause 404 errors, so just use the next day
            getSymbols(
            paste0(currency, "USD=X"),
            from = as.character(as.Date(launch_date) + 1),
            auto.assign = FALSE)[[1,4]]
        })
        return(xrate)
    }
}

br_data_xch <- br_data %>%
    filter(currency != "NULL") %>%
    mutate(static_usd_rate = map2_dbl(currency, launched_at, exchange_USD))

# Re-read wr_successful for consistency of classes across sessions
wr_successful <- read_csv("data/webrobots_aggregated_successful_blurb.csv")

br_like_wr <- br_data_xch %>%
    mutate(
        usd_pledged = pledged/static_usd_rate,
        id = kickstarter_id
    ) %>%
    select_(.dots = names(wr_successful))

write_csv(br_like_wr, "data/benrugg_like_webrobots_blurb.csv")

# Merge both data sets -------------------------------------------------------

br_not_wr <- br_like_wr %>%
    filter(
        !(id %in% wr_successful$id),
        state %in% c("successful", "failed")
    )

games_all <- rbind(wr_successful, br_not_wr)

# Remove repeats to lessen ambiguity
repeat_names <- as_tibble(table(games_all$name)) %>% filter(n > 1) %>% .[[1]]

games_norepeats <- games_all %>%
    filter(!(name %in% repeat_names))

write_csv(games_norepeats, "data/games_wrbr_blurb.csv")
