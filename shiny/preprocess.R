### Pre-Shiny data processing ### --------------------------------------------

# Games filtering/mutation ---------------------------------------------------

games_raw <- read_csv(
    "data/bgg_games_20170415.csv",
    col_types = "ciiiiddidddcc"
)

games <- games_raw %>%
    mutate(titleyear = paste0(Title, " (", Year, ")")) %>%
    filter(
        GeekRating != 0,
        !is.na(SuggestedPlayers),
        AvgWeight != 0,
        PlayTime != 0,
        MinPlayers != 0,
        MaxPlayers != 0
    ) %>%
    arrange(desc(GeekRating)) %>%
    mutate(
        solo = MinPlayers == 1,
        two_player = (MinPlayers == 2 & MaxPlayers == 2),
        MinAge = ifelse(MinAge == 0, SuggestedAge, MinAge),
        log_playtime = log(PlayTime)
    ) %>%
    filter(!is.na(MinAge)) %>%
    select(Title, Year, titleyear, AvgRating, MinAge, SuggestedPlayers,
           solo, two_player, log_playtime, AvgWeight,
           Categories, Mechanics)

write_csv(select(games, -c(Categories, Mechanics)),
          "data/bgg_games_notags.csv")

gamesk <- read_csv("~/bgg/kickstarter/bgg_kick_merged.csv")

filtered <- gamesk %>%
    filter(
        !is.na(SuggestedPlayers),
        AvgWeight != 0,
        PlayTime != 0,
        MinPlayers != 0,
        MaxPlayers != 0
    ) %>%
    mutate(
        solo = MinPlayers == 1,
        two_player = (MinPlayers == 2 & MaxPlayers == 2),
        MinAge = ifelse(MinAge == 0, SuggestedAge, MinAge),
        log_pledges = log10(usd_pledged),
        log_playtime = log(PlayTime),
        titleyear = paste0(Title, " (", Year, ")")
    ) %>%
    filter(!is.na(MinAge)) %>%
    arrange(desc(usd_pledged)) %>%
    select(Title, launched_at, titleyear, usd_pledged, MinAge,
           log_playtime, AvgWeight, Categories, Mechanics)

write_csv(select(filtered, -c(Categories, Mechanics)),
          "shiny/game_profiles/data/bgg_kick_notags.csv")


# Parse and index tags -------------------------------------------------------

strip_and_split <- function(strings){
    lapply(strings, function(s)
        strsplit(substr(s, 2, nchar(s) - 1), "\\|")[[1]])
}

tag_coefs <- read_csv("data/ratings_tags_coefs.csv", col_types = "ccd")
tag_index <- lapply(split(tag_coefs, tag_coefs$type),
                    function(x) setNames(1:nrow(x), x$tag))
tag_coefs <- tag_coefs %>%
    rowwise %>%
    mutate(index = tag_index[[type]][tag]) %>%
    ungroup
write_csv(tag_coefs, "data/ratings_tags_coefs_indexed.csv")

tag_coefs_k <- read_csv("data/pledges_tags_coefs.csv", col_types = "ccd")
tag_index_k <- lapply(split(tag_coefs_k, tag_coefs_k$type),
                    function(x) setNames(1:nrow(x), x$tag))
tag_coefs_k <- tag_coefs_k %>%
    rowwise %>%
    mutate(index = tag_index_k[[type]][tag]) %>%
    ungroup
write_csv(tag_coefs_k, "data/pledges_tags_coefs_indexed.csv")


tags_to_indices <- function(taglist, type) {
    sapply(unlist(taglist), function(x) tag_index[[type]][x]) %>%
        na.omit %>%
        as.vector
}

cats_list <- strip_and_split(games$Categories) %>%
    lapply(function(x) tags_to_indices(x, "Category"))
mechs_list <- strip_and_split(games$Mechanics) %>%
    lapply(function(x) tags_to_indices(x, "Mechanic"))

cats_list_str <- sapply(cats_list, function(line)
    paste(line, collapse = ","))
mechs_list_str <- sapply(mechs_list, function(line)
    paste(line, collapse = ","))

cats_file <- file("data/cats_sig_index.txt")
writeLines(cats_list_str, cats_file)
close(cats_file)

mechs_file <- file("data/mechs_sig_index.txt")
writeLines(mechs_list_str, mechs_file)
close(mechs_file)


tags_to_indices_k <- function(taglist, type) {
    sapply(unlist(taglist), function(x) tag_index_k[[type]][x]) %>%
        na.omit %>%
        as.vector
}

cats_list_k <- strip_and_split(filtered$Categories) %>%
    lapply(function(x) tags_to_indices_k(x, "Category"))
mechs_list_k <- strip_and_split(filtered$Mechanics) %>%
    lapply(function(x) tags_to_indices_k(x, "Mechanic"))

cats_list_str_k <- sapply(cats_list_k, function(line)
    paste(line, collapse = ","))
mechs_list_str_k <- sapply(mechs_list_k, function(line)
    paste(line, collapse = ","))

cats_file_k <- file("data/cats_sig_index_k.txt")
writeLines(cats_list_str_k, cats_file_k)
close(cats_file_k)

mechs_file_k <- file("data/mechs_sig_index_k.txt")
writeLines(mechs_list_str_k, mechs_file_k)
close(mechs_file_k)
