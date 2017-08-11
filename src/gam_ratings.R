library(tidyverse)
library(mgcv)
library(caret)
library(broom)
library(doMC)

# Read in games database and preprocess --------------------------------------

setwd("~/bgg")

games <- read_csv(
    "bgg_games_20170415.csv",
    col_types = "ciiiiddidddcc"
)

filtered <- games %>%
    filter(
        GeekRating != 0,
        !is.na(SuggestedPlayers),
        AvgWeight != 0,
        PlayTime != 0,
        MinPlayers != 0,
        MaxPlayers != 0,
        Year >= 1900
    )

clean <- filtered %>%
    mutate(
        solo = MinPlayers == 1,
        two_player = (MinPlayers == 2 & MaxPlayers == 2),
        MinAge = ifelse(MinAge == 0, SuggestedAge, MinAge),
        log_playtime = log(PlayTime)
    ) %>%
    filter(!is.na(MinAge))

numgames <- nrow(clean)
# 12141

# Convert tags to sparse columns ---------------------------------------------

strip_and_split <- function(strings){
    lapply(strings, function(s)
        strsplit(substr(s, 2, nchar(s) - 1), "\\|")[[1]])
}

clean_lists <- clean %>%
    mutate(
        Categories = strip_and_split(Categories),
        Mechanics = strip_and_split(Mechanics)
    )

cats_raw <- unique(unlist(clean_lists$Categories))
mechs_raw <- unique(unlist(clean_lists$Mechanics))

cats_df <- lapply(cats_raw, function(cat)
    map_int(clean_lists$Categories, function(gametags)
        cat %in% gametags)) %>%
    setNames(paste0("Cat_", make.names(cats_raw))) %>%
    as_tibble

mechs_df <- lapply(mechs_raw, function(mech)
    map_int(clean_lists$Mechanics, function(gametags)
        mech %in% gametags)) %>%
    setNames(paste0("Mech_", make.names(mechs_raw))) %>%
    as_tibble

tags_features <- clean_lists %>%
    select(-c(Categories, Mechanics)) %>%
    cbind(cats_df) %>%
    cbind(mechs_df)

# Model fitting --------------------------------------------------------------

notags <- tags_features %>%
    select(-c(starts_with("Cat_"), starts_with("Mech_")))

set.seed(385)
train_ind <- sample.int(numgames, floor(numgames*0.8))
training <- notags[train_ind,]

gam1 <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                solo + two_player + s(log_playtime) + s(AvgWeight),
            data = training)

gam2 <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                solo + two_player + s(AvgWeight),
            data = training)

gam3 <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                solo + s(log_playtime) + s(AvgWeight),
            data = training)

gam4 <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                solo + s(AvgWeight),
            data = training)

# gam1 (full model) is best by GCV score, but definitely looks under-smoothed
# Retry with REML

gam5 <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                solo + two_player + s(log_playtime) + s(AvgWeight),
            data = training, method = "REML")

# Not really any better, so let's leave it as-is

# Simple test plotting -------------------------------------------------------

gam1_plotdata <- plot(gam1)

gam1_weight_df <- gam1_plotdata[[5]][c("x", "fit", "se")] %>%
    lapply(as.vector) %>%
    as_tibble %>%
    mutate(
        fit = fit + coef(gam1)[["(Intercept)"]],
        lower = fit - se,
        upper = fit + se
    ) %>%
    select(-se)

ggplot(training) +
    geom_point(aes(AvgWeight, AvgRating)) +
    geom_line(
        data = gam1_weight_df,
        aes(x = x, y = fit),
        color = "red"
    ) +
    geom_line(
        data = gam1_weight_df,
        aes(x = x, y = lower),
        linetype = "dashed",
        color = "red"
    ) +
    geom_line(
        data = gam1_weight_df,
        aes(x = x, y = upper),
        linetype = "dashed",
        color = "red"
    )

# Regress tags on residuals --------------------------------------------------

tags_train <- tags_features %>%
    select(c(starts_with("Cat_"), starts_with("Mech_"))) %>%
    .[train_ind,]

tags_resid <- tags_train[,colSums(tags_train) > 1] %>%
    mutate(resid = resid(gam1))

registerDoMC(cores = 7)

enet <- train(resid ~ ., data = tags_resid, method = "glmnet",
              trControl = trainControl(method = "cv", number = 10))

enet_coefs <- coef(enet$finalModel, s = enet$bestTune$lambda) %>%
    tidy %>%
    select(-column) %>%
    filter(row != "(Intercept)") %>%
    arrange(desc(abs(value)))

# Performance measures -------------------------------------------------------

tags_test <- tags_features %>%
    select(c(starts_with("Cat_"), starts_with("Mech_"))) %>%
    .[-train_ind,]

test_pred_gam <- predict(gam1, notags[-train_ind,])
test_pred_enet <- predict(enet, tags_test)

testing_y <- notags[-train_ind, "AvgRating"]

R2(testing_y, test_pred_gam + test_pred_enet)
# 0.4614382

testing_df <- tibble(
    obs = testing_y,
    gam = test_pred_gam,
    enet = test_pred_enet
) %>%
    mutate(both = gam + enet)

ggplot(testing_df, aes(obs, both)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    coord_fixed()

# Output results -------------------------------------------------------------

# GAM

plot_as_df <- function(gamlist) {
    gamlist[c("x", "fit", "se")] %>%
        lapply(as.vector) %>%
        as_tibble %>%
        mutate(fit = fit + coef(gam1)[["(Intercept)"]])
}

for (i in 1:5) {
    write_csv(plot_as_df(gam1_plotdata[[i]]),
              paste0("model_results/ratings_",
                     gam1_plotdata[[i]]$xlab,
                     "_gam_fit.csv"))
}

# GAM parametric terms

# In order to get standard errors for individual group means instead of the
# contrasts, exclude the intercept and choose the order of parametric terms

gam_2p <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                  two_player + solo + s(log_playtime) + s(AvgWeight) - 1,
              data = training)

gam_solo <- gam(AvgRating ~ s(Year) + s(MinAge) + s(SuggestedPlayers) +
                    solo + two_player + s(log_playtime) + s(AvgWeight) - 1,
                data = training)

twop <- summary(gam_2p)$p.table %>%
    tidy %>%
    rename(
        var = .rownames,
        coef = Estimate,
        se = Std..Error
    ) %>%
    select(var, coef, se) %>%
    .[1:2,]

solo <- summary(gam_solo)$p.table %>%
    tidy %>%
    rename(
        var = .rownames,
        coef = Estimate,
        se = Std..Error
    ) %>%
    select(var, coef, se) %>%
    .[1:2,]

write_csv(rbind(twop, solo), "model_results/ratings_gam_parametric.csv")

# Tag residual coefficients

cats_named <- setNames(cats_raw, paste0("Cat_", make.names(cats_raw)))
mechs_named <- setNames(mechs_raw, paste0("Mech_", make.names(mechs_raw)))
tags_named <- c(cats_named, mechs_named)

coefs_clean <- enet_coefs %>%
    mutate(
        tag = tags_named[row],
        type = ifelse(startsWith(row, "Cat_"), "Category", "Mechanic"),
        coef = trimws(format(round(value, 5), scientific = FALSE))
    ) %>%
    select(tag, type, coef)

write_csv(coefs_clean, "model_results/ratings_tags_coefs.csv")

# Testing set predicted and observed
write_csv(testing_df, "model_results/ratings_testing_pred_obs.csv")
