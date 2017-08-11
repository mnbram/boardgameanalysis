library(tidyverse)
library(mgcv)
library(caret)
library(broom)

# Read in games database and preprocess --------------------------------------

setwd("~/bgg")

games <- read_csv("kickstarter/bgg_kick_merged.csv")

filtered <- games %>%
    filter(
        !is.na(SuggestedPlayers),
        AvgWeight != 0,
        PlayTime != 0,
        MinPlayers != 0,
        MaxPlayers != 0
    )

clean <- filtered %>%
    mutate(
        solo = MinPlayers == 1,
        two_player = (MinPlayers == 2 & MaxPlayers == 2),
        MinAge = ifelse(MinAge == 0, SuggestedAge, MinAge),
        log_pledges = log10(usd_pledged),
        log_playtime = log(PlayTime)
    ) %>%
    filter(!is.na(MinAge))

numgames <- nrow(clean)
# 1100

# Convert tags to sparse columns ---------------------------------------------

clean_lists <- clean %>%
    mutate(
        Categories = strsplit(Categories, "\\|"),
        Mechanics = strsplit(Mechanics, "\\|")
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

set.seed(2578)
train_ind <- sample.int(numgames, floor(numgames*0.85))
training <- notags[train_ind,]

gam1 <- gam(log_pledges ~ s(launched_at) + s(MinAge) + s(SuggestedPlayers) +
                solo + two_player + s(log_playtime) + s(AvgWeight),
            data = training)

gam2 <- gam(log_pledges ~ s(launched_at) + s(MinAge) +
                solo + two_player + s(log_playtime) + s(AvgWeight),
            data = training)

gam3 <- gam(log_pledges ~ s(launched_at) + s(MinAge) +
                solo + s(log_playtime) + s(AvgWeight),
            data = training)

gam4 <- gam(log_pledges ~ s(launched_at) +
                solo + s(log_playtime) + s(AvgWeight),
            data = training)

gam5 <- gam(log_pledges ~ s(launched_at) + s(MinAge) + SuggestedPlayers +
                solo + s(log_playtime) + s(AvgWeight),
            data = training)

# gam3 is best by GCV score
# Possibly under-smoothed for AvgWeight; retry with REML

gam6 <- gam(log_pledges ~ s(launched_at) + s(MinAge) +
                solo + s(log_playtime) + s(AvgWeight),
            data = training, method = "REML")

# Simple test plotting -------------------------------------------------------

gam6_plotdata <- plot(gam6)

gam6_weight_df <- gam6_plotdata[[4]][c("x", "fit", "se")] %>%
    lapply(as.vector) %>%
    as_tibble %>%
    mutate(
        fit = fit + coef(gam6)[["(Intercept)"]],
        lower = fit - se,
        upper = fit + se
    ) %>%
    select(-se)

ggplot(training) +
    geom_point(aes(AvgWeight, log_pledges)) +
    geom_line(
        data = gam6_weight_df,
        aes(x = x, y = fit)
    ) +
    geom_line(
        data = gam6_weight_df,
        aes(x = x, y = lower),
        linetype = "dashed"
    ) +
    geom_line(
        data = gam6_weight_df,
        aes(x = x, y = upper),
        linetype = "dashed"
    )

# Regress tags on residuals --------------------------------------------------

tags_train <- tags_features %>%
    select(c(starts_with("Cat_"), starts_with("Mech_"))) %>%
    .[train_ind,]

tags_resid <- tags_train[,colSums(tags_train) > 1] %>%
    mutate(resid = resid(gam6))

enet <- train(resid ~ ., data = tags_resid, method = "glmnet",
              trControl = trainControl(method = "cv", number = 10))
# Results in LASSO anyway (alpha = 1)

enet_coefs <- coef(enet$finalModel, s = enet$bestTune$lambda) %>%
    tidy %>%
    select(-column) %>%
    filter(row != "(Intercept)") %>%
    arrange(desc(abs(value)))

# Performance measures -------------------------------------------------------

tags_test <- tags_features %>%
    select(c(starts_with("Cat_"), starts_with("Mech_"))) %>%
    .[-train_ind,]

test_pred_gam <- predict(gam6, notags[-train_ind,])
test_pred_enet <- predict(enet, tags_test)

testing_y <- notags[-train_ind, "log_pledges"]

R2(testing_y, test_pred_gam + test_pred_enet)
# 0.4214563

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

# Write model results --------------------------------------------------------

# GAM

plot_as_df <- function(gamlist) {
    gamlist[c("x", "fit", "se")] %>%
        lapply(as.vector) %>%
        as_tibble %>%
        mutate(fit = fit + coef(gam6)[["(Intercept)"]])
}

for (i in 1:length(gam6_plotdata)) {
    write_csv(plot_as_df(gam6_plotdata[[i]]),
              paste0("model_results/pledges_",
                     gam6_plotdata[[i]]$xlab,
                     "_gam_fit.csv"))
}

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

write_csv(coefs_clean, "model_results/pledges_tags_coefs.csv")

# Predicted/observed
write_csv(testing_df, "model_results/pledges_testing_pred_obs.csv")
