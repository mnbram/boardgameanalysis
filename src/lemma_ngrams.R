library(tidyverse)
library(tidytext)
library(stringr)
library(glmnet)
library(doParallel)

setwd("~/bgg/kickstarter")

lemma_files <- list.files("data/lemmas", full.names = TRUE)
texts <- lapply(lemma_files, readLines)
text_lengths <- map_int(texts, length)

kids <- map_int(lemma_files, function(x)
    as.integer(str_match(x, "[0-9]+")))

lemmas <- tibble(
    k_id = rep(kids, text_lengths),
    text = unlist(texts)
)

rm(texts)


# Remove lemmatized pronouns and parsing mistakes as stop words
stop_plus <- c(stop_words$word, "pron", "â")

unigrams_bow <- lemmas %>%
    unnest_tokens(type, text) %>%
    count(k_id, type, sort = TRUE) %>%
    ungroup() %>%
    filter(!type %in% stop_plus) %>%
    mutate(type = str_replace_all(type, fixed("_"), "-"))

bigrams_bow <- lemmas %>%
    unnest_tokens(type, text, token = "ngrams", n = 2) %>%
    count(k_id, type, sort = TRUE) %>%
    ungroup() %>%
    separate(type, c("word1", "word2"), sep = " ") %>%
    filter(
        !word1 %in% stop_plus,
        !word2 %in% stop_plus
    ) %>%
    unite(type, word1, word2, sep = "__")

common <- rbind(unigrams_bow, bigrams_bow) %>%
    group_by(type) %>%
    summarize(freq_in_campaigns = length(unique(k_id))/length(lemma_files)) %>%
    filter(freq_in_campaigns >= 0.001)

sparse_unibi_bow <- rbind(unigrams_bow, bigrams_bow) %>%
    filter(type %in% common$type) %>%
    cast_sparse(k_id, type)


br_data <- read_delim("data/benrugg.csv", delim = ",",
                      escape_backslash = TRUE, escape_double = FALSE) %>%
    filter(
        sub_category == "Tabletop Games",
        kickstarter_id %in% kids
    ) %>%
    select(kickstarter_id, state) %>%
    slice(match(as.integer(rownames(sparse_unibi_bow)), kickstarter_id))

states <- setNames(ifelse(br_data$state == "successful", 1, 0),
                   br_data$kickstarter_id) %>% as.matrix()
colnames(states) <- "success_"


sparse_results_bow <- cbind(states, sparse_unibi_bow)


registerDoParallel(cores = 6)


penalties <- rep(1, ncol(sparse_results_bow)-1)
penalties[grep("__", colnames(sparse_results_bow[,-1]))] <- 0.5

glm_coefs <- function() {
    glm_bow <- cv.glmnet(
        sparse_results_bow[,-1], as.factor(sparse_results_bow[,1]),
        alpha = 1, family = "binomial", penalty.factor = penalties,
        standardize = FALSE, type.measure = "auc", parallel = TRUE)
    coef(glm_bow, s = "lambda.min") %>%
        tidy() %>%
        filter(value != 0)
}

set.seed(984)
glm_coefs_cv <- lapply(1:100, function(x) glm_coefs())


glm100_coef <- lapply(glm_coefs_cv, function(x) {
    x %>%
        filter(row != "(Intercept)") %>%
        select(row, value) %>%
        as_tibble()
}) %>%
    bind_rows() %>%
    group_by(row) %>%
    summarize(n = n(), mean = mean(value)) %>%
    arrange(desc(n), desc(abs(mean)))

term_counts <- apply(sparse_unibi_bow, 2, sum)
term_freqs <- tidy(term_counts) %>%
    filter(names %in% glm100_coef$row) %>%
    mutate(freq = x/nrow(sparse_unibi_bow))

glm100_coef_full <- glm100_coef %>%
    left_join(term_freqs, by = c("row" = "names")) %>%
    rename(term = row, nmodels = n, coef = mean) %>%
    select(term, nmodels, coef, freq)
    
write_csv(glm100_coef_full, "glm100_coef_all.csv")
