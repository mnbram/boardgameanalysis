library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forcats)
library(stringr)
library(tidyr)
library(extrafont)

# Font setup ------------------------------------------------------------------

if (!(dir.exists("~/fonts"))) {
    dir.create("~/.fonts")
}
file.copy("www/Raleway-Regular.ttf", "~/.fonts/")
system("fc-cache -f ~/.fonts")

# Data import -----------------------------------------------------------------

games <- read_csv("data/bgg_games_notags.csv", col_types = "cicdddlldd")
games_k <- read_csv("data/bgg_kick_notags.csv", col_types = "cccdidd") %>%
    mutate(
        log_pledges = log10(usd_pledged),
        launched_at = as.POSIXct(
            as.integer(launched_at), origin = "1970-01-01", tz = "GMT")
    )

game_index <- setNames(1:nrow(games), games$titleyear)
game_index_k <- setNames(1:nrow(games_k), games_k$titleyear)

cats_list <- readLines("data/cats_sig_index.txt") %>%
    sapply(function(x) strsplit(x, ",")) %>%
    sapply(as.integer) %>%
    unname

mechs_list <- readLines("data/mechs_sig_index.txt") %>%
    sapply(function(x) strsplit(x, ",")) %>%
    sapply(as.integer) %>%
    unname

cats_list_k <- readLines("data/cats_sig_index_k.txt") %>%
    sapply(function(x) strsplit(x, ",")) %>%
    sapply(as.integer) %>%
    unname

mechs_list_k <- readLines("data/mechs_sig_index_k.txt") %>%
    sapply(function(x) strsplit(x, ",")) %>%
    sapply(as.integer) %>%
    unname

glm100 <- read_csv("data/glm100_coef_all.csv", col_types = "cidd") %>%
    mutate(
        term = str_replace(str_replace(term, "__", " "), "_", "-"),
        order_nc = nmodels + abs(coef),
        sign = coef > 0
    ) %>%
    rename(
        Coefficient = coef,
        Frequency = freq,
        `# of models` = nmodels
    )

# Load and process model data -------------------------------------------------

gamfiles <- sapply(
    c("Year", "MinAge", "SuggestedPlayers", "log_playtime", "AvgWeight"),
    function(var) paste0("data/ratings_", var, "_gam_fit.csv"))

gamfiles_k <- sapply(
    c("launched_at", "MinAge", "log_playtime", "AvgWeight"),
    function(var) paste0("data/pledges_", var, "_gam_fit.csv"))

gamdata <- lapply(gamfiles, function(f) read_csv(f, col_types = "ddd"))
gamdata_k <- lapply(gamfiles_k, function(f) read_csv(f, col_types = "ddd"))
gamdata_k[['launched_at']] <- gamdata_k[['launched_at']] %>%
    mutate(x = as.POSIXct(
        as.integer(x), origin = "1970-01-01", tz = "GMT"))


parametric_coef <- read_csv("data/ratings_gam_parametric.csv",
                            col_type = "cdd")

two_player_fit <- parametric_coef[1:2,] %>%
    mutate(two_player = as.logical(substr(var, 11, nchar(var))))

solo_fit <- parametric_coef[3:4,] %>%
    mutate(solo = as.logical(substr(var, 5, nchar(var))))

tag_coefs <- read_csv("data/ratings_tags_coefs_indexed.csv",
                      col_types = "ccdi")

tag_coefs_k <- read_csv("data/pledges_tags_coefs_indexed.csv",
                      col_types = "ccdi")

# Plotting setup --------------------------------------------------------------

bgcolor <- "#f7f3e5"
textcolor <- "#555555"
gridcolor <- "#bbbbbb"
hilite1 <- "#e41a1c"
hilite2 <- "#0539bc"
pointsize <- 1.5
alphaval <- 0.05
selines <- "36"

commontheme <- theme_bw(base_size = 18) +
    theme(
        plot.background = element_rect(fill = bgcolor, size = 0),
        panel.background = element_blank(),
        text = element_text(color = textcolor, family = "Raleway"),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = gridcolor, linetype = "15",
                                        size = 1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20),
        legend.position = "none"
    )

profile_base <- function(varstring, row) {
    xlabs <- setNames(
        c("Complexity", "Play time (minutes)", "Suggested # of players",
          "Minimum age", "Year"),
        c("AvgWeight", "log_playtime", "SuggestedPlayers", "MinAge", "Year"))
    basic <- ggplot(games) +
        geom_point(
            aes_string(varstring, "AvgRating"),
            alpha = alphaval, size = pointsize, color = textcolor) +
        geom_line(
            data = gamdata[[varstring]],
            aes(x = x, y = fit),
            color = hilite1, size = 1
        ) +
        geom_ribbon(
            data = gamdata[[varstring]],
            aes(x = x, ymin = fit - se, ymax = fit + se),
            fill = hilite1,
            alpha = 0.25
        ) +
        labs(
            x = xlabs[[varstring]],
            y = "Average rating"
        ) +
        commontheme
    if (varstring == "log_playtime") {
        basic +
            scale_x_continuous(
                breaks = sapply(c(10, 60, 1440), log),
                labels = function(x) round(exp(x))
            )
    } else if (varstring == "SuggestedPlayers") {
        if (is.na(row)) {
            basic + xlim(1, 16)
        } else {
            basic + xlim(1, max(16, games$SuggestedPlayers[row]))
        }
    } else if (varstring == "MinAge") {
        if (is.na(row)) {
            basic + xlim(1, 21)
        } else {
            basic + xlim(1, max(21, games$MinAge[row]))
        }
    } else if (varstring == "Year") {
        if (is.na(row)) {
            basic +
                scale_x_continuous(
                    limits = c(1950, 2017),
                    breaks = c(1950, 1970, 1990, 2010)
                )
        } else {
            basic +
                scale_x_continuous(
                    limits = c(min(1950, games$Year[row]), 2017),
                    breaks = function(x) {
                        numyears <- 2010 - min(x)
                        interval <- (numyears / 3) - (numyears / 3) %% 10
                        seq(2010, min(x), -interval)
                    })
        }
    } else {
        basic
    }
}

profile_point <- function(varstring, row) {
    profile_base(varstring, row) +
        geom_point(
            data = games[row,],
            aes_string(varstring, "AvgRating"),
            size = 4,
            shape = 21,
            fill = hilite2,
            color = bgcolor,
            stroke = 1.5
        )
}

profile_plot <- function(varstring, row) {
    if (is.na(row)) {
        profile_base(varstring, row)
    } else {
        profile_point(varstring, row)
    }
}

profile_base_k <- function(varstring, row) {
    xlabs <- setNames(
        c("Complexity", "Play time (minutes)", "Minimum age", "Launch date"),
        c("AvgWeight", "log_playtime", "MinAge", "launched_at"))
    basic <- ggplot(games_k) +
        geom_point(
            aes_string(varstring, "log_pledges"),
            alpha = alphaval*2, size = pointsize, color = textcolor) +
        geom_line(
            data = gamdata_k[[varstring]],
            aes(x = x, y = fit),
            color = hilite1, size = 1
        ) +
        geom_ribbon(
            data = gamdata_k[[varstring]],
            aes(x = x, ymin = fit - se, ymax = fit + se),
            fill = hilite1,
            alpha = 0.25
        ) +
    scale_y_continuous(
        breaks = 3:6,
        labels = c("$1,000", "$10,000", "$100,000", "$1,000,000")) +
        labs(
            x = xlabs[[varstring]],
            y = "Kickstarter pledges"
        ) +
        commontheme
    if (varstring == "log_playtime") {
        basic +
            scale_x_continuous(
                breaks = sapply(c(5, 30, 180), log),
                labels = function(x) round(exp(x))
            )
    } else {
        basic
    }
}

profile_point_k <- function(varstring, row) {
    profile_base_k(varstring, row) +
        geom_point(
            data = games_k[row,],
            aes_string(varstring, "log_pledges"),
            size = 4,
            shape = 21,
            fill = hilite2,
            color = bgcolor,
            stroke = 1.5
        )
}

profile_plot_k <- function(varstring, row) {
    if (is.na(row)) {
        profile_base_k(varstring, row)
    } else {
        profile_point_k(varstring, row)
    }
}

sign_colors_c <- ifelse(
    arrange(glm100, abs(Coefficient))$sign,
    hilite2, hilite1)

sign_colors_f <- ifelse(
    arrange(glm100, Frequency)$sign,
    hilite2, hilite1)

sign_colors_nc <- ifelse(
    arrange(glm100, order_nc)$sign,
    hilite2, hilite1)

# Server function -------------------------------------------------------------

shinyServer(function(input, output, session) {
    
    observeEvent(input$goto_report, {
        updateNavbarPage(session, "topnav", selected = "report")
        runjs("window.scrollTo(0, 0);")
    })

    observeEvent(input$goto_ratings, {
        updateNavbarPage(session, "topnav", selected = "ratings")
        runjs("window.scrollTo(0, 0);")
    })

    observeEvent(input$goto_pledges, {
        updateNavbarPage(session, "topnav", selected = "pledges")
        runjs("window.scrollTo(0, 0);")
    })

    observeEvent(input$goto_success, {
        updateNavbarPage(session, "topnav", selected = "success")
        runjs("window.scrollTo(0, 0);")
    })
    
    updateSelectizeInput(session, "gameselect_r", choices = game_index,
                         server = TRUE)
    
    updateSelectizeInput(session, "gameselect_p", choices = game_index_k,
                         server = TRUE)
    
    getRow <- reactive({as.integer(input$gameselect_r)})
    getRow_k <- reactive({as.integer(input$gameselect_p)})
    
    dim_check <- function(plot) {
        p <- plot
        if (is.null(input$dimension) || input$dimension >= 768) {
            p <- p + theme(axis.title.y = element_blank())
        }
        p
    }

    output$year <- renderPlot({
        inputRow <- getRow()
        dim_check(profile_plot("Year", inputRow))
    })

    output$avgweight <- renderPlot({
        inputRow <- getRow()
        dim_check(profile_plot("AvgWeight", inputRow))
    })
    
    output$playtime <- renderPlot({
        inputRow <- getRow()
        dim_check(profile_plot("log_playtime", inputRow))
    })
    
    output$players <- renderPlot({
        inputRow <- getRow()
        dim_check(profile_plot("SuggestedPlayers", inputRow))
    })
    
    output$minage <- renderPlot({
        inputRow <- getRow()
        dim_check(profile_plot("MinAge", inputRow))
    })
    
    output$launched <- renderPlot({
        inputRow <- getRow_k()
        dim_check(profile_plot_k("launched_at", inputRow))
    })
    
    output$avgweight_k <- renderPlot({
        inputRow <- getRow_k()
        dim_check(profile_plot_k("AvgWeight", inputRow))
    })
    
    output$playtime_k <- renderPlot({
        inputRow <- getRow_k()
        dim_check(profile_plot_k("log_playtime", inputRow))
    })
    
    output$minage_k <- renderPlot({
        inputRow <- getRow_k()
        dim_check(profile_plot_k("MinAge", inputRow))
    })
    
    output$onetwo <- renderPlot({
        
        inputRow <- getRow()
        
        solo <- dim_check(
            ggplot(games, aes(solo, AvgRating)) +
                geom_violin(fill = bgcolor, color = textcolor) +
                geom_segment(
                    data = solo_fit,
                    aes(y = coef, yend = coef,
                        x = solo + 0.55, xend = solo + 1.45),
                    color = hilite1, size = 1
                ) +
                geom_point(
                    data = games[inputRow,],
                    size = 4,
                    shape = 21,
                    fill = hilite2,
                    color = bgcolor,
                    stroke = 1.5
                ) +
                labs(x = "Has solo\noption", y = "Average rating") +
                scale_x_discrete(labels = c("No", "Yes")) +
                commontheme +
                theme(panel.grid.major.x = element_blank())
        )
        
        twop <- dim_check(
            ggplot(games, aes(two_player, AvgRating)) +
                geom_violin(fill = bgcolor, color = textcolor) +
                geom_segment(
                    data = two_player_fit,
                    aes(y = coef, yend = coef,
                        x = two_player + 0.55, xend = two_player + 1.45),
                    color = hilite1, size = 1
                ) +
                geom_point(
                    data = games[inputRow,],
                    size = 4,
                    shape = 21,
                    fill = hilite2,
                    color = bgcolor,
                    stroke = 1.5
                ) +
                labs(x = "Two-player\nonly", y = "Average rating") +
                scale_x_discrete(labels = c("No", "Yes")) +
                commontheme +
                theme(panel.grid.major.x = element_blank())
        )
        
        grid.arrange(solo, twop, nrow = 1)
        
    })
    
    output$tags_header <- renderUI({
        inputRow <- getRow()
        cats <- cats_list[[inputRow]]
        mechs <- mechs_list[[inputRow]]
        if (length(cats) > 0 & length(mechs) > 0) {
            return(tags$h2("Effects of tags on residual average rating",
                        class = "tag-header"))
        }
    })
    
    output$tags_footer <- renderUI({
        inputRow <- getRow()
        cats <- cats_list[[inputRow]]
        mechs <- mechs_list[[inputRow]]
        if (length(cats) > 0 & length(mechs) > 0) {
            return(tags$hr(class = "tags-top-hr"))
        }
    })
    
    output$cats_header <- renderUI({
        inputRow <- getRow()
        cats <- cats_list[[inputRow]]
        if (length(cats) > 0) {
            return(tagList(tags$hr(class = "tags-hr"),
                           tags$h3("Categories", class = "coef-header")))
        } else {
            return(NULL)
        }
    })
    
    output$mechs_header <- renderUI({
        inputRow <- getRow()
        mechs <- mechs_list[[inputRow]]
        if (length(mechs) > 0) {
            return(tagList(tags$hr(class = "tags-hr"),
                           tags$h3("Mechanics", class = "coef-header")))
        } else {
            return(NULL)
        }
    })
    
    output$tags_header_k <- renderUI({
        inputRow <- getRow_k()
        cats <- cats_list_k[[inputRow]]
        mechs <- mechs_list_k[[inputRow]]
        if (length(cats) > 0 & length(mechs) > 0) {
            return(tags$h2("Effects of tags on residual pledge totals",
                        class = "tag-header"))
        }
    })
    
    output$tags_footer_k <- renderUI({
        inputRow <- getRow_k()
        cats <- cats_list_k[[inputRow]]
        mechs <- mechs_list_k[[inputRow]]
        if (length(cats) > 0 & length(mechs) > 0) {
            return(tags$hr(class = "tags-top-hr"))
        }
    })
    
    output$cats_header_k <- renderUI({
        inputRow <- getRow_k()
        cats <- cats_list_k[[inputRow]]
        if (length(cats) > 0) {
            return(tagList(tags$hr(class = "tags-hr"),
                           tags$h3("Categories", class = "coef-header")))
        } else {
            return(NULL)
        }
    })
    
    output$mechs_header_k <- renderUI({
        inputRow <- getRow_k()
        mechs <- mechs_list_k[[inputRow]]
        if (length(mechs) > 0) {
            return(tagList(tags$hr(class = "tags-hr"),
                           tags$h3("Mechanics", class = "coef-header")))
        } else {
            return(NULL)
        }
    })
    
    output$catcoefs <- renderUI({
        inputRow <- getRow()
        cats <- cats_list[[inputRow]]
        tagrows <- tag_coefs %>%
            filter(
                type == "Category",
                index %in% cats
            )
        lapply(seq(1, length.out = nrow(tagrows)), function(i) {
            coef <- tagrows$coef[i]
            sign <- ifelse(coef < 0, "-negative",
                           "-positive")
            tags$div(
                tags$p(tagrows$tag[i],
                  span(sprintf("%+.4f", coef),
                       class = paste0("text", sign)),
                  class = "coef-name"),
                tags$div(class="progress",
                    tags$div(class=paste0("progress-bar progress", sign),
                        role="progressbar",
                        "aria-valuenow"=as.character(abs(coef)),
                        "aria-valuemin"="0",
                        "aria-valuemax"="0.24",
                        style=paste0("width: ", 100*abs(coef)/0.24, "%")
                    )
                )
            )
        })
    })
    
    output$mechcoefs <- renderUI({
        inputRow <- getRow()
        mechs <- mechs_list[[inputRow]]
        tagrows <- tag_coefs %>%
            filter(
                type == "Mechanic",
                index %in% mechs
            )
        lapply(seq(1, length.out = nrow(tagrows)), function(i) {
            coef <- tagrows$coef[i]
            sign <- ifelse(coef < 0, "-negative",
                           "-positive")
            tags$div(
                tags$p(tagrows$tag[i],
                  span(sprintf("%+.4f", coef),
                       class = paste0("text", sign)),
                  class = "coef-name"),
                tags$div(class="progress",
                    tags$div(class=paste0("progress-bar progress", sign),
                        role="progressbar",
                        "aria-valuenow"=as.character(abs(coef)),
                        "aria-valuemin"="0",
                        "aria-valuemax"="0.24",
                        style=paste0("width: ", 100*abs(coef)/0.24, "%")
                    )
                )
            )
        })
    })
    
    output$catcoefs_k <- renderUI({
        inputRow <- getRow_k()
        cats <- cats_list_k[[inputRow]]
        tagrows <- tag_coefs_k %>%
            filter(
                type == "Category",
                index %in% cats
            )
        lapply(seq(1, length.out = nrow(tagrows)), function(i) {
            coef <- tagrows$coef[i]
            sign <- ifelse(coef < 0, "-negative",
                           "-positive")
            tags$div(
                tags$p(tagrows$tag[i],
                       span(sprintf("%+.4f", coef),
                            class = paste0("text", sign)),
                       class = "coef-name"),
                tags$div(class="progress",
                         tags$div(class=paste0("progress-bar progress", sign),
                                  role="progressbar",
                                  "aria-valuenow"=as.character(abs(coef)),
                                  "aria-valuemin"="0",
                                  "aria-valuemax"="0.24",
                                  style=paste0("width: ", 100*abs(coef)/0.24, "%")
                         )
                )
            )
        })
    })
    
    output$mechcoefs_k <- renderUI({
        inputRow <- getRow_k()
        mechs <- mechs_list_k[[inputRow]]
        tagrows <- tag_coefs_k %>%
            filter(
                type == "Mechanic",
                index %in% mechs
            )
        lapply(seq(1, length.out = nrow(tagrows)), function(i) {
            coef <- tagrows$coef[i]
            sign <- ifelse(coef < 0, "-negative",
                           "-positive")
            tags$div(
                tags$p(tagrows$tag[i],
                       span(sprintf("%+.4f", coef),
                            class = paste0("text", sign)),
                       class = "coef-name"),
                tags$div(class="progress",
                         tags$div(class=paste0("progress-bar progress", sign),
                                  role="progressbar",
                                  "aria-valuenow"=as.character(abs(coef)),
                                  "aria-valuemin"="0",
                                  "aria-valuemax"="0.24",
                                  style=paste0("width: ", 100*abs(coef)/0.24, "%")
                         )
                )
            )
        })
    })
    
    nlp_sorting <- reactive({
        input$nlp_sort
    })
    
    output$nlp <- renderPlot({
        termplot <- function(ordervar) {
            sign_colors <- sign_colors_c
            if (ordervar == "freq") {
                sign_colors <- sign_colors_f
            }
            if (ordervar == "nmodels") {
                sign_colors <- sign_colors_nc
            }
            
            psetup <- NULL
            dotpalette <- c(hilite1, hilite2, rep(textcolor, 4))
            
            if (is.null(input$dimension) || input$dimension >= 768) {
                if (ordervar == "coef") {
                    psetup <- glm100 %>%
                        select(-order_nc) %>%
                        mutate(coef2 = Coefficient) %>%
                        gather(measure, value, -c(term, sign, coef2)) %>%
                        ggplot(aes(value, fct_reorder(term, abs(coef2))))
                }
                if (ordervar == "freq") {
                    psetup <- glm100 %>%
                        select(-order_nc) %>%
                        mutate(freq2 = Frequency) %>%
                        gather(measure, value, -c(term, sign, freq2)) %>%
                        ggplot(aes(value, fct_reorder(term, freq2)))
                }
                if (ordervar == "nmodels") {
                    psetup <- glm100 %>%
                        gather(measure, value, -c(term, sign, order_nc)) %>%
                        ggplot(aes(value, fct_reorder(term, order_nc)))    
                }    
            } else {
                if (ordervar == "coef") {
                    psetup <- glm100 %>%
                        select(-order_nc) %>%
                        mutate(coef2 = Coefficient) %>%
                        gather(measure, value, -c(term, sign, coef2)) %>%
                        filter(measure == "Coefficient") %>%
                        ggplot(aes(value, fct_reorder(term, abs(coef2))))
                    dotpalette <- c(hilite1, hilite2)
                }
                if (ordervar == "freq") {
                    psetup <- glm100 %>%
                        select(-order_nc) %>%
                        mutate(freq2 = Frequency) %>%
                        gather(measure, value, -c(term, sign, freq2)) %>%
                        filter(measure == "Frequency") %>%
                        ggplot(aes(value, fct_reorder(term, freq2)))
                    dotpalette <- rep(textcolor, 2)
                }
                if (ordervar == "nmodels") {
                    psetup <- glm100 %>%
                        gather(measure, value, -c(term, sign, order_nc)) %>%
                        filter(measure == "# of models") %>%
                        ggplot(aes(value, fct_reorder(term, order_nc)))
                    dotpalette <- rep(textcolor, 2)
                }
            }
            
            zeroline <- data.frame(
                interc = 0, measure = "Coefficient"
            )
            
            p <- psetup +
                geom_vline(data = zeroline, aes(xintercept = interc),
                           color = textcolor, linetype = "dashed") +
                geom_point(size = 1.5, aes(color = interaction(sign, measure))) +
                facet_grid(.~measure, scales = "free_x") +
                scale_x_continuous(position = "top") +
                scale_color_manual(values = dotpalette) +
                theme(
                    plot.background = element_rect(fill = bgcolor, size = 0),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.y = element_line(color = gridcolor,
                                                      linetype = "dotted"),
                    panel.background = element_blank(),
                    axis.ticks.x = element_line(color = textcolor),
                    axis.ticks.y = element_blank(),
                    axis.title = element_blank(),
                    axis.text.x = element_text(color = textcolor),
                    axis.text.y = element_text(color = sign_colors, vjust = 0.4),
                    strip.background = element_blank(),
                    strip.placement = "outside",
                    panel.spacing = unit(1, "lines"),
                    text = element_text(family = "Raleway", color = textcolor),
                    legend.position = "none"
                )
            if (is.null(input$dimension) || input$dimension >= 768) {
                p
            } else {
                p + theme(axis.text.x = element_text(size = 6))
            }
        }
        
        termplot(nlp_sorting())
    },
    res = 120)
    
})
