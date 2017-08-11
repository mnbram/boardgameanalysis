library(shiny)
library(shinyjs)
library(shinycssloaders)

shinyUI(
    tagList(

        tags$head(tagList(
            
            tags$link(
                rel = "stylesheet",
                href = "profstyle.css"
            ),
            
            tags$link(
                rel = "stylesheet", 
                href = paste0(
                    "https://fonts.googleapis.com/css?family=",
                    "Playfair+Display:400i|Raleway"
                )
            ),
            
            # Script for listening to window resize events and save width,
            # compiled for two Stack Overflow posts:
            #
            # https://stackoverflow.com/questions/36995142/get-the-size-of-
            # the-window- in-shiny
            #
            # https://stackoverflow.com/questions/5489946/jquery-how-to-wait-
            # for-the-end-of-resize-event-and-only-then-perform-an-ac
            #
            tags$script('
            var dimension = 0;
            $(document).on("shiny:connected", function(e) {
                dimension = window.innerWidth;
                Shiny.onInputChange("dimension", dimension);
            });
            function resizedw(){
                dimension = window.innerWidth;
                Shiny.onInputChange("dimension", dimension);
            }
            var doit;
            window.onresize = function(){
                clearTimeout(doit);
                doit = setTimeout(resizedw, 500);
            };
            

            var loading = true;
            $(document).on("shiny:busy", function(e) {
                loading = true;
                Shiny.onInputChange("loading", loading);
            });
            $(document).on("shiny:idle", function(e) {
                loading = false;
                Shiny.onInputChange("loading", loading);
            });

            
            function resizenav() {
                $("#report-nav").width($("#report-nav-col").width());
            }
            $(document).ready(function () {
                $(window).resize(function () {
                    resizenav()
                });
            });
            $(document).on("shown.bs.tab", "a[data-toggle=\'tab\']", function(e) {
                resizenav()
            });
            
            '),
            
            tags$script(src = "jquery.scrollTo.min.js"),
            tags$script(src = "scrollnav.js")
        )),
        
        useShinyjs(),
        
        navbarPage(
            "Board game analysis", id = "topnav",
            inverse = TRUE,
            
            tabPanel(
                "Intro", value = "intro",
                includeHTML("www/intro.html")
            ),
            
            tabPanel(
                "Full Report", value = "report",
                includeHTML("www/report.html")
            ),
            
            navbarMenu(
                "Explore",
                tabPanel(
                    "User ratings", value = "ratings",
                    fluidRow(
                        column(
                            12,
                            selectizeInput(
                                "gameselect_r", choices = NULL, label = NULL,
                                width = "400px", options = list(
                                    placeholder =
                                        "> Enter a game's title to display it below")
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            6, offset = 3,
                            conditionalPanel(
                                condition = "input.loading",
                                tags$div(
                                    class="progress loading-bar",
                                    tags$div(
                                        class=paste(
                                            "progress-bar",
                                            "progress-bar-striped",
                                            "active",
                                            "loading"
                                        ),
                                        role="progressbar",
                                        "aria-valuenow"="1",
                                        "aria-valuemin"="0",
                                        "aria-valuemax"="1",
                                        style="width: 100%",
                                        span("Loading …", class = "loading-text")
                                    ))
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            uiOutput("tags_header")
                        )  
                    ),
                    fluidRow(
                        column(
                            6,
                            uiOutput("cats_header"),
                            uiOutput("catcoefs")
                        ),
                        column(
                            6,
                            uiOutput("mechs_header"),
                            uiOutput("mechcoefs")
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            uiOutput("tags_footer")
                        )
                    ),
                    fluidRow(
                        column(
                            1,
                            conditionalPanel(
                                condition = "input.dimension >= 768",
                                tags$img(src = "average_rating_yaxis.png",
                                         id = "yaxis-rating")
                            )
                        ),
                        column(
                            10,
                            
                            fluidRow(
                                column(
                                    6,
                                    plotOutput("year")
                                ),
                                column(
                                    6,
                                    plotOutput("avgweight")
                                )
                            ),
                            fluidRow(
                                column(
                                    6,
                                    plotOutput("players")
                                ),
                                column(
                                    6,
                                    plotOutput("minage")
                                )
                            ),
                            fluidRow(
                                column(
                                    6,
                                    plotOutput("playtime")
                                ),
                                column(
                                    6,
                                    plotOutput("onetwo")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            tags$div(
                                HTML(paste0(
                                    "<p>Data from ",
                                    "<a href=\"https://www.boardgamegeek.com\">",
                                    "BoardGameGeek</a>.</p>"
                                )),
                                class = "footer"
                            )
                        )
                    )
                ),
                
                tabPanel(
                    "Kickstarter pledges", value = "pledges",
                    fluidRow(
                        column(
                            12,
                            selectizeInput(
                                "gameselect_p", choices = NULL, label = NULL,
                                width = "400px", options = list(
                                    placeholder =
                                        "> Enter a game's title to display it below")
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            6, offset = 3,
                            conditionalPanel(
                                condition = "input.loading",
                                tags$div(
                                    class="progress loading-bar",
                                    tags$div(
                                        class=paste(
                                            "progress-bar",
                                            "progress-bar-striped",
                                            "active",
                                            "loading"
                                        ),
                                        role="progressbar",
                                        "aria-valuenow"="1",
                                        "aria-valuemin"="0",
                                        "aria-valuemax"="1",
                                        style="width: 100%",
                                        span("Loading …", class = "loading-text")
                                    ))
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            uiOutput("tags_header_k")
                        )  
                    ),
                    fluidRow(
                        column(
                            6,
                            uiOutput("cats_header_k"),
                            uiOutput("catcoefs_k")
                        ),
                        column(
                            6,
                            uiOutput("mechs_header_k"),
                            uiOutput("mechcoefs_k")
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            uiOutput("tags_footer_k")
                        )
                    ),
                    fluidRow(
                        column(
                            1,
                            conditionalPanel(
                                condition = "input.dimension >= 768",
                                tags$img(src = "kickstarter_pledges_yaxis.png",
                                         id = "yaxis-pledges")
                            )
                        ),
                        column(
                            10,
                            
                            fluidRow(
                                column(
                                    6,
                                    plotOutput("launched")
                                ),
                                column(
                                    6,
                                    plotOutput("avgweight_k")
                                )
                            ),
                            fluidRow(
                                column(
                                    6,
                                    plotOutput("playtime_k")
                                ),
                                column(
                                    6,
                                    plotOutput("minage_k")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            tags$div(
                                HTML(paste0(
                                    "<p>Data from ",
                                    "<a href=\"https://www.boardgamegeek.com\">",
                                    "BoardGameGeek</a> and ",
                                    "<a href=\"https://www.kickstarter.com\">",
                                    "Kickstarter</a>.</p>"
                                )),
                                class = "footer"
                            )
                        )
                    )
                ),
                
                tabPanel(
                    "Kickstarter success/failure", value = "success",
                    fluidRow(
                        column(
                            12,
                            includeHTML("www/explore_descriptions_top.html")
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            selectInput(
                                "nlp_sort", label = "Sort by:",
                                choices = c(
                                    "Coefficient" = "coef",
                                    "Frequency" = "freq",
                                    "# of models" = "nmodels"
                                ),
                                selectize = FALSE
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            conditionalPanel(
                                condition = "input.loading",
                                tags$div(
                                    class="progress loading-bar",
                                    tags$div(
                                        class=paste(
                                            "progress-bar",
                                            "progress-bar-striped",
                                            "active",
                                            "loading"
                                        ),
                                        role="progressbar",
                                        "aria-valuenow"="1",
                                        "aria-valuemin"="0",
                                        "aria-valuemax"="1",
                                        style="width: 100%",
                                        span("Loading …", class = "loading-text")
                                    ))
                            )
                        )
                    ),
                    fluidRow(
                        column(
                            12,
                            plotOutput("nlp", height = "11000px")
                        )
                    )
                )
            )
        )
    )
)
