# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

launchApp <- function() {

  pacman::p_load(shiny, DBI, dbplyr, shinyjs, shinyalert, tidyverse, shinyBS,
                 spsComps, magrittr, shinyWidgets, plotly, ggpol, gt,
                 reshape2, textdata, ggnewscale, shinydashboard,
                 shinycssloaders, tidytext, tm, reactable,
                 wordcloud2, sentimentr, RColorBrewer, stringr,
                 cleanNLP, topicmodels, ggwordcloud,
                 marker, htmltools, fontawesome)

  cnlp_init_udpipe()


  #----- Definition of Functions ------------------------------------------------#

  csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
    tags$button(
      tagList(icon("download"), label),
      onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
    )
  }

  lexicon_colors <- list(
    bing = c(positive = '#67a9cf',
             negative = '#ef8a62'),
    loughran = c(constraining = '#fccde5',
                 litigious = '#fb8072',
                 negative = '#ef8a62',
                 positive = '#67a9cf',
                 superfluous = '#fccde5',
                 uncertainty = '#ffffb3'),
    afinn = c('-5' = '#67001f',
              '-4' = '#b2182b',
              '-3' = '#d6604d',
              '-2' = '#f4a582',
              '-1' = '#fddbc7',
              '0' = '#f7f7f7',
              '1' = '#d1e5f0',
              '2' = '#92c5de',
              '3' = '#4393c3',
              '4' = '#2166ac',
              '5' = '#053061')
  )



  sentiment_highlighter <- function(text,
                                    lexicon = c('bing', 'loughran', 'afinn'),
                                    token = 'words',
                                    lexicon_dir = '.') {
    colors <- NULL
    sentiments <- NULL
    if(lexicon[1] == 'nrc') {
      # sentiments <- tidytext::get_sentiments("nrc")
      sentiments <- textdata::lexicon_nrc(dir = lexicon_dir)
    } else if(lexicon[1] == 'bing') {
      # sentiments <- tidytext::get_sentiments('bing')
      sentiments <- tidytext::get_sentiments("bing")
    } else if(lexicon[1] == 'afinn') {
      # sentiments <- tidytext::get_sentiments("afinn")
      sentiments <- textdata::lexicon_afinn(dir = lexicon_dir)
      sentiments$sentiment <- as.character(sentiments$value)
    } else if(lexicon[1] == 'loughran') {
      # sentiments <- tidytext::get_sentiments("loughran")
      sentiments <- textdata::lexicon_loughran(dir = lexicon_dir)
    } else {
      stop(paste0('Unknown lexicon: ', lexicon[1]))
    }
    colors <- lexicon_colors[[lexicon[1]]]
    colors_df <- data.frame(sentiment = names(colors),
                            color = colors,
                            row.names = NULL)

    paragraphs <- strsplit(text, '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = token,
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word')) |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens$html <- tokens$token
    sentiment_rows <- !is.na(tokens$color)
    tokens[sentiment_rows,]$html <- paste0("<span style='background-color: ",
                                           tokens[sentiment_rows,]$color,
                                           "; word-wrap: break-word; display: inline; white-space: pre-wrap;' class='tooltip2'>",
                                           # ADD HOVER EFFECT!!!!
                                           # "<span class='tooltiptext2'>",
                                           # tokens[sentiment_rows,]$sentiment,
                                           # "</span>",
                                           tokens[sentiment_rows,]$token,
                                           "</span>")
    html <- ''
    for(i in seq_len(length(paragraphs))) {
      html <- paste0(html, '<p>',
                     paste0(tokens[tokens$paragraph == i,]$html, collapse = ' '),
                     '</p>')
    }
    # html <- paste0(tokens$html, collapse = ' ')
    return(html)
  }


  sentiment_plot <- function(text,
                             lexicon = c('bing', 'loughran', 'afinn'),
                             lexicon_dir = '.'
  ) {
    if(lexicon[1] == 'nrc') {
      # sentiments <- tidytext::get_sentiments("nrc")
      sentiments <- textdata::lexicon_nrc(dir = lexicon_dir)
    } else if(lexicon[1] == 'bing') {
      sentiments <- tidytext::get_sentiments("bing")
    } else if(lexicon[1] == 'afinn') {
      # sentiments <- tidytext::get_sentiments("afinn")
      sentiments <- textdata::lexicon_afinn(dir = lexicon_dir)
      sentiments$sentiment <- as.character(sentiments$value)
    } else if(lexicon[1] == 'loughran') {
      # sentiments <- tidytext::get_sentiments("loughran")
      sentiments <- textdata::lexicon_loughran(dir = lexicon_dir)
    } else {
      stop(paste0('Unknown lexicon: ', lexicon[1]))
    }
    colors <- lexicon_colors[[lexicon[1]]]
    colors_df <- data.frame(sentiment = names(colors),
                            color = colors,
                            row.names = NULL)

    paragraphs <- strsplit(text, '\n')[[1]]

    tokens <- data.frame(paragraph = seq_len(length(paragraphs)),
                         text = paragraphs) |>
      dplyr::mutate(text = stringr::str_remove_all(text, '  ')) |>
      tidytext::unnest_tokens(token, text,
                              token = 'words',
                              to_lower = FALSE,
                              strip_punct = FALSE) |>
      dplyr::mutate(token_lower = tolower(token)) |>
      dplyr::left_join(sentiments, by = c('token_lower' = 'word'), multiple = 'all') |>
      dplyr::left_join(colors_df, by = c('sentiment' = 'sentiment'))

    tokens_tab <- table(tokens$sentiment) |> as.data.frame()

    ggplot2::ggplot(tokens_tab, ggplot2::aes(x = Var1, y = Freq, fill = Var1)) +
      ggplot2::geom_bar(stat = 'identity') +
      ggplot2::geom_text(ggplot2::aes(label = Freq), hjust = -0.1) +
      ggplot2::expand_limits(y = max(tokens_tab$Freq) + max(tokens_tab$Freq) * .05) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_x_discrete(limits = names(colors)) +
      ggplot2::coord_flip() +
      ggplot2::xlab('') +
      ggplot2::ylab('Frequency') +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'none')
  }



  # Define the UI
  ui <- navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    ###############################################.
    ## Header ----
    ###############################################.

    "Database Management",

    ###############################################.
    ## Sidebar & Data Panel ----
    ###############################################.

    tabPanel("Home", icon = icon("home"), value = "home",
             tags$head(
               tags$script(src = "https://platform.twitter.com/widgets.js", charset = "utf-8"),
               tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Mono", rel = "stylesheet"),
               tags$style(HTML('
      * {
        font-family: Roboto Mono;
        font-size: 100%;
      }
      #sidebar {
         background-color: #fff;
         border: 0px;
      }
      .rt-th {
        display: none;
      }
      .rt-noData {
        display: none;
      }
      .rt-pagination-nav {
        float: left;
        width: 100%;
      }
    '))
             ),

             fluidRow(
               column(1,  style = "width: 5px;"),
               column(11,
                      h3("Welcome to the Database Management Shiny App.", class = "data-main-title"),
                      p(
                        br(),
                        "If you want access to the repository of this package, see ",
                        a("here.", href = "https://github.com/IsaacBravo/ShinyNews",
                          target = "_blank",
                          class = "here-pop-up",
                          id = "here"
                        )
                      ),
                      br()
               )),
             fluidRow(
               column(1,  style = "width: 5px;"),
               column(11,
                      h3("Features:"),
                      p(
                        "Explore the key features of the Database Management Shiny App:",
                        tags$ul(
                          tags$li("Setting up database access: Easily configure and set up your database connection."),
                          tags$li("Guided Search: Perform a targeted search in all news data based on keywords or source."),
                          tags$li("Choose Date Range: Filter data by selecting a specific date range."),
                          tags$li("Run Query: Execute custom queries and create tables with the click of a button."),
                          tags$li("Source Search: Search within the source code for specific terms."),
                          tags$li("Table Results: View and analyze the results of your queries in an interactive table.")
                        ),
                        hr())
               )),

               fluidRow(
                 column(1,  style = "width: 5px;"),
                 column(11,
                        h3("Rules of Use:"),
                        p(
                          "The following are some rules of use for extracting data from the database:",
                          tags$ul(
                            tags$li("Check if the folder path matches the folder path of the removable disk where the database is located."),
                            tags$li("To generate the table the user must complete the three available filters: keywords, date range and source code. Then click on the 'Create Table' button."),
                            tags$li("To generate a new data table, the user must first click on the 'Reset' button, and after completing point 1 of this list. "),
                            tags$li("Due to the time required to extract the data. It is recommended that the user access and download the data, using the combination 'Keywords' + '1 Year' + '1 Source Code'.")
                          ),
                          hr()
                        )
                 )),
             fluidRow(
               column(1,  style = "width: 5px;"),
               column(11,
                      h3("Dependencies:"),
                      p(
                        "Please before installing this app-package make sure you have installed the following packages:",
                        DT::DTOutput("tbl_packages"),
                      hr())
               ))
             ),



    tabPanel("Main Panel", icon = icon("table"), value = "table",


             fluidPage(

               tags$style(
                 HTML(".custom-well-panel, .custom-well-panel2, .custom-well-panel3, .custom-well-panel4 {
    color: #2E3E51; /* Text color: #7D8A8B */
    text-align: left; /* Align left */
    border: 1px solid #EDEDED; /* Border color: #EDEDED */
    background: transparent; /* Transparent background: #ECF0F1 */
    border-radius: 10px; /* Rounded corners */
    font-size: 16px; /* Adjust the font size as needed */
    min-height: 110px; /* Set the desired height in pixels */
    font-family: Roboto Mono;
  }"),
                 HTML(".run-query-panel {
    color: #2E3E51; /* Text color: #7D8A8B */
    text-align: left; /* Align left */
    border: 1px solid white; /* Border color: #EDEDED */
    background: transparent; /* Transparent background: #ECF0F1 */
    border-radius: 10px; /* Rounded corners */
    font-size: 16px; /* Adjust the font size as needed */
    min-height: 110px; /* Set the desired height in pixels */
        display: flex; /* Enable flexbox layout */
    flex-direction: column; /* Arrange items vertically */
    justify-content: center; /* Center items vertically */
    align-items: center; /* Center items horizontally */
    font-family: Roboto Mono;
  }"),
                 HTML(".run-query-panel > .shiny-input-container {
    margin: 10px; /* Add space around the input container (buttons) */
  }"),
                 HTML(".custom-h4 {
    font-weight: bold; /* Make h3 bold */
    font-family: Roboto Mono;
  }"),

                 HTML(".custom-h5 {
    font-weight: bold; /* Make h3 bold */
    color: white; /* Text color: #7D8A8B */
    font-family: Roboto Mono;
  }"),
                 HTML("
    .custom-div {
      display: flex;
      align-items: center; /* Center vertically */
    }"),
                 HTML("
    code {
      color: white;
      background-color: #41BC9C;
      padding: 2px;
      font-size: 105%;
    }"),
                 HTML(".custom-button {
    width: 100%; /* Set the width to a fixed value or percentage */
  }")
               ),

               fluidRow(
                 column(8,
                        div(
                          h4("Setting up database access", class = "custom-h4"),
                          wellPanel(
                            id = "wellPanelId1",
                            class = "custom-well-panel",
                            fluidRow(column(10, textInput("path_input", "Enter new folder path...",
                                                          value = "E:/Emmy_Project/Projects/FactivaAPI/tidy/sparkDFCoreCl.parquet"))))))),

               hr(),

               fluidRow(
                 div(
                   column(4,
                          div(
                            h4("Guided Search", class = "custom-h4"),
                            wellPanel(
                              id = "wellPanelId2",
                              class = "custom-well-panel2",
                              fluidRow(column(12, textInput("filter_regex", "Search in all News for:", placeholder = "Enter keywords or source")))
                            )
                          )),
                   column(4,
                          div(
                            h4("Choose Date Range", class = "custom-h4"),
                            wellPanel(
                              id = "wellPanelId3",
                              class = "custom-well-panel3",
                              fluidRow(column(12,   dateRangeInput("daterange1", "Date range:",
                                                                   start = "2001-01-01",
                                                                   end   = "2023-01-01"))),
                            ))),
                   column(4,
                          div(
                            h4("Run Query", class = "custom-h4"),
                            fluidRow(
                              class = "run-query-panel",

                              column(2),
                              column(5, actionButton("create_button", "Create Table", class = "custom-button"),
                                     tags$div(style = "margin-top: 10px;")),
                              column(5, actionButton("reset_button", "Reset", class = "custom-button")))
                          ))
                 )
               ),

               fluidRow(
                 div(
                   column(4,
                          div(
                            h4("Source Search", class = "custom-h4"),
                            wellPanel(
                              id = "wellPanelId2",
                              class = "custom-well-panel2",
                              fluidRow(column(12,

                                              textInput("filter_code", "Search in source code for:", placeholder = "Enter source code")
                              )))))
                 )),

               hr(),
               fluidRow(
                 column(12,
                        h3("Table Results:", class = "custom-h3"),
                        br()
                 )
               ),
               shinycssloaders::withSpinner(DT::DTOutput("tbl"), type = 5, color = "#0275D8", size = 1)

             )),

    tabPanel("Analysis", icon = icon("calculator"), value = "calculator",
             # Define sidebar UI
             sidebarLayout(
               sidebarPanel(
                 textAreaInput("input_text", label = "Enter Text:", width = "100%", height = "200px"),
                 tags$style(
                   HTML("#input_text { width: 100%; }")
                 ),
                 textInput("highlight_words", label = "Highlight Words (comma-separated):"),
                 p("Enter the keyword(s) to be highlighted in the text, based on the valence."),
                 br(),
                 selectInput(
                   inputId = "n_topics",
                   label = "How many topics does your text explore?",
                   choices = c("No idea" = NA,
                               "2" = 2,
                               "3" = 3,
                               "4" = 4,
                               "5" = 5,
                               "More than 5" = 6
                   ),
                   selected = "No idea"
                 ),
                 br(),
                 selectInput('sentiment_lexicon',
                             choices = c('Bing binary sentiment' = 'bing',
                                         # 'NRC Word-Emotion Association' = 'nrc',
                                         'Loughran-McDonald Sentiment' = 'loughran',
                                         'AFINN-111 dataset' = 'afinn'),
                             selected = 'bing',
                             label = 'Sentiment lexicon:')
               ),
               mainPanel(
                 p("This application allows you to copy a text from any type of file to run
        a basic and exploratory text analysis."),
                 p(tags$ul(tags$li("The first tab focuses on displaying the text loaded in the application.
                        This tab can provides a brief summary of the text loaded, and also allows
                        to the user highlight keywords, or positive and negative words."),
                           tags$li("The second set presents a basic text analysis, showing the
                        words frequency of the text loaded, along with a visualization
                        of the data."),
                           tags$li("The third set presents a sentiment analysis based on the
                        package sentimentr, where the user can check the valence by
                         sentence, and the tendency of the valence across the text."))),
                 hr(),
                 tabsetPanel(
                   tabPanel("Highlight Text",
                            h3("Summary Text:"),
                            reactableOutput("summary_table"),
                            h3("Main Text:"),
                            div(
                              class = "container",
                              div(
                                class = "well",
                                id = "text-to-mark",
                                uiOutput("highlighted_text")
                              )),
                            useMarker(),
                            tags$head(
                              tags$style(
                                ".red{background-color:#FFB8C3;}
                        .blue{background-color:#6ECFEA;}"
                              )
                            )

                   ),
                   tabPanel("Text Analyzer",
                            h3("Explore Text:"),
                            align = "center",
                            br(),

                            fluidRow(
                              column(2),
                              column(3, actionButton("word_freq_button", "Show Summary Tables")),
                              column(2),
                              column(3, actionButton("plot_freq_button", "Show Summary Plots")),
                              column(2)
                            ),
                            fluidRow(
                              column(6,
                                     shinycssloaders::withSpinner(reactableOutput("word_freq_table")),
                                     align = "center"
                              ),
                              column(6,
                                     wordcloud2Output("cloud"),
                                     align = "center"
                              )
                            ),
                            hr(),
                            fluidRow(
                              column(6,
                                     reactableOutput("table_entities"),
                                     align = "center"
                              ),
                              column(6,
                                     shinycssloaders::withSpinner(plotOutput("plot_entities")),
                                     align = "center"
                              )
                            )


                   ),
                   tabPanel("Sentiment Analyzer",
                            h3("Sentiment Exploration:"),
                            actionButton("analyze_sentiment_button", "Analyze Sentiment"),
                            align = "center",
                            fluidRow(
                              column(12,
                                     br(),
                                     p(HTML("<i>If you wish to see the output from sentiment_by line by line with positive/negative sentences highlighted, check out this button!</i>")),
                                     align = "center")
                            ),
                            br(),
                            fluidRow(
                              shiny::column(
                                8,
                                shinycssloaders::withSpinner(shiny::htmlOutput('sentiment_text'))
                              ),
                              shiny::column(
                                4,
                                shinycssloaders::withSpinner(shiny::plotOutput('sentiment_text_plot', height = '400px'))
                              )
                            ),
                            br(),
                            fluidRow(
                              column(2),
                              column(8,
                                     reactableOutput("table_sentences_avg"),
                                     align = "center"
                              ),
                              column(2)
                            ),
                            br(),
                            fluidRow(
                              column(2),
                              column(8,
                                     reactableOutput("table_sentences"),
                                     align = "center"
                              ),
                              column(2)
                            ),
                            br(),
                            fluidRow(
                              column(2),
                              column(8,
                                     shinycssloaders::withSpinner(plotOutput("plot_sentiment")),
                                     align = "center"
                              ),
                              column(2)
                            )
                   ),
                   tabPanel("Topic Analyzer",
                            h3("Topic Exploration:"),
                            align = "center",
                            fluidRow(
                              column(2),
                              column(2, actionButton("test_topic_button", "LDA Test")),
                              column(1),
                              column(2, actionButton("analyze_topic_button", "LDA Analysis")),
                              column(1),
                              column(2, actionButton("viz_topic_button", "LDA Plots")),
                              column(2)
                            ),
                            fluidRow(
                              column(12,
                                     br(),
                                     p(HTML("<i>If you wish to see the some topic analysis, check out these buttons!</i>")),
                                     align = "center")
                            ),
                            br(),
                            fluidRow(
                              column(4, shinycssloaders::withSpinner(plotOutput("topic_test"))),
                              column(4,
                                     shinycssloaders::withSpinner(reactableOutput("topic_table")),
                                     br(),
                                     shinycssloaders::withSpinner(plotOutput("topic_cloud"))),
                              column(4, shinycssloaders::withSpinner(plotOutput("topic_plot"))),
                            )
                   )

                 )
               )
             )





             ),

    tabPanel("About", icon = icon("info"), value = "info",
             fluidRow(
               column(1,  style = "width: 5px;"),
               column(11,
                      h3("About the Database Management Shiny App.",
                         class = "data-main-title"),
                      p("This Shiny App is designed to help you manage and analyze data from your database efficiently. ",
                        "It provides a user-friendly interface to perform various tasks related to database access, guided search, and more."), p(
                        "If you want access to the repository of this package, see ",
                        a("here.", href = "https://github.com/IsaacBravo/ShinyNews",
                          target = "_blank",
                          class = "here-pop-up",
                          id = "here"
                        )),
                      hr(),
                      HTML("<p>(Made by <a href='https://twitter.com/IsaacBr45419303'>@IsaacBr</a>. Source code <a href='https://github.com/IsaacBravo/ShinyNews'>on GitHub</a>.)</p>")
                      ),
                      br()
               ),
             fluidRow(
                 column(1,  style = "width: 5px;"),
                 column(11,
                        h3("Source Codes: ",
                           class = "data-main-title"),
                        p("Here you can check the code/full name/language of each publisher on the database:"),
                        DT::DTOutput("tbl_source"))
             ))
  )




  # Define the server logic
  server <- function(input, output, session) {

    # create loaders
    tbl <- addLoader$new("tbl", type = "facebook")

    data <- reactiveVal(NULL)
    timing <- reactiveVal(NULL)


    data_packages <- data.frame(
      `Shiny-verse` = c("shiny","shinyjs","shinyalert", "shinyWidgets",
                        "shinydashboard", "shinycssloaders","shinyBS",
                        "htmltools", "marker", "spsComps"),
      `Visualisations` = c("plotly", "ggpol","ggnewscale","wordcloud2",
                           "RColorBrewer","ggwordcloud","fontawesome",
                           "", "", ""),

      `Data Base` = c("DBI", "dbplyr", "tidyverse", "reactable", "gt", "DT",
                      "","","",""),
      `Analysis` = c("magrittr", "reshape2", "textdata", "tm", "sentimentr",
                   "stringr", "cleanNLP", "topicmodels","","")
      )

    data_source <- data.frame(
        source_code = c("NURNBN", "RZEPOL", "TURALG", "RBBTRK", "NQTEE", "ATJC", "NS", "THESUN", "NSONL",
                        "GAZWYB", "SAARZG", "J", "NSONL", "NTLN", "RTLTRK", "GRDN", "GRDN", "DWELT",
                        "WSJO", "TELUK", "J", "ARDTRK", "BRETRK", "GRULTD", "SFNM", "BILPUS", "LIVECH",
                        "SRFTRK", "PHLI", "LVSN", "BRATRK", "SDDZ", "TAGSS", "WSJO", "FKENG", "STPT",
                        "TNWI", "BALMMT", "GRDN", "WELTON", "DT", "SDDZ", "FKTPO", "BILDE", "MSP",
                        "NRCO", "EVETIM", "DN", "BRATRK", "SATTRK", "J", "WP", "GAZWYB", "OSTSEZ",
                        "WPCOM", "TAGON", "GAZEPL", "TELUK", "wsjo", "THESUK", "NYPO", "BAYTRK",
                        "WESKU", "OSTSON", "LIVEONL", "SLMO", "NS", "GRULTD", "ZBILD", "MDRTRK",
                        "WP", "NDRTRK", "NSHR"),
        source_name = c("Nürnberger Nachrichten", "Rzeczpospolita", "Thüringer Allgemeine",
                        "RBB Transkripte", "Bournemouth Echo", "The Atlanta Journal - Constitution",
                        "Evening Standard", "The Sun", "Evening Standard Online",
                        "Gazeta Wyborcza & Wyborcza.pl", "Saarbrücker Zeitung",
                        "The Wall Street Journal", "London Evening Standard Online",
                        "NBC News: Nightly News", "RTL Transkripte", "The Guardian", "The Guardian",
                        "Die Welt", "The Wall Street Journal Online", "The Telegraph Online",
                        "The Wall Street Journal", "ARD Transkripte", "Radio Bremen TV Transkripte",
                        "Guardian.co.uk", "The Santa Fe New Mexican", "BILD Plus",
                        "Liverpool Echo", "SR Fernsehen Transkripte", "The Philadelphia Inquirer",
                        "Las Vegas Sun", "BR Alpha Transkripte", "Süddeutsche Zeitung",
                        "Der Tagesspiegel", "The Wall Street Journal Online", "Fakt",
                        "Tampa Bay Times", "Times of Northwest Indiana",
                        "Ballymoney & Moyle Times", "The Guardian", "WELT online",
                        "The Daily Telegraph", "Süddeutsche Zeitung", "Fakt", "bild.de",
                        "Star-Tribune", "The Northern Echo", "Evening Times", "Deseret News",
                        "ARD Alpha Transkripte", "Sat.1 Transkripte", "The Wall Street Journal",
                        "The Washington Post", "Gazeta Wyborcza", "Ostsee-Zeitung",
                        "Washington Post.com", "Der Tagesspiegel Online", "Gazeta.pl",
                        "Telegraph.co.uk", "The Wall Street Journal Online", "thesun.co.uk",
                        "New York Post", "Bayerisches Fernsehen Transkripte", "Weser Kurier",
                        "Ostsee-Zeitung Online", "liverpoolecho.co.uk", "St. Louis Post-Dispatch",
                        "London Evening Standard", "Guardian Unlimited", "BILD",
                        "MDR Transkripte", "The Washington Post", "NDR Transkripte",
                        "PBS: PBS NewsHour"),
        language_code = c("de", "pl", "de", "de", "en", "en", "en", "en", "en", "pl", "de", "en", "en", "en", "de", "en",
                          "en", "DE", "de", "en", "en", "en", "de", "de", "en", "de", "en", "de", "en", "de", "en", "en",
                          "de", "DE", "de", "en", "en", "en", "en", "en", "en", "en", "de", "en", "de", "pl", "de", "en",
                          "de", "en", "en", "de", "en", "de", "en", "de", "en", "en", "en", "en", "de", "DE", "pl", "de",
                          "en", "de", "en", "en", "en", "de", "en", "en", "de")
      )

    observeEvent(input$create_button, {
      regexQCode <- input$filter_code
      regexQ <- input$filter_regex


      # Measure the time it takes to execute the query
      timing_start <- Sys.time()

      query <- paste0("CREATE TABLE source_data AS (SELECT body, language_code, source_name, source_code, publisher_name, title, publication_date_mdy, word_count FROM '", input$path_input, "/*snappy.parquet' WHERE (publication_date_mdy >='", input$daterange1[1], "' AND publication_date_mdy <= '", input$daterange1[2],  "' AND source_code = '", regexQCode, "'));")

      con <- dbConnect(duckdb::duckdb(), ":memory:")
      dbExecute(con, query)

      newDF <- tbl(con, "source_data") %>%
        filter(grepl(regexQ, title, ignore.case = TRUE) | grepl(regexQ, body, ignore.case = TRUE)) %>%
        collect()

      # Record the time taken
      timing_end <- Sys.time()
      timing_duration <- timing_end - timing_start
      timing(timing_duration)

      data(newDF)

      dbDisconnect(con, shutdown = TRUE)
    })

    observeEvent(input$reset_button, {
      data(NULL)
      timing(NULL)
    })

    output$tbl <- DT::renderDT({
      on.exit(tbl$hide())
      tbl$show()
      Sys.sleep(1)

      if (is.null(data())) {
        return(NULL)
      }

      DT::datatable(data(),
                    style = 'bootstrap',
                    rownames = FALSE,
                    extensions = c('Buttons', 'FixedHeader', 'KeyTable'),
                    plugins = 'natural',
                    options = list(dom = 'Bfrtip', pageLength = 1, buttons = list(
                      list(extend = "collection", buttons = c('csv', 'excel', 'pdf'),
                           text = "Download Current Page", filename = "page",
                           exportOptions = list(
                             modifier = list(page = "current")
                           )
                      ),
                      list(extend = "collection", buttons = c('csv', 'excel', 'pdf'),
                           text = "Download Full Results", filename = "data",
                           exportOptions = list(
                             modifier = list(page = "all")
                           )
                      )
                    )))


    })


    observe({
      if (!is.null(timing())) {
        shinyalert("Timing Information", paste("Query execution time:\n", timing(),
                                               "\nNumber of rows: ", nrow(data())))
      }
    })

    ###############################################.
    ## ObserveEvents ----
    ###############################################.

    observeEvent(input$plot_freq_button, {
      req(input$input_text)  # Ensure input is available
      plt_table_udipe()
      word_frequency_cloud()
    })

    observeEvent(input$word_freq_button, {
      req(input$input_text)  # Ensure input is available
      word_frequency()
      table_entities_udipe()
    })

    observeEvent(input$input_text, {
      # Get the sentences from the input text
      req(input$input_text)

      sentences_df <-
        sentiment(input$input_text) %>%
        rename("sentence_ID" = "element_id") %>%
        rename("text_ID" = "sentence_id")

      # Output the extracted sentences
      output$table_sentences <- renderReactable({
        reactable(sentences_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    })

    observeEvent(input$analyze_sentiment_button, {
      req(input$input_text)  # Ensure input is available

      input$input_text %>%
        sentiment_by() %>%
        sentimentr:: highlight()
    })

    observeEvent(input$viz_topic_button, {
      req(input$input_text)
      topic_model_plot()
    })

    observeEvent(input$test_topic_button, {
      req(input$input_text)
      topic_model_test()
    })

    observeEvent(input$analyze_topic_button, {
      req(input$input_text)
      topic_word_plot()
    })

    observe({
      highlight_text()
    })


    marker <- marker$new("#text-to-mark")

    observeEvent(input$text, {
      marker$
        unmark(className = "red")$
        mark(input$text, className = "red", send_marked = TRUE, send_not_matched = TRUE)
    })

    observeEvent(input$text2, {
      marker$
        unmark(className = "blue")$
        mark(input$text2, className = "blue")
    })

    ###############################################.
    ## Server Functions ----
    ###############################################.

    highlight_text <- function(words = NULL, background_color = "#E2F0D9", text_color = "red", append = FALSE) {
      input_text <- input$input_text
      if (is.null(words)) {
        words <- character(0)
        if (!is.null(input$highlight_words)) {
          words <- strsplit(input$highlight_words, ",\\b")[[1]]
        }
      }

      highlighted_text <- input_text
      highlight_count <- 0

      for (word in words) {
        highlighted_text <- gsub(word, paste0("<span style='background-color: ", background_color, ";color:", text_color, "; font-weight: bold'>", word, "</span>"), highlighted_text, ignore.case = TRUE)
        highlight_count <- highlight_count + length(gregexpr(word, input_text, ignore.case = TRUE)[[1]])
      }

      # Add line breaks to the highlighted text
      highlighted_text <- gsub("\n", "<br>", highlighted_text)

      if (!append) {
        output$highlighted_text <- renderUI({
          HTML(highlighted_text)
        })
      }
    }

    word_frequency <- function() {
      req(input$input_text)  # Ensure input is available

      input_text <- tolower(input$input_text)

      # Remove punctuation
      input_text <- gsub("[[:punct:]]", "", input_text)

      corpus <- Corpus(VectorSource(input_text))
      corpus <- tm_map(corpus, content_transformer(tolower))
      dtm <- DocumentTermMatrix(corpus)
      freq_matrix <- as.matrix(dtm)
      word_freq <- colSums(freq_matrix)

      all_word_freq_df <- data.frame(
        Word = names(word_freq),
        Frequency = word_freq,
        stringsAsFactors = FALSE
      )

      output$word_freq_table <- renderReactable({
        reactable(all_word_freq_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    }

    table_entities_udipe <- function() {
      req(input$input_text)  # Ensure input is available

      annotation <- cnlp_annotate(input = input$input_text)

      annotation_df <- annotation$token %>%
        select(id = doc_id, token, lemma, entity = upos)

      output$table_entities <- renderReactable({
        reactable(annotation_df, showPageInfo = TRUE, searchable = TRUE,
                  filterable = TRUE, showPageSizeOptions = TRUE,
                  rownames = FALSE,
                  pageSizeOptions = c(12, 24),
                  defaultPageSize = 12, bordered = TRUE, striped = TRUE, highlight = TRUE)
      })
    }

    plt_table_udipe <- function() {
      req(input$input_text)  # Ensure input is available

      annotation <- cnlp_annotate(input = input$input_text)

      output$plot_entities <- renderPlot({

        annotation$token %>%
          select(id = doc_id, token, lemma, entity = upos) %>%
          group_by(entity) %>%
          summarize(total = n()) %>%
          mutate(entity = fct_reorder(entity, total)) %>%
          ggplot( aes(x=entity, y=total)) +
          geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
          coord_flip() +
          xlab("Entities") +
          xlab("Frequency") +
          theme_bw()
      })
    }

    word_frequency_cloud <- function() {
      req(input$input_text)
      input_text <- tolower(input$input_text)

      # Remove punctuation
      input_text <- gsub("[[:punct:]]", "", input_text)

      corpus <- Corpus(VectorSource(input_text))
      corpus <- tm_map(corpus, content_transformer(tolower))
      dtm <- DocumentTermMatrix(corpus)
      freq_matrix <- as.matrix(dtm)
      word_freq <- colSums(freq_matrix)

      all_word_freq_df <- data.frame(
        Word = names(word_freq),
        Frequency = word_freq,
        stringsAsFactors = FALSE
      )

      # Filter out words with lower frequencies for the word cloud
      min_freq <- 2
      filtered_word_freq_df <- all_word_freq_df[all_word_freq_df$Frequency >= min_freq, ]

      output$cloud <- renderWordcloud2({wordcloud2::wordcloud2(filtered_word_freq_df)})

    }

    topic_model_plot <- function() {

      req(input$input_text)
      req(input$n_topics)
      input_text <- tolower(input$input_text)

      wordsdf <- corpus::text_split(input_text) %>%
        mutate(text = as.vector(text)) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

      doc_matrix <- wordsdf %>%
        group_by(word) %>%
        tally() %>%
        # cast_dtm() needs a document ID
        mutate(doc = rep("1", length(unique(word)))) %>%
        cast_dtm(doc, word, n)

      ap_lda <- LDA(doc_matrix, k = ifelse(input$n_topics != "NA",
                                           as.numeric(as.character(input$n_topics)),
                                           # setting arbitrarily, they can change it
                                           # according to what comes out
                                           3), control = list(seed = 1234))

      ap_topics <- tidy(ap_lda, matrix = "beta")
      ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        slice_max(beta, n = 10) %>%
        ungroup() %>%
        arrange(topic, -beta)

      output$topic_plot <- renderPlot({
        ap_top_terms %>%
          mutate(term = reorder_within(term, beta, topic)) %>%
          ggplot(aes(beta, term, fill = factor(topic))) +
          geom_col(show.legend = FALSE) +
          facet_wrap(~ topic, scales = "free") +
          scale_y_reordered() +
          theme_minimal()
      })
    }

    topic_model_test <- function() {

      req(input$input_text)
      req(input$n_topics)
      input_text <- tolower(input$input_text)

      wordsdf <- corpus::text_split(input_text) %>%
        mutate(text = as.vector(text)) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

      doc_matrix <- wordsdf %>%
        group_by(word) %>%
        tally() %>%
        # cast_dtm() needs a document ID
        mutate(doc = rep("1", length(unique(word)))) %>%
        cast_dtm(doc, word, n)

      FindTopics_result <- ldatuning::FindTopicsNumber(
        doc_matrix,
        topics = seq(2, 20, by = 1),
        metrics = c("CaoJuan2009",  "Deveaud2014"),
        method = "Gibbs",
        control = list(seed = 1234),
        verbose = TRUE
      )

      output$topic_test <- renderPlot({
        ldatuning::FindTopicsNumber_plot(FindTopics_result)
      })

    }

    topic_word_plot <- function() {

      req(input$input_text)
      req(input$n_topics)
      input_text <- tolower(input$input_text)

      wordsdf <- corpus::text_split(input_text) %>%
        mutate(text = as.vector(text)) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

      doc_matrix <- wordsdf %>%
        group_by(word) %>%
        tally() %>%
        # cast_dtm() needs a document ID
        mutate(doc = rep("1", length(unique(word)))) %>%
        cast_dtm(doc, word, n)

      ap_lda <- LDA(doc_matrix, k = ifelse(input$n_topics != "NA",
                                           as.numeric(as.character(input$n_topics)),
                                           # setting arbitrarily, they can change it
                                           # according to what comes out
                                           3), control = list(seed = 1234))

      exampleTermData <- terms(ap_lda, 10)
      word_list_topics <- exampleTermData %>% as.data.frame()

      output$topic_cloud <- renderPlot({

        word_list_topics %>%
          rownames_to_column("id") %>%
          mutate(id = as.numeric(id)) %>%
          pivot_longer(-id, names_to = "topic", values_to = "term") %>%
          ggplot(aes(label = term, size = rev(id), color = topic, alpha = rev(id))) +
          geom_text_wordcloud(seed = 123) +
          facet_wrap(~topic, scales = "free") +
          scale_alpha_continuous(range = c(0.4, 1)) +
          # scale_color_manual(values = c( "dodgerblue4", "firebrick4", "darkgreen")) +
          theme_minimal() +
          theme(strip.background = element_rect(fill = "firebrick"),
                strip.text.x = element_text(colour = "white"))
      })

    }



    ###############################################.
    ## server Outputs ----
    ###############################################.

    output$plot_sentiment <- renderPlot({
      req(input$input_text)
      input_text <- tolower(input$input_text)

      input_text %>%
        get_sentences() %>%
        sentiment() %>%
        plot() +
        theme_minimal() +
        labs(title = "Plotting Valence at the Sentence Level") +
        theme(plot.title = element_text(face = "bold"))

    })

    output$summary_table <- renderReactable({
      input_text <- input$input_text
      word_count <- length(unlist(strsplit(input_text, "\\s+")))
      char_count <- nchar(gsub("\\s+", "", input_text))
      sentence_count <- length(gregexpr("[.!?]", input_text)[[1]])

      reactable(data.frame(
        "Statistic" = c("Word Count", "Character Count", "Sentence Count"),
        "Count" = c(word_count, char_count, sentence_count)), showPageInfo = FALSE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE)
    })

    output$summary_sent_table <- renderReactable({
      req(input$input_text)
      input_text <- tolower(input$input_text)

      input_text %>%
        get_sentences() %>%
        sentiment() %>%
        reactable(
          showPageInfo = FALSE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE)
    })

    output$highlighted_text <- renderUI({
      highlight_text(append = TRUE)
      HTML(highlighted_text)
    })

    ##### Sentiment view of the text
    output$sentiment_text <- shiny::renderText({
      shiny::req(input$input_text)

      thetext <- input$input_text


      return(sentiment_highlighter(thetext,
                                   lexicon = input$sentiment_lexicon,
                                   lexicon_dir = '.'))
    })

    output$sentiment_text_plot <- shiny::renderPlot({
      shiny::req(input$input_text)

      thetext <- input$input_text

      sentiment_plot(thetext,
                     lexicon = input$sentiment_lexicon,
                     lexicon_dir = '.')
    })

    output$topic_table <- renderReactable({
      req(input$input_text)
      req(input$n_topics)
      input_text <- tolower(input$input_text)

      wordsdf <- corpus::text_split(input_text) %>%
        mutate(text = as.vector(text)) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)

      doc_matrix <- wordsdf %>%
        group_by(word) %>%
        tally() %>%
        mutate(doc = rep("1", length(unique(word)))) %>%
        cast_dtm(doc, word, n)

      ap_lda <- LDA(doc_matrix, k = ifelse(input$n_topics != "NA",
                                           as.numeric(as.character(input$n_topics)),
                                           # setting arbitrarily, they can change it
                                           # according to what comes out
                                           3), control = list(seed = 1234))

      exampleTermData <- terms(ap_lda, 10)

      reactable(exampleTermData, showPageInfo = FALSE, searchable = FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE)

    })

    output$tbl_source <- DT::renderDT({

      DT::datatable(data_source)


    })

    output$tbl_packages <- DT::renderDT({

      DT::datatable(data_packages)


    })

    observe({
      if (!is.null(timing())) {
        shinyalert("Timing Information", paste("Query execution time:\n", timing(),
                                               "\nNumber of rows: ", nrow(data())))
      }
    })

  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))

}
