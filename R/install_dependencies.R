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

install_dependencies <- function() {

  if(!requireNamespace("shiny", quietly = TRUE)) {
    install.packages("shiny")
    library(shiny, character.only = TRUE)
  }

  if(!requireNamespace("shinyjs", quietly = TRUE)) {
    install.packages("shinyjs")
    library(shinyjs, character.only = TRUE)
  }

  if(!requireNamespace("shinyalert", quietly = TRUE)) {
    install.packages("shinyalert")
    library(shinyalert, character.only = TRUE)
  }

  if(!requireNamespace("shinyWidgets", quietly = TRUE)) {
    install.packages("shinyWidgets")
    library(shinyWidgets, character.only = TRUE)
  }

  if(!requireNamespace("shinydashboard", quietly = TRUE)) {
    install.packages("shinydashboard")
    library(shinydashboard, character.only = TRUE)
  }

  if(!requireNamespace("shinycssloaders", quietly = TRUE)) {
    install.packages("shinycssloaders")
    library(shinycssloaders, character.only = TRUE)
  }

  if(!requireNamespace("shinyBS", quietly = TRUE)) {
    install.packages("shinyBS")
    library(shinyBS, character.only = TRUE)
  }

  if(!requireNamespace("htmltools", quietly = TRUE)) {
    install.packages("htmltools")
    library(htmltools, character.only = TRUE)
  }

  if(!requireNamespace("marker", quietly = TRUE)) {
    install.packages("marker")
    library(marker, character.only = TRUE)
  }

  if(!requireNamespace("spsComps", quietly = TRUE)) {
    install.packages("spsComps")
    library(spsComps, character.only = TRUE)
  }

  if(!requireNamespace("plotly", quietly = TRUE)) {
    install.packages("plotly")
    library(plotly, character.only = TRUE)
  }

  if(!requireNamespace("ggpol", quietly = TRUE)) {
    install.packages("ggpol")
    library(ggpol, character.only = TRUE)
  }

  if(!requireNamespace("ggnewscale", quietly = TRUE)) {
    install.packages("ggnewscale")
    library(ggnewscale, character.only = TRUE)
  }

  if(!requireNamespace("wordcloud2", quietly = TRUE)) {
    install.packages("wordcloud2")
    library(wordcloud2, character.only = TRUE)
  }

  if(!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
    library(RColorBrewer, character.only = TRUE)
  }

  if(!requireNamespace("ggwordcloud", quietly = TRUE)) {
    install.packages("ggwordcloud")
    library(ggwordcloud, character.only = TRUE)
  }

  if(!requireNamespace("fontawesome", quietly = TRUE)) {
    install.packages("fontawesome")
    library(fontawesome, character.only = TRUE)
  }

  if(!requireNamespace("DBI", quietly = TRUE)) {
    install.packages("DBI")
    library(DBI, character.only = TRUE)
  }

  if(!requireNamespace("dbplyr", quietly = TRUE)) {
    install.packages("dbplyr")
    library(dbplyr, character.only = TRUE)
  }

  if(!requireNamespace("lubridate", quietly = TRUE)) {
    install.packages("lubridate")
    library(lubridate, character.only = TRUE)
  }

  if(!requireNamespace("reactable", quietly = TRUE)) {
    install.packages("reactable")
    library(reactable, character.only = TRUE)
  }

  if(!requireNamespace("gt", quietly = TRUE)) {
    install.packages("gt")
    library(gt, character.only = TRUE)
  }

  if(!requireNamespace("DT", quietly = TRUE)) {
    install.packages("DT")
    library(DT, character.only = TRUE)
  }

  if(!requireNamespace("magrittr", quietly = TRUE)) {
    install.packages("magrittr")
    library(magrittr, character.only = TRUE)
  }

  if(!requireNamespace("reshape2", quietly = TRUE)) {
    install.packages("reshape2")
    library(reshape2, character.only = TRUE)
  }

  if(!requireNamespace("textdata", quietly = TRUE)) {
    install.packages("textdata")
    library(textdata, character.only = TRUE)
  }

  if(!requireNamespace("tm", quietly = TRUE)) {
    install.packages("tm")
    library(tm, character.only = TRUE)
  }

  if(!requireNamespace("sentimentr", quietly = TRUE)) {
    install.packages("sentimentr")
    library(sentimentr, character.only = TRUE)
  }

  if(!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr")
    library(stringr, character.only = TRUE)
  }

  if(!requireNamespace("cleanNLP", quietly = TRUE)) {
    install.packages("cleanNLP")
    library(cleanNLP, character.only = TRUE)
  }

  if(!requireNamespace("topicmodels", quietly = TRUE)) {
    install.packages("topicmodels")
    library(topicmodels, character.only = TRUE)
  }
}


