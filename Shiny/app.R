#
# Fragenkatalog Sportbootführerschein Binnen
# Quelle: https://www.elwis.de/DE/Sportschifffahrt/Sportbootfuehrerscheine/Fragenkatalog-Binnen/Fragenkatalog-Binnen-neu-node.html
#
# Kapitäne: Philipp und Janine
# Ahoi!
#

library(shiny)
library(rvest)
library(tidyverse)

# Datenaufbereitung --------------------------------------------------------------------------------

## Fragenkatalog Basis
url <- "https://www.elwis.de/DE/Sportschifffahrt/Sportbootfuehrerscheine/Fragenkatalog-Binnen/Basisfragen/Basisfragen-node.html"
html <- read_html(url)
elements <- html %>% html_elements(css = "p, ol, img")
texts_basis <- tibble(
  type = elements %>% html_name(),
  text = elements %>% html_text2(),
  src  = ifelse(type == "img", elements %>% html_attr("src"), NA),
  cat  = "Basis"
)
stand <- pull(texts_basis[grep("^Stand:", texts_basis$text), "text"])

## Fragenkatalog Binnen
url <- "https://www.elwis.de/DE/Sportschifffahrt/Sportbootfuehrerscheine/Fragenkatalog-Binnen/Spezifische-Fragen-Binnen/Spezifische-Fragen-Binnen-node.html"
html <- read_html(url)
elements <- html %>% html_elements(css = "p, ol, img")
texts_binnen <- tibble(
  type = elements %>% html_name(),
  text = elements %>% html_text2(),
  src  = ifelse(type == "img", elements %>% html_attr("src"), NA),
  cat  = "Binnen"
)

## Fragenkatalog Segeln
url <- "https://www.elwis.de/DE/Sportschifffahrt/Sportbootfuehrerscheine/Fragenkatalog-Binnen/Spezifische-Fragen-Segeln/Spezifische-Fragen-Segeln-node.html"
html <- read_html(url)
elements <- html %>% html_elements(css = "p, ol, img")
texts_segeln <- tibble(
  type = elements %>% html_name(),
  text = elements %>% html_text2(),
  src  = ifelse(type == "img", elements %>% html_attr("src"), NA),
  cat  = "Segeln"
)

## Fragenkatalog gesamt
texts_clean <-
  bind_rows(texts_basis, texts_binnen, texts_segeln) %>%
  filter(!(type == "p" & text == "")) %>%
  mutate(
    q_no = str_extract(text, "^([0-9]+)\\.", group = TRUE),
    q_no = ifelse(text %>% str_detect("^Stand:"), -999, q_no),
    q_no = as.numeric(q_no)
  ) %>%
  fill(q_no, .direction = "down") %>%
  filter(! (q_no == -999 | is.na(q_no)))

img_srcs <-
  texts_clean %>%
  filter(type == "img") %>%
  select(q_no, src) %>%
  filter(!is.na(src)) %>%
  group_by(q_no) %>%
  # nest(src = src)
  mutate(
    src = list(src)
  ) %>%
  ungroup()

df <- texts_clean %>%
  filter(type != "img") %>%
  select(-src) %>%
  pivot_wider(names_from = type, values_from = text) %>%
  mutate(
    p = p %>% str_remove("^[0-9]+\\. "),
    ol = map(ol, function(x) str_split_1(x, "\\n\\n\\n")),
  ) %>%
  left_join(img_srcs, by = "q_no") %>%
  rename(question = p,
         answers = ol,
         id = q_no,
         image_urls = src)

# # Check für Fragen mit mehreren Images
# df <- df[109:113,]

# UI -----------------------------------------------------------------------------------------------
ui <- fluidPage(

  fluidRow(
    column(width = 12,
      img(src = "https://www.elwis.de/SiteGlobals/Frontend/Images/wsvlogo.png?__blob=normal&v=1",
          height = "100px", style = "margin-bottom: 15px; margin-top: 15px; vertical-align: middle;"),
      h1("Sportbootführerschein Binnen", style = "display: inline;")
    ),
    br(),
  ),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "cat_filter", "Fragenkatalog(e):",
        choices = unique(df$cat), selected = c("Basis", "Binnen"), inline=TRUE,
      ),
      br(),
      uiOutput("question_ui"),
      actionButton("submit", "Check", icon = icon("question-circle", lib = "font-awesome")),
      br(), br(),
      actionButton("next_btn", "Nächste Frage", icon = icon("fast-forward")),
      width = 6
    ),
    mainPanel(
      h4("Ergebnis"),
      uiOutput("result_ui"),
      width = 6
    )
  ),

  tags$footer(
    p(HTML(paste0(
        "Offizieller Fragenkatalog, Stand: ", stand, " — ",
        '<a href="https://www.elwis.de/DE/Sportschifffahrt/Sportbootfuehrerscheine/Fragenkatalog-Binnen/Fragenkatalog-Binnen-neu-node.html" target="_blank">elwis.de</a>'
      )
    ),
    style = "color: gray;
        text-align: left;
        padding: 10px;
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #f9f9f9;
        margin: 0;
      ")
  )
)

# Server ------------------------------------------------------------------------------------------

server <- function(input, output, session) {

  # NEW: reactive that filters the questions by category
  filtered_df <- reactive({
    req(input$cat_filter)  # only proceed if at least one category selected
    df %>% filter(cat %in% input$cat_filter)
  })

  # NEW: whenever category selection changes, reshuffle and reset index
  observeEvent(input$cat_filter, {
    n <- nrow(filtered_df())
    if (n > 0) {
      question_order(sample(n))
      current_index(1)
    }
  })


  # zufällige Reihenfolge einmal pro App-Start erzeugen
  question_order <- reactiveVal(sample(nrow(filtered_df)))
  current_index <- reactiveVal(1)  # Position in der Permutation

  # Hilfsfunktion: aktuelle Frage zurückgeben
  current_question <- reactive({
    filtered_df()[question_order()[current_index()], ]
  })

  # Antworten mischen
  shuffled <- reactiveVal()

  observe({
    q <- current_question()
    ans <- sample(q$answers[[1]])
    shuffled(
      tibble(
        option = ans,
        is_correct = ans == q$answers[[1]][1]
      )
    )
  })

  output$question_ui <- renderUI({
    q <- current_question()
    ans <- shuffled()

    # falls Bilder vorhanden sind, img-Tag(s) erzeugen
    imgs <- NULL
    if (!is.null(q$image_urls[[1]])) {
      imgs <- lapply(q$image_urls[[1]], function(url) {
        tags$img(src = url, style = "max-width: 100%; margin-bottom: 10px;")
      })
    }

    tagList(
      h4(q$question),
      imgs,
      radioButtons("selected", "", choices = ans$option)
    )
  })


  observeEvent(input$submit, {
    req(input$selected)
    q <- current_question()
    ans <- shuffled()
    chosen <- ans %>% filter(option == input$selected)

    feedback <- if (chosen$is_correct) "✅ Richtig!" else "❌ Falsch."

    output$result_ui <- renderUI({
      tagList(
        p(feedback),
        p(paste("Richtige Antwort:", q$answers[[1]][1])),
        p(paste("Fragenkatalog:", q$cat)),
        p(paste("Frage-ID:", q$id))
      )
    })
  })

  observeEvent(input$next_btn, {
    new_index <- current_index() + 1

    if (new_index > nrow(filtered_df())) {
      question_order(sample(nrow(filtered_df())))
      new_index <- 1
    }

    current_index(new_index)

    output$result_ui <- renderUI({})
  })
}
# Run ----------------------------------------------------------------------------------------------

shinyApp(ui, server)
