library(tidyr)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)

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

## Fragenkatalog gesamt
texts_clean <-
  bind_rows(texts_basis, texts_binnen) %>%
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
  nest(src = src)

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

### Check für Fragen mit mehreren Images
# df <- df[109:113,]

# DAtenaufbereitung für Anki -----------------------------------
set.seed(1)

# Medienordner
media_dir <- "./anki/anki_media"
if(!dir.exists(media_dir)) dir.create(media_dir)

# -----------------------------
# Hilfsfunktionen
# -----------------------------

# Text bereinigen: Zeilenumbrüche -> <br>, Tabs entfernen
clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\r\n|\r|\n", "<br>", x)
  x <- gsub("\t", " ", x)
  trimws(x)
}

# Antworten mischen + richtigen Buchstaben bestimmen
prepare_answers <- function(ans_vec) {
  ans_letters <- LETTERS[1:length(ans_vec)]
  ans_texts   <- ans_vec

  # Antwortoptionen für die Vorderseite (mit <br>)
  text_block <- paste0(ans_letters, ". ", ans_texts, collapse = "<br>")

  # Korrekte Antwort identifizieren
  correct_index <- which(ans_letters == attr(ans_vec, "correct")) # falls markiert
  if(length(correct_index) == 0) correct_index <- 1 # fallback

  list(
    text    = text_block,
    correct = ans_letters[correct_index],
    correct_text = ans_texts[correct_index]
  )
}



# Bilder herunterladen + <img>-Tag erzeugen
prepare_images <- function(urls, qid) {
  urls <- unlist(urls)
  if(length(urls) == 0) return("")

  img_tags <- c()
  for(i in seq_along(urls)) {
    url <- as.character(urls[i])
    if(is.na(url) || url == "") next
    ext <- tools::file_ext(url)
    if(ext == "") ext <- "jpg"
    fname <- paste0("q", qid, "_", i, ".", ext)
    destfile <- file.path(media_dir, fname)
    if(!file.exists(destfile)) {
      tryCatch(download.file(url, destfile, mode="wb", quiet=TRUE),
               error=function(e) message("Fehler beim Download: ", url))
    }
    # Bildblock mit <br> davor und dahinter
    img_tags <- c(img_tags, sprintf('<br><img src="%s"><br>', fname))
  }
  paste(img_tags, collapse = "")
}



# -----------------------------
# Hauptverarbeitung
# -----------------------------

anki_df <- df %>%
  mutate(
    answers_prep   = map(answers, prepare_answers),
    answers_text   = map_chr(answers_prep, "text"),
    correct_letter = map_chr(answers_prep, "correct"),
    correct_text   = map_chr(answers_prep, "correct_text"),
    image_html     = map2_chr(image_urls, id, prepare_images),
    front = paste0(
      "Frage ", id, " (", cat, ")<br>",
      question, "<br>",
      image_html,
      answers_text
    ),
    back = paste0(correct_letter, ". ", correct_text)  # ← angepasst
  ) %>%
  select(front, back)


# -----------------------------
# Export für Anki
# -----------------------------
write.table(anki_df,
            file = "./anki/anki_cards.tsv",
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")

message("Export fertig! Datei 'anki_cards.tsv' und Bilder in '", media_dir, "'")



anki_df2 <- anki_df %>%
  select(back, front)
write.table(anki_df2,
            file = "./anki/anki_cards_v2.tsv",
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")
