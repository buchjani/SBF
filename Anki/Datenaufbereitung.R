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
prepare_answers <- function(ans_list) {
  ans <- clean_text(unlist(ans_list))
  if(length(ans) < 4) ans <- c(ans, rep("", 4 - length(ans))) # falls zu kurz
  letters_vec <- LETTERS[1:4]

  correct_answer <- ans[1]        # erste Antwort ist korrekt
  idx <- sample(1:4)              # zufällig mischen
  shuffled <- ans[idx]

  correct_letter <- letters_vec[which(shuffled == correct_answer)]
  answers_text <- paste0(letters_vec, ". ", shuffled, collapse = "<br>")

  list(text = answers_text, correct = correct_letter)
}


# Bilder herunterladen + <img>-Tag erzeugen
prepare_images <- function(urls, qid) {
  # URLs in Character-Vektor konvertieren
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
    img_tags <- c(img_tags, sprintf('<img src="%s">', fname))
  }

  paste(img_tags, collapse = "<br>")  # immer Länge 1 zurückgeben
}


# -----------------------------
# Hauptverarbeitung
# -----------------------------

anki_df <- df %>%
  mutate(
    cat_clean      = clean_text(cat),
    question_clean = clean_text(question),
    answers_prep   = map(answers, prepare_answers),
    answers_text   = map_chr(answers_prep, "text"),
    correct_letter = map_chr(answers_prep, "correct"),
    image_html     = map2_chr(image_urls, id, prepare_images),
    front = paste0(
      "Frage ", id, " (", cat_clean, ")<br>",
      question_clean, "<br><br>",
      answers_text,
      ifelse(image_html == "", "", paste0("<br>", image_html))
    ),
    back = clean_text(correct_letter)
  ) %>%
  transmute(front, back)

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

