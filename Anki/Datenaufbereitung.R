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

### Check für Fragen mit mehreren Images
# df <- df[109:113,]


## Datenaufbereitung für Anki (https://docs.ankiweb.net/importing/text-files.html)
# Medienordner anlegen
dir_img <- "./Anki/anki_media"
if(!dir.exists(dir_img)) dir.create(dir_img)

# Hilfsfunktion: Antworten mischen, Richtige finden
prepare_answers <- function(answers) {
  letters_vec <- LETTERS[1:4]
  idx <- sample(1:4)  # zufällige Reihenfolge
  shuffled <- answers[idx]
  correct_letter <- letters_vec[which(idx == 1)] # weil answers[1] die richtige ist
  answers_text <- paste0(letters_vec, ". ", shuffled, collapse = "<br>")
  list(text = answers_text, correct = correct_letter)
}

# Hilfsfunktion: Bilder herunterladen + HTML-Tag erzeugen
prepare_images <- function(urls, qid) {
  if(length(urls) == 0 || all(is.na(urls))) return("")
  img_tags <- c()
  for(i in seq_along(urls)) {
    url <- urls[[i]]
    if(is.na(url) || url == "") next
    # Dateiname erzeugen
    ext <- tools::file_ext(url)
    if(ext == "") ext <- "jpg"
    fname <- paste0("q", qid, "_", i, ".", ext)
    destfile <- file.path(dir_img, fname)
    # Download (falls noch nicht vorhanden)
    if(!file.exists(destfile)) {
      tryCatch({
        download.file(url, destfile, mode = "wb", quiet = TRUE)
      }, error = function(e) message("Fehler beim Download: ", url))
    }
    img_tags <- c(img_tags, sprintf('<img src="%s">', fname))
  }
  paste(img_tags, collapse = "<br>")
}

# Hauptverarbeitung
anki_df <- df %>%
  mutate(
    answers_prep = map(answers, prepare_answers),
    answers_text = map_chr(answers_prep, "text"),
    correct_letter = map_chr(answers_prep, "correct"),
    image_html = map2_chr(image_urls, id, prepare_images),
    front = paste0("Frage ", id, " (", cat, ")<br>",
                   question, "<br><br>",
                   answers_text, "<br>",
                   image_html),
    back = correct_letter
  ) %>%
  select(front, back)

# CSV exportieren (Tab-getrennt für Anki)
write.table(anki_df,
            file = "./Anki/anki_cards.csv",
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")
