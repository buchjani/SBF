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
    ol = map(ol, function(x) str_remove(x, "\\n\\n")),
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
if (!dir.exists(media_dir)) dir.create(media_dir)

# Hilfsfunktion Text aufräumen
clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\r\n|\r|\n", " ", x)  # harte Umbrüche ersetzen
  x <- gsub("\t", " ", x)
  trimws(x)
}

# Antworten mischen
prepare_answers <- function(ans_list) {
  ans <- unlist(ans_list, use.names = FALSE)
  ans <- vapply(ans, clean_text, FUN.VALUE = character(1))
  if (length(ans) < 4) ans <- c(ans, rep("", 4 - length(ans)))

  letters_vec <- LETTERS[1:4]
  correct_answer <- ans[1]
  idx <- sample(1:4)
  shuffled <- ans[idx]

  correct_pos <- match(correct_answer, shuffled)
  correct_letter <- letters_vec[correct_pos]
  correct_text   <- shuffled[correct_pos]

  # jede Option mit <br> am Ende
  answers_text <- paste0(letters_vec, ". ", shuffled, "<br>", collapse = "")

  list(
    text = answers_text,
    correct_letter = correct_letter,
    correct_text = correct_text
  )
}

# Bilder
prepare_images <- function(urls, qid) {
  urls <- unlist(urls, use.names = FALSE)
  if (length(urls) == 0 || all(is.na(urls))) return("")

  tags <- character(0)
  for (i in seq_along(urls)) {
    url <- as.character(urls[i])
    if (is.na(url) || url == "") next

    ext <- tools::file_ext(url)
    if (ext == "") ext <- "jpg"
    fname <- paste0("q", qid, "_", i, ".", ext)
    destfile <- file.path(media_dir, fname)

    if (!file.exists(destfile)) {
      tryCatch(
        download.file(url, destfile, mode = "wb", quiet = TRUE),
        error = function(e) message("Fehler beim Download: ", url)
      )
    }
    tags <- c(tags, sprintf("<img src=\"%s\"><br>", fname))
  }
  paste(tags, collapse = "")
}

# Hauptverarbeitung
anki_df <- df %>%
  mutate(
    cat_clean      = clean_text(cat),
    question_clean = clean_text(question),

    ans_p          = map(answers, prepare_answers),
    answers_text   = map_chr(ans_p, "text"),
    correct_letter = map_chr(ans_p, "correct_letter"),
    correct_text   = map_chr(ans_p, "correct_text"),

    image_html     = map2_chr(image_urls, id, prepare_images),

    # FRONT: genau definierte Struktur
    front = paste0(
      "Frage ", id, " (", cat_clean, ")<br>",
      question_clean, "<br>",
      image_html,                # falls leer, kommt nix
      "<br>",                    # fester Abstand
      answers_text
    ),

    back = paste0(correct_letter, ". ", correct_text)
  ) %>%
  transmute(front, back)

# -----------------------------
# Export (eine Zeile pro Karte)
# -----------------------------
# Tab-getrennt, ohne Header, UTF-8 (ohne BOM)
write.table(anki_df,
            file = "./anki/anki_cards.tsv",
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")

# Front, Back verdreht weil es offenbar andersrum angezeigt wird
anki_df2 <- anki_df %>%
  select(back, front)
write.table(anki_df2,
            file = "./anki/anki_cards_v2.tsv",
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8")
