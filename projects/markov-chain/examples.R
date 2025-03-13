library(tidyverse)

# tyvvm Claude: https://claude.ai/share/b82ea41f-0b6f-4c96-8ead-9b22f2fdd838
source("funs_basic-model.R")
source("funs_ngram-model.R")


# words | next_word | n | probability
# ----------------------------------
# the   | dog       | 7 | 0.467 (7/15)
# the   | cat       | 5 | 0.333 (5/15)
# the   | house     | 3 | 0.200 (3/15)


text_alice <- readLines("text/alice.txt", warn = FALSE) |> paste(collapse = " ")

model_basic <- build_markov_model_basic(text_alice)

generate_text_basic(model_basic)


model_2 <- build_markov_model(text_alice, n = 2)

generate_text(
  model_2,
  num_sentences = 5,
  temperature = 1
)

model_3 <- build_markov_model(text_alice, n = 3)

generate_text(
  model_3,
  num_sentences = 5,
  temperature = 1
)


# All 9 Skywalker-saga Star Wars movies (no *Rogue One* or *Solo*)
text_star_wars <- readLines("text/star_wars_all.txt", warn = FALSE) |> paste(collapse = " ")

model_sw_2 <- build_markov_model(text_star_wars, n = 2)

generate_text(
  model_sw_2,
  num_sentences = 5,
  temperature = 1
)


# A bunch of books!
txt_files <- list.files(
  path = "text",
  pattern = "\\.txt$",
  full.names = TRUE
)

text_a_ton_of_things <- txt_files |>
  map(\(x) readLines(x, warn = FALSE)) |>
  map_chr(\(x) paste(x, collapse = " ")) |>
  paste(collapse = " ")

model_huge_2 <- build_markov_model(text_a_ton_of_things, n = 2)

generate_text(
  model_huge_2,
  num_sentences = 5,
  temperature = 1
)
