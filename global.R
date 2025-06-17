library(shiny); library(dplyr); library(reticulate); library(text2vec)

py_require("sentence-transformers==2.6.1")
st <- import("sentence_transformers")$SentenceTransformer("all-MiniLM-L6-v2")

df <- read.csv("combined_df_clean.csv", sep = ";") |>
  filter(!is.na(title)) |>
  mutate(title = trimws(title)) |>
  filter(title != "")

title_embeddings <- readRDS("title_embeddings.rds")
