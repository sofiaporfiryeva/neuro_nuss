library(shiny)
library(dplyr)
library(reticulate)
library(text2vec)


# подключаем питон (переустановила пакет reticulate)

py_require(
  packages = "sentence-transformers==2.6.1",
  python_version = ">=3.10"
)

sentence_transformers <- import("sentence_transformers")
model <- sentence_transformers$SentenceTransformer("all-MiniLM-L6-v2")



# загружаю библиографию
combined_df_clean <- read.csv("combined_df_clean.csv", 
                              sep = ";", 
                              stringsAsFactors = FALSE)


# навсякий случай чисти заголовки (удаляем NA и пустые строки)
combined_df_clean <- combined_df_clean %>%
  filter(!is.na(title)) %>%
  mutate(title = trimws(title)) %>%
  filter(title != "")

title_embeddings <- readRDS("title_embeddings.rds")

# семантический поиск 

semantic_search <- function(query, k = 5) {
  query <- trimws(query)
  if (query == "") return(NULL)                  
  
  # эмбеддинг запроса
  q_emb <- model$encode(query)
  
  # сos sim
  sim <- sim2(title_embeddings,
              matrix(q_emb, ncol = length(q_emb)),
              method = "cosine", norm = "l2")[, 1]
  
  # поиск наиболее похожих записей
  idx <- head(order(sim, decreasing = TRUE), k)
  combined_df_clean[idx, ] |>
    mutate(similarity = round(sim[idx], 3))
}

# ui
ui <- fluidPage(
  titlePanel("📚 Ask Martha Nussbaum: What Should I Read?"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("query", "Enter a theme or question:", placeholder = "e.g. joy, classical ethics, fear of death"),
      sliderInput("n", "Number of recommendations:", min = 1, max = 15, value = 5),
      actionButton("go", "Get Recommendations"),
      br(), br(),
      textOutput("status")
    ),
    
    mainPanel(
      tableOutput("results")
    )
  )
)


# server

server <- function(input, output, session) {
  
  # реактив: кнопка → поиск
  results <- eventReactive(input$go, {
    req(input$query)                                   # если ввода нет, то нет и поиска
    output$status <- renderText("🔎 Searching…")
    
    res <- semantic_search(input$query,
                           k = input$n)                # поиск
    
    if (is.null(res)) {
      output$status <- renderText(
        "Martha needs a moment to philosophize about that one. Try again soon — she’s probably debating with Aristotle!"
      )
    } else {
      output$status <- renderText("Found some readings!")
    }
    res                                                # возвращает (NULL или df)
  })
  
  # вывод таблицы
  output$results <- renderTable({
    req(results())                                     # покажем, только если есть
    results()[, c("type", "title", "author", "year", "similarity")]
  })
}
# poehali!
shinyApp(ui, server)
