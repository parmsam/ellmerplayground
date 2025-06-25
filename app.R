library(shiny)
library(shinychat)
library(ellmer)
library(stringr)
library(purrr)

chat_funcs <- ls("package:ellmer") |>
  str_subset("^chat_")
names(chat_funcs) <- chat_funcs |>
  str_remove_all("chat_")

models_funcs <- ls("package:ellmer") |>
  str_subset("^models_")

model_list <- lapply(models_funcs, function(func) {
  func <- get(func, envir = asNamespace("ellmer"))
  models <- tryCatch({
    func()$id
  }, error = function(e) {
    NULL
  })
})

read_md <- function(md_file){
  readLines(md_file) |>
    paste(collapse = "\n")
}

snake_to_title <- function(snake_case) {
  stringr::str_replace_all(snake_case, "_", " ") |>
    stringr::str_to_title()
}

available_models <- unlist(model_list)

welcome_message <- read_md("welcome_message.md")

ui <- bslib::page_sidebar(
  title = "Ellmer LLM playground",
  sidebar = tags$div(
    selectInput(
      "chat_function",
      "Provider",
      selected = "chat_openai",
      choices = chat_funcs
    ),
    selectizeInput(
      "model",
      "Model",
      selected = "gpt-4o-mini",
      choices = available_models,
      options = list(create = TRUE)
    ),
    textAreaInput(
      "system_prompt",
      "System Prompt",
      height = "10rem",
      value = NULL,
      placeholder = "Describe desired model behavior (tone, tool usage, response style)"
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel("Optional API arguments", uiOutput("api_args"))
    ),
    tags$br(),
    actionButton("clear", "Clear chat"),
    tags$br(),
    tags$br(),
    bookmarkButton()
  ),
  tags$h5("Chat"),
  tags$div(
    chat_ui(
      "chat", 
      messages = welcome_message
    )
  )
)

server <- function(input, output, session) {
  output$api_args <- renderUI({
    available_params <- formals(ellmer::params)
    ignore_params <- "..."
    param_names <- names(available_params[!names(available_params) %in% ignore_params])
    character_params <- "stop_sequences"
    map(param_names, ~ {
      if (!.x %in% character_params) {
        numericInput(.x, snake_to_title(.x), value = isolate(input[[.x]]))
      } else {
        textInput(.x, snake_to_title(.x), value = isolate(input[[.x]]))
      }
    })
  })
  
  params_selected <- reactive({
    list(temperature = input$temperature,
         max_tokens = input$max_tokens)
  })
  
  chat <- reactive({
    ellmer::chat_openai(
      model = input$model,
      system_prompt = input$system_prompt,
      api_args = params_selected()
    )
  })
  
  observeEvent(input$chat_user_input, {
    stream <- chat()$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
  
  observeEvent(input$clear, {
    chat_clear("chat")
  })
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
  
}

enableBookmarking("url")
shinyApp(ui, server, enableBookmarking = "url")
