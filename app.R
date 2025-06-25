library(shiny)
library(shinychat)
library(ellmer)
library(stringr)


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

available_models <- unlist(model_list)

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
      value = NULL,
      placeholder = "Describe desired model behavior (tone, tool usage, response style)"
    ),
    numericInput("temperature", "Temperature", NULL),
    numericInput("max_tokens", "Max Tokens", NULL),
  ),
  tags$div(chat_ui("chat"))
)

server <- function(input, output, session) {
  params_selected <- reactive({
    list(
      temperature = input$temperature,
      max_tokens = input$max_tokens
    )
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
}

shinyApp(ui, server)
