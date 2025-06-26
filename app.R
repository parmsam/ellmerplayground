library(shiny)
library(bslib)
library(shinychat)
library(ellmer)
library(stringr)
library(purrr)
library(jsonlite)

source("R/utilities.R")

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

available_params <- formals(ellmer::params)
ignore_params <- "..."
available_param_names <- names(available_params[!names(available_params) %in% ignore_params])
character_params <- "stop_sequences"

available_tools <- unlist(lapply(search(), function(env_name) {
  tryCatch({
    objs <- ls(envir = as.environment(env_name))
    tool_defs <- objs[sapply(objs, function(obj_name) {
      obj <- get(obj_name, envir = as.environment(env_name))
      inherits(obj, "ellmer::ToolDef")
    })]
    tool_defs
  }, error = function(e) character(0))
}))

welcome_message <- read_md("welcome_message.md")

ui <- bslib::page_sidebar(
  title = "Ellmer LLM playground",
  sidebar = sidebar(
    tags$div(
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
      selectizeInput(
        "tools",
        "Available Tools",
        choices = available_tools,
        selected = NULL,
        multiple = TRUE
      ),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel("Optional API arguments", uiOutput("api_args"))
      ),
      tags$br(),
      actionButton("clear", "Clear chat"),
      tags$br(),
      tags$br(),
      downloadButton("export_json", "Export Inputs to JSON", 
                     class = "btn-outline-secondary"),
      tags$br(),
      tags$br(),
      bookmarkButton()
    ),
    width = "30%"
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
    map(available_param_names, ~ {
      if (!.x %in% character_params) {
        numericInput(.x, snake_to_title(.x), value = isolate(input[[.x]]))
      } else {
        textInput(.x, snake_to_title(.x), value = isolate(input[[.x]]))
      }
    })
  })
  
  params_selected <- reactive({
    params_list <- lapply(available_param_names, function(param) {
      if (input[[param]] == "" || is.na(input[[param]])) {
        NULL
      } else {
        input[[param]]
      }
    })
    names(params_list) <- available_param_names
    params_list
  })
  
  chat <- reactive({
    print(params_selected())
    ellmer::chat_openai(
      model = input$model,
      system_prompt = input$system_prompt,
      api_args = params_selected()
    )
  })
  
  observe({
    if (!is.null(input$chat_function)){
      purrr::walk(input$tools, ~ {
        tool_func <- get(.x)
        chat()$register_tool(tool_func)
      })
    }
  })
  
  observeEvent(input$chat_user_input, {
    stream <- chat()$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
  
  observeEvent(input$clear, {
    chat_clear("chat")
  })
  
  output$export_json <- downloadHandler(
    filename = function() {
      paste0("ellmer_inputs_", Sys.Date(), ".json")
    },
    content = function(file) {
      input_list <- reactiveValuesToList(input)
      
      jsonlite::write_json(input_list, file, pretty = TRUE, auto_unbox = TRUE)
    },
    contentType = "application/json"
  )
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
  
}

enableBookmarking("url")
shinyApp(ui, server)
