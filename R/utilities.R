#' Gets the current time in the given time zone.
#'
#' @return The current time in the given time zone.
get_current_time <- function() {
  Sys.time()
}

#' Tool to help register the `get_current_time` function with ellmer
tool_get_current_time <- ellmer::tool(
  get_current_time,
  description = "Gets the current time in the users time zone."
)

#' Tool to draw random numbers from a normal distributionß
tool_rnorm <- ellmer::tool(
  rnorm,
  description = "Draw numbers from a random normal distribution",
  arguments = list(
    n = ellmer::type_integer("The number of observations. Must be a positive integer."),
    mean = ellmer::type_number("The mean value of the distribution."),
    sd = ellmer::type_number("The standard deviation of the distribution. Must be a non-negative number.")
  )
)

read_md <- function(md_file){
  readLines(md_file) |>
    paste(collapse = "\n")
}

snake_to_title <- function(snake_case) {
  stringr::str_replace_all(snake_case, "_", " ") |>
    stringr::str_to_title()
}

clean_list <- function(x) {
  lapply(x, function(element) {
    if (is.null(element) || 
        (length(element) == 1 && (is.na(element) || identical(element, "")))) {
      NULL
    } else {
      element
    }
  })
}
