#' Gets the current time in the given time zone.
#'
#' @return The current time in the given time zone.
get_current_time <- function() {
  Sys.time()
}

#' Tool to help register the `get_current_time` function with ellmer
tool_get_current_time <- ellmer::tool(
  get_current_time,
  "Gets the current time in the users time zone."
)

#' Tool to draw random numbers from a normal distributionß
tool_rnorm <- tool(
  rnorm,
  "Drawn numbers from a random normal distribution",
  n = type_integer("The number of observations. Must be a positive integer."),
  mean = type_number("The mean value of the distribution."),
  sd = type_number("The standard deviation of the distribution. Must be a non-negative number."),
  .annotations = tool_annotations(
    title = "Draw Random Normal Numbers",
    read_only_hint = TRUE,
    open_world_hint = FALSE
  )
)
