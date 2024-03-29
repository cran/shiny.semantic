% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkbox.R
\name{multiple_checkbox}
\alias{multiple_checkbox}
\alias{multiple_radio}
\title{Create Semantic UI multiple checkbox}
\usage{
multiple_checkbox(
  input_id,
  label,
  choices,
  choices_value = choices,
  selected = NULL,
  position = "grouped",
  type = NULL,
  ...
)

multiple_radio(
  input_id,
  label,
  choices,
  choices_value = choices,
  selected = choices_value[1],
  position = "grouped",
  type = "radio",
  ...
)
}
\arguments{
\item{input_id}{Input name. Reactive value is available under \code{input[[input_id]]}.}

\item{label}{Text to be displayed with checkbox.}

\item{choices}{Vector of labels to show checkboxes for.}

\item{choices_value}{Vector of values that should be used for corresponding choice.
If not specified, \code{choices} is used by default.}

\item{selected}{The value(s) that should be chosen initially.
If \code{NULL} the first one from \code{choices} is chosen.}

\item{position}{Specified checkmarks setup. Can be \code{grouped} or \code{inline}.}

\item{type}{Type of checkbox or radio.}

\item{...}{Other arguments to be added as attributes of the
tag (e.g. style, childrens etc.)}
}
\description{
This creates a multiple checkbox using Semantic UI styles.
}
\details{
The following \code{type}s are allowed:
\itemize{
\item{NULL} The standard checkbox (default)
\item{toggle} Each checkbox has a toggle form
\item{slider} Each checkbox has a simple slider form
}
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {
  # Checkbox
  library(shiny)
  library(shiny.semantic)

  ui <- function() {
      shinyUI(
        semanticPage(
          title = "Checkbox example",
          h1("Checkboxes"),
          multiple_checkbox("checkboxes", "Select Letters", LETTERS[1:6], selected = "A"),
          p("Selected letters:"),
          textOutput("selected_letters"),
          tags$br(),
          h1("Radioboxes"),
          multiple_radio("radioboxes", "Select Letter", LETTERS[1:6], selected = "A"),
          p("Selected letter:"),
          textOutput("selected_letter")
       )
     )
  }

  server <- shinyServer(function(input, output) {
     output$selected_letters <- renderText(paste(input$checkboxes, collapse = ", "))
     output$selected_letter <- renderText(input$radioboxes)
  })

  shinyApp(ui = ui(), server = server)
}

}
