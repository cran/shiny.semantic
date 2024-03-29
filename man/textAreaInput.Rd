% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{textAreaInput}
\alias{textAreaInput}
\title{Create a semantic Text Area input}
\usage{
textAreaInput(inputId, label, value = "", width = NULL, placeholder = NULL)
}
\arguments{
\item{inputId}{Input name. Reactive value is available under \code{input[[input_id]]}.}

\item{label}{character with label put above the input}

\item{value}{Pass value if you want to have default text.}

\item{width}{The width of the input, eg. "40px"}

\item{placeholder}{Text visible in the input when nothing is inputted.}
}
\description{
Create a text area input control for entry of unstructured text values.
}
\examples{
## Only run examples in interactive R sessions
if (interactive()) {
ui <- semanticPage(
  textAreaInput("a", "Area:", value = "200", width = "200px"),
  verbatimTextOutput("value")
)
server <- function(input, output, session) {
  output$value <- renderText({ input$a })
}
shinyApp(ui, server)
}
}
