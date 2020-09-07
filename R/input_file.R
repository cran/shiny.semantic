file_input <- function(input_id, label, multiple = FALSE, accept = NULL,
                      width = NULL, button_label = "Browse...", button_color = "teal",
                      label_color = "", placeholder = "No file selected") {

  restored_value <- restoreInput(id = input_id, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restored_value) && !is.data.frame(restored_value)) {
    warning("Restored value for ", input_id, " has incorrect format.")
    restored_value <- NULL
  }

  if (!is.null(restored_value)) {
    restored_value <- jsonlite::toJSON(restored_value, strict_atomic = FALSE)
  }

  input_tag <- tags$input(
    id = input_id,
    name = input_id,
    type = "file",
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restored_value
  )

  if (multiple)
    input_tag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    input_tag$attribs$accept <- paste(accept, collapse=',')

  #shiny::div(class = "ui left action input",
  #           shiny::tags$button(class = paste("ui", button_color, "labeled button"), "Browse:"),
  #           input_tag
  #)

  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", shiny::validateCssUnit(width), ";"),
      div(class = glue::glue("ui pointing below {label_color} label"), label),
          div(class = "input-group",
              tags$label(class = "input-group-btn input-group-prepend",
                          div(class = "btn btn-default btn-file",
                              button_label, #button("","abc"),
                              input_tag
                          )
              ),
              tags$input(type = "text", class = "form-control",
                         placeholder = placeholder, readonly = "readonly"
              )
      )
  )
}
