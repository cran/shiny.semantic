#' Show steps
#'
#' @param id ID of the Steps that will be displayed.
#' @param steps_list A list of steps generated by single_steps.
#' @param class (Optional) A character string with the semantic class to be
#' added to the steps element.
#' @seealso single_steps
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny.semantic)
#'  ui <- semanticPage(
#'  title = "Steps Example",
#'  shiny::tagList(
#'    h2("Steps example"),
#'    shiny.semantic::steps(
#'      id = "steps",
#'      steps_list = list(
#'          single_step(
#'            id = "step_1",
#'            title = "Step 1",
#'            description = "It's night?",
#'            icon_class = "moon"
#'          ),
#'          single_step(
#'            id = "step_2",
#'            title = "Step 2",
#'            description = "Order some food",
#'            icon_class = "bug"
#'          ),
#'          single_step(id = "step_3",
#'                      title = "Step 3",
#'                      description = "Feed the Kiwi",
#'                      icon_class = "kiwi bird"
#'                    )
#'      )
#'    ),
#'    h3("Actions"),
#'    shiny.semantic::action_button("step_1_complete", "Make it night"),
#'    shiny.semantic::action_button("step_2_complete", "Call the insects"),
#'    shiny.semantic::action_button("step_3_complete", "Feed the Kiwi"),
#'    shiny.semantic::action_button("hungry_kiwi", "Kiwi is hungry again"),
#'  )
#')
#'
#'  server <- function(input, output, session) {
#'    observeEvent(input$step_1_complete, {
#'      toggle_step_state("step_1")
#'    })
#'
#'    observeEvent(input$step_2_complete, {
#'      toggle_step_state("step_2")
#'    })
#'
#'    observeEvent(input$step_3_complete, {
#'      toggle_step_state("step_3")
#'    })
#'
#'    observeEvent(input$hungry_kiwi, {
#'      toggle_step_state("step_1", FALSE)
#'      toggle_step_state("step_2", FALSE)
#'      toggle_step_state("step_3", FALSE)
#'    })
#'
#'  }
#'
#'  shiny::shinyApp(ui, server)
#' }
#' @rdname steps
#' @export
steps <- function(id, steps_list, class = NULL) {
    steps_class <- if (is.null(class)) "ui steps" else sprintf("ui %s steps", class) # nolint
    shiny::div(
        id = id,
        class = steps_class,
        steps_list
    )
}



#' Creates a single step to be used inside of a list of steps by the steps
#' function
#'
#' @param id The \code{input} slot that will be used to access the value.
#' @param title A character that will be the title of the ste
#' @param description A character that will fill the description of the step
#' @param icon_class A character which will be correpond to a fomantic icon
#' class to be used in the step
#' @param step_class A character representing a class to be passed to the step
#'
#' @seealso steps
#'
#' @rdname single_step
#' @export
single_step <- function(id, title, description = NULL, icon_class = NULL,
                        step_class = NULL) {
    step_icon <- if (is.null(icon_class)) NULL else tags$i(
        class = paste(icon_class, "icon")
    )
    step_description <- if (is.null(description)) NULL else shiny::div(
        class = "description",
        description
    )
    step_title <- shiny::div(class = "title", title)
    step_class <- if(is.null(step_class)) "step" else sprintf("%s step", step_class) # nolint

    shiny::div(
        id = id,
        class = step_class,
        step_icon,
        shiny::div(
            class = "content",
            step_title,
            step_description)
    )
}

#' Toggle step state
#'
#' @param id ID of step to be toggled
#' @param state State of the step, \code{TRUE} stands for enabled
#' @param automatic_steps Whether to toggle focus of next step automatically
#' @param asis When used inside of Shiny module, \code{TRUE} will disable adding
#'   the namespace to \code{id}
#'
#' @seealso steps
#'
#' @rdname toggle_step_state
#' @export
toggle_step_state <- function(id, state = TRUE, automatic_steps = TRUE,
                              asis = TRUE) {
    session <- shiny::getDefaultReactiveDomain()
    # Make sure set_attribute_by_id works with namespaces (shiny modules)
    if (inherits(session, "session_proxy") && !asis) session$ns(id) else id
    parameters <- list(
        "step_id" = id,
        "state" = state,
        "automatic_steps" = automatic_steps
    )
    session$sendCustomMessage("toggle_step_state", parameters)
}
