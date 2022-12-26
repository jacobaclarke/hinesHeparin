#' main UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_main_ui <- function(id) {
    ns <- NS(id)
    shiny.quartz::QCard(
        title = "Heparin Nomogram Calculator",
        shiny.quartz::VStack(
        shiny.mui::TextField.shinyInput(
            ns("weight"),
            label = "Patient Weight (kg)",
            value = 75
             ),
             shiny.mui::Typography(
                variant = "h6",
                "Initial Values"
             ),
             
             shiny::tableOutput(ns("acs")),
            shiny.mui::Typography(
                variant = "h6",
                "Interval Values"
             ),
             shiny.mui::Alert(severity = "info", 
                "The maximum weight for the below calculations in 100kg"
             ),
            shiny::tableOutput(ns("nomograms"))
        )
    )
}


#' main Server Funciton
#'
#' @noRd
#' @importFrom dplyr mutate
mod_main_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

first <- reactive(
    ifelse(as.numeric(input$weight) > 100, 100, input$weight) %>% as.numeric()
)


output$acs <- shiny::renderTable({
    bolus <- 80 * as.integer(input$weight)
    data.frame(ACS = c(
        "Initial Rate: 18 units/kg/hr (max 1800 units/hr)",
        "Initial Bolus: 80 units/kg (max 4000 units)"
    ), Value = c(
        18 * first(),
        ifelse(bolus > 4000, 4000, bolus)
    ))
})

output$nomograms <- shiny::renderTable({
    dplyr::tibble(
        PTT = c(
            "<43.1",
            "43.1 - 59.8",
            "59.9-104.5",
            "104.6 - 126.9",
            ">126.9"
        ),
        "Rebolus (calculation)" = c(
            "80 units/kg",
            "40 units/kg",
            NA,
            NA,
            NA
        ),
        "Rate (calculation)" = c(
            "4 units/kg/hr",
            "2 units/kg/hr",
            NA,
            "2 units/kg/hr",
            "3 units/kg/hr"
        ),
        "Rebolus (units)" = c(
            80 * first(),
            40 * first(),
            NA,
            NA,
            NA
        ),
        "Rate (units/hour)" = c(
            4 * first(),
            2 * first(),
            NA,
            2 * first(),
            3 * first()
        )
    )
})

    })
}

