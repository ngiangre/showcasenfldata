box::use(
    shiny[moduleServer, NS, uiOutput, reactive, req],
    dplyr[select, mutate, distinct, everything, pull, filter],
    reactable[reactable, colDef, JS],
    shiny.blueprint[reactOutput, renderReact, MultiSelect.shinyInput, Card],
    nflreadr[load_player_stats]
)

#' @export
ui <- function(id){
    ns <- NS(id)
    shiny.blueprint::Card(
        shiny.blueprint::reactOutput(ns('table_filter')),
        reactable::reactableOutput(ns('table'))
    )
}

#' @export
server <- function(id,data){
    moduleServer(id, function(input,output,session){
        ns <- session$ns
        output$table_filter <- renderReact({
            MultiSelect.shinyInput(
                ns('table_filter'),
                   data |> 
                       distinct(player_display_name,recent_team) |> 
                       mutate(lst = list(list(text = player_display_name[1],
                                              label = recent_team[1])),
                              .by = everything()) |> 
                       pull(lst),
                   selected = "Josh Allen"
            )
        })
        selectedItems <- reactive({
            req(input$table_filter$selectedItems)
            purrr::map_chr(input$table_filter$selectedItems,~{.x[['text']]})
        })
        filtered_data <- reactive({
            req(selectedItems())
            filter(data,player_display_name %in% selectedItems()) |> 
                select(player_display_name,
                       where(is.numeric)) |> 
                select(-season) |> 
                mutate(across(where(is.numeric),function(x)round(x,2)))
        })
        output$table <- reactable::renderReactable({
            req(filtered_data())
            reactable::reactable(filtered_data(),
                                 highlight = TRUE,
                                 bordered = TRUE,
                                 defaultColDef = colDef(
                                     width=150,
                                     align = 'center',
                                     headerStyle = list(background = "gray90"),
                                     aggregate = JS("
                                                    function(values, rows) {
                                                      // input:
                                                      //  - values: an array of all values in the group
                                                      //  - rows: an array of row data values (optional)
                                                      //
                                                      // output:
                                                      //  - an aggregated value
                                                      function getAvg(grades) {
                                                                  const total = grades.reduce((acc, c) => acc + c, 0);
                                                                  return total / grades.length;
                                                                }
                                                                return Math.round(getAvg(values)).toFixed(2)
                                                    }
                                                  ")
                                 ),
                                 columns = list(
                                     player_display_name = colDef(
                                         sticky="left"
                                     ),
                                     week = colDef(
                                         sticky="left"
                                     )
                                 ),
                                 groupBy = 'player_display_name')
        })
    })
}