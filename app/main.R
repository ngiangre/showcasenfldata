box::use(
    shiny.blueprint[H1, Card],
    shiny[moduleServer, NS, tagList, h1],
    app/view/table
)
#' @export
ui <- function(id) {
    ns <- NS(id)
    
    shiny.blueprint::Card(
        shiny.blueprint::H1('Player Stats for 2023 season'),
        table$ui(ns('2023playerstats'))
    )
}

#' @export
server <- function(id) {
    moduleServer(id, function(input, output, session) {
        table$server("2023playerstats",data = nflreadr::load_player_stats(2023))
    })
}