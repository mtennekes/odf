library(tidyverse)
library(sf)
library(tmap)
library(shiny)

?shinyApp


## test multiline

# Read commuting data (from Marko Roos / Yvonne Gootzen)
od <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_od.rds")
points <- readRDS("~/pCloudDrive/flows_commutingNL/data/throughput/gm_centroids.rds")


# filter
#od <- od %>% filter(value>= 100)

# Create odf object
x <- odf(od, points, col_flow = "value", col_type = "mode")

# extract spatial points (sf) object from odf object
map_points <- odf_points(x)

# extract spatial lines (sf) objects, disregarding type
map_flows <- odf_flows(x, by_type = TRUE, by_via = FALSE, incl_total = TRUE)

map_flows <- map_flows %>%
  filter(flow >= 100)

modes <- levels(map_flows$type)

gms <- as.character((map_flows %>%
  st_set_geometry(NULL) %>%
  filter(type == "total") %>%
  group_by(orig) %>%
  summarize(flow = sum(flow, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(flow)) %>%
  select(orig))$orig)

gms <- c("GM0363", "GM0599", "GM0518", "GM0344")
nms <- sort(map_points$name[!map_points$id %in% gms])
gms <- c(gms, map_points$id[match(nms, map_points$name)])
nms <- map_points$name[match(gms, map_points$id)]


ui <- fluidPage(
  titlePanel("Commuting in the Netherlands"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode", "Mode of transport", modes),
      checkboxGroupInput("col_muni", "Colored municipalities", nms)
    ),
    mainPanel(
      tmapOutput("map")
    )
  )
)


server <- function(input, output, session) {
  get_map <- reactive({
    cols <- map_points$id[match(input$col_muni, map_points$name)]
    n <- length(cols)
    map_flows %>%
      filter(type == input$mode) %>%
      mutate(sel = ifelse(orig %in% cols, match(orig, cols), 0))
  })

  output$map <- renderTmap({
    tm_shape(map_flows %>% filter(type == "total")) +
      tm_lines(lwd = "flow", scale = 10, zindex = 401) +
    tm_shape(map_points) +
      tm_dots()
  })

  observe({
    mapf <- get_map()

    print(table(mapf$sel))

    pal <-tmaptools::get_brewer_pal("Dark2", 9, plot = FALSE)[c(9, 1:8)]

    tmapProxy("map", session, {
      tm_remove_layer(401) +
      tm_shape(mapf) +
        tm_lines(col = "sel", lwd = "flow", scale = 10, zindex = 401, palette = pal, style = "cat", legend.col.show = FALSE, legend.lwd.show = FALSE) +
      tm_shape(map_points) +
        tm_dots()
    })
  })
}

shinyApp(ui, server)
