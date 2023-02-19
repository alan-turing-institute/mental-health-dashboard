# https://stackoverflow.com/questions/38713809/how-to-dynamically-change-the-size-of-leaflet-map-in-shiny-r

library(shiny)
library(shinydashboard)
library(tmap)
library(dplyr)

VIEW_MODE <- "view"

tmap_mode(VIEW_MODE)

ui <- dashboardPage(
  dashboardHeader(title = "Mental Health"),

  dashboardSidebar(sidebarMenu(
    menuItem(
      "Mental Health",
      tabName = "mental_health",
      icon = icon("house-user")
    ),

    menuItem("Team", tabName = "team", icon = icon("car"))
  )),

  dashboardBody(tabItems(
    # tabItem("mental_health",
    #         box(tmapOutput("map")),
    #         box(
    #           selectInput(
    #             "var",
    #             "Select local authority district",
    #             choices = c("London", unique(gisCity$LAD11NM)),
    #             selected = "London"
    #           )
    #         )),


    tabItem("mental_health",
            fluidPage(
              fluidRow(column(12,
                              tmapOutput("map"))),
              fluidRow(column(6,
                              plotOutput("chart")),
                       column(
                         6,
                         selectInput(
                           "var",
                           "Select local authority district",
                           choices = c("London", unique(gisCity$LAD11NM)),
                           selected = "London"
                         )
                       ))
            )),

    tabItem("team", fluidPage(h1("Team")))
  ))
)


server <- function(input, output, session) {
  output$correlation_plot <- renderPlot({
    plot(iris$Sepal.Length,
         iris[[input$features]],
         xlab = "Sepal length",
         ylab = "Feature")
  })

  output$map <- renderTmap({
    if (input$var == "London") {
      gisZone <- myCity
    } else{
      gisZone <- myCity %>% filter(input$var == LAD11NM)
    }

    tm_shape(gisZone) +
      tm_borders(lwd = 0.5, alpha = 0.5) + tm_fill(col = "GHQ1",
                                                   palette = "Reds",
                                                   title = "Subjective well-being:\n1 (the least distressed) to 32 (the most distressed)") + tm_layout(title = CITY, title.size = TITLE_SIZE)

  })

  output$chart <- renderPlot({
    if (input$var == "London")
    {
      mypop <- pop
    } else{
      gisZone <- gisCity %>% filter(input$var == LAD11NM)

      mypop <- pop %>% filter(ZoneID %in% gisZone$ZoneID)
    }

    mypop$GHQ1 <- mypop$GHQ1 / 36 * 10
    mypop$sat_health <- mypop$sat_health / 7 * 10
    mypop$sat_income <- mypop$sat_income / 7 * 10
    mypop$sat_life <- mypop$sat_life / 7 * 10
    #mypop$lonely <- mypop$lonely / 3 * 10

    pop_long <-
      mypop %>% select(sex, GHQ1, sat_health, sat_income, sat_life) %>% pivot_longer(!sex, names_to = "indicator", values_to = "score")

    ggplot(pop_long %>% na.omit(),
           aes(fill = sex, x = indicator, y = score)) + geom_bar(position = "dodge",
                                                                 stat = "summary",
                                                                 fun = "mean") + scale_y_continuous(limits = c(0, 10))
  })
}

shinyApp(ui, server)
