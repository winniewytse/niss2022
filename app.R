library(shiny)
library(shinydashboard)
library(tidyverse)
library(maps)
library(usmap)
library(readxl)
library(viridis)
# remotes::install_github("JVAdams/jvamisc")
library(jvamisc)
############Import Data#########################################################
dat <- read_excel("NAEP_read_dat.xls",  range = cell_rows(3:57), col_types = c("guess", rep("numeric",22)))%>%
  as.data.frame()
# dat[1, ] <- c("State", rep(c("Estimates", "SE"), 11))
# colnames(dat) <- c("State", rep(c(1998, 2002, seq(2003,2019, by = 2)), each = 2))
colnames(dat)[-1] <- sort(c(paste0(c(1998, 2002, seq(2003,2019, by = 2)), "_est"), paste0(c(1998, 2002, seq(2003,2019, by = 2)), "_SE")))
dat <- dat[-1,]
dat_long <- dat %>%
  pivot_longer(!State, names_to = c("year","par"), names_sep = "_", values_to = "score")
dat_long <- within(dat_long,
                   c(State <- factor(State),
                     year <- factor(year),
                     par <- factor(par, labels = c("est", "SE")))) %>%
  mutate(state = tolower(str_replace_all(
    str_replace_all(State, "[[:digit:]][[:punct:]]", ""), 
    "[\\\\]", "")
  ), 
  abbr = state.abb[match(state, tolower(state.name))])

longplot <- function(variable){
  dat = dat_long %>% filter(par == "est", State %in% variable)
  ggplot(data = dat, aes(x = year, y = score, color = State, group = State)) +
    geom_line()
}
dat_map <- dat_long %>%
  filter(!state %in% c("United States", 
                       "Department of Defense\n   Education\n   Activity (DoDEA)")) %>%
  merge(map_data("state") %>%
          rename(state = region), 
        ., by = "state", all.x = TRUE)

############Shiny App###########################################################
ui <- dashboardPage(
  dashboardHeader(title = "National Assessment of Educational Progress (NAEP) 
                  reading scale score of 8th-grade public school students",
                  titleWidth = 400),
  dashboardSidebar(width = 150,
                   sidebarMenu(
                     menuItem(
                       "Instruction",
                       tabName = "Reference",
                       icon = icon("dashboard")
                     ),
                     menuItem(
                       "Across the States",
                       tabName = "heatmap",
                       icon = icon("dashboard")
                     ),
                     menuItem("Across Time", 
                              tabName = "longitudinal", 
                              icon = icon("dashboard"))
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Reference",
              fluidPage(
                box(
                  title = "Instruction",
                  status = "primary",
                  width = 12,
                  solidHeader = FALSE,
                  p(
                    "This app is intended to"
                  ))
              )
              #1st tab item parent
      ),
      tabItem(tabName = "heatmap",
              fluidPage(selectInput("year", label = "Year", 
                                    choices = c(levels(dat_map$year))), 
                        plotOutput("usmap", click = "plot_click"), 
                        br(), 
                        tableOutput("info"))
              #2nd tab item parent
      ), 
      tabItem(tabName = "longitudinal",
              fluidPage(selectInput("variable", label = "Variable", choices = c(levels(dat_long$State))),
                        verbatimTextOutput("Graphs"),
                        plotOutput("graph")),
              tableOutput("table"),
      ))
    #3rd tab item parent
  )
)

server <- function(input, output, session) {
  variable <- reactive({
    get(input$variable, dat_long$State)
  })
  output$graph <- renderPlot({
    longplot(variable)
  })
  output$table <- renderTable({
    dat_long%>%
      filter(State == "Alaska")
  })
  
  map_df <- reactive({
    dat_map %>%
      filter(year == input$year, par == "est")
  })
  labels <- reactive({
    aggregate(cbind(long, lat) ~ abbr, 
              data = map_df(), FUN = function(x) mean(range(x)))
  })
  output$usmap <- renderPlot({
    ggplot(data = map_df(), aes(x = long, y = lat)) + 
      geom_polygon(aes(fill = score, group = group)) +
      scale_fill_viridis(option = "G") +
      geom_label(data = labels(), aes(long, lat, label = abbr), size = 2.5, 
                 label.padding = unit(.15, "lines"), 
                 alpha = .7) +
      theme_void() +
      labs(title = paste0(
        "Average NAEP Reading Scale Score of 8th-grade Public School Students in ",
        input$year
      )) + 
      theme(legend.position = "bottom",
            legend.key.width = unit(3, "cm"),
            legend.key.height = unit(.2, "cm"))
  })
  click_data <- reactiveValues(trigger = 0, x = NA, y = NA)
  observe({
    req(input$plot_click)
    isolate(click_data$trigger <- 1)
    click_data$x <- input$plot_click$x
    click_data$y <- input$plot_click$y
  })
  output$info <- renderTable({
    selected <- ifelse(click_data$trigger,
                       latlong2(data.frame(x = click_data$x,
                                           y = click_data$y)),
                       NA)
    if (is.na(selected)) {
      data.frame(Details = "Select a state to view details. ")
    } else {
      dat_long %>% filter(state == selected)
    }
  })
}

shinyApp(ui, server)