library(shiny)
library(tidyverse)
library(pivottabler)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(maps)
library(usmap)
library(readxl)
library(viridis)
# remotes::install_github("JVAdams/jvamisc")
library(jvamisc)
# devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)

############Import Data#########################################################
dat <- read_excel("NAEP_read_dat.xls",  range = cell_rows(3:57), col_types = c("guess", rep("numeric",22)))%>%
  as.data.frame()
# dat[1, ] <- c("State", rep(c("Estimates", "SE"), 11))
# colnames(dat) <- c("State", rep(c(1998, 2002, seq(2003,2019, by = 2)), each = 2))
colnames(dat)[-1] <- sort(c(paste0(c(1998, 2002, seq(2003,2019, by = 2)), "_est"), paste0(c(1998, 2002, seq(2003,2019, by = 2)), "_SE")))
dat <- dat[-1,]
dat_long <- dat %>%
  pivot_longer(!State, names_to = c("year","par"), names_sep = "_", values_to = "score") %>%
  mutate(State = (str_replace_all(
    str_replace_all(State, "[[:digit:]][[:punct:]]", ""),
    "[\\\\]", "")
  ),
  state = tolower(State), 
  abbr = state.abb[match(state, tolower(state.name))])
dat_long <- within(dat_long,
                   c(State <- factor(State),
                     year <- factor(year),
                     par <- factor(par, labels = c("est", "SE"))))

longplot <- function(variable){
  dat = dat_long %>% filter(par == "est", State %in% variable)
  ggplot(data = dat, aes(x = year, y = score, color = State, group = State)) +
    geom_line() +
    theme_bw() +
    labs(x = "Year", y = "Score", color = "State/Area(s)")
}

long_tab <- dat_long %>%
  select(!c(state, abbr)) %>%
  pivot_wider(names_from = "par", values_from = score) 
colnames(long_tab) <- c("State", "Year", "Reading Score", "Standard Error")

tab_se <- function(variable){
  long_tab %>% 
    filter(State %in% variable)
}

dat_map <- dat_long %>%
  filter(!State %in% c("United States", 
                       "Department of Defense\n   Education\n   Activity (DoDEA)")) %>%
  right_join(fifty_states %>% rename(state = id), by = "state", all.x = TRUE)

which_state <- function(long, lat) {
  if (long > -123.4745 & long < -112.6042 & lat > 23.3612 & lat < 30.7632) {
    state <- "alaska"
  } else if (long > -111.7076 & long < -103.4148 & lat > 23.5204 & lat < 28.0571) {
    state <- "hawaii"
  } else {
    state <- latlong2(data.frame(x = long, y = lat))
  }
  return(state)
}

############Shiny App###########################################################
ui <- dashboardPage(
  dashboardHeader(title = "National Assessment of Educational Progress (NAEP) 
                  reading scale score of 8th-grade public school students",
                  titleWidth = 400),
  dashboardSidebar(width = 200,
                   sidebarMenu(
                     menuItem(
                       "Instruction",
                       tabName = "Reference",
                       icon = icon("dashboard")
                     ),
                     menuItem(
                       "Across States",
                       tabName = "heatmap",
                       icon = icon("dashboard")
                     ),
                     menuItem("Across Time", 
                              tabName = "longitudinal", 
                              icon = icon("dashboard"))
                   )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ), 
    tabItems(
      tabItem(tabName = "Reference",
              fluidPage(
                box(
                  title = "Instructions",
                  status = "primary",
                  width = 12,
                  solidHeader = FALSE,
                  p(
                    "This app is intended to show the average National Assessment of 
                    Educational Progress (NAEP) reading scale score of 8th-grade public 
                    school students by state. This dataset covers the years from 1998 to 2019."
                  ), 
                  p(
                    'The "Across States" tab shows students" reading scale scores by a heat map of 
                    the United States, where you can click on a specific State and obtain 
                    details of the reading scores of students in that State. '
                  ), 
                  p(
                    'On the "Across Time" tab, you can select your interested states (single or multiple) 
                    and view the trend over time. You will get a line graph and a table that captures 
                    both the scores and standard errors.'
                  )),
                
                box(
                  title = "Reference",
                  status = "primary",
                  width = 12,
                  solidHeader = FALSE,
                  p("The data for the contest is from the Digest of Education Statistics 
                    from the National Center for Education Statistics  
                    (https://nces.ed.gov/programs/digest/current_tables.asp). "
                  )),
                
                box(
                  title = "Note",
                  status = "primary",
                  width = 12,
                  solidHeader = FALSE,
                  p(
                    "If you have any questions, please email us at yzhang97@usc.edu and wingyeet@usc.edu ."
                  )
                )
              )
              #1st tab item parent
      ),
      tabItem(tabName = "heatmap",
              fluidPage(selectInput("year", label = "Year", 
                                    choices = c(levels(dat_map$year))),
                        selectInput("state", label = "State", 
                                    choices = c(levels(dat_long$State))), 
                        box(
                          id = "output_usmap", 
                          state = "primary", 
                          solidHeader = FALSE, 
                          width = 12, 
                          plotOutput("usmap", click = "plot_click", 
                                     height = "550px"), 
                          h6('* Areas shaded in gray had missing values.')
                        ),
                        br(), 
                        box(
                          id = "output_info", 
                          state = "primary", 
                          solidHeader = FALSE, 
                          width = 12, 
                          tableOutput("info"), 
                          h6('* A dash line indicates a missing value.')
                        ))
              #2nd tab item parent
      ), 
      tabItem(tabName = "longitudinal",
              fluidPage(selectInput("variable", label = "Select your interest state(s)",
                                    choices = c(levels(dat_long$State)), multiple = TRUE),
                        box(
                          id = "outputgraph",
                          title = "The Trend of Average Reading Scores of Students From Selected States in 1998-2019",
                          status = "primary",
                          solidHeader = FALSE,
                          width = 12,
                          plotOutput("graph")
                        ),
                        box(
                          id = "outputTableBox",
                          title = "Average Reading Scores of Students From Selected States in 1998-2019",
                          status = "primary",
                          solidHeader = FALSE,
                          width = 12,
                          tableOutput("table"))
              )
              
      )
      #3rd tab item parent
    )
  )
)

server <- function(input, output, session) {
  var_sel <- reactive({
    input$variable
  })
  output$graph <- renderPlot({
    longplot(var_sel())
  })
  output$table <- renderTable({
    tab_se(var_sel())
  })
  
  #### Heatmap ####
  
  # filter data for the selected year
  map_df <- reactive({
    dat_map %>%
      filter(year == input$year, par == "est")
  })
  # determine the locations of the label of each State 
  labels <- reactive({
    aggregate(cbind(long, lat) ~ abbr, 
              data = map_df(), FUN = function(x) mean(range(x)))
  })
  # create a US heatmap
  output$usmap <- renderPlot({
    ggplot(data = map_df(), aes(x = long, y = lat)) + 
      geom_polygon(aes(fill = score, group = group)) +
      scale_fill_viridis(option = "G", direction = -1) +
      geom_label(data = labels(), aes(long, lat, label = abbr), size = 3, 
                 label.padding = unit(.15, "lines"), 
                 alpha = .7) +
      theme_void() +
      labs(title = paste0(
        "Average NAEP Reading Scale Score of 8th-grade Public School Students in ",
        input$year
      ), fill = "Score") + 
      theme(legend.position = "bottom",
            legend.key.width = unit(3, "cm"),
            legend.key.height = unit(.2, "cm"))
  })
  # stores reactive values from on-click
  click_data <- reactiveValues(trigger = FALSE, x = NA, y = NA)
  # triggered if user clicks on a valid state in the plot
  observe({
    req(input$plot_click)
    click_data$trigger <- TRUE
    click_data$x <- input$plot_click$x
    click_data$y <- input$plot_click$y
  })
  # triggered if user changes the state input 
  observeEvent(input$state, {
    click_data$trigger <- FALSE
  })
  # create a table for the selected State
  output$info <- renderTable({
    # if no on-click activity or clicked on an invalid location, return NA
    clicked <- ifelse(click_data$trigger,
                      which_state(click_data$x, click_data$y), NA)
    # if clicked is NA, return the selected state from input
    selected <- ifelse(is.na(clicked),
                       tolower(input$state), clicked)
    # table with confidence intervals
    dat_long %>%
      filter(year == input$year, state == selected) %>%
      pivot_wider(names_from = par, values_from = score) %>%
      mutate(ci_lo = est - 2 * SE, 
             ci_up = est + 2 * SE) %>%
      mutate_at(vars(est:ci_up), ~ as.character(round(., 2))) %>%
      replace(is.na(.), "-") %>%
      mutate(`Score (SE)` = paste0(est, " (", SE, ")"), 
             `Confidence Interval` = paste0("[", ci_lo, ", ", ci_up, "]")) %>%
      select(!c(state, abbr, est, SE, ci_lo, ci_up)) %>%
      rename(Year = year)
  })
}

shinyApp(ui, server)