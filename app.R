library(shiny)
library(shinydashboard)
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
                     par <- factor(par, labels = c("est", "SE"))))

longplot <- function(variable){
  dat = dat_long %>% filter(par == "est", State %in% variable)
  ggplot(data = dat, aes(x = year, y = score, color = State, group = State)) +
    geom_line()
}

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
              fluidPage()
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
}

shinyApp(ui, server)