## Name: Shelmith N. Kariuki
##Date: 9th July, 2018
##Description: Creating a simple that inputs respondent's data and saves it

##Load the required library

library(shiny)

ui<-fluidPage(
  titlePanel("Shelmith's Data Collection APP"),
  sidebarPanel(
    tags$html(
      tags$body(
        h4('Introduction'),
        p("I love self development alot. I have always loved Shiny, and all the amazing 
          things it can do. So I am creating this small app that collects respondents' data
          and saves it")
  ))),
  mainPanel(
    fluidRow(
    textInput("name","Please enter your name"),
    textInput("age","Please insert your age"),
    selectInput("location","Please select your locality",
                c("Kinoo","Kawangware","Nyeri","Nairobi","Mombasa")),
    selectInput("marital","Please select your marital status",
                c("Single","Married","Divorced","Complicated")),
    selectInput("educ","Please Indicate your highest level of education",
                c("Primary","Secondary","Diploma","Degree","Masters","PhD")),
    textInput("weather","Please indicate your career goals"),
    actionButton("save","Save",icon = icon("fa-save"))),
    br(),
    fluidRow(
    DT::dataTableOutput("responses", width = 800), tags$hr(),
    downloadButton("download","Download")
  )))

# Define server logic 
server <- function(input, output,session) {
  fields <- c("name","age","location","marital","educ","weather")
  
  #create a data frame called responses
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- bind_rows(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  
  # Whenever a field is filled, aggregate all form data
  #formData is a reactive function
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when save is clicked)
  output$responses <- DT::renderDataTable({
    input$save
    loadData()
  })
  
  data<-loadData()
  output$download<-downloadHandler(
    filename = function() {
    paste("Shelmith's_AppData", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
 
}

shinyApp(ui,server)
