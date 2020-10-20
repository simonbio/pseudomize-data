## Set up environment ---------------------------------------------------------------
pcks <- c("digest", "shiny")

landscape_setup <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg))
    install.packages(new_pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
landscape_setup(pcks)



# Define UI for data pseudomizer app ----
ui <- fluidPage(
  
  # Shiny theme
  theme = shinytheme('lumen'),
  
  # App title ----
  titlePanel("Pseudomize features"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput("variable", "Name of variable to pseudonymize",
                placeholder = "e.g., Identifier"),
      helpText("Case sensitive!"),
      
      numericInput("seed", 'Integer "password" to seed by',
                   value = 1234,
                   min = 1),
      
      # Downlload Button
      downloadButton("download", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"), 
      
      br(), br(),
      
      # Computed hash digest
      tableOutput("results")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  data_in <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  
  output$contents <- renderTable({data_in()})
  
  
  data_out <- reactive({
    
    #  Function generates a lookup table that associates each unique identifier to an PSN.
    get_lookup_table <- function(data, id.var) {
      if (any(duplicated(data[, id.var]))) warning("Duplicate id values in data.")
      to_anon <- subset(data, select = id.var)
      lookup_table <- data.frame(id = data[, id.var], key = unname(apply(to_anon, 1, digest,
                                                                         algo = "xxhash64", seed = input$seed)))
      return(lookup_table)
    }
    
    # Generate a lookup table
    suppressWarnings(lookup_table <- get_lookup_table(data = data_in(), id.var =  input$variable))
    
    # Replace names with PSN
    add_PSN <- function(data, id.var, lookup.table) {
      data[, id.var] <- lookup.table[, "key"][match(data[, id.var], lookup.table[, "id"])]
      return(data)
    }
    
    add_PSN(data = data_in(), id.var = input$variable, lookup.table = lookup_table)
    
  }) 
  
  output$results <- renderTable({data_out()})
  
  
  # Downloadable csv of selected dataset ----
  output$download <- downloadHandler(
    filename = function() {
      paste("pseudo_", Sys.Date() , "_", input$file1, sep = "")
    },
    content = function(file) {
      write.csv(data_out(), file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)

