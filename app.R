library(shiny)

final_results <- readRDS('./final_results.RDS')
f <- unlist(final_results, recursive = FALSE)

models <- unique(sapply(f, function(x){x$mod_mehtod}, simplify = TRUE))
mod <- sapply(models, function(x){substr(x, 23, nchar(x))}, simplify = TRUE, USE.NAMES = FALSE)
names(models) <- mod

imp_met <- unique(sapply(f, function(x){x$imp_method}, simplify = TRUE))
imp <- sapply(imp_met, function(x){substr(x, 12, nchar(x))}, simplify = TRUE, USE.NAMES = FALSE)
names(imp_met) <- imp

ids <- unique(sapply(f, function(x){x$dataset_id}, simplify = TRUE, USE.NAMES = FALSE))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wyniki"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("mod",
                        label = "Select model", 
                        choices = models),
            selectInput("imp",
                        label = "Select imputation method",
                        choices = imp_met),
            selectInput("id",
                        label = "Select dataset id",
                        choices = ids)
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("cm"),
           tableOutput("cr"),
           plotOutput("roc"),
           plotOutput("pr")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    l <- reactive({
        x <- f[sapply(f, function(x, val){x$mod_mehtod == val}, val = input$mod, simplify = T)]
        x <- x[sapply(x, function(x, val){x$imp_method == val}, val = input$imp, simplify = T)]
        df_ <- x[sapply(x, function(x, val){x$dataset_id == val}, val = input$id, simplify = T)]
        l <- df_[[1]]
        l
        })
    
    require(PRROC)
    
    output$cm <- renderTable({
        x <- l()
        x$confusion_matrix
    })
    
    output$cr <- renderTable({
        x <- l()
        x$classification_report
    })
    
    output$roc <- renderPlot({
        x <- l()
        
        fg_rf <- x$raw[x$true==1]
        bg_rf <- x$raw[x$true==0]
        roc_rf <- roc.curve(scores.class0 = fg_rf, scores.class1 = bg_rf, curve = T)
        
        plot(roc_rf)
    })
    
    output$pr <- renderPlot({
        x <- l()
        
        fg_rf <- x$raw[x$true==1]
        bg_rf <- x$raw[x$true==0]
        pr_rf <- pr.curve(scores.class0 = fg_rf, scores.class1 = bg_rf, curve = T)
        plot(pr_rf)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
