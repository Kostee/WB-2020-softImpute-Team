library(shiny)
library(magrittr)
final_results <- readRDS('./final_results.RDS')
f <- unlist(final_results, recursive = FALSE)

final_results2 <- readRDS('./final_results_2.RDS')
f2 <- unlist(final_results2, recursive = FALSE)


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
            textOutput("dane"),
            tableOutput("danetab"),
           textOutput("cmt"),
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
    
    l2 <- reactive({
        x <- f2[sapply(f, function(x, val){x$mod_mehtod == val}, val = input$mod, simplify = T)]
        x <- x[sapply(x, function(x, val){x$imp_method == val}, val = input$imp, simplify = T)]
        df_ <- x[sapply(x, function(x, val){x$dataset_id == val}, val = input$id, simplify = T)]
        l2 <- df_[[1]]
        l2
    })
    
    require(PRROC)
    
    output$dane <- renderText("Info")
    
    output$danetab <- renderTable({
        x <- l()
        id <- x$dataset_id
        
        j <- jsonlite::read_json(paste0('dependencies/datasets/openml_dataset_', id, "/dataset.json"))
        
        w <- c(as.integer(j$number_of_instances), 
                  as.integer(j$number_of_features),
                  as.integer(j$number_of_missing),
                  (100 * j$number_of_missing/(j$number_of_instances * j$number_of_features)),
                  as.double(x$imputation_time, units ='secs'))
        out <- as.matrix(w) %>% as.data.frame()
        rownames(out) <- c("number of instances", "number of features", "number of missing", "% of missing", "imputation time [s]")
        out
    }, rownames = T)
    
    
    output$cmt <- renderText("Confusion matrix")
    
    output$cm <- renderTable({
        x <- l2()
        x$confusion_matrix %>% as.matrix()
        as.data.frame.matrix(x$confusion_matrix)
    
        }, rownames = T)
    
    output$cr <- renderTable({
        x <- l2()
        x$classification_report
    })
    
    output$roc <- renderPlot({
        x <- l()
        clases <- unique(x$true)
        fg_rf <- x$raw[x$true==clases[2]]
        bg_rf <- x$raw[x$true==clases[1]]
        roc_rf <- roc.curve(scores.class0 = fg_rf, scores.class1 = bg_rf, curve = T)
        if (roc_rf$auc < 0.5){
            roc_rf <- roc.curve(scores.class0 = bg_rf, scores.class1 = fg_rf, curve = T)
        }
        plot(roc_rf)
    })
    
    output$pr <- renderPlot({
        x <- l()
        
        clases <- unique(x$true)
        fg_rf <- x$raw[x$true==clases[2]]
        bg_rf <- x$raw[x$true==clases[1]]
        pr_rf <- pr.curve(scores.class0 = fg_rf, scores.class1 = bg_rf, curve = T)
        roc_rf <- roc.curve(scores.class0 = fg_rf, scores.class1 = bg_rf)
        if (roc_rf$auc < 0.5){
            pr_rf <- pr.curve(scores.class0 = bg_rf, scores.class1 = fg_rf, curve = T)
        }
        plot(pr_rf)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
