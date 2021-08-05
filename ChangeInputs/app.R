#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(data.table)
library(TSrepr)
library(protoABC)
library(parallel)

setwd("C:\\Users\\vargh\\OneDrive - Queensland University of Technology\\University Studies\\Year 5\\Thesis\\R Project\\FiTABM")
source('Code_abhi/00-ABC_required_functions.R')
source('Code_abhi/02-Model_development.R')
source('Code_abhi/02B-Model_development_Collapse.R')
source('01-required_functions.R')
source('02-run_functions.R')

load_data()

getDTthreads(verbose=TRUE)
setDTthreads(1)


x_true <- c(0.237, 0.233, 0.066, 0.464, 0.751) #<- These are the 'true' chosen vals simulating it.
x_sim <- c(0.237,
           0.233,
           0.07914644,
           0.4155911,
           0.6946182)

n_links <- 10
n_agents <- 10000
a_net <- sample_k_regular(n_agents, n_links, directed = FALSE, multiple = FALSE)
adj_net  <- get.adjacency(a_net)
misc_args <- list(obs_summ = NULL,
                  number_of_agents = n_agents,
                  adj = adj_net)
#OBSERVED DATA
obs_data_ag_fast <- abm_pv_model_collapse(x_true[1:4], x_true[5], misc_args$number_of_agents, misc_args$adj)
obs_data_summ_fast <- simulate_summary(obs_data_ag_fast, obs_data_ag_fast)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Change Inputs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("w_inc",
                        "Weight - Income aspect",
                        min = 0,
                        max = 1,
                        value = 0.24),
            uiOutput("w_soc"),
            uiOutput("w_ec"),
            uiOutput("w_cap"),
            sliderInput("thresh",
                        "Utility Threshold for Adoption",
                        min = 0,
                        max = 1,
                        value = 0.75),
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ModelOutput"),
           textOutput("print_weights"),
           textOutput("print_thresh")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$w_soc <- renderUI({
        sliderInput("w_soc", "Weight - Society aspect", min = 0,  max = 1 - input$w_inc, value = 0.23)  
    })
    
    output$w_ec <- renderUI({
        sliderInput("w_ec", "Weight - Economic (Payback Period) aspect", min = 0,  max = 1 - input$w_inc - input$w_soc, value = 0.08)  
    })
    
    output$w_cap <- renderUI({
        limits <- 1 - input$w_inc - input$w_soc - input$w_ec
        sliderInput("w_cap", "Weight - Capital Cost (Upfront) aspect", min = limits,  max =limits, value = limits)  
    })
    
    output$print_weights <- renderText({
        paste("You have selected weights:", " w_inc = ",input$w_inc, " w_soc = ",input$w_soc, " w_ec = ",input$w_ec, " w_cap = ",input$w_cap)
    })
    
    output$print_thresh <- renderText({
        paste("Threshold is: ", input$thresh)
    })

    output$ModelOutput <- renderPlot({
        w_cap <- 1 - input$w_inc - input$w_soc - input$w_ec
        myvals <- c()
        myvals <- c(input$w_inc, input$w_soc, input$w_ec, w_cap, input$thresh)
        sim_data_ag_fast <- abm_pv_model_collapse(myvals[1:4], myvals[5], misc_args$number_of_agents, misc_args$adj)
        sim_data_summ_fast <- simulate_summary(sim_data_ag_fast, obs_data_ag_fast)
        
        #Plot Output
        
        # Create a first line
        time_vec <- 1:length(sim_data_summ_fast)
        plot(time_vec, sim_data_summ_fast, type = "b", frame = FALSE, pch = 19, 
             col = "red", xlab = "x", ylab = "y")
        # Add a second line
        lines(time_vec, obs_data_summ_fast, pch = 18, col = "blue", type = "b", lty = 2)
        # Add a legend to the plot
        legend("topleft", legend=c("Simulated", "Observed"),
               col=c("red", "blue"), lty = 1:2, cex=0.8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
