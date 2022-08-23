library(shiny); library(readxl); library(tidyverse); library(mcr); library(shinythemes)

options(digits = 4)

df_example_1 <- read.csv("test.csv", na.strings = "")
df_example_2 <- read.csv("test2.csv", na.strings = "")

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("compaRison - Comparison of two measurements -"),
    
    # Select Shiny theme ----
    theme = shinytheme("flatly"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        sidebarPanel(
            
            # Side panel for "Table" tab ----
            conditionalPanel(condition = "input.tabselected == 1",
                             
                             h3("Please select your file"),
                             
                             radioButtons(
                                 "data_input", "",
                                 choices =  list("Example data 1" = 1,
                                                 "Example data 2" = 2,
                                                 "Upload file (.csv/.txt)" = 3),
                                 selected =  1),
                             
                             conditionalPanel(
                                 condition = "input.data_input=='1'",
                                 p('Example data 1: 20 individuals, One outlier in b')),
                             
                             conditionalPanel(
                                 condition = "input.data_input=='2'",
                                 p('Example data 2: 100 individuals, Original data is provided from the published paper (Schmidt RL, et al. PLoS One 2015)')),
                             
                             conditionalPanel(
                                 condition = "input.data_input=='3'",
                                 h4('Make sure the variables you want to compare are in 2nd and 3rd column')),

                             conditionalPanel(
                                 condition = "input.data_input=='3'",
                                 fileInput("file",
                                       label = "",
                                       accept = c("text/csv"),
                                       multiple = FALSE,
                                       width = "100%"))
            ),
            
            # Side panel for "Scatter plot" tab ----
            conditionalPanel(condition = "input.tabselected == 2",
                             h3("Regression line"),
                             checkboxInput(inputId = "ols",
                                           label = "Least square method",
                                           value = TRUE),
                             checkboxInput(inputId = "deming",
                                           label = "Deming regression",
                                           value = TRUE),
                             checkboxInput(inputId = "W_deming",
                                           label = "Weighted Deming regression",
                                           value = FALSE),
                             checkboxInput(inputId = "pb",
                                           label = "Passing-Bablok regression (Take some time)",
                                           value = TRUE),
                             checkboxInput(inputId = "pb_large",
                                           label = "Passing-Bablok regression (Approximative P-B regression)",
                                           value = FALSE),
                             h3("Aesthetics"),
                             sliderInput("pointSize", "Size of the datapoints", 0, 10, 4),  
                             sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.8),
                             h3("Scaling"),
                             numericInput("plot_height", "Plot height (# pixels): ", value = 600),
                             numericInput("plot_width", "Plot width (# pixels):", value = 800),
                             h3("Labels"),
                             textInput("lab_x", "X-axis:", value = "Method A"),
                             textInput("lab_y", "Y-axis:", value = "Method B"),
                             sliderInput("title_sz", "Font size of title", 0, 30, 24),
                             sliderInput("label_sz", "Font size of labels", 0, 30, 20),
                             sliderInput("leg_title_sz", "Font size of legend title", 0, 30, 18),
                             sliderInput("leg_label_sz", "Font size of legend text", 0, 30, 16),
                             h3("Change Axis limits"),
                             checkboxInput(inputId = "change_limits1",
                                           label = "Change limits of X-axis",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.change_limits1 == true",
                                 numericInput("xlim_lower1", "X limit (Lower): ", value = NA),
                                 numericInput("xlim_upper1", "X limit (Upper): ", value = NA)),
                             checkboxInput(inputId = "change_limits2",
                                           label = "Change limits of Y-axis",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.change_limits2 == true",
                                 numericInput("ylim_lower1", "Y limit (Lower): ", value = NA),
                                 numericInput("ylim_upper1", "Y limit (Upper): ", value = NA))
            ),
            
            # Side panel for "BA plot" tab ----
            conditionalPanel(condition = "input.tabselected == 3",
                             h3("Select plot"),
                             radioButtons("plot_type", "Choose one", 
                                          choices =  list("y-axis: Difference [absolute]" = 1,
                                                          "y-axis: Difference/Method A * 100 [percentage]" = 2,
                                                          "y-axis: Difference [log2 transformed]" = 3),
                                          selected =  1),
                             h3("Statistics"),
                             radioButtons("stats", "Choose one", 
                                          choices =  list("Parametric" = 1,
                                                          "Non-parametric" = 2),
                                          selected =  1),
                             h3("Aesthetics"),
                             sliderInput("pointSize1", "Size of the datapoints", 0, 10, 4),  
                             sliderInput("alphaInput1", "Visibility of the data", 0, 1, 0.8),
                             h3("Scaling"),
                             numericInput("plot_height1", "Plot height (# pixels): ", value = 600),
                             numericInput("plot_width1", "Plot width (# pixels):", value = 600),
                             h3("Labels"),
                             textInput("lab_x1", "X-axis:", value = "Mean of A and B"),
                             textInput("lab_y1", "Y-axis:", value = "Difference of A and B"),
                             sliderInput("title_sz1", "Font size of title", 0, 30, 24),
                             sliderInput("label_sz1", "Font size of labels", 0, 30, 20),
                             h3("Change Axis limits"),
                             checkboxInput(inputId = "change_limits3",
                                           label = "Change limits of X-axis",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.change_limits3 == true",
                                 numericInput("xlim_lower2", "X limit (Lower): ", value = NA),
                                 numericInput("xlim_upper2", "X limit (Upper): ", value = NA)),
                             checkboxInput(inputId = "change_limits4",
                                           label = "Change limits of Y-axis",
                                           value = FALSE),
                             conditionalPanel(
                                 condition = "input.change_limits4 == true",
                                 numericInput("ylim_lower2", "Y limit (Lower): ", value = NA),
                                 numericInput("ylim_upper2", "Y limit (Upper): ", value = NA))
            ),
        ),

        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output ----
            tabsetPanel(type = "tabs", id = "tabselected",
                        
                        # Main panel for "Table" tab ----
                        tabPanel("Table", value = 1,
                                 h2("Summary stats"),
                                 br(),
                                 verbatimTextOutput("summary"),
                                 br(),
                                 h2("Input Data"),
                                 br(),
                                 dataTableOutput("outFile")),
                        
                        # Main panel for "Scatter plot" tab ----
                        tabPanel("Scatter plot with regression", value = 2,
                                 h2("Regression summary"),
                                 br(),
                                 verbatimTextOutput("reg_summary"),
                                 h2("Scatter plot with regression lines"),
                                 br(),
                                 downloadButton("downloadPlotPDF1", "Download pdf-file"),
                                 downloadButton("downloadPlotPNG1", "Download png-file"),
                                 plotOutput("plot1")),
                        
                        # Main panel for "BA plot" tab ----
                        tabPanel("BA plot", value = 3,
                                 h2("Summary for BA plot"),
                                 br(),
                                 verbatimTextOutput("loa_summary"),
                                 h2("Bland-Altman plot"),
                                 h4("Solid line shows the mean difference of two methods; Dotted lines show the limits of agreement (LOA) of mean difference"),
                                 br(),
                                 downloadButton("downloadPlotPDF2", "Download pdf-file"),
                                 downloadButton("downloadPlotPNG2", "Download png-file"),
                                 plotOutput("plot2")),
                        
                        # Main panel for "About" tab ----
                        tabPanel("About", value = 4,
                                 includeHTML("about.html"))
            )
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
    inFile <- reactive({
        
        if (input$data_input == 1){
            data <- df_example_1
        }
        
        else if (input$data_input == 2) {
            data <- df_example_2
        }
        
        else if (input$data_input == 3) {
            tmp <- input$file
            
            if (is.null(tmp)) {
                return(NULL)
            } else {
                data <- read.csv(tmp$datapath, header = TRUE)
                return(data)
            }
        }
    })

    output$outFile <- renderDataTable({
        data.frame(inFile())
    })
    
    # Generate a Summary statistics ----
    output$summary <- renderPrint({
        summary(inFile()[,c(2,3)])
    })

    # Generate a Scatter plot with regression lines ----
    output$plot1 <- renderPlot(
        
        width = function() input$plot_width,
        height = function() input$plot_height,

        {
            
            plot(plotdata1())
            
        })
    
    plotdata1 <- reactive({
            
        x <- inFile()[,2]
        y <- inFile()[,3]
        
        model <- lm(y ~ x); coef <- coef(model)
        Deming.reg <- mcreg(x, y, method.reg = "Deming")
        WDeming.reg <- mcreg(x, y, method.reg = "WDeming")
        PB.reg <- mcreg(x, y, method.reg = "PaBa")
        PB_large.reg <- mcreg(x, y, method.reg = "PaBaLarge")
        
        p <- ggplot(inFile(), aes(x = x, y = y)) +
            theme_bw() +
            labs(x = input$lab_x, y = input$lab_y, color = "Regression") +
            scale_x_continuous(limits = c(0, NA)) +
            scale_y_continuous(limits = c(0, NA)) +
            theme(axis.title = element_text(size = input$title_sz),
                  axis.text = element_text(size = input$label_sz),
                  legend.title = element_text(size = input$leg_title_sz),
                  legend.text = element_text(size = input$leg_label_sz))
        
        if(input$ols == TRUE){

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000"))

            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE) {
            
            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$W_deming == TRUE) {
            
            p <- p +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$pb == TRUE) {
            
            p <- p +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#0072B5B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$pb_large == TRUE) {
            
            p <- p +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#0072B5B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#0072B5B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$W_deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#0072B5B2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$W_deming == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#0072B5B2", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#0072B5B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb_large == TRUE) {
            
            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#0072B5B2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#0072B5B2", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#0072B5B2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#0072B5B2", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#0072B5B2", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#0072B5B2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#0072B5B2", "#20854EB2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }

        if(input$ols == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#000000", "#0072B5B2", "#20854EB2", "#E18727B2")) 
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#0072B5B2", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            p <- p +
                geom_abline(aes(intercept = coef[1], slope = coef[2], 
                                color = "Least square regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = Deming.reg@para[1], slope = Deming.reg@para[2], 
                                color = "Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = WDeming.reg@para[1], slope = WDeming.reg@para[2], 
                                color = "Weighted Deming regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB.reg@para[1], slope = PB.reg@para[2], 
                                color = "Passing-Bablok regression"),
                            show.legend = TRUE) +
                geom_abline(aes(intercept = PB_large.reg@para[1], slope = PB_large.reg@para[2], 
                                color = "Passing-Bablok regression (large)"),
                            show.legend = TRUE) +
                geom_point(size = input$pointSize,
                           alpha = input$alphaInput) +
                scale_color_manual(values = c("#BC3C29B2", "#000000", "#0072B5B2", "#20854EB2", "#E18727B2"))
            
            if(input$change_limits1 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5))
            }
            
            if(input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
            
            if(input$change_limits1 == TRUE & input$change_limits2 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower1, input$xlim_upper1), 
                                       breaks = seq(input$xlim_lower1, input$xlim_upper1, 
                                                    (input$xlim_upper1 - input$xlim_lower1) / 5)) +
                    scale_y_continuous(limits = c(input$ylim_lower1, input$ylim_upper1), 
                                       breaks = seq(input$ylim_lower1, input$ylim_upper1, 
                                                    (input$ylim_upper1 - input$ylim_lower1) / 5))
            }
        }
        
        p
        
    })
    
    # Download a scatter plot as PDF ----
    output$downloadPlotPDF1 <- downloadHandler(
        filename <- function() {
            paste("Scatter", ".pdf", sep = "")
        },
        content <- function(file) {
            pdf(file, width = input$plot_width/72, height = input$plot_height/72)
            
            plot(plotdata1())
            
            dev.off()
        },
        contentType = "application/pdf"
    ) 
    
    # Download a scatter plot as PNG ----
    output$downloadPlotPNG1 <- downloadHandler(
        filename <- function() {
            paste("Scatter", ".png", sep = "")
        },
        content <- function(file) {
            png(file, width = input$plot_width * 4, height = input$plot_height * 4, res = 300)
            
            plot(plotdata1())
            
            dev.off()
        },
        contentType = "application/png"
    ) 

    # Generate a summary of the data ----
    output$reg_summary <- renderPrint({
        
        x <- inFile()[,2]
        y <- inFile()[,3]
        
        model <- lm(y ~ x); coef <- coef(model)
        Deming.reg <- mcreg(x, y, method.reg = "Deming")
        WDeming.reg <- mcreg(x, y, method.reg = "WDeming")
        PB.reg <- mcreg(x, y, method.reg = "PaBa")
        PB_large.reg <- mcreg(x, y, method.reg = "PaBaLarge")
        
        table <- c("If you choose regression methods from left panel, results will pop up")
        
        if(input$ols == TRUE) {
            
            table <- paste0("Least square regression:",
                           "y = ",
                           round(coef[1], 3),
                           " + ",
                           round(coef[2], 3),
                           " x")
        }
        
        if(input$deming == TRUE) {
            
            table <- paste0("Deming regression:",
                           "y = ",
                           round(Deming.reg@para[1], 3),
                           " + ",
                           round(Deming.reg@para[2], 3),
                           " x")
        }
        
        if(input$W_deming == TRUE) {
            
            table <- paste0("Weighted Deming regression:",
                           "y = ",
                           round(WDeming.reg@para[1], 3),
                           " + ",
                           round(WDeming.reg@para[2], 3),
                           " x")
        }
        
        if(input$pb == TRUE){
            
            table <- paste0("Passing-Bablok regression:",
                           "y = ",
                           round(PB.reg@para[1], 3),
                           " + ",
                           round(PB.reg@para[1], 3),
                           " x")
        }
        
        if(input$pb_large == TRUE){
            
            table <- paste0("Passing-Bablok regression (large):",
                           "y = ",
                           round(PB_large.reg@para[1], 3),
                           " + ",
                           round(PB_large.reg@para[1], 3),
                           " x")
        }

        if(input$ols == TRUE & input$deming == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                            round(coef[1], 3),
                                                            " + ",
                                                            round(coef[2], 3),
                                                            "x"),
                          "Deming regression: " = paste0("y = ",
                                                      round(Deming.reg@para[1], 3),
                                                      " + ",
                                                      round(Deming.reg@para[2], 3),
                                                      " x"))
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                            round(coef[1], 3),
                                                            " + ",
                                                            round(coef[2], 3),
                                                            "x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                               round(WDeming.reg@para[1], 3),
                                                               " + ",
                                                               round(WDeming.reg@para[2], 3),
                                                               " x"))
        }
        
        if(input$ols == TRUE & input$pb == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                            round(coef[1], 3),
                                                            " + ",
                                                            round(coef[2], 3),
                                                            "x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                              round(PB.reg@para[1], 3),
                                                              " + ",
                                                              round(PB.reg@para[2], 3),
                                                              " x"))
        }
        
        if(input$ols == TRUE & input$pb_large == TRUE) {
            
            table <- list("Least square regression: " = paste0("y = ",
                                                            round(coef[1], 3),
                                                            " + ",
                                                            round(coef[2], 3),
                                                            "x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                      round(PB_large.reg@para[1], 3),
                                                                      " + ",
                                                                      round(PB_large.reg@para[2], 3),
                                                                      " x"))
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                      round(Deming.reg@para[1], 3),
                                                      " + ",
                                                      round(Deming.reg@para[2], 3),
                                                      " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                               round(WDeming.reg@para[1], 3),
                                                               " + ",
                                                               round(WDeming.reg@para[2], 3),
                                                               " x"))
        }
        
        if(input$deming == TRUE & input$pb == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                      round(Deming.reg@para[1], 3),
                                                      " + ",
                                                      round(Deming.reg@para[2], 3),
                                                      " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                              round(PB.reg@para[1], 3),
                                                              " + ",
                                                              round(PB.reg@para[2], 3),
                                                              " x"))
        }
        
        if(input$deming == TRUE & input$pb_large == TRUE) {
            
            table <- list("Deming regression: " = paste0("y = ",
                                                      round(Deming.reg@para[1], 3),
                                                      " + ",
                                                      round(Deming.reg@para[2], 3),
                                                      " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                      round(PB_large.reg@para[1], 3),
                                                                      " + ",
                                                                      round(PB_large.reg@para[2], 3),
                                                                      " x"))
        }
        
        if(input$W_deming == TRUE & input$pb == TRUE) {

            table <- list("Weighted Deming regression: " = paste0("y = ",
                                                               round(WDeming.reg@para[1], 3),
                                                               " + ",
                                                               round(WDeming.reg@para[2], 3),
                                                               " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                              round(PB.reg@para[1], 3),
                                                              " + ",
                                                              round(PB.reg@para[2], 3),
                                                              " x"))
        }
        
        if(input$W_deming == TRUE & input$pb_large == TRUE) {

            table <- list("Weighted Deming regression: " = paste0("y = ",
                                                               round(WDeming.reg@para[1], 3),
                                                               " + ",
                                                               round(WDeming.reg@para[2], 3),
                                                               " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                      round(PB_large.reg@para[1], 3),
                                                                      " + ",
                                                                      round(PB_large.reg@para[2], 3),
                                                                      " x"))
        }
        
        if(input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Passing-Bablok regression: " = paste0("y = ",
                                                              round(PB.reg@para[1], 3),
                                                              " + ",
                                                              round(PB.reg@para[2], 3),
                                                              " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                      round(PB_large.reg@para[1], 3),
                                                                      " + ",
                                                                      round(PB_large.reg@para[2], 3),
                                                                      " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                            round(coef[1], 3),
                                                            " + ",
                                                            round(coef[2], 3),
                                                            "x"),
                          "Deming regression: " = paste0("y = ",
                                                      round(Deming.reg@para[1], 3),
                                                      " + ",
                                                      round(Deming.reg@para[2], 3),
                                                      " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                               round(WDeming.reg@para[1], 3),
                                                               " + ",
                                                               round(WDeming.reg@para[2], 3),
                                                               " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb_large == TRUE) {
            
            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }

        if(input$ols == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"))
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$ols == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"))
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$ols == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Deming regression: " = paste0("y = ",
                                                        round(Deming.reg@para[1], 3),
                                                        " + ",
                                                        round(Deming.reg@para[2], 3),
                                                        " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        if(input$ols == TRUE & input$deming == TRUE & input$W_deming == TRUE & input$pb == TRUE & input$pb_large == TRUE) {

            table <- list("Least square regression: " = paste0("y = ",
                                                              round(coef[1], 3),
                                                              " + ",
                                                              round(coef[2], 3),
                                                              "x"),
                          "Deming regression: " = paste0("y = ",
                                                         round(Deming.reg@para[1], 3),
                                                         " + ",
                                                         round(Deming.reg@para[2], 3),
                                                         " x"),
                          "Weighted Deming regression: " = paste0("y = ",
                                                                 round(WDeming.reg@para[1], 3),
                                                                 " + ",
                                                                 round(WDeming.reg@para[2], 3),
                                                                 " x"),
                          "Passing-Bablok regression: " = paste0("y = ",
                                                                round(PB.reg@para[1], 3),
                                                                " + ",
                                                                round(PB.reg@para[2], 3),
                                                                " x"),
                          "Passing-Bablok regression (large): " = paste0("y = ",
                                                                        round(PB_large.reg@para[1], 3),
                                                                        " + ",
                                                                        round(PB_large.reg@para[2], 3),
                                                                        " x"))
        }
        
        table
        
    })
    
    # Generate a BA plot ----
    output$plot2 <- renderPlot(
        
        width = function() input$plot_width1,
        height = function() input$plot_height1,

        {
            
            plot(plotdata2())
            
        })
    
    plotdata2 <- reactive({
        
        x <- inFile()[,2]
        y <- inFile()[,3]
        
        if (input$plot_type == 1 & input$stats == 1) {
            
            D <- x - y
            mean_diff <- mean(D)
            lower <- mean(D) - 1.96 * sd(D)
            upper <- mean(D) + 1.96 * sd(D)
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = x - y)) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = mean_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = input$lab_y1) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
        }
        
        if (input$plot_type == 2 & input$stats == 1) {
            
            DP <- ((x - y) / x) * 100
            mean_diff <- mean(DP)
            lower <- mean(DP) - 1.96 * sd(DP)
            upper <- mean(DP) + 1.96 * sd(DP)
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = ((x - y) / x) * 100)) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = mean_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = paste0("%", input$lab_y1)) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
        }
        
        if (input$plot_type == 3 & input$stats == 1) {
            
            DL <- log2(x/y)
            mean_diff <- mean(DL)
            lower <- mean(DL) - 1.96 * sd(DL)
            upper <- mean(DL) + 1.96 * sd(DL)
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = log2(x/y))) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = mean_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = paste0(input$lab_y1, " (log2 trasnformed)")) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            } 
        }
        
        if (input$plot_type == 1 & input$stats == 2) {
            
            D <- x - y
            med_diff <- median(D)
            upper <- quantile(D, probs = 0.95)[[1]]
            lower <- quantile(D, probs = 0.05)[[1]]
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = x - y)) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = med_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = input$lab_y1) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
        }
        
        if (input$plot_type == 2 & input$stats == 2) {
            
            DP <- ((x - y) / x) * 100
            med_diff <- median(DP)
            upper <- quantile(DP, probs = 0.95)[[1]]
            lower <- quantile(DP, probs = 0.05)[[1]]
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = ((x - y) / x) * 100)) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = med_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = paste0("%", input$lab_y1)) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
        }
        
        if (input$plot_type == 3 & input$stats == 2) {
            
            DL <- log2(x/y)
            med_diff <- median(DL)
            upper <- quantile(DL, probs = 0.95)[[1]]
            lower <- quantile(DL, probs = 0.05)[[1]]
            
            p <- ggplot(inFile(), aes(x = (x + y) / 2, y = log2(x/y))) +
                geom_point(size = input$pointSize1,
                           alpha = input$alphaInput1, color = "#000000") +
                geom_hline(yintercept = med_diff) +
                geom_hline(yintercept = lower, color = "#696969", 
                           linetype = "dashed") +
                geom_hline(yintercept = upper, color = "#696969", 
                           linetype = "dashed") +
                theme_bw() +
                labs(x = input$lab_x1, y = paste0(input$lab_y1, " (log2 trasnformed)")) +
                theme(axis.title = element_text(size = input$title_sz1),
                      axis.text = element_text(size = input$label_sz1))
            
            if(input$change_limits3 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2))
            } 
            
            if(input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
            
            if(input$change_limits3 == TRUE & input$change_limits4 == TRUE) {
                
                p <- p +
                    scale_x_continuous(limits = c(input$xlim_lower2, input$xlim_upper2)) +
                    scale_y_continuous(limits = c(input$ylim_lower2, input$ylim_upper2))
            }
        }
        
        p
        
    })
    
    # Download a BA plot as PDF ----
    output$downloadPlotPDF2 <- downloadHandler(
        filename <- function() {
            paste("BAplot", ".pdf", sep = "")
        },
        content <- function(file) {
            pdf(file, width = input$plot_width/72, height = input$plot_height/72)
            
            plot(plotdata2())
            
            dev.off()
        },
        contentType = "application/pdf"
    ) 
    
    # Download a BA plot as PNG ----
    output$downloadPlotPNG2 <- downloadHandler(
        filename <- function() {
            paste("BAplot", ".png", sep = "")
        },
        content <- function(file) {
            png(file, width = input$plot_width * 4, height = input$plot_height * 4, res = 300)
            
            plot(plotdata2())
            
            dev.off()
        },
        contentType = "application/png"
    ) 
    
    # Generate a summary of the data ----
    output$loa_summary <- renderPrint({
        
        x <- inFile()[,2]
        y <- inFile()[,3]
        
        if(input$plot_type == 1 & input$stats == 1) {
            
            D <- x - y
            mean_diff <- mean(D)
            upper <- mean(D) + 1.96 * sd(D)
            lower <- mean(D) - 1.96 * sd(D)
            
            table <- list("Difference" = round(mean_diff, 3),
                          "Upper limit of agreement" = round(upper, 3),
                          "Lower limit of agreement" = round(lower, 3))
        }
        
        if(input$plot_type == 2 & input$stats == 1) {
            
            DP <- ((x - y) / x) * 100
            mean_diff <- mean(DP)
            upper <- mean(DP) + 1.96 * sd(DP)
            lower <- mean(DP) - 1.96 * sd(DP)
            
            table <- list("Difference (%)" = round(mean_diff, 3),
                          "Upper limit of agreement (%)" = round(upper, 3),
                          "Lower limit of agreement (%)" = round(lower, 3))
        }
        
        if(input$plot_type == 3 & input$stats == 1) {
            
            DL <- log2(x/y)
            mean_diff <- mean(DL)
            upper <- mean(DL) + 1.96 * sd(DL)
            lower <- mean(DL) - 1.96 * sd(DL)
            
            table <- list("Difference" = round(mean_diff, 3),
                          "Upper limit of agreement" = round(upper, 3),
                          "Lower limit of agreement" = round(lower, 3))
        }
        
        if(input$plot_type == 1 & input$stats == 2) {
            
            D <- x - y
            med_diff <- median(D)
            upper <- quantile(D, probs = 0.95)[[1]]
            lower <- quantile(D, probs = 0.05)[[1]]
            
            table <- list("Difference" = round(med_diff, 3),
                          "Upper limit of agreement" = round(upper, 3),
                          "Lower limit of agreement" = round(lower, 3))
        }
        
        if(input$plot_type == 2 & input$stats == 2) {
            
            DP <- ((x - y) / x) * 100
            med_diff <- median(DP)
            upper <- quantile(DP, probs = 0.95)[[1]]
            lower <- quantile(DP, probs = 0.05)[[1]]
            
            table <- list("Difference (%)" = round(med_diff, 3),
                          "Upper limit of agreement (%)" = round(upper, 3),
                          "Lower limit of agreement (%)" = round(lower, 3))
        }
        
        if(input$plot_type == 3 & input$stats == 2) {
            
            DL <- log2(x/y)
            med_diff <- median(DL)
            upper <- quantile(DL, probs = 0.95)[[1]]
            lower <- quantile(DL, probs = 0.05)[[1]]
            
            table <- list("Difference" = round(med_diff, 3),
                          "Upper limit of agreement" = round(upper, 3),
                          "Lower limit of agreement" = round(lower, 3))
        }
        
        table
        
    })
}

# Create Shiny app ----
shinyApp(ui, server)