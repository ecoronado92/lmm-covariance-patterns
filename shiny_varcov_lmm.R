# Load libraries
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(sjmisc)
library(lme4)
library(insight)
library(reshape2)
library(shinycssloaders)
library(plotly)
library(viridis)

# Set base path and source vcov sparse matrix generator script
base_path = "./"
source(file.path(base_path,"build_samp_mat.R"))

 ######################
 ####### SET UI #######
 ######################

ui <- fluidPage(
  # theme, styling
  theme = shinytheme("superhero"),
  tags$head(
    tags$style("label{font-family: Helvetica;}"),
    tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }")),
    tags$style("#form{font-style: italic; font-size: 16px}"),
    tags$style("#fit{background-color: #0dc5c1;}"),
    tags$style("#form_div{border-style: solid; border-color:#EBEBEB; border-width:1px;
               padding-bottom:10px; padding-left:10px; padding-right:10px;}")),
  
  # Title
  titlePanel("LMM Variance-Covariance Structure"),
  
  # Sidebar options
  sidebarLayout(
    sidebarPanel(
      # Choose dataset (ONLY WORKS WITH RDS FILES!! add option + file to current directory)
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("","srrs2","nels")),
      
      uiOutput("state"),    # If srrs2, this get loaded (required)
      uiOutput("response"), # select response var
      uiOutput("fe"),       # Select fixed effects (required)
      
      # Add Random effects
      checkboxInput("rand_opt", 
                    label = "Add Random Effects", 
                    value = FALSE),
      
      conditionalPanel( 
        condition = "input.rand_opt == true",
        uiOutput("rint"), # Add random intercept drop down
        
        # Add random slope
        checkboxInput("rand_slope_opt", 
                      label = "Add random slope", 
                      value = FALSE),
        
        conditionalPanel(
          condition = "input.rand_slope_opt == true",
          uiOutput("rslope") # Add random slop drop down
          )
        ),
      
      actionButton("fit", "Fit Model"), # runs when clicked
      uiOutput("help") # Help for nels data
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        column(12,
               # top div, holds formula builder
              div(id = "form_div",
                  h4(id = "form_id", "Formula:",
                   tags$style("#form_id{color: #EBEBEB;}")),
                  textOutput("form")
                 ),
              
              # correlation text and plot outputs
              uiOutput("correlation"),  
              fluidRow(column(12, plotlyOutput("vcov_plot") %>% 
                                withSpinner(color="#0dc5c1")))
              )
        )
      
    ) # end of Main Panel
  )
) # end of UI

###########################
####### SET SERVER #######
##########################

server <- function(input, output) {
  
  # Get possible rds files
  fils <- list.files(base_path, pattern = ".*rds")
  
  # Load data
  get_df <- reactive({
    fil_name <- paste0(input$dataset, ".rds")
    fil_idx <- match(fil_name, tolower(fils))
    return(readRDS(file.path(base_path, fils[fil_idx])))
  })
  
  # Help text code
  output$help <- renderUI({
    if (input$dataset == "nels"){
    helpText("Note: visualization shows sub-sampled nels data 
             for rendering purposes (groups with >30 obs) ")
    }
  })
 
  # State drop down menu code
  output$state <- renderUI({
    if (input$dataset == "srrs2"){
      curr_df <- get_df()
      selectInput("state_select",
                  label = "Select a state:",
                  choices = c("",unique(as.character(curr_df$state2))))
    }
  })
  
  # Response var options drop down based on data selected
  output$response <- renderUI({
    curr_df <- get_df()
    selectInput(inputId = "y",
                label = "Response variable",
                choices = c("",sort(names(curr_df))))
  })
  
  # Fixed effects drop down options (excludes response var selection dynamically)
  output$fe <- renderUI({
    curr_df <- get_df()
    n_resp <- names(curr_df)
    n_resp <- n_resp[!str_detect(n_resp, input$y)]
    
    pickerInput(
      inputId = "fe_x", 
      label = "Fixed Effects", 
      choices = c("",sort(n_resp)), 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  # Random intercept drop down (exludes response selection dynamically)
  output$rint <- renderUI({
    curr_df <- get_df()
    n_resp <- names(curr_df)
    n_resp <- n_resp[!str_detect(n_resp, input$y)]
    selectInput(inputId = "grp",
                label = "Grouping Variable (Rand Int)",
                choices = c("", sort(n_resp)))
    })
  
  # Random slope selection (excludes response var selection dynamically)
  output$rslope <- renderUI({
    curr_df <- get_df()
    n_resp <- names(curr_df)
    n_resp <- n_resp[!str_detect(n_resp, paste0(input$y,"|",input$grp))]
    selectInput(inputId = "slope_var",
                label = "Random Slope",
                choices = c("", sort(n_resp)))
  })

  # build LM or LMM formula based on selections
  build_formula <- reactive({
    # lm formula
    y_var <- input$y
    fe <- paste(input$fe_x, collapse = " + ")
    
    # Add random intercept to formula
    if (input$rand_opt){
      rint <- paste0("(1|",input$grp,")")
    } else {
      rint <- ""
    }
    
    # Add random slope to formula
    if (input$rand_slope_opt){
      rslope <- paste0("(1 + ", input$slope_var, "|",input$grp,")")
    } else {
      rslope <- ""
    }
    
    # complete formula based on selection, dynamically update
    if( !sjmisc::is_empty(y_var) && !sjmisc::is_empty(fe)){
      base_expr <- paste(y_var, "~", fe)
      if (!sjmisc::is_empty(rslope)){
        form_expr <- paste(base_expr, rslope, sep = " + ")
      } else if (!sjmisc::is_empty(rint)) {
        form_expr <- paste(base_expr, rint, sep = " + ")
      } else {
        form_expr <- base_expr
      }
      
    } else {
      form_expr <- "Please select a response variable AND fixed effects"
    }
    
    return(form_expr)
  })
  
  # Render formula dynamically
  output$form <- renderText({
    out_text <- build_formula()
    paste(out_text)
   
  })
  
  # Based on formula, fit model when button click
  generate_model <- eventReactive(input$fit,{
    form <- build_formula()
    df <- get_df()
    
    # Filter srrs2 based on state selection
    if (input$dataset == "srrs2"){
      df <- df %>% filter(state == input$state_select)
    }
    
    # Fit lm or lmer models
    if (str_detect(form, "\\(")){
      lmer(form, data = df)
    } else {
      lm(form, data = df)
    }
    
  })
  
  # build var covar sparse matrix based on model output and data
  build_vcov <- eventReactive(generate_model(),{
    df <- get_df()
    mod <- generate_model()
    
    # Filter by state for srrs2, subsample nels for rendering
    if (input$dataset == "srrs2"){
      df <- df %>% filter(state == input$state_select)
      
    } else if (input$dataset == "nels"){
      cnts  <- df %>% group_by(school) %>% count()
      grps  <- cnts %>% ungroup() %>%  filter( n > 30)
      df    <- df %>% 
        filter(school %in% grps$school)
    }
   
    # If LMM, get variance and covariances
    if (class(mod)[1] == "lmerMod"){
      
      resid_var <-  get_variance_residual(mod)
      rint_var  <- get_variance_intercept(mod)
      vcov_mat  <- build_samp_mat_struct(df, input$grp) # call sourced script
      
      # Random slope var cov structure
      if (input$rand_slope_opt){
        rslop_var     <- get_variance_slope(mod)
        corr          <- get_correlation_slope_intercept(mod)
        cov_var       <- corr*(sqrt(rint_var)*sqrt(rslop_var))
        samp_cov      <- rint_var + rslop_var + 2*cov_var
        samp_var      <- samp_cov + resid_var 
          
        vcov_mat       <- samp_cov*vcov_mat
        diag(vcov_mat) <- samp_var
        
      } else { # Random intercept var covar structure
        samp_cov       <- rint_var  
        samp_var       <- samp_cov + resid_var
        
        vcov_mat       <- samp_cov*vcov_mat
        diag(vcov_mat) <- rint_var + resid_var
      }
      
    } else { # If simple lm, just get residual variance and create diagonal
      samp_cov <- 0
      samp_var <- summary(mod)$sigma^2
      
      vcov_mat <- Diagonal(nrow(df)) * samp_var
    }
    
    return(list(vcov_mat,c(samp_var, samp_cov)))
    
    
  })
  
  # Compute and output correlation within groups
  output$correlation <- renderUI({
    var_cov <- build_vcov()[[2]]
    corr <- var_cov[2]/var_cov[1]
    withMathJax(sprintf("$$ \\rho_{y_i} = %.03f \\text{ (within-group correlation) }$$", 
                        corr))
    
  })
  
  # Plot var covar matrix as heatmap with plotly
  output$vcov_plot <- renderPlotly({
    vcov_mat <- build_vcov()[[1]]
    vcov_mat <- melt(as.matrix(vcov_mat)) %>% 
      mutate(text = paste0("Value: ", round(value, 3), "\n",
                           "observation: ", Var1))
    
    p <- ggplot(vcov_mat, aes(x = Var1, y = Var2, 
                              fill = value, text = text)) + 
      geom_tile() +
      scale_fill_distiller(direction = 1) +
      coord_cartesian(xlim = NULL, ylim = NULL, expand = FALSE) +
      labs(x = "y", y = "y", 
           title = paste(input$dataset, "dataset (n =", max(unique(vcov_mat$Var1)), ")")) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip="text")
  })
  
 
}

# Run app
shinyApp(ui, server)
