#######
#######  NOTE: YOU MUST UPDATE PATH_TO_DATA WITH CORRECT PATH FOR YOUR COMPUTER
#######  Supports XLSX, CSV, TXT (comma seperated), and sas7bdat file types
PATH_TO_DATA = '~/Downloads/data/final2_sampled100.sas7bdat'
PATH_TO_DATA = '~/Downloads/data/FINAL2_SAMPLED100.XLSX'
PATH_TO_DATA = '~/Downloads/data/FINAL2_SAMPLED100.csv'

# Dependent Variable Name
DEP_VAR = "RESP"


#Re-code variables with meta data from data dictionary 
USE_LONG_VAR_NAMES = FALSE # SWITCHING TO TRUE IS NOT FULLY TESTED





##MISC STUFF##

options(stringsAsFactors = FALSE)

packages <- c("shiny", "ggplot2", "dplyr", "haven", "DT", "scales", "openxlsx", "Hmisc", "tools", "stargazer", "knitr", "gridExtra", "data.table")
for (package in packages) {
  message(sprintf("Checking that %s is installed...", package))
  if (!package %in% row.names(installed.packages())) {
    install.packages(package, repos = "https://cran.rstudio.com")
  }
}
library(dplyr)
library(ggplot2)
library(haven)
library(shiny)
library(DT)
library(scales)
library(openxlsx)
library(Hmisc)
library(tools)
library(stargazer)
library(knitr)
library(gridExtra)
library(data.table)

##READ DATA##

process_data <- function(file) {
  ext <- tolower(file_ext(file))
  if (ext == "xlsx") {
    message(sprintf("Processing xlsx file at \n\t %s", file))
    data <- read.xlsx(file, sheet=1, colNames = TRUE)
    return(data)
  } else if (ext == "csv" || ext == "txt") {
    message(sprintf("Processing txt/csv file at \n\t %s", file))
    data <- read.csv(file, header = TRUE, sep = ",")
    return(data)
  } else if (ext == "sas7bdat") {
    message(sprintf("Processing sas file at \n\t %s", file))
    data <- haven::read_sas(file)
    return(data)
  } else {
    message(sprintf("Error: Unable to determine file type at \n\t %s", file))
  }
}

# Read the data and save as var: data
if (!"RESP" %in% names(data)){
  data <- process_data(PATH_TO_DATA)
  to_numeric <- c("Zip", "Zip4", "V13")
  for (i in names(data)) {
    if (class(data[[i]]) == "integer" || i %in% to_numeric) {
      data[[i]] <- as.numeric(data[[i]])
    }
  }
}
# Randomly sample data
set.seed(621)
data <- data[sample(nrow(data)), ]


## Functions used within Shiny
cut_bins <- function(data, output_variable, input_variable, bin_number) {
  message("Making bin groupings")
  df <- data[,c(output_variable, input_variable)]
  df$output_variable <- df[[output_variable]]
  df$input_variable <- df[[input_variable]]
  
  variable_type <- class(df[[input_variable]])
  unique <- length(unique(df$input_variable))
  
  if ((variable_type == "numeric" || variable_type == "integer")&(unique>20)) {
    message(sprintf("Variable %s is numeric & has more than 20 values... coercing to bins...", 
                    input_variable))
    df$group_var <- ntile(x = df[[input_variable]], n = bin_number)
  }else{
    message(sprintf("Variable %s is %s...grouping on levels",
                    input_variable, variable_type))
    df$group_var <- df[[input_variable]]
  }
  
  message("Calculating response rates...")
  df <- df %>% group_by(group_var) %>%
    summarise(average_output = mean(output_variable, na.rm = TRUE)) %>%
    ungroup() %>% arrange(desc(average_output))
  message("Calculation complete")
  return(df)
}

table_data <- function(data, output_variable, input_variable, bin_number) {
  message("Creating data for table...")
  df<-data[,c(output_variable, input_variable)]
  
  variable_type <- class(df[[input_variable]])
  df$input_variable <- df[[input_variable]]
  df$output_variable <- df[[output_variable]]
  unique <- length(unique(df$input_variable))
  
  if ((variable_type == "numeric")&(unique>20)) {
    message(sprintf("Variable %s is numeric & has more than 20 values... table will show bin width...", 
                    input_variable))
    df$group_var <- ntile(x = df[[input_variable]], n = bin_number)
  }else{
    message(sprintf("Variable %s is %s...table will show FREQs",
                    input_variable, variable_type))
    df$group_var <- df[[input_variable]]
  }
  
  message("Calculating table data...")
  
  if (variable_type == "numeric") {
    df <- df %>% group_by(group_var) %>%
      summarise(freq = length(input_variable),
                avg_resp = mean(output_variable, na.rm=TRUE),
                min = min(input_variable),
                mean = mean(input_variable, na.rm = TRUE),
                max = max(input_variable)
      ) %>%
      ungroup() %>% arrange(group_var)
    message("Calculation complete")
    return(df)
  }else{
    df <- df %>% group_by(group_var) %>%
      summarise(freq = length(input_variable),
                avg_resp = mean(output_variable, na.rm=TRUE),
                min="NA",
                mean="NA",
                max="NA"
      ) %>%
      ungroup() %>% arrange(group_var)
    message("Calculation complete")
    return(df)
  }
}

## Summary Stats & Graph Code ##

# Function to produce random sample of data
head_data <- function(data, input_var, obs) {
  table <- data[1:obs, c('RESP',input_var)]
  return(table)
}

# Function to produce numeric var summary statistics
numeric_summary <- function(data, input_var) {
  input_var <- c('RESP', input_var)
  table <- data[, input_var]
  colN <- c('Min', 'First Q', 'Median', 'Mean', 'Third Q', "Max", "NAs")
  y <- data.frame(matrix(nrow = length(input_var), ncol = length(colN), dimnames=list(input_var, colN)))
  colnames(y) <- colN
  for (i in input_var) {
    sdf <- summary(table[[i]])
    for (j in 1:length(sdf)) {
      y[i,j] <- sdf[[j]]
    }
  }
  return(y)
}

# Function to produce character var summary statistics
character_summary <- function(data, input_var) {
  colN <- c('Mode: Response', 'Mode: Count', 'Least Frequent: Response', 'Least Frequent: Count', 
            'Five Most Common Responses', 'Five Least Common Responses', 'NA\'s')
  y <- data.frame(matrix(nrow = length(input_var), ncol = length(colN), dimnames=list(input_var, colN)))
  colnames(y) <- colN
  for (i in input_var) {
    l <- sort(table(factor(data[[i]])))
    if (length(l) < 2) {
      y <- y[!rownames(y) %in% i,]
      next
    }
    n <- l[which(names(l) == "")][[1]]
    l <- l[-which(names(l) == "")]
    mn <- head(l, 1)
    mx <- tail(l, 1)
    if (length(l) > 9) {
      top_five <- paste0(tail(names(l),5), sep = " ", collapse = "")
      bottom_five <- paste0(head(names(l),5), sep = " ", collapse = "")
    } else {
      top_five <- bottom_five <- "Too few factors"
    }
    r <- c(names(mx), mx[[1]], names(mn), mn[[1]], top_five, bottom_five, n)
    for (j in 1:length(r)) {
      y[i, j] <- r[j]
    }
  }
  return(y)
}

# Produce appropriate Summary Statistics (Character vs Numeric)
summary_tables <- function(data, input_var, class) {
  var_list <- c()
  if (class == 'character') {
    for (i in input_var) {
      if (class(data[[i]]) == 'character') {
        var_list <- c(var_list, i)
      }
    }
    t <- character_summary(data, var_list)
  } else if (class == 'numeric') {
    for (i in input_var) {
      if (class(data[[i]]) != 'character') {
        var_list <- c(var_list, i)
      }
    }
    t <- numeric_summary(data, var_list)
  }
  return(t)
}

# Function to produce grid of histograms
hist_plot <- function(data, input_var) {
  pl <- as.numeric(length(input_var))
  histList <- list()
  for (i in input_var){
    hist_title <- paste0("Variable: ", i)
    hist_data <- data.frame(x = data[[i]])
    if (class(data[[i]]) == "character" || class(data[[i]]) == "logical") {
      hist_data$x <- as.factor(hist_data$x)
      if (length(unique(hist_data$x)) < 21 || i == 'State') {
        histList[[i]] <- ggplot(data=hist_data, aes(x)) + 
          geom_bar() +
          ggtitle(hist_title)+
          xlab(i)+
          ylab("Count")+
          theme_bw() +           
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"))
      } else {
        pl <- pl - 1
        next
      }
    } else {
      if (length(unique(hist_data$x)) < 10) {
        hist_data$x <- as.factor(hist_data$x)
        histList[[i]] <- ggplot(data=hist_data, aes(x)) + 
          geom_bar() +
          ggtitle(hist_title)+
          xlab(i)+
          ylab("Count")+
          theme_bw() +           
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"))
      } else {
        histList[[i]] <- ggplot(data=hist_data, aes(x)) + 
          geom_histogram() +
          ggtitle(hist_title)+
          xlab(i)+
          ylab("Count")+
          theme_bw() +           
          theme(panel.border = element_blank(), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), 
                axis.line = element_line(colour = "black"))
      }
    }
  }
  hist <- marrangeGrob(histList, nrow = round(sqrt(pl),0), ncol = ceiling(sqrt(pl)), top = NULL)
  return(hist)
} 


##Shiny Stuff##
server <- function(input, output) {
  output$chart <- renderPlot({
    chart_data <- cut_bins(data = data, output_variable = DEP_VAR,
                           input_variable = input$variable,
                           bin_number = input$bin_number)
    
    chart = ggplot(data = chart_data, aes(x = factor(group_var), y = average_output)) +
      geom_bar(stat="identity") +
      theme(plot.title = element_text(size=15, face="bold"),
            axis.title.x = element_text(size=15),
            axis.text.x = element_text(size=11),
            axis.title.y = element_text(size=15),
            axis.text.y = element_text(size=11)
      ) +
      ggtitle("Response by Bin") +
      labs(x="Bins", y="% Response")+
      scale_y_continuous(labels=percent)
    return(chart)
  })
  
  output$table <- renderDataTable({
    data_table <- table_data(data = data, output_variable = DEP_VAR,
                             input_variable = input$variable,
                             bin_number = input$bin_number)
    if (any(data_table$min != "NA")) {
      data_table$min <- round(data_table$min, 0)
      data_table$mean <- round(data_table$mean, 0)
      data_table$max <- round(data_table$max, 0)
      data_table$avg_resp <- scales::percent(data_table$avg_resp)
    }else{
      data_table$avg_resp <- scales::percent(data_table$avg_resp)
    }
    table <-    data_table
    return(table)
  },
  options=list(paging=FALSE, searching=FALSE, bFilter=0))
  
  output$table2 <- renderDataTable(merged,
                                   options=list(paging=FALSE, searching=FALSE, bFilter=0))
  
  # Generate summary stats character variables
  output$charactervars <- renderDataTable({
    summary_table <- summary_tables(data, input$var, 'character')
    table <- summary_table
    return(table)
  }, options=list(paging=FALSE, searching=FALSE, bFilter=0))
  
  # Generate summary stats
  output$numericvars <- renderDataTable({
    summary_table <- summary_tables(data, input$var, 'numeric')
    table <- summary_table
    return(table)
  }, options=list(paging=FALSE, searching=FALSE, bFilter=0))
  
  # Generate table data from random sample
  output$view <- renderDataTable({
    head_table <- head_data(data, input$var, input$obs)
    table <- head_table
    return(table)
  },  options=list(paging=FALSE, searching=FALSE, bFilter=0))
  
  # Generate Histogram Output
  output$histogram <- renderPlot({
    Hplot <- hist_plot(data, input$var)
    return(Hplot)
  })
}

ui <- navbarPage("Jeff's Model Thing",
                 tabPanel("Bin & Graph",
                          {
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput(inputId = "bin_number", label = "Set Number of Bins",
                                            value = 10, min = 1, max = 50),
                                radioButtons(inputId = "variable", label = "Pick a variable",
                                             choices = names(data)[!names(data) %in% DEP_VAR])),
                              mainPanel(
                                fluidRow(
                                  plotOutput("chart"),
                                  br(), br(),
                                  dataTableOutput("table")
                                )))
                          }),
                 tabPanel("Summary Stats",
                          {
                            sidebarLayout(
                              sidebarPanel(
                                numericInput(inputId = "obs", label = "Number of observations to view:", 10),
                                checkboxGroupInput(inputId = "var", label = "Pick a variable",
                                                   choices = names(data)[!names(data) %in% DEP_VAR],
                                                   selected = names(data)[3:5])),
                              mainPanel(
                                h3("Histogram & Bar Charts", align = "center"),
                                plotOutput("histogram"),
                                h3("Summary Statistics", align = "center"),
                                h4("Numeric Variables", align = "center"),
                                dataTableOutput("numericvars"),
                                h4("Character Variables", align = "center"),
                                dataTableOutput("charactervars"),
                                h3("Random Sample of Data", align = "center"),
                                dataTableOutput("view")
                              ))
                          })
)

shinyApp(ui = ui, server = server)