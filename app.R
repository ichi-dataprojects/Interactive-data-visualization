
setwd("D:\\setebe\\Data science\\2024\\NBS")

data <- read.csv("NBSCENSUS.csv")

attach(data)


library(shiny)
library(plotly)


# Define UI for application
ui <- fluidPage(
  titlePanel("Interactive Data visualization of Tanzania census conducted from 2002, 2012 and 2022"),
  
  sidebarLayout(
    sidebarPanel(
      # Sidebar input for choosing plot type
      selectInput("plot_type", "Select Plot Type:", 
                  choices = c("Bar Plot", "Grouped Bar Plot", "Scatter Plot", "Line Plot"))
    ),
    
    mainPanel(
      # Output(s)
      uiOutput("plots")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$plots <- renderUI({
    plot_output_list <- list()
    
    # Dynamically create plot outputs based on selected plot type
    if (input$plot_type == "Bar Plot") {
      plot_output_list[[1]] <- plotlyOutput("bar_plot1")
      plot_output_list[[2]] <- plotlyOutput("bar_plot2")
      plot_output_list[[3]] <- plotlyOutput("bar_plot3")
    } else if (input$plot_type == "Grouped Bar Plot") {
      plot_output_list[[1]] <- plotlyOutput("grouped_bar_plot1")
      plot_output_list[[2]] <- plotlyOutput("grouped_bar_plot2")
      plot_output_list[[3]] <- plotlyOutput("grouped_bar_plot3")
    } else if (input$plot_type == "Scatter Plot") {
      plot_output_list[[1]] <- plotlyOutput("scatter_plot1")
      plot_output_list[[2]] <- plotlyOutput("scatter_plot2")
      plot_output_list[[3]] <- plotlyOutput("scatter_plot3")
      plot_output_list[[4]] <- plotlyOutput("scatter_plot4")
      plot_output_list[[5]] <- plotlyOutput("scatter_plot5")
      plot_output_list[[6]] <- plotlyOutput("scatter_plot6")
    } else if (input$plot_type == "Line Plot") {
      plot_output_list[[1]] <- plotlyOutput("line_plot1")
      plot_output_list[[2]] <- plotlyOutput("line_plot2")
      plot_output_list[[3]] <- plotlyOutput("line_plot3")
      plot_output_list[[4]] <- plotlyOutput("line_plot4")
      plot_output_list[[5]] <- plotlyOutput("line_plot5")
      plot_output_list[[6]] <- plotlyOutput("line_plot6")
    }
    
    do.call(tagList, plot_output_list)
  })
  
  output$bar_plot1 <- renderPlotly({
    # Data for the first basic bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive bar plot using Plotly with a title
    plot_ly(data, x = ~Region, y = ~census2002, type = 'bar', name = '2002 census') %>%
      layout(title = "Bar Plot showing distribution of 2002 census")
  })
  
  output$bar_plot2 <- renderPlotly({
    # Data for the second basic bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive bar plot using Plotly with a title
    plot_ly(data, x = ~Region, y = ~census2012, type = 'bar', name = '2012 census') %>%
      layout(title = "Bar Plot showing distribution of 2012 census")
  })
  
  output$bar_plot3 <- renderPlotly({
    # Data for the third basic bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive bar plot using Plotly with a title
    plot_ly(data, x = ~Region, y = ~census2022, type = 'bar', name = '2022 census') %>%
      layout(title = "Bar Plot showing distribution of 2022 census")
  })
  
  output$grouped_bar_plot1 <- renderPlotly({
    # Data for the first grouped bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive grouped bar plot using Plotly with a title
    plot_ly(data, x = ~Region) %>%
      add_trace(y = ~census2002, type = 'bar', name = '2002 census') %>%
      add_trace(y = ~census2012, type = 'bar', name = '2012 census') %>%
      layout(title = "Grouped Bar showing distribution of 2002 and 2012 census")
  })
  
  output$grouped_bar_plot2 <- renderPlotly({
    # Data for the second grouped bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive grouped bar plot using Plotly
    plot_ly(data, x = ~Region) %>%
      add_trace(y = ~census2012, type = 'bar', name = 'census 2012') %>%
      add_trace(y = ~census2022, type = 'bar', name = 'census 2022')%>%
      layout(title = "Grouped Bar showing distribution of 2012 and 2022 census")
  })
  
  output$grouped_bar_plot3 <- renderPlotly({
    # Data for the third grouped bar plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create an interactive grouped bar plot using Plotly
    plot_ly(data, x = ~Region) %>%
      add_trace(y = ~census2002, type = 'bar', name = '2002 census') %>%
      add_trace(y = ~census2022, type = 'bar', name = '2022 census')%>%
      layout(title = "Grouped Bar showing distribution of 2002 and 2022 census")
  })
  
  output$scatter_plot1 <- renderPlotly({
    # Data for the first scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with linear regression line and loess smoother
    scatter_plot <- plot_ly(data, x = ~census2002, y = ~census2012, 
                            color = ~Population, type = "scatter", mode = "markers",
                            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title= "There is a positive Relationship of 2002 census and 2012 census Populations",
             xaxis = list(title = "2002 census"),
             yaxis = list(title = "2012 census"),
             showlegend = FALSE)
    
    # Add linear regression line
    scatter_plot <- scatter_plot %>%
      add_lines(x = ~census2002, y = ~predict(lm(census2012 ~ census2002, data)), 
                line = list(color = "blue", width = 2),
                name = "Linear Regression Line")
    
  })
  
  output$scatter_plot2 <- renderPlotly({
    # Data for the second scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with linear regression line and loess smoother
    scatter_plot <- plot_ly(data, x = ~census2012, y = ~census2022, 
                            color = ~Population, type = "scatter", mode = "markers",
                            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title= "There is a positive Relationship of 2012 census and 2022 census Populations",
             xaxis = list(title = "2012 census"),
             yaxis = list(title = "2022 census"),
             showlegend = FALSE)
    
    # Add linear regression line
    scatter_plot <- scatter_plot %>%
      add_lines(x = ~census2012, y = ~predict(lm(census2022 ~ census2012, data)), 
                line = list(color = "blue", width = 2),
                name = "Linear Regression Line")
    
  })
  
  output$scatter_plot3 <- renderPlotly({
    # Data for the third scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with linear regression line and loess smoother
    scatter_plot <- plot_ly(data, x = ~census2002, y = ~census2022, 
                            color = ~Population, type = "scatter", mode = "markers",
                            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title= "There is a positive Relationship of 2002 census and 2022 census Populations",
             xaxis = list(title = "2002 census"),
             yaxis = list(title = "2022 census"),
             showlegend = FALSE)
    
    # Add linear regression line
    scatter_plot <- scatter_plot %>%
      add_lines(x = ~census2012, y = ~predict(lm(census2002 ~ census2022, data)), 
                line = list(color = "blue", width = 2),
                name = "Linear Regression Line")
  })
  
  output$scatter_plot4 <- renderPlotly({
    # Data for the fourth scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with customization
    plot_ly(data, x = ~census2002, y = ~census2012, color = ~Population, type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title = "Relationship of 2002 census and 2012 census Populations",
             xaxis = list(title = "Census of 2002"),
             yaxis = list(title = "Census of 2012"),
             legend = list(title = "Development status"),
             shapes = list(list(type = "line", x0 = -2, x1 = 2, y0 = -2, y1 = 2,
                                line = list(color = "red", width = 2, dash = "dot"))))
  })
  
  output$scatter_plot5 <- renderPlotly({
    # Data for the fifth scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with customization
    plot_ly(data, x = ~census2012, y = ~census2022, color = ~Population, type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title = "Relationship of 2012 census and 2022 census Populations",
             xaxis = list(title = "Census of 2002"),
             yaxis = list(title = "Census of 2012"),
             legend = list(title = "Development status"),
             shapes = list(list(type = "line", x0 = -2, x1 = 2, y0 = -2, y1 = 2,
                                line = list(color = "red", width = 2, dash = "dot"))))
  })
  
  output$scatter_plot6 <- renderPlotly({
    # Data for the sixth scatter plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create scatter plot with customization
    plot_ly(data, x = ~census2002, y = ~census2022, color = ~Population, type = "scatter", mode = "markers",
            marker = list(size = 10, opacity = 0.7, symbol = "circle")) %>%
      layout(title = "Relationship of 2012 census and 2022 census Populations",
             xaxis = list(title = "Census of 2002"),
             yaxis = list(title = "Census of 2022"),
             legend = list(title = "Development status"),
             shapes = list(list(type = "line", x0 = -2, x1 = 2, y0 = -2, y1 = 2,
                                line = list(color = "red", width = 2, dash = "dot"))))
  })
  
  output$line_plot1 <- renderPlotly({
    # Data for the first line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create line plot with another categorical variable
    line_plot <- plot_ly(data, x = ~Region, y = ~census2002,
                         type = "scatter",
                         line = list(width = 2)) %>%
      layout(title = "Line plot showing trend of 2002 census",
             xaxis = list(title = "Regions"),
             yaxis = list(title = "2002 Census"),
             showlegend = FALSE)  # Remove legend
  })
  
  output$line_plot2 <- renderPlotly({
    # Data for the second line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create line plot with another categorical variable
    line_plot <- plot_ly(data, x = ~Region, y = ~census2012,
                         type = "scatter",
                         line = list(width = 2)) %>%
      layout(title = "Line plot showing trend of 2012 census",
             xaxis = list(title = "Regions"),
             yaxis = list(title = "2012 Census"),
             showlegend = FALSE)  # Remove legend
  })
  
  output$line_plot3 <- renderPlotly({
    # Data for the third line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create line plot with another categorical variable
    line_plot <- plot_ly(data, x = ~Region, y = ~census2022,
                         type = "scatter",
                         line = list(width = 2)) %>%
      layout(title = "Line plot showing trend of 2022 census",
             xaxis = list(title = "Regions"),
             yaxis = list(title = "2022 Census"),
             showlegend = FALSE)  # Remove legend
  })
  
  output$line_plot4 <- renderPlotly({
    # Data for the fourth line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create the plot
    line_plot <- plot_ly(data, x = ~Region, y = ~census2002, type = 'scatter', 
                         mode = 'lines', name = '2002 census') %>%
      add_trace(y = ~census2012, y = ~census2022, name = '2012 census')
    
    # Add layout options
    line_plot <- line_plot %>% layout(
      title = "Line plot showing trend of 2002 census 2012 census",
      xaxis = list(title = "Regions"),
      yaxis = list(title = "Census")
    )
  })
  
  output$line_plot5 <- renderPlotly({
    # Data for the fifth line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create the plot
    line_plot <- plot_ly(data, x = ~Region, y = ~census2002, type = 'scatter', 
                         mode = 'lines', name = '2002 census') %>%
      add_trace(y = ~census2022, y = ~census2022, name = '2022 census')
    
    # Add layout options
    line_plot <- line_plot %>% layout(
      title = "Line plot showing trend of 2002 census 2022 census",
      xaxis = list(title = "Regions"),
      yaxis = list(title = "Census")
    )
  })
  
  output$line_plot6 <- renderPlotly({
    # Data for the sixth line plot (example data)
    data <- read.csv("NBSCENSUS.csv")
    
    # Create the plot
    line_plot <- plot_ly(data, x = ~Region, type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~census2002, name = '2002 census') %>%
      add_trace(y = ~census2012, name = '2012 census') %>%
      add_trace(y = ~census2022, name = '2022 census')
    
    # Add layout options
    line_plot <- line_plot %>% layout(
      title = "Line Plot showing trend of 2002,2012 and 2022 census",
      xaxis = list(title = "Region"),
      yaxis = list(title = "Tanzania census")
    )
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)

