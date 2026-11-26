
# ============================================================
# PROJECT: CUSTOMER SEGMENTATION USING R
# INTERACTIVE SHINY DASHBOARD
# ============================================================

# Aim: To develop an interactive customer segmentation
# dashboard using R Shiny and K-Means clustering.

print("Swayam Arora - 1/24/SET/BCS/349")

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# ============================================================
# LOAD DATA
# ============================================================

customers <- read.csv("data/Segmented_Customers.csv")

customers$Cluster <- as.factor(customers$Cluster)
customers$Gender <- as.factor(customers$Gender)

# Column names
income_col <- "Annual.Income..k.."
spending_col <- "Spending.Score..1.100."

# Cluster summary
cluster_summary <- customers %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Average_Age = round(mean(Age), 2),
    Average_Income = round(mean(.data[[income_col]]), 2),
    Average_Spending = round(mean(.data[[spending_col]]), 2),
    .groups = "drop"
  )

# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel("Customer Segmentation Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Dashboard Filters"),
      
      selectInput(
        "gender",
        "Select Gender:",
        choices = c("All", levels(customers$Gender)),
        selected = "All"
      ),
      
      selectInput(
        "cluster",
        "Select Customer Cluster:",
        choices = c("All", levels(customers$Cluster)),
        selected = "All"
      ),
      
      sliderInput(
        "age",
        "Select Age Range:",
        min = min(customers$Age),
        max = max(customers$Age),
        value = range(customers$Age)
      ),
      
      actionButton(
        "reset",
        "Reset Filters"
      ),
      
      hr(),
      
      p("Customer Segmentation System"),
      p("Developed using R and Shiny")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # ==================================================
        # TAB 1: OVERVIEW
        # ==================================================
        
        tabPanel(
          "Overview",
          
          br(),
          
          h3("Customer Overview"),
          
          fluidRow(
            
            column(
              4,
              wellPanel(
                h4("Total Customers"),
                h2(textOutput("total_customers"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Average Income"),
                h2(textOutput("average_income"))
              )
            ),
            
            column(
              4,
              wellPanel(
                h4("Average Spending"),
                h2(textOutput("average_spending"))
              )
            )
            
          ),
          
          h4("Customer Gender Distribution"),
          plotOutput("gender_plot"),
          
          h4("Customer Age Distribution"),
          plotOutput("age_plot")
          
        ),
        
        # ==================================================
        # TAB 2: CUSTOMER ANALYSIS
        # ==================================================
        
        tabPanel(
          "Customer Analysis",
          
          br(),
          
          h3("Customer Data Analysis"),
          
          plotOutput("income_plot"),
          
          plotOutput("spending_plot"),
          
          plotOutput("income_spending_plot")
          
        ),
        
        # ==================================================
        # TAB 3: CUSTOMER SEGMENTATION
        # ==================================================
        
        tabPanel(
          "Customer Segmentation",
          
          br(),
          
          h3("K-Means Customer Clustering"),
          
          p("Customers are divided into five groups based on annual income and spending score."),
          
          plotOutput("cluster_plot", height = "500px")
          
        ),
        
        # ==================================================
        # TAB 4: CLUSTER INSIGHTS
        # ==================================================
        
        tabPanel(
          "Cluster Insights",
          
          br(),
          
          h3("Customer Cluster Profiles"),
          
          tableOutput("cluster_table"),
          
          br(),
          
          h4("Number of Customers in Each Cluster"),
          
          plotOutput("cluster_count_plot")
          
        ),
        
        # ==================================================
        # TAB 5: CUSTOMER EXPLORER
        # ==================================================
        
        tabPanel(
          "Customer Explorer",
          
          br(),
          
          h3("Explore Individual Customers"),
          
          DTOutput("customer_table"),
          
          br(),
          
          downloadButton(
            "download_data",
            "Download Segmented Data"
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Filter customers
  filtered_customers <- reactive({
    
    data <- customers
    
    if (input$gender != "All") {
      data <- data %>%
        filter(Gender == input$gender)
    }
    
    if (input$cluster != "All") {
      data <- data %>%
        filter(Cluster == input$cluster)
    }
    
    data <- data %>%
      filter(
        Age >= input$age[1],
        Age <= input$age[2]
      )
    
    data
  })
  
  
  # Reset filters
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "gender",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "cluster",
      selected = "All"
    )
    
    updateSliderInput(
      session,
      "age",
      value = range(customers$Age)
    )
    
  })
  
  
  # ==================================================
  # OVERVIEW OUTPUTS
  # ==================================================
  
  output$total_customers <- renderText({
    
    nrow(filtered_customers())
    
  })
  
  output$average_income <- renderText({
    
    round(
      mean(filtered_customers()[[income_col]]),
      2
    )
    
  })
  
  output$average_spending <- renderText({
    
    round(
      mean(filtered_customers()[[spending_col]]),
      2
    )
    
  })
  
  
  # Gender distribution
  output$gender_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(x = Gender, fill = Gender)
    ) +
      geom_bar() +
      labs(
        title = "Gender Distribution",
        x = "Gender",
        y = "Number of Customers"
      ) +
      theme_minimal()
    
  })
  
  
  # Age distribution
  output$age_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(x = Age)
    ) +
      geom_histogram(
        binwidth = 5,
        fill = "skyblue",
        color = "black"
      ) +
      labs(
        title = "Age Distribution",
        x = "Age",
        y = "Frequency"
      ) +
      theme_minimal()
    
  })
  
  
  # ==================================================
  # CUSTOMER ANALYSIS OUTPUTS
  # ==================================================
  
  output$income_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(x = .data[[income_col]])
    ) +
      geom_histogram(
        fill = "orange",
        color = "black",
        binwidth = 10
      ) +
      labs(
        title = "Annual Income Distribution",
        x = "Annual Income (k$)",
        y = "Frequency"
      ) +
      theme_minimal()
    
  })
  
  
  output$spending_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(x = .data[[spending_col]])
    ) +
      geom_histogram(
        fill = "lightgreen",
        color = "black",
        binwidth = 10
      ) +
      labs(
        title = "Spending Score Distribution",
        x = "Spending Score",
        y = "Frequency"
      ) +
      theme_minimal()
    
  })
  
  
  output$income_spending_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(
        x = .data[[income_col]],
        y = .data[[spending_col]],
        color = Cluster
      )
    ) +
      geom_point(size = 3) +
      labs(
        title = "Income vs Spending Score",
        x = "Annual Income (k$)",
        y = "Spending Score"
      ) +
      theme_minimal()
    
  })
  
  
  # ==================================================
  # CUSTOMER SEGMENTATION OUTPUT
  # ==================================================
  
  output$cluster_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(
        x = .data[[income_col]],
        y = .data[[spending_col]],
        color = Cluster
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.8
      ) +
      labs(
        title = "Customer Segmentation Using K-Means",
        x = "Annual Income (k$)",
        y = "Spending Score",
        color = "Cluster"
      ) +
      theme_minimal()
    
  })
  
  
  # ==================================================
  # CLUSTER INSIGHTS OUTPUTS
  # ==================================================
  
  output$cluster_table <- renderTable({
    
    cluster_summary %>%
      filter(
        Cluster %in% levels(droplevels(filtered_customers()$Cluster))
      )
    
  })
  
  
  output$cluster_count_plot <- renderPlot({
    
    ggplot(
      filtered_customers(),
      aes(x = Cluster, fill = Cluster)
    ) +
      geom_bar() +
      labs(
        title = "Customers in Each Cluster",
        x = "Cluster",
        y = "Number of Customers"
      ) +
      theme_minimal()
    
  })
  
  
  # ==================================================
  # CUSTOMER EXPLORER
  # ==================================================
  
  output$customer_table <- renderDT({
    
    datatable(
      filtered_customers(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  
  # ==================================================
  # DOWNLOAD SEGMENTED DATA
  # ==================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      "Segmented_Customers.csv"
    },
    
    content = function(file) {
      
      write.csv(
        filtered_customers(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
}


# ============================================================
# RUN SHINY APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)

