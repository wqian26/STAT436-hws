library(tidyverse)
library(tsibble)
library(feasts)
library(plotly)
library(slider)
library(shiny)
library(ggridges)
library(ggplot2)
library(dplyr)
library(DT) 
theme_set(theme_bw())

df <- read_csv('https://raw.githubusercontent.com/wqian26/STAT436-hws/refs/heads/main/hosp_eolchronic.csv')
df1 <- df %>%
  rename(
    care_intensity = `Hospital Care Intensity Index during the Last Two Years of Life`,
    medicare_reimb = `Total Medicare Reimbursements per Decedent during the Last Two Years of Life`,
    inpatient_days = `Inpatient Days per Decedent during the Last Two Years of Life`,
    deaths_in_hospital = `Percent of Deaths Occurring In Hospital`,
    hospice_enrolled = `Percent of Decedents Enrolled In Hospice during the Last Six Months of Life`,
    avg_copayments = `Average Co-Payments per Decedent during the Last Two Years of Life`,
    dme_reimbursements = `Reimbursements for Durable Medical Equipment per Decedent during the Last Two Years of Life`,
    icu_deaths = `Percent of Deaths Associated With ICU Admission`,
    name = `Hospital Name`
  ) %>%
  select(HRR, State, System, name, care_intensity, medicare_reimb, inpatient_days, 
         deaths_in_hospital, hospice_enrolled, avg_copayments, dme_reimbursements, icu_deaths) %>%
  mutate(
    care_intensity = as.numeric(care_intensity),
    medicare_reimb = as.numeric(medicare_reimb),
    inpatient_days = as.numeric(inpatient_days),
    deaths_in_hospital = as.numeric(deaths_in_hospital),
    hospice_enrolled = as.numeric(hospice_enrolled),
    avg_copayments = as.numeric(avg_copayments),
    dme_reimbursements = as.numeric(dme_reimbursements),
    icu_deaths = as.numeric(icu_deaths)
  ) %>%
  mutate(care_intensity=ifelse(care_intensity < 0, NA, care_intensity),
         icu_deaths = ifelse(icu_deaths < 0, NA, icu_deaths),
         deaths_in_hospital = ifelse(deaths_in_hospital < 0, NA, deaths_in_hospital),
         hospice_enrolled = ifelse(hospice_enrolled < 0, NA, hospice_enrolled)
         )

df1 <- df1 %>%
  mutate(Region = case_when(
    State %in% c('ME', 'NH', 'VT', 'MA', 'RI', 'CT') ~ 'New England',
    State %in% c('NY', 'NJ', 'PA') ~ 'Middle Atlantic',
    State %in% c('OH', 'MI', 'IN', 'IL', 'WI') ~ 'East North Central',
    State %in% c('MN', 'IA', 'MO', 'ND', 'SD', 'NE', 'KS') ~ 'West North Central',
    State %in% c('DE', 'MD', 'DC', 'VA', 'WV', 'NC', 'SC', 'GA', 'FL') ~ 'South Atlantic',
    State %in% c('KY', 'TN', 'MS', 'AL') ~ 'East South Central',
    State %in% c('AR', 'LA', 'OK', 'TX') ~ 'West South Central',
    State %in% c('MT', 'ID', 'WY', 'NV', 'UT', 'CO', 'AZ', 'NM') ~ 'Mountain',
    State %in% c('WA', 'OR', 'CA', 'AK', 'HI') ~ 'Pacific',
    TRUE ~ 'Other'
  )) %>%
  filter(!(Region %in% 'Other'))


write.csv(df1, "hw2.csv", row.names = FALSE)



histogram <- function(df, x_var, lower_quantile=0.05, upper_quantile=0.95) {
  filtered <- df %>%
    filter(
      df[[x_var]] >= quantile(df[[x_var]], lower_quantile, na.rm = TRUE) &
        df[[x_var]] <= quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
    )
  ggplot(filtered, aes_string(x = x_var, fill = "Region")) +
    geom_histogram(alpha = 0.7) +
    labs(x = x_var, y = "Frequency", title =paste("Histogram of", names(choices_list)[which(choices_list == x_var)])) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


boxplot <- function(data, x_var) {
  y_var <- sym(x_var)
  ggplot(data, aes(x = Region, y = !!y_var, fill = Region)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", names(choices_list)[which(choices_list == x_var)]), x = 'Region', y = x_var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
scatterplot <- function(df, x_var, y_var, lower_quantile = 0.05, upper_quantile = 0.95) {
  lower_x <- quantile(df[[x_var]], lower_quantile, na.rm = TRUE)
  upper_x <- quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
  lower_y <- quantile(df[[y_var]], lower_quantile, na.rm = TRUE)
  upper_y <- quantile(df[[y_var]], upper_quantile, na.rm = TRUE)
  
  filtered <- df %>%
    filter(
      df[[x_var]] >= quantile(df[[x_var]], lower_quantile, na.rm = TRUE) &
        df[[x_var]] <= quantile(df[[x_var]], upper_quantile, na.rm = TRUE)
    )
  ggplot(filtered, aes_string(x = x_var, y = y_var, color = "State")) +
    geom_point() + 
    labs(x = x_var, y = y_var,title = paste("Scatterplot of", names(choices_list)[which(choices_list == x_var)], "vs.", names(choices_list)[which(choices_list == y_var)]))
}


overlay_histogram <- function(df, selected_, x_var) {
  sub_df <- filter(df, selected_)
  p <- ggplot(df, aes_string(x = x_var, fill = 'State', text = 'name')) +
    geom_histogram(alpha = 0.3, binwidth = 1000) +  
    geom_histogram(data = sub_df, fill = "red", binwidth = 1000, alpha = 0.5) + 
    scale_fill_brewer(palette = "Set2") +
    labs( x = x_var, y = "Frequency", title = paste("Overlay Histogram of", names(choices_list)[which(choices_list ==x_var)]))
  ggplotly(p, tooltip = "text") %>%
    style(hoveron = "fill")
}


filter_df <- function(df, selected_) {
  filter(df, selected_)
}


choices_list <- setNames(
  c("care_intensity", "medicare_reimb", "inpatient_days", "deaths_in_hospital",
    "hospice_enrolled", "avg_copayments", "dme_reimbursements", "icu_deaths"),
  c("Hospital Care Intensity Index (Last 2 Years)", 
    "Total Medicare Reimbursements (Last 2 Years)",  
    "Inpatient Days (Last 2 Years)", 
    "Percent of Deaths Occurring In Hospital", 
    "Percent Enrolled in Hospice (Last 6 Months)",  
    "Average Co-Payments (Last 2 Years)",  
    "Reimbursements for Durable Medical Equipment (Last 2 Years)", 
    "Percent of Deaths Associated with ICU Admission")
)

ui <- fluidPage(
  titlePanel("Hospital Care Metrics: Interactive Visualization"),
  h4("Overview"),
  p("This dataset from Dartmouth Atlas of Health Care collects a wide range of healthcare metrics from various hospitals across the United States. The metrics have been meticulously compiled to facilitate an analysis of healthcare efficiency, care intensity, patient outcomes, and overall hospital performance."),
  h4("Interpretation of variables"), 
  p("Care Intensity Index: Measures the amount and nature of healthcare services provided."), 
  p("Medicare Reimbursements: Reflects the financial aspects of healthcare delivery-how hospitals manage resources."), 
  p("Inpatient Days:Indicates the average length of hospital stays."), 
  p("ICU Deaths:Critical care outcomes, highlighting areas for improvement in life-saving interventions."), 
  p("Hospice Enrollment:percentage of patients receiving palliative care."), 
  h4('Implications'), 
  p("The analysis of this dataset can support extensive research into hospital management efficiency, healthcare policy reform, and strategic planning. Understanding the relationships between care intensity and patient outcomes may  guide policy decisions aimed at optimizing healthcare expenditures while maximizing patient health outcomes."),
  p(" Insights may also  aid in addressing disparities in healthcare access and quality, ensuring that effective, equitable healthcare is available to all population segments."),

  sidebarLayout(
    sidebarPanel(
      h4('Instructions'),
      h5("Step 1: Select X-axis variable"),
      p("Choose the variable you want to display on the X-axis. "),
      selectInput("x_var", "X-axis variable:", 
                  choices = choices_list, 
                  selected = "medicare_reimb"),  
      h5("Step 2: Select Y-axis variable"),
      p("Select the variable for the Y-axis. This allows you to compare two metrics, e.g. care intensity vs. Medicare reimbursements."),
      
      selectInput("y_var", "Select Y-axis variable:", 
                  choices = choices_list, 
                  selected = "care_intensity"),  
      
      h5("Step 3: Adjust the Data Range (Optional)"),
      p("These sliders help you remove extreme values(outliers)."),
      p("Use  'Lower Percentile' slider to exclude lowest values in the data. If you set the slider to 5%, the lowest 5% of hospitals with very small values will be removed."),
      p("Similarly, use  'Upper Percentile' slider to exclude the highest values in the data. If you set this to 95%, the top 5% of hospitals with unusually high values will be removed."),
      sliderInput("lower_quantile", 
                  "Lower Percentile (Exclude extreme Low-Value Outliers)", 
                  min = 0, max = 0.5, value = 0.05),
      
      sliderInput("upper_quantile", 
                  "Upper Percentile (Exclude extreme High-Value Outliers)", 
                  min = 0.5, max = 1, value = 0.95),
      
      h5("Step 4: Select Region (Optional)"),
      p("Filter the dataset by region to focus on specific geographic areas you are interested in"),
      selectInput("region", "Select Regions:", choices = unique(df1$Region),
                  selected = unique(df1$Region))
    ),
    
    mainPanel(
      fluidRow(
        p("This boxplot and histogram show the distribution of the X-axis variable across regions"),
        column(6, plotOutput("boxplot1")),
        column(6, plotOutput("histogram_plot"))
        ),  
      
      fluidRow(
        p("This scatterplot shows the relationship between the X-axis and Y-axis variables. You can brush over the points to highlight specific data for analysis."),
        column(6, plotOutput("scatterall")),
        column(6, plotOutput("scatterplot", brush = brushOpts("plot_brush")))
        
      ),
      fluidRow(
        h4 ('REMEMBER to Brush certain areas on the scatterplot on the right side, so that the specific information related to the selection will appear in the follwing histogram and DataTable'),
        p("This overlay histogram compares the distribution of all data points with the highlighted data points selected from the scatterplot."),
        column(10, plotlyOutput("histogram2"))),
      fluidRow(
        h4("Data Table"),
        p("The table below shows the filtered data based on the region and variable selections. It dynamically updates when you brush points on the scatterplot."),
        column(12, DTOutput("table")))
    )
  )
)

server <- function(input, output) {
  output$histogram_plot <- renderPlot({
    histogram(df1, input$x_var, input$lower_quantile, input$upper_quantile)
  })
  
  output$boxplot1 <- renderPlot({boxplot(df1, input$x_var)})
  output$scatterall <- renderPlot({
    df<- df1 
    x_var <- input$x_var
    y_var<- input$y_var
    filtered <- df %>%
      filter(
        df[[x_var]] >= quantile(df[[x_var]], input$lower_quantile, na.rm = TRUE) &
          df[[x_var]] <= quantile(df[[x_var]], input$upper_quantile, na.rm = TRUE)
      )
    ggplot(filtered, aes_string(x = x_var, y = y_var, color = "Region")) +
      geom_point(alpha = 0.8)
  })
  filtered_df2 <- reactive({
    req(input$region)
    df1 %>%
      filter(Region %in% input$region)
  })
  
  selected <- reactiveVal(rep(TRUE, nrow(df1)))

  observeEvent(input$plot_brush, {
    selected(brushedPoints(filtered_df2(), input$plot_brush, allRows = TRUE)$selected_)
  })
  
  output$scatterplot <- renderPlot({
    scatterplot(filtered_df2(), input$x_var, input$y_var)
  })

  output$histogram2 <- renderPlotly({
    overlay_histogram(filtered_df2(), selected(), input$x_var)
  })

  output$table <- renderDT(filter_df(filtered_df2(), selected()))
}


shinyApp(ui = ui, server = server)

