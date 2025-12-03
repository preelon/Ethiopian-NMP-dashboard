library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(scales)

#--- reading the required data
hmis_jul_oct <- read_csv("data/july_oct_hmis_cleaned-with-pop.csv")

# --- Prepare the data ---
hmis_jul_oct <- hmis_jul_oct |>
  mutate(
    month = month(greg_date, label = TRUE, abbr = TRUE),
    year = year(greg_date),
    month_year = paste(month, year, sep = "-")
  )

regions <- c("Addis Ababa", "Afar", "Amhara","Benishangul Gumuz",
             "Central", "Dire Dawa", "Gambella","Harari", "Oromia",
             "Sidama","Somali", "South","South West","Tigray")

months <- hmis_jul_oct |>
  distinct(month_year, greg_date) |>
  arrange(greg_date) |>
  pull(month_year)

indicators <- sort(unique(as.character(hmis_jul_oct$data_type)))

# --- UI ---
ui <- page_sidebar(
  title = "NMP Dashboard",
  sidebar = sidebar(
    selectInput(
      inputId = "region",
      label = "Select Region",
      multiple = TRUE,
      choices = c("All Regions", regions),  # Add an “All Regions” option
      selected = "All Regions"
    ),
    selectInput(
      inputId = "month_year",
      label = "Select Month-Year",
      multiple = TRUE,
      choices = months,
      selected = months,
    ),
    selectInput(
      inputId = "data_type",
      label = "Select Data type",
      choices = indicators,
      selected = indicators,
      multiple = T
    ),
  ),
  
  # ---- Horizontal Value Boxes at Top ----
  layout_column_wrap(
    width = 1/4,        # 4 boxes in a row
    height = "50px",   # compact height
    gap = "1rem",       # spacing between boxes
    
    value_box(
      title = "Total Tests",
      value = textOutput("vb_tested"),
      #showcase = bsicons::bs_icon("activity"),
      theme = "info",
      style = "font-size: 20px; font-weight: bold;",  # reduce font size
      height = "50px"
    ),
    value_box(
      title = "Total Confirmed Cases",
      value = textOutput("vb_confirmed"),
      #showcase = bsicons::bs_icon("activity"),
      theme = "info",
      height = "50px"
    ),
    value_box(
      title = "Total Cases (Conf + Presumed)",
      value = textOutput("vb_allcases"),
      #showcase = bsicons::bs_icon("activity"),
      theme = "info",
      height = "50px"
    ),
    value_box(
      title = "Malaria-Related Death",
      value = textOutput("vb_death"),
      #showcase = bsicons::bs_icon("activity"),
      theme = "danger",
      height = "50px"
    )
  ),
  
  # Plots below KPIs
  layout_column_wrap(
    width = 1/2, gap = "1rem",
    
    # Plot 1: Test and positivity trend
    card(
      card_header("Tests & Positivity Trend"),
      plotOutput("plot_test_positivity", height = "300px")
    ),
    
    # Plot 2: Species proportion
    card(
      card_header("Species Proportion"),
      plotOutput("plot_species_proportion", height = "300px")
    ),
    
    # Plot 3: Incidence rate per 1000
    card(
      card_header("Incidence rate (per 1000)"),
      plotOutput("plot_incidence", height = "300px")
    ),
    
    # Plot 4: Death rate per 100000
    card(
      card_header("Malaria-related Death"),
      plotOutput("plot_death_rate", height = "300px")
    )
  ))


# --- Server ---
server <- function(input, output, session) {
  
  # ---- Reactive filtered dataset ----
  filtered_data <- reactive({
    hmis_jul_oct |>
      filter(
        # If "All Regions" is selected or nothing is selected, do not filter
        if (!is.null(input$region) && !"All Regions" %in% input$region) region %in% input$region else TRUE,
        if (!is.null(input$month_year) && length(input$month_year) > 0) month_year %in% input$month_year else TRUE,
        if (!is.null(input$data_type) && length(input$data_type) > 0) data_type %in% input$data_type else TRUE
      )
  })
  
  # ---- KPI 1: Total Tests ----
  output$vb_tested <- renderText({
    filtered_data() |>
      filter(data_type == "tested") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total) |>
      format(big.mark = ",")
  })
  
  # ---- KPI 2: Confirmed Cases ----
  output$vb_confirmed <- renderText({
    filtered_data() |>
      filter(data_type == "positives") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total) |>
      format(big.mark = ",")
  })
  
  # ---- KPI 3: All Cases (Confirmed + Presumed) ----
  output$vb_allcases <- renderText({
    filtered_data() |>
      filter(data_type %in% c("positives", "presumed")) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total) |>
      format(big.mark = ",")
  })
  
  # ---- KPI 4: Malaria-Related Death ----
  output$vb_death <- renderText({
    filtered_data() |>
      filter(!data_type %in% c("presumed", "tested", "positives"),
             dept == "IPD", out_come == "Mortality") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total) |>
      format(big.mark = ",")
  })
  
  # plot 1: Test and Positivity trend
  output$plot_test_positivity <- renderPlot({
    
    # Prepare data
    df <- filtered_data() |>
      group_by(month_year) |>
      summarise(
        total_tested = sum(value[data_type == "tested"], na.rm = TRUE),
        total_confirmed = sum(value[data_type == "positives"], na.rm = TRUE)
      ) |>
      mutate(
        positivity = (total_confirmed / total_tested) * 100
      ) |>
      arrange(month_year)
    
    # For secondary axis scaling
    max_tests <- max(df$total_tested, na.rm = TRUE)
    
    ggplot(df, aes(x = month_year)) +
      # --- Bars for total tests ---
      geom_col(aes(y = total_tested), fill = "cyan4", alpha = 0.8) +
      
      # --- Line for positivity rate (%) ---
      geom_line(
        aes(y = positivity * max_tests / 100),
        color = "orange1",
        size = 1.5,
        group = 1
      ) +
      geom_point(
        aes(y = positivity * max_tests / 100),
        color = "black",
        size = 3
      ) +
      
      # --- Y-axis formatting ---
      scale_y_continuous(
        name = "Total Tests",
        labels = scales::comma,   # avoid scientific notation
        sec.axis = sec_axis(
          trans = ~ . / max_tests * 100,
          name = "Positivity (%)"
        )
      ) +
      
      # --- Labels & theme ---
      labs(
        x = "Month-Year",
        #title = "Testing Volume and Positivity Rate Over Time"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        axis.title.y.right = element_text(face = "bold"),
        axis.title.y.left  = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
      )
  })
  
  # Plot 2: species proportion
  output$plot_species_proportion <- renderPlot({
    filtered_data() |>
      filter(data_type %in% c("pf_conf","pv_conf","mixed_conf")) |>
      mutate(data_type = factor(data_type,
                                levels = c("pf_conf", "pv_conf", "mixed_conf"))) |>
      group_by(data_type) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      mutate(prop = total / sum(total),                               # calculate proportion
             label = scales::percent(prop, accuracy = 0.1)) |>        # format label
      ggplot(aes(x = "", y = prop, fill = data_type)) +               # use prop for proper pie
      geom_col() +
      coord_polar(theta = "y") +
      geom_text(aes(label = label), 
                position = position_stack(vjust = 0.5),               # center labels
                size = 4, fontface = "bold") +
      scale_fill_manual(
        values = c(
          "pf_conf" = "cyan4",       # red shade
          "pv_conf" = "orange1",       # blue shade
          "mixed_conf" = "grey"     # green/teal shade
        )
      ) +
      theme_void() +
      labs(fill = "Species")
  })
  
  # Plot 3: Incidence rate
  output$plot_incidence <- renderPlot({
    
    # Start from filtered_data() for incidence components
    df <- filtered_data() |> 
      filter(data_type %in% c("tested", "positives", "presumed")) |>
      pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |> 
      group_by(region, month_year) |>
      summarise(
        positives = sum(positives, na.rm = TRUE),
        presumed  = sum(presumed, na.rm = TRUE),
        tested    = sum(tested, na.rm = TRUE),
        population = dplyr::first(population),
        .groups = "drop"
      ) |> 
      mutate(
        tpr = positives / tested,
        adjusted_positive = positives + (presumed * tpr),
        incidence = (adjusted_positive / population) * 1000
      )
    
    # Aggregate nationally if no regions selected
    if (is.null(input$region) || length(input$region) == 0 || "All Regions" %in% input$region) {
      df <- df |> 
        group_by(month_year) |> 
        summarise(
          adjusted_positive = sum(adjusted_positive, na.rm = TRUE),
          population = sum(population, na.rm = TRUE),
          incidence = (adjusted_positive / population) * 1000,
          region = "National",
          .groups = "drop"
        )
    } else {
      # Filter for selected regions
      df <- df |> filter(region %in% input$region)
    }
    
    # Plot
    ggplot(df, aes(x = reorder(region, incidence), y = incidence)) +
      geom_col(position = "dodge", fill= "cyan4") +
      labs(
        x = "Region / National",
        y = "Incidence per 1000"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal(base_size = 13) +
      coord_flip() +
      theme(axis.title = element_text(face = "bold"))
  })
  
  # Plot 4: Death rate
  output$plot_death_rate <- renderPlot({
    
    # Start from filtered_data() for deaths
    df <- filtered_data() |>
      filter(!data_type %in% c("tested","positives","presumed"),
             dept == "IPD",
             out_come == "Mortality") |>
      group_by(region) |>
      summarise(
        total_deaths = sum(value, na.rm = TRUE),
        population   = dplyr::first(population),
        .groups = "drop"
      ) |>
      mutate(rate_per_100k = total_deaths / population * 100000)
    
    # If no regions selected, calculate national totals
    if (is.null(input$region) || length(input$region) == 0 || "All Regions" %in% input$region) {
      df <- df |> 
        summarise(
          total_deaths = sum(total_deaths, na.rm = TRUE),
          population   = sum(population, na.rm = TRUE),
          rate_per_100k = total_deaths / population * 100000
        ) |>
        mutate(region = "National")
    }
    
    # Plot
    ggplot(df, aes(x = reorder(region, rate_per_100k), y = rate_per_100k, fill = region)) +
      geom_col(show.legend = FALSE, fill= "cyan4") +
      labs(
        x = "Region / National",
        y = "Deaths per 100,000"
      ) +
      theme_minimal(base_size = 13) +
      coord_flip() +
      theme(axis.title = element_text(face = "bold"))
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)
