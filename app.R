library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(scales)
library(plotly)

#--- reading the required data
hmis_jul_oct <- read_csv("data/july_oct_hmis_cleaned-with-pop.csv")

# --- Prepare the data ---
hmis_jul_oct <- hmis_jul_oct |>
  mutate(
    month = month(greg_date, label = TRUE, abbr = TRUE),
    year = year(greg_date),
    month_year = paste(month, year, sep = "-"),
    month_year = factor(month_year, levels = c("Jul-2025", "Aug-2025",
                                               "Sep-2025", "Oct-2025")))

regions <- c("Addis Ababa", "Afar", "Amhara","Benishangul Gumuz",
             "Central", "Dire Dawa", "Gambella","Harari", "Oromia",
             "Sidama","Somali", "South","South West","Tigray")

months <- hmis_jul_oct |>
  distinct(month_year, greg_date) |>
  arrange(greg_date) |>
  pull(month_year)

indicators <- sort(unique(as.character(hmis_jul_oct$data_type)))

# adding bootstrap theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone",   # try: "journal", "cosmo", "minty", "flately
  primary = "#0072B2",     # NMP-friendly blue
  secondary = "#E69F00",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)


# --- UI ---
ui <- page_sidebar(
  title = "NMEP Dashboard",
  theme = app_theme,
  sidebar = sidebar(
    # Logo at the top
    div(
      style = "display: flex; justify-content: center; align-items: center; padding: 10px 0;",
      img(
        src = "logo.png",
        style = "height: 200px; width: auto;"  # preserves aspect ratio
      )
    ),
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
    value_box(
      title = "Total Tests", # Title
      value = textOutput("vb_tested"),         # Value to show
      #showcase = bsicons::bs_icon("laptop"), #include an icon
      theme = "blue" # change the box theme colour
    ), 
    
    value_box(
      title = "Total Confirmed Cases", 
      value = textOutput("vb_confirmed"), 
      #showcase = bsicons::bs_icon("calendar"), 
      #showcase_layout = "top right", 
      theme = "blue" 
    ), 
    
    # 3rd value box
    value_box(title = "Total Cases (Conf + Presumed)", 
              value = textOutput("vb_allcases"),
              theme = "blue"),
    
    #4th value box
    value_box(title = "Malaria-Related Death", 
              value = textOutput("vb_death"),
              theme = "red"),
  ),
  # Plots below KPIs
  layout_column_wrap(
    width = 1/2, gap = "1rem",
    
    # Plot 1: Test and positivity trend
    card(
      card_header("Tests & Positivity Trend"),
      plotlyOutput("plot_test_positivity", height = "300px")
    ),
    
    # Plot 2: Species proportion
    card(
      card_header("Species Proportion"),
      plotlyOutput("plot_species_proportion", height = "300px")
    ),
    
    # Plot 3: Incidence rate per 1000
    card(
      card_header("Parasitic Incidence rate (per 1000)"),
      plotlyOutput("plot_incidence", height = "300px")
    ),
    
    # Plot 4: Death rate per 100000
    card(
      card_header("Malaria-related Death"),
      plotlyOutput("plot_death_rate", height = "300px")
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
  output$plot_test_positivity <- renderPlotly({
    
    df <- filtered_data() |>
      group_by(month_year) |>
      summarise(
        total_tested = sum(value[data_type == "tested"], na.rm = TRUE),
        total_confirmed = sum(value[data_type == "positives"], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(TPR = total_confirmed / total_tested * 100)
    
    plot_ly(df, x = ~month_year) |>
      add_bars(
        y = ~total_tested,
        name = "Total Tests",
        marker = list(color = "#008B8B"),
        hovertemplate = "Tests: %{y:,}<extra></extra>"
      ) |>
      add_lines(
        y = ~TPR,
        name = "TPR (%)",
        yaxis = "y2",
        mode = "lines+markers", #adding points to the line graph
        line = list(color = "#FFA500", width = 3),
        marker = list(size = 7, color = "#FFA500"), 
        hovertemplate = "TPR: %{y:.1f}%<extra></extra>"
      ) |>
      layout(
        yaxis = list(
          title = "Total Tests",
          tickformat = ","
        ),
        yaxis2 = list(
          title = "TPR (%)",
          overlaying = "y",
          side = "right",
          showticklabels = FALSE,   # 👈 hide numbers
          ticks = "" 
        ),
        xaxis = list(title = "Month-Year"),
        #legend = list(orientation = "h"),
        showlegend = FALSE,
        hovermode = "x unified"
      )
  })
  
  # Plot 2: species proportion with hover percentages
  output$plot_species_proportion <- renderPlotly({
    
    df <- filtered_data() |>
      filter(data_type %in% c("pf_conf","pv_conf","mixed_conf")) |>
      group_by(data_type) |>
      summarise(total = sum(value, na.rm = TRUE))
    
    plot_ly(df, labels = ~data_type, values = ~total, type = 'pie',
            textinfo = 'none', hoverinfo = 'label+percent',
            marker = list(colors = c("#808080", "#008B8B", "#FFA500"))) %>%
      layout(
        margin = list(l = 0, r = 0, t = 0, b = 0)  # remove extra space
      )
  })
  # Plot 3: Incidence rate
  output$plot_incidence <- renderPlotly({
    
    # --- Prepare data ---
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
    
    # --- Aggregate nationally if "All Regions" is selected or nothing selected ---
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
    
    # --- Create ggplot object ---
    parasitic_inc <- ggplot(df, aes(
      x = reorder(region, incidence), 
      y = incidence,
      text = paste0("Parasitic Incidence: ", round(incidence, 2), " per 1,000")  # hover text
    )) +
      geom_col(fill = "cyan4", position = "identity") +
      labs(
        x = "",
        y = "Incidence per 1000"
      ) +
      scale_y_continuous(labels = scales::comma_format()) +
      theme_minimal(base_size = 11) +
      coord_flip() +
      theme(
        axis.title = element_text(face = "plain")  # not bold
      )
    
    # --- Convert to plotly ---
    ggplotly(parasitic_inc, tooltip = "text")
  })
  
  
  # Plot 4: Death rate
  output$plot_death_rate <- renderPlotly({
    
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
    } else {
      # Filter for selected regions
      df <- df |> filter(region %in% input$region)
    }
    
    # Plot
    death_plot <- ggplot(df, aes(
      x = reorder(region, rate_per_100k),
      y = rate_per_100k,
      text = paste0("Death: ", round(rate_per_100k, 2), " per 100,000")
    )) +
      geom_col(show.legend = FALSE, fill = "cyan4") +
      labs(
        x = "Region / National",
        y = "Deaths per 100,000"
      ) +
      theme_minimal(base_size = 11) +
      coord_flip() +
      theme(
        axis.title = element_text(face = "plain")  # remove bold
      )
    
    ggplotly(death_plot, tooltip = "text")
  })
}

# --- Run the App ---
shinyApp(ui = ui, server = server)



