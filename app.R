# --- Libraries ---
library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(scales)
library(plotly)
library(readr)

# --- Read Data ---
hmis_jul_nov <- read_csv("data/jul-nov-zone-cleaned-pop-integrated-latest.csv")

# --- Prepare Data ---
hmis_jul_nov <- hmis_jul_nov |>
  mutate(
    region = as.character(region),
    zone   = as.character(zone)
  ) |>
  mutate(
    month = month(greg_date, label = TRUE, abbr = TRUE),
    year = year(greg_date),
    month_year = paste(month, year, sep = "-"),
    month_year = factor(
      month_year,
      levels = c("Jul-2025", "Aug-2025", "Sep-2025", "Oct-2025", "Nov-2025")
    )
  ) |>
  mutate(
    age_range = str_trim(age_range),
    age_range = recode(
      age_range,
      "1 - 4"   = "1–4",
      "5 - 14"  = "5–14",
      "15 - 29" = "15–29",
      "30 - 64" = "30–64",
      "<1"      = "<1",
      "65+"     = "65+"
    ),
    age_range = factor(
      age_range,
      levels = c("<1", "1–4", "5–14", "15–29", "30–64", "65+")
    )
  ) |>
  select(-greg_month, -greg_year)

# --- Options ---
regions <- c("Addis Ababa", "Afar", "Amhara","Benishangul Gumuz",
             "Central", "Dire Dawa", "Gambella","Harari", "Oromia",
             "Sidama","Somali", "South","South West","Tigray")

months <- levels(hmis_jul_nov$month_year)  # proper month-year labels
ages <- levels(hmis_jul_nov$age_range)     # <- FIXED: preserves factor order
indicators <- sort(unique(as.character(hmis_jul_nov$data_type)))

# --- Theme ---
app_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone",
  primary = "#0072B2",
  secondary = "#E69F00",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# --- Region-Zone Lookup ---
region_zone_lookup <- hmis_jul_nov |>
  distinct(region, zone) |>
  arrange(region, zone)

# --- UI ---
ui <- page_sidebar(
  title = "NMEP Dashboard",
  theme = app_theme,
  sidebar = sidebar(
    # Logo
    div(
      style = "display: flex; justify-content: center; align-items: center; padding: 10px 0;",
      img(src = "logo.png", style = "height: 200px; width: auto;")
    ),
    # Region
    selectInput(
      inputId = "region",
      label = "Select Region",
      multiple = TRUE,
      choices = c("All Regions", regions),
      selected = "All Regions"
    ),
    # Zone
    selectInput(
      inputId = "zone",
      label = "Select Zone (optional)",
      choices = c("All Zones"),
      multiple = TRUE,
      selected = "All Zones"
    ),
    # Month-Year
    selectInput(
      inputId = "month_year",
      label = "Select Month-Year",
      multiple = TRUE,
      choices = c("All Months", months),
      selected = "All Months"
    ),
    # Age Group
    selectInput(
      inputId = "age_range",
      label = "Select Age Group",
      multiple = TRUE,
      choices = c("All Ages", ages),
      selected = "All Ages"
    )
  ),
  
  # Value Boxes
  layout_column_wrap(
    value_box(title = "Total Tests", value = textOutput("vb_tested"), theme = "blue"),
    value_box(title = "Total Confirmed Cases", value = textOutput("vb_confirmed"), theme = "blue"),
    value_box(title = "Total Cases (Conf + Presumed)", value = textOutput("vb_allcases"), theme = "blue"),
    value_box(title = "Malaria-Related Death", value = textOutput("vb_death"), theme = "red")
  ),
  
  # Plots
  layout_column_wrap(
    width = 1/2, gap = "1rem",
    card(card_header("Tests & Positivity Trend"), plotlyOutput("plot_test_positivity", height = "300px")),
    card(card_header("Species Proportion"), plotlyOutput("plot_species_proportion", height = "300px")),
    card(card_header("Parasitic Incidence rate (per 1000)"), plotlyOutput("plot_incidence", height = "300px")),
    card(card_header("Malaria-related Death"), plotlyOutput("plot_death_rate", height = "300px"))
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Update Zone Choices
  observeEvent(input$region, {
    zones <- if (is.null(input$region) || "All Regions" %in% input$region) {
      region_zone_lookup |> distinct(zone) |> arrange(zone) |> pull(zone)
    } else {
      region_zone_lookup |> filter(region %in% input$region) |> distinct(zone) |> arrange(zone) |> pull(zone)
    }
    updateSelectInput(session, "zone", choices = c("All Zones", zones), selected = "All Zones")
  })
  
  # Main filtered dataset (KPIs, plots)
  filtered_data_main <- reactive({
    df <- hmis_jul_nov
    
    if (!is.null(input$region) && !"All Regions" %in% input$region) df <- df |> filter(region %in% input$region)
    if (!is.null(input$zone) && !"All Zones" %in% input$zone) df <- df |> filter(zone %in% input$zone)
    if (!is.null(input$month_year) && !"All Months" %in% input$month_year) df <- df |> filter(month_year %in% input$month_year)
    if (!is.null(input$age_range) && !"All Ages" %in% input$age_range) df <- df |> filter(age_range %in% input$age_range)
    
    df
  })
  
  # Regional dataset for incidence / death plots
  filtered_data_regional <- reactive({
    df <- hmis_jul_nov
    
    if (!is.null(input$region) && !"All Regions" %in% input$region) df <- df |> filter(region %in% input$region)
    if (!is.null(input$month_year) && !"All Months" %in% input$month_year) df <- df |> filter(month_year %in% input$month_year)
    if (!is.null(input$age_range) && !"All Ages" %in% input$age_range) df <- df |> filter(age_range %in% input$age_range)
    
    df
  })
  
  # KPIs
  output$vb_tested <- renderText({
    filtered_data_main() |> filter(data_type == "tested") |> summarise(total = sum(value, na.rm = TRUE)) |> pull(total) |> format(big.mark = ",")
  })
  
  output$vb_confirmed <- renderText({
    filtered_data_main() |> filter(data_type == "positives") |> summarise(total = sum(value, na.rm = TRUE)) |> pull(total) |> format(big.mark = ",")
  })
  
  output$vb_allcases <- renderText({
    filtered_data_main() |> filter(data_type %in% c("positives", "presumed")) |> summarise(total = sum(value, na.rm = TRUE)) |> pull(total) |> format(big.mark = ",")
  })
  
  output$vb_death <- renderText({
    filtered_data_main() |> filter(!data_type %in% c("presumed", "tested", "positives"),
                                   department == "IPD", outcome == "Mortality") |>
      summarise(total = sum(value, na.rm = TRUE)) |> pull(total) |> format(big.mark = ",")
  })
  
  # Plots
  output$plot_test_positivity <- renderPlotly({
    df <- filtered_data_main() |>
      group_by(month_year) |>
      summarise(
        total_tested = sum(value[data_type == "tested"], na.rm = TRUE),
        total_confirmed = sum(value[data_type == "positives"], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(TPR = total_confirmed / total_tested * 100)
    
    plot_ly(df, x = ~month_year) |>
      add_bars(y = ~total_tested, name = "Total Tests", marker = list(color = "#008B8B"),
               hovertemplate = "Tests: %{y:,}<extra></extra>") |>
      add_trace(
        y = ~TPR,
        name = "TPR (%)",
        yaxis = "y2",
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#FFA500", width = 3),
        marker = list(size = 7, color = "#FFA500"),
        hovertemplate = "TPR: %{y:.1f}%<extra></extra>"
      ) |>
      layout(yaxis = list(title = "Total Tests", tickformat = ","),
             yaxis2 = list(title = "TPR (%)", overlaying = "y", side = "right", showticklabels = FALSE, ticks = ""),
             xaxis = list(title = "Month-Year"),
             showlegend = FALSE, hovermode = "x unified")
  })
  
  output$plot_species_proportion <- renderPlotly({
    df <- filtered_data_main() |> filter(data_type %in% c("pf_conf", "pv_conf", "mixed_conf")) |>
      group_by(data_type) |> summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    
    plot_ly(df, labels = ~data_type, values = ~total, type = "pie", textinfo = "none", hoverinfo = "label+percent",
            marker = list(colors = c("#808080", "#008B8B", "#FFA500"))) |>
      layout(margin = list(l = 0, r = 0, t = 0, b = 0))
  })
  
  output$plot_incidence <- renderPlotly({
    df <- filtered_data_regional() |>
      filter(data_type %in% c("tested", "positives", "presumed")) |>
      pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |>
      group_by(region, month_year) |>
      summarise(positives = sum(positives, na.rm = TRUE),
                presumed = sum(presumed, na.rm = TRUE),
                tested = sum(tested, na.rm = TRUE),
                population = first(population), .groups = "drop") |>
      mutate(tpr = positives / tested,
             adjusted_positive = positives + (presumed * tpr),
             incidence = (adjusted_positive / population) * 1000)
    
    if (is.null(input$region) || "All Regions" %in% input$region) {
      df <- df |> group_by(month_year) |>
        summarise(adjusted_positive = sum(adjusted_positive, na.rm = TRUE),
                  population = sum(population, na.rm = TRUE),
                  incidence = (adjusted_positive / population) * 1000,
                  region = "National", .groups = "drop")
    } else {
      df <- df |> filter(region %in% input$region)
    }
    
    ggplotly(ggplot(df, aes(x = reorder(region, incidence), y = incidence,
                            text = paste0("Incidence: ", round(incidence, 2), " per 1,000"))) +
               geom_col(fill = "cyan4") + coord_flip() + theme_minimal() +
               labs(x = "", y = "Incidence per 1,000"), tooltip = "text")
  })
  
  output$plot_death_rate <- renderPlotly({
    df <- filtered_data_regional() |>
      filter(!data_type %in% c("tested", "positives", "presumed"),
             department == "IPD", outcome == "Mortality") |>
      group_by(region) |>
      summarise(total_deaths = sum(value, na.rm = TRUE),
                population = first(population), .groups = "drop") |>
      mutate(rate_per_100k = total_deaths / population * 100000)
    
    if (is.null(input$region) || "All Regions" %in% input$region) {
      df <- df |> summarise(total_deaths = sum(total_deaths, na.rm = TRUE),
                            population = sum(population, na.rm = TRUE),
                            rate_per_100k = total_deaths / population * 100000) |>
        mutate(region = "National")
    } else {
      df <- df |> filter(region %in% input$region)
    }
    
    ggplotly(ggplot(df, aes(x = reorder(region, rate_per_100k), y = rate_per_100k,
                            text = paste0("Deaths: ", round(rate_per_100k, 2), " per 100,000"))) +
               geom_col(fill = "cyan4") + coord_flip() + theme_minimal() +
               labs(x = "Region / National", y = "Deaths per 100,000"), tooltip = "text")
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)

