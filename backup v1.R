# Libraries
library(tidyverse)
library(lubridate)
library(shiny)
library(bslib)
library(scales)
library(plotly)
library(readr)
library(sf)

# Read Data
hmis_jul_dec <- read_csv("data/jul-dec-zone-cleaned-pop-integrated-latest.csv")

# read SF for map
sf_woreda <- st_read("data/sf_cleaned/shape_file_for_mapping_cleaned.shp")

# Map-ready sf 
sf_woreda <- sf_woreda |>
  st_make_valid() |>        # fixing invalid geometries
  st_transform(4326) |>   
  st_zm(drop = TRUE)        


sf_zone <- sf_woreda |>
  group_by(region, zone) |>
  summarise(.groups = "drop") |>
  st_cast("MULTIPOLYGON") #i used this code to force all geometries to MULTIPOLYGON because plotly can't use mixed geometry type which resulted when i used st_union before

ethiopia_bbox <- st_bbox(sf_woreda)

sf_region <- sf_woreda |>
  group_by(region) |>
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  st_make_valid()

# Preparing Data
hmis_jul_dec <- hmis_jul_dec |>
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
  select(-greg_month, -greg_year)
# Region Options 
regions <- c("Addis Ababa", "Afar", "Amhara","Benishangul Gumuz",
             "Central", "Dire Dawa", "Gambella","Harari", "Oromia",
             "Sidama","Somali", "South","South West","Tigray")

months <- levels(hmis_jul_dec$month_year)  # proper month-year labels
indicators <- sort(unique(as.character(hmis_jul_dec$data_type)))

zone_population <- hmis_jul_dec |>
  distinct(region, zone, population)

# Theme
app_theme <- bs_theme(
  version = 5,
  bootswatch = "sandstone",
  primary = "#0072B2",
  secondary = "#E69F00",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# Region-Zone Lookup 
region_zone_lookup <- hmis_jul_dec |>
  distinct(region, zone) |>
  arrange(region, zone)


ui <- page_sidebar(
  title = "NMEP Dashboard",
  theme = app_theme,
  
  tags$head(
    tags$style(HTML("
    /* Value box numbers: shrink aggressively, never clip */
    .bslib-value-box .value,
    .bslib-value-box .value-box-value {
      font-size: clamp(0.85rem, 2.5vw, 2.2rem) !important;
      white-space: nowrap;
      overflow: visible;      /* ← CRITICAL */
      line-height: 1.15;
      text-align: center;
    }

    /* Center content nicely */
    .bslib-value-box .value-box-body {
      display: flex;
      flex-direction: column;
      justify-content: center;
    }

    /* Titles */
    .bslib-value-box .title,
    .bslib-value-box .value-box-title {
      font-weight: 400;
      text-align: center;
    }
  "))
  ),
  
  
  
  # =====================
  # SIDEBAR (GLOBAL)
  # =====================
  sidebar = sidebar(
    # Logo
    div(
      style = "display: flex; justify-content: center; align-items: center; padding: 10px 0;",
      img(src = "logo.png", style = "height: 250px; width: auto;")
    ),
    
    # Region
    selectizeInput(
      inputId = "region",
      label = "Select Region",
      choices = c("All Regions", unique(hmis_jul_dec$region)),
      selected = "All Regions",
      multiple = TRUE
    ),
    
    # Zone
    selectizeInput(
      inputId = "zone",
      label = "Select Zone (optional)",
      choices = "All Zones",
      selected = "All Zones",
      multiple = TRUE
    ),
    
    # Month-Year
    selectizeInput(
      inputId = "month_year",
      label = "Select Month-Year",
      choices = c("All Months", months),
      selected = "All Months",
      multiple = TRUE
    ),
  ),
  
  # =====================
  # MAIN BODY (TABS)
  # =====================
  navset_tab(
    # -------------------------------------------------
    # TAB 1: OVERVIEW
    # -------------------------------------------------
    nav_panel(
      "Overview",
      
      card(
        card_header("About This Dashboard"),
        card_body(
          p("This dashboard presents routine malaria surveillance data reported through the national Health Management Information System (HMIS)."),
          p("It is designed to support the National Malaria Elimination Program (NMEP) in monitoring malaria burden, trends, and geographic distribution.")
        )
      ),
      
      card(
        card_header("Data Sources"),
        card_body(
          tags$ul(
            tags$li("DHIS2/HMIS disease (for species segregations & clinically diagnosed malaria cases) and service data (for malaria tests and positive results):Last updated: 21 January 2026, 13:00 EAT (UTC+3)"),
            tags$li("Zone-level population projections (projected from 2022 population from CSA)"),
            tags$li("Reporting period selectable by month")
          )
        )
      ),
      
      card(
        card_header("Key Indicators"),
        card_body(
          tags$ul(
            tags$li(strong("Test Positivity Rate (TPR): "),
                    "Confirmed malaria cases divided by the number tested."),
            tags$li(strong("Incidence Rate: "),
                    "Estimated malaria cases per 1,000 population per month (annualized if monthly view)."),
            tags$li(strong("Death Rate: "),
                    "Malaria-related deaths per 100,000 population (annualized if monthly view).")
          )
        )
      ),
      
      card(
        card_header("Methodological Notes"),
        card_body(
          p("Deaths: Malaria deaths were derived exclusively from inpatient department (IPD)."),
          p("Case Adjustment: To account for periods and locations with incomplete malaria testing, reported clinical cases were adjusted by multiplying clinical case counts by the test positivity rate (TPR)."),
          p("Risk Population: For incidence and death rate calculations, the population at risk was assumed to be 69% of the total population, in line with the Malaria National Strategic Plan (NSP) 2024/25–2026/27.")
        )
      ),
      
      card(
        card_header("How to Use"),
        card_body(
          p("Use the filters on the left to select geography & time period."),
          p("All charts and indicators automatically update based on the selected inputs."),
          p("Note: Age and sex disaggregation was planned but not implemented due to data reliability issues encountered when these dimensions were applied during HMIS data extraction.")
        )
      )
    ),
    
    # -------------------------------------------------
    # TAB 2: DASHBOARD
    # -------------------------------------------------
    nav_panel(
      "Dashboard",
      theme = app_theme,
      
      # ROW 1: MAP + KPIs
      layout_columns(
        col_widths = c(4, 2, 2, 2, 2),
        
        card(
          card_header("Selected Geography"),
          plotlyOutput("region_map", height = "150px"),
          style = "min-height: 150px;"
        ),
        
        value_box(title = "Total Tests", value = textOutput("vb_tested"), theme = "blue"),
        value_box(title = "Confirmed Cases", value = textOutput("vb_confirmed"), theme = "blue"),
        value_box(title = "All Cases", value = textOutput("vb_allcases"), theme = "blue"),
        value_box(title = "Malaria Deaths", value = textOutput("vb_death"), theme = "red")
      ),
      
      
      # ROW 2: PLOTS
      layout_column_wrap(
        width = 1/2,
        gap = "1rem",
        
        # Tests & Positivity
        card(
          card_header("Tests & Positivity Trend"),
          radioButtons(
            inputId = "tpr_toggle",
            label = NULL,
            choices = c("Aggregated", "Separate"),
            selected = "Aggregated",
            inline = TRUE
          ),
          plotlyOutput("plot_test_positivity", height = "350px"),
          #card_footer("TPR represents the aggregated test positivity rate across selected geographies unless separated view is selected.")
        ),
        
        # Species proportion
        card(
          card_header("Species Proportion"),
          radioButtons(
            inputId = "species_display_type",
            label = NULL,
            choices = c("Aggregate" = "aggregate", "By Month" = "by_month"),
            selected = "aggregate",
            inline = TRUE
          ),
          plotlyOutput("plot_species_proportion", height = "300px")
        ),
        
        # Incidence Rate
        card(
          card_header(
            "Parasitic Incidence Rate (per 1,000)",
            # Radio buttons inside the header
            radioButtons(
              inputId = "incidence_view",
              label = NULL,
              choices = c("Aggregated", "Monthly"),
              inline = TRUE
            )
          ),
          plotlyOutput("plot_incidence", height = "350px")
        ),
        
        # Death rate plot card
        card(
          card_header(
            "Malaria-related Death Rate",
            # Radio buttons inside the header
            radioButtons(
              inputId = "death_view",
              label = NULL,
              choices = c("Aggregated", "Monthly"),
              inline = TRUE
            )
          ),
          plotlyOutput("plot_death_rate", height = "350px")
        )
        
      )
    )
  )
)




# Server
server <- function(input, output, session) {
  
  # Helper: format KPI value or show No Data
  kpi_value <- function(x, suffix = "") {
    if (length(x) == 0 || is.na(x) || x == 0) {
      return("No data")
    }
    paste0(format(x, big.mark = ","), suffix)
  }
  
  no_data_plot <- function(msg = "No data available for selected inputs") {
    plot_ly() |>
      layout(
        annotations = list(
          x = 0.5,
          y = 0.5,
          text = msg,
          showarrow = FALSE,
          font = list(size = 14)
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  }
  # -----------------------------
  # 1 Make region selection mutually exclusive
  observeEvent(input$region, ignoreInit = TRUE, {
    sel <- input$region
    if (is.null(sel) || length(sel) == 0) return()
    
    if ("All Regions" %in% sel && length(sel) > 1) {
      updateSelectizeInput(session, "region", selected = "All Regions")
    }
    if (!"All Regions" %in% sel && length(sel) == 0) {
      updateSelectizeInput(session, "region", selected = "All Regions")
    }
  })
  
  # -----------------------------
  # 2️ Update zones dynamically based on selected region
  observeEvent(input$region, {
    zones <- if (is.null(input$region) || "All Regions" %in% input$region) {
      region_zone_lookup |> distinct(zone) |> arrange(zone) |> pull(zone)
    } else {
      region_zone_lookup |>
        filter(region %in% input$region) |>
        distinct(zone) |>
        arrange(zone) |>
        pull(zone)
    }
    
    updateSelectizeInput(
      session,
      "zone",
      choices = c("All Zones", zones),
      selected = "All Zones"
    )
  })
  
  # -----------------------------
  # 3️ Make zone selection mutually exclusive
  observeEvent(input$zone, ignoreInit = TRUE, {
    sel <- input$zone
    if (is.null(sel) || length(sel) == 0) return()
    if ("All Zones" %in% sel && length(sel) > 1) {
      updateSelectizeInput(session, "zone", selected = "All Zones")
    }
    if (!"All Zones" %in% sel && length(sel) == 0) {
      updateSelectizeInput(session, "zone", selected = "All Zones")
    }
  })
  
  # -----------------------------
  # 4 Month-Year mutual exclusivity
  observeEvent(input$month_year, ignoreInit = TRUE, {
    sel <- input$month_year
    if (is.null(sel) || length(sel) == 0) return()
    if ("All Months" %in% sel && length(sel) > 1) {
      updateSelectizeInput(session, "month_year", selected = "All Months")
    }
    if (!"All Months" %in% sel && length(sel) == 0) {
      updateSelectizeInput(session, "month_year", selected = "All Months")
    }
  })
  
  # -----------------------------
  # Reactive dataset
  filtered_data_main <- reactive({
    req(input$region, input$zone, input$month_year)
    df <- hmis_jul_dec
    
    if (!"All Regions" %in% input$region)
      df <- df |> filter(region %in% input$region)
    
    if (!"All Zones" %in% input$zone)
      df <- df |> filter(zone %in% input$zone)
    
    if (!"All Months" %in% input$month_year)
      df <- df |> filter(month_year %in% input$month_year)
    
    df
  })
  
  # -----------------------------
  # Map logic with corrected casing
  output$region_map <- renderPlotly({
    req(input$region, input$zone)
    
    selected_regions <- input$region
    selected_zones   <- input$zone
    
    map_data <- sf_zone |>
      mutate(
        sel = case_when(
          # Selected zones explicitly
          !("All Zones" %in% selected_zones) & zone %in% selected_zones ~ "Selected",
          
          # Selected regions, all zones
          !("All Regions" %in% selected_regions) & ("All Zones" %in% selected_zones) & region %in% selected_regions ~ "Selected",
          
          # National level
          ("All Regions" %in% selected_regions) & ("All Zones" %in% selected_zones) ~ "All",
          
          TRUE ~ "Other"
        )
      )
    
    p <- ggplot(map_data) +
      geom_sf(
        aes(fill = sel, text = paste("Zone:", zone, "<br>Region:", region)),
        color = "#FFFFFF",
        linewidth = 0.25
      ) +
      scale_fill_manual(
        values = c(
          "Selected" = "#0072B2",
          "All"      = "#0072B2",
          "Other"    = scales::alpha("#D9D9D9", 0.7)
        )
      ) +
      theme_void() +
      theme(plot.margin = margin(2, 2, 2, 2))
    
    ggplotly(p, tooltip = "text") |>
      layout(
        showlegend = FALSE,
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )
  })
  
  # KPIs
  output$vb_tested <- renderText({
    
    total_tested <- filtered_data_main() |>
      filter(data_type == "tested") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_tested)
  })
  
  output$vb_confirmed <- renderText({
    
    total_confirmed <- filtered_data_main() |>
      filter(data_type == "positives") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_confirmed)
  })
  
  
  
  output$vb_allcases <- renderText({
    total_cases <- filtered_data_main() |>
      filter(data_type %in% c("positives", "presumed")) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_cases)
  })
  
  
  output$vb_death <- renderText({
    total_deaths <- filtered_data_main() |>
      filter(
        !data_type %in% c("presumed", "tested", "positives"),
        department == "IPD",
        outcome == "Mortality"
      ) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_deaths)
  })
  
  
  # Tests & Positivity Trend 
  output$plot_test_positivity <- renderPlotly({
    
    df_all <- filtered_data_main()
    
    total_tested <- df_all |>
      filter(data_type == "tested") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    # ---- No data check ----
    if (nrow(df_all) == 0 || is.na(total_tested) || total_tested == 0) {
      return(no_data_plot())
    }
    
    base_df <- df_all |> filter(data_type %in% c("tested", "positives"))
    
    # Decide grouping for bars
    group_var <- if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
      "zone"
    } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
      "region"
    } else {
      "national"
    }
    
    # Aggregate for bars
    if (group_var %in% c("zone", "region")) {
      df_bars <- base_df |>
        group_by(across(all_of(c(group_var, "month_year", "data_type")))) |>
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |>
        rename(group = all_of(group_var))
    } else {
      df_bars <- base_df |>
        group_by(month_year, data_type) |>
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |>
        mutate(group = "National")
    }
    
    # Reorder groups by total tested
    group_order <- df_bars |>
      group_by(group) |>
      summarise(total_tests = sum(tested, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(total_tests)) |>
      pull(group)
    
    df_bars <- df_bars |>
      mutate(group = factor(group, levels = group_order))
    
    # ---- TPR lines ----
    if (input$tpr_toggle == "Aggregated") {
      tpr_df <- df_bars |>
        group_by(month_year) |>
        summarise(
          tested = sum(tested, na.rm = TRUE),
          positives = sum(positives, na.rm = TRUE),
          TPR = positives / tested * 100,
          .groups = "drop"
        )
      
      tpr_line <- list(data = tpr_df, y = ~TPR, color = I("#000000"))
      
    } else {
      # Separate TPR lines per group (region/zone)
      tpr_df <- df_bars |>
        mutate(TPR = ifelse(tested > 0, positives / tested * 100, 0))
      
      tpr_line <- list(data = tpr_df, y = ~TPR, color = ~group)
    }
    
    # ---- Plot ----
    p <- plot_ly()
    
    # Add test bars
    p <- p |>
      add_bars(
        data = df_bars,
        x = ~month_year,
        y = ~tested,
        color = ~group,
        customdata = ~group,
        hovertemplate = "%{customdata}<br>Tests: %{y:,}<extra></extra>"
      )
    
    # Add TPR line(s)
    # TPR trace
    if (input$tpr_toggle == "Aggregated") {
      # Aggregated line (national/selection-level)
      tpr_df <- df_bars |>
        group_by(month_year) |>
        summarise(
          tested = sum(tested, na.rm = TRUE),
          positives = sum(positives, na.rm = TRUE),
          TPR = positives / tested * 100,
          .groups = "drop"
        )
      
      p <- p |>
        add_trace(
          data = tpr_df,
          x = ~month_year,
          y = ~TPR,
          type = "scatter",
          mode = "lines+markers",
          yaxis = "y2",
          line = list(color = "#000000", width = 3),
          hovertemplate = "TPR: %{y:.1f}%<extra></extra>"
        )
      
    } else {
      # Separate lines per group (region/zone)
      tpr_df <- df_bars |>
        group_by(month_year, group) |>
        summarise(
          tested = sum(tested, na.rm = TRUE),
          positives = sum(positives, na.rm = TRUE),
          TPR = positives / tested * 100,
          .groups = "drop"
        )
      
      p <- p |>
        add_trace(
          data = tpr_df,
          x = ~month_year,
          y = ~TPR,
          color = ~group,                 # line keeps group color
          type = "scatter",
          mode = "lines+markers",
          yaxis = "y2",
          line = list(width = 3),
          marker = list(
            color = "black",
            size = 7
          ),
          hovertemplate = paste(
            "%{customdata}<br>",
            "TPR: %{y:.1f}%<extra></extra>"
          ),
          customdata = ~group
        )
      
    }
    
    
    # Layout
    p |>
      layout(
        barmode = "group",
        showlegend = FALSE,
        yaxis = list(title = "Number of Tests"),
        yaxis2 = list(
          title = "TPR (%)",
          overlaying = "y",
          side = "right",
          showgrid = FALSE,
          showticklabels = FALSE
        ),
        xaxis = list(title = "")
      )
  })
  
  
  output$plot_species_proportion <- renderPlotly({
    
    df_raw <- filtered_data_main() |>
      filter(data_type %in% c("pf_conf", "pv_conf", "mixed_conf"))
    
    # ---- NO DATA LOGIC ----
    total_cases <- sum(df_raw$value, na.rm = TRUE)
    if (is.na(total_cases) || total_cases == 0) return(no_data_plot())
    
    # Map codes to full names
    df_raw <- df_raw |>
      mutate(
        species_name = recode(
          data_type,
          "pf_conf"    = "P. falciparum",
          "pv_conf"    = "P. vivax",
          "mixed_conf" = "Mixed (Pf/Pv)"
        )
      )
    
    # ---- AGGREGATION BASED ON TOGGLE ----
    if (input$species_display_type == "aggregate") {
      # Aggregate across months
      df <- df_raw |>
        group_by(species_name) |>
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      
      # Pie chart
      plot_ly(
        df,
        labels = ~species_name,
        values = ~total,
        type = "pie",
        textinfo = "none",
        hoverinfo = "label+percent",
        marker = list(colors = c("#808080", "#008B8B", "#FFA500"))
      ) |>
        layout(margin = list(l = 0, r = 0, t = 0, b = 0))
      
    } else {
      # By month — species composition per month
      df <- df_raw |>
        group_by(month_year, species_name) |>
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
        ungroup()
      
      # Make sure all species types are represented for each month
      species_levels <- c("P. falciparum", "P. vivax", "Mixed (Pf/Pv)")
      df <- df |>
        tidyr::complete(
          month_year = unique(df$month_year),
          species_name = species_levels,
          fill = list(total = 0)
        )
      
      # Calculate proportion per month for hover
      df <- df |>
        group_by(month_year) |>
        mutate(
          prop = total / sum(total, na.rm = TRUE) * 100,
          hover_text = paste0("Species: ", species_name, "<br>",
                              "Cases: ", total, "<br>",
                              "Proportion: ", round(prop, 1), "%")
        ) |>
        ungroup()
      
      # Stacked bar chart
      plot_ly(
        df,
        x = ~month_year,
        y = ~total,
        color = ~species_name,
        type = "bar",
        hovertext = ~hover_text,   
        hoverinfo = "text",        
        marker = list(colors = c("#808080", "#008B8B", "#FFA500"))
      ) |>
        layout(
          barmode = "stack",
          xaxis = list(title = ""),
          yaxis = list(title = "cases count"),
          legend = list(title = list(text = "Species"))
        )
    }
    
  })
  
  
  # 1 Incidence Plot with Toggle
  output$plot_incidence <- renderPlotly({
    
    # -------------------------
    # 1. Prepare numerator
    # -------------------------
    cases_df <- filtered_data_main() %>%
      filter(data_type %in% c("tested", "positives", "presumed")) %>%
      pivot_wider(
        names_from = data_type,
        values_from = value,
        values_fill = 0
      ) %>%
      group_by(region, zone, month_year) %>%
      summarise(
        positives = sum(positives, na.rm = TRUE),
        presumed  = sum(presumed, na.rm = TRUE),
        tested    = sum(tested, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        tpr = ifelse(tested > 0, positives / tested, 0),
        adjusted_positive = positives + presumed * tpr
      )
    
    # -------------------------
    # 2. Population (denominator)
    # -------------------------
    pop_df <- zone_population %>%
      mutate(pop_at_risk = population*0.69)
    
    df <- cases_df %>%
      left_join(pop_df, by = c("region", "zone")) %>%
      mutate(
        monthly_incidence = (adjusted_positive / pop_at_risk) * 1000,
        annualized_incidence = monthly_incidence * 12
      )
    
    # -------------------------
    # 3. Toggle logic
    # -------------------------
    if(input$incidence_view == "Aggregated") {
      
      # ---------------------
      # Aggregated per geography
      # ---------------------
      if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
        plot_df <- df %>%
          filter(zone %in% input$zone) %>%
          group_by(zone) %>%
          summarise(
            annualized_incidence = mean(annualized_incidence, na.rm = TRUE),
            .groups = "drop"
          )
        x_var <- "zone"
        
      } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
        plot_df <- df %>%
          group_by(region) %>%
          summarise(
            annualized_incidence = sum(adjusted_positive, na.rm = TRUE) / 
              sum(pop_at_risk, na.rm = TRUE) * 1000,
            .groups = "drop"
          )
        x_var <- "region"
        
      } else {
        plot_df <- df %>%
          summarise(
            annualized_incidence = sum(adjusted_positive, na.rm = TRUE) / 
              sum(pop_at_risk, na.rm = TRUE) * 1000
          ) %>%
          mutate(region = "National")
        x_var <- "region"
      }
      
      # ---------------------
      # Column plot
      # ---------------------
      p <- ggplot(plot_df, aes_string(
        x = paste0("reorder(", x_var, ", annualized_incidence)"),
        y = "annualized_incidence",
        text = "paste0(round(annualized_incidence, 2), ' per 1,000')"
      )) +
        geom_col(fill = "cyan4") +
        coord_flip() +
        labs(x = "", y = "Annualized incidence per 1,000") +
        theme_minimal()
      
    } else {
      
      # ---------------------
      # Monthly time series
      # ---------------------
      # Determine which geography variable to use
      if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
        plot_df <- df %>% filter(zone %in% input$zone)
        geo_var <- "zone"
      } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
        plot_df <- df %>% filter(region %in% input$region)
        geo_var <- "region"
      } else {
        plot_df <- df %>% mutate(region = "National")
        geo_var <- "region"
      }
      
      # Group by month + geography for line plot
      plot_df <- plot_df %>%
        group_by(month_year, !!sym(geo_var)) %>%
        summarise(
          annualized_incidence = sum(adjusted_positive, na.rm = TRUE) / 
            sum(pop_at_risk, na.rm = TRUE) * 1000,
          .groups = "drop"
        )
      
      # ---------------------
      # Plot lines
      # ---------------------
      p <- ggplot(plot_df, aes_string(
        x = "month_year",
        y = "annualized_incidence",
        color = geo_var,
        group = geo_var,
        text = "paste0(round(annualized_incidence, 2), ' per 1,000')"
      )) +
        geom_line(size = 0.8) +
        geom_point(size = 1) +
        labs(x = "Month-Year", y = "Annualized incidence per 1,000") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  # ----------------------
  # Death plot
  output$plot_death_rate <- renderPlotly({
    
    # -------------------------
    # 1. Prepare numerator (deaths)
    # -------------------------
    deaths_df <- filtered_data_main() %>%
      filter(
        department == "IPD",
        outcome == "Mortality",
        !data_type %in% c("tested", "positives", "presumed")
      ) %>%
      group_by(region, zone, month_year) %>%
      summarise(
        deaths = sum(value, na.rm = TRUE),
        .groups = "drop"
      )
    
    # -------------------------
    # 2. Population (denominator)
    # -------------------------
    pop_df <- zone_population %>%
      mutate(pop_at_risk = population*0.69)
    
    df <- deaths_df %>%
      left_join(pop_df, by = c("region", "zone")) %>%
      mutate(
        monthly_rate_per_100k = (deaths / pop_at_risk) * 100000,
        annualized_rate_per_100k = monthly_rate_per_100k * 12
      )
    
    # -------------------------
    # 3. Toggle logic
    # -------------------------
    if(input$death_view == "Aggregated") {
      
      # Aggregated by selected geography
      if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
        plot_df <- df %>%
          filter(zone %in% input$zone) %>%
          group_by(zone) %>%
          summarise(
            annualized_rate_per_100k = mean(annualized_rate_per_100k, na.rm = TRUE),
            .groups = "drop"
          )
        x_var <- "zone"
        
      } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
        plot_df <- df %>%
          group_by(region) %>%
          summarise(
            annualized_rate_per_100k = sum(deaths, na.rm = TRUE) / 
              sum(pop_at_risk, na.rm = TRUE) * 100000,
            .groups = "drop"
          )
        x_var <- "region"
        
      } else {
        plot_df <- df %>%
          summarise(
            annualized_rate_per_100k = sum(deaths, na.rm = TRUE) / 
              sum(pop_at_risk, na.rm = TRUE) * 100000
          ) %>%
          mutate(region = "National")
        x_var <- "region"
      }
      
      # Column chart
      p <- ggplot(plot_df, aes_string(
        x = paste0("reorder(", x_var, ", annualized_rate_per_100k)"),
        y = "annualized_rate_per_100k",
        text = "paste0(round(annualized_rate_per_100k, 2), ' per 100,000')"
      )) +
        geom_col(fill = "cyan4") +
        coord_flip() +
        labs(x = "", y = "Annualized mortality per 100,000") +
        theme_minimal()
      
    } else {
      
      # -------------------------
      # Monthly time series
      # -------------------------
      if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
        plot_df <- df %>% filter(zone %in% input$zone)
        geo_var <- "zone"
      } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
        plot_df <- df %>% filter(region %in% input$region)
        geo_var <- "region"
      } else {
        plot_df <- df %>% mutate(region = "National")
        geo_var <- "region"
      }
      
      # Group by month + geography
      plot_df <- plot_df %>%
        group_by(month_year, !!sym(geo_var)) %>%
        summarise(
          annualized_rate_per_100k = sum(deaths, na.rm = TRUE) / 
            sum(pop_at_risk, na.rm = TRUE) * 100000,
          .groups = "drop"
        )
      
      # Line plot
      p <- ggplot(plot_df, aes_string(
        x = "month_year",
        y = "annualized_rate_per_100k",
        color = geo_var,
        group = geo_var,
        text = "paste0(round(annualized_rate_per_100k, 2), ' per 100,000')"
      )) +
        geom_line(size = 0.8) +
        geom_point(size = 1) +
        labs(x = "Month-Year", y = "Annualized mortality per 100,000") +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  
  
}

# --- Run App ---
shinyApp(ui = ui, server = server)

