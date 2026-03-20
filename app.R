#interim back up with aggregate view working
class(hmis$greg_year_num)
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
hmis <- read_csv("data/jan-2020-Feb-2026-zone-cleaned-pop-integrated.csv")

# read SF for map & clean th zone names to reconcile it with keb level sf
sf_woreda <- st_read("data/sf_cleaned/shape_file_for_mapping_cleaned.shp")|>
  mutate(zone= case_when(zone == "Finfine Special" ~ "Shager City",
                         zone == "Itang Special Woreda" ~ "Itang Special",
                         zone == "Mirab Omo" ~ "West Omo",
                         zone %in% c("Dire Dawa Urban", "Dire Dawa Rural") ~ "Dire Dawa",
                         TRUE ~ zone))
unique(sf_woreda$zone)

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
# Create a sequence of months from Jan 2020 to Feb 2026
month_seq <- seq(
  from = as.Date("2020-01-01"), 
  to   = as.Date("2026-02-01"), 
  by   = "month"
)


# Format the sequence as "Mon-YYYY"
month_yr_levels <- format(month_seq, "%b-%Y")

# standardizing
hmis <- hmis |>
  mutate(
    region = as.character(region),
    zone   = as.character(zone)
  ) |>
  mutate(
    #month = month(greg_date, label = TRUE, abbr = TRUE),
    #year = year(greg_date),
    #month_year = paste(greg_month, gre_year, sep = "-"),
    month_year = factor(month_year, levels = month_yr_levels)
  ) |>
  mutate(
    greg_month = factor(greg_month,
                        levels = c("Jan", "Feb", "Mar", "Apr",
                                   "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", 
                                   "Dec")),
    # Factor for UI (ordered dropdown)
    greg_year_ui = factor(greg_year,
                          levels = c("2020","2021", "2022", "2023", 
                                     "2024", "2025", "2026")),
    # Numeric for calculations
    greg_year_num = as.numeric(greg_year)
  ) |>
  select(-greg_year)  # drop the original greg_year_num if you want

#select(-greg_month, -greg_year_num)
# Region Options 
regions <- c("Addis Ababa", "Afar", "Amhara","Benishangul Gumuz",
             "Central", "Dire Dawa", "Gambella","Harari", "Oromia",
             "Sidama","Somali", "South","South West","Tigray")

months <- levels(hmis$month_year)  # proper month-year labels
indicators <- sort(unique(as.character(hmis$data_type)))

zone_population <- hmis |>
  distinct(region, zone, greg_year_num,population) |>  # keep population!
  mutate(
    region = as.character(region),
    zone = as.character(zone),
    population = as.numeric(population)  # make numeric to avoid "non-numeric argument" error
  )

is.list(zone_population$population)
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
region_zone_lookup <- hmis |>
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
      choices = c("All Regions", unique(hmis$region)),
      selected = "All Regions",
      multiple = TRUE
    ),
    
    # Zone
    selectizeInput(
      inputId = "zone",
      label = "Select Zone",
      choices = "All Zones",
      selected = "All Zones",
      multiple = TRUE
    ),
    
    # Year filter
    selectizeInput(
      inputId = "year",
      label = "Select Year",
      choices = c("All Years", as.character(levels(hmis$greg_year_ui))),
      selected = "All Years",
      multiple = TRUE
    ),
    
    # Month filter
    selectizeInput(
      inputId = "month",
      label = "Select Month",
      choices = c("All Months", as.character(levels(hmis$greg_month))),
      selected = "All Months",
      multiple = TRUE
    ),
    # facility_type filter
    selectizeInput(
      inputId = "facility_type",
      label = "Select Facility Type",
      choices = c("All Facilities", unique(hmis$facility_type)),
      selected = "All Facilities",
      multiple = TRUE
    )
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
            tags$li("DHIS2/HMIS disease (for species segregations & clinically diagnosed malaria cases) and service data (for malaria tests and positive results)-> Last updated: 20 March 2026, 11:00 EAT (UTC+3)"),
            tags$li("Zone-level population projections (projected from 2022 population from CSA)"),
            tags$li("The data currently used covers a period from Jan 2020 to Feb 2026; reporting periods are selectable by both month & year.")
          )
        )
      ),
      
      card(
        card_header("Key Indicators"),
        card_body(
          tags$ul(
            tags$li(strong("Test Positivity Rate (TPR): "),
                    "Confirmed malaria cases divided by the number tested."),
            tags$li(strong("Annual Parasitic Incidence (API): "),
                    "Adjusted malaria cases per 1,000 population (annualized when displayed in monthly view)."),
            
            tags$li(strong("Mortality Rate: "),
                    "Malaria-related deaths per 100,000 population at risk (annualized when displayed in monthly view)."),
            
            tags$li(strong("Note:"),
                    "When aggregated view is selected, values represent the mean annualized rate across selected years."),
            
            tags$li(strong("Data Completeness & Timeliness: "),  
                    "The dashboard shows completeness and timeliness metrics for the HMIS service reports only. Non-zero reporting for completeness and timeliness started after mid-2022, so the plots are filtered to start from 2022.")
          )
        )
      ),
      
      card(
        card_header("Methodological Notes"),
        card_body(
          p(strong("Case Adjustment: "),
            "Clinical malaria cases were adjusted using the test positivity rate (TPR) to better approximate true malaria burden."),
          
          p(strong("Deaths: "),
            "Malaria deaths were derived from inpatient department (IPD) records and include confirmed malaria cases only."),
          
          p(strong("Population Denominator: "),
            "Population data are applied at zone level and aggregated to region or national level as needed. When multiple years are selected, the average population across years is used."),
          
          p(strong("Risk Population: "),
            "For mortality calculations, 69% of the total population is assumed to be at risk, based on the National Malaria Strategic Plan (2024/25–2026/27)."),
          
          p(strong("Time Adjustment: "),
            "For incomplete reporting periods, case and death counts are annualized to ensure comparability across time."),
          
          p(strong("Important: "),
            "Facility type filters are not applied to incidence and mortality indicators, as rates require complete population denominators for valid interpretation.")
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
              inputId = "api_view",
              label = NULL,
              choices = c("Aggregated", "By Month"),
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
              choices = c("Aggregated", "By Month"),
              inline = TRUE
            )
          ),
          plotlyOutput("plot_death_rate", height = "350px")
        ),
        
        # ROW 3: REPORTING PERFORMANCE
        card(
          card_header("Reporting Completeness"),
          
          radioButtons(
            inputId = "completeness_view",
            label = NULL,
            choices = c("Aggregate", "By Month"),
            selected = "Aggregate",
            inline = TRUE
          ),
          
          plotlyOutput("plot_completeness", height = "300px"),
        ),
        
        card(
          card_header("Reporting Timeliness"),
          radioButtons(
            inputId = "timeliness_view",
            label = NULL,
            choices = c("Aggregate", "By Month"),
            selected = "Aggregate",
            inline = TRUE
          ),
          
          plotlyOutput("plot_timeliness", height = "300px")
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
    if (is.null(sel)) return()
    
    # If user selects specific regions, remove "All Regions"
    if ("All Regions" %in% sel && length(sel) > 1) {
      updateSelectizeInput(
        session,
        "region",
        selected = setdiff(sel, "All Regions")
      )
    }
    
    # If nothing is selected, revert to "All Regions"
    if (length(sel) == 0) {
      updateSelectizeInput(
        session,
        "region",
        selected = "All Regions"
      )
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
  # Make zone selection mutually exclusive (All vs specific zones)
  observeEvent(input$zone, ignoreInit = TRUE, {
    sel <- input$zone
    if (is.null(sel)) return()
    
    # If user selects specific zones, remove "All Zones"
    if ("All Zones" %in% sel && length(sel) > 1) {
      updateSelectizeInput(
        session,
        "zone",
        selected = setdiff(sel, "All Zones")
      )
    }
    
    # If nothing is selected, revert to "All Zones"
    if (length(sel) == 0) {
      updateSelectizeInput(
        session,
        "zone",
        selected = "All Zones"
      )
    }
  })
  
  
  
  # -----------------------------
  # Year mutual exclusivity
  observeEvent(input$year, ignoreInit = TRUE, {
    sel <- input$year
    
    if (is.null(sel)) return() 
    # if a specific year is selected remove all years option
    if ("All Years" %in% sel && length(sel) >1) {
      updateSelectizeInput(
        session,
        "year",
        selected = setdiff(sel, "All Years")
      )
    }
    #if nothing is selected, revert to "All Years"
    if (length(sel) ==0){
      updateSelectizeInput(
        session,
        "year",
        selected = "All Years"
      )
    }
  })
  
  # 4 Month mutual exclusivity
  observeEvent(input$month, ignoreInit = TRUE, {
    sel <- input$month
    
    if (is.null(sel)) return() 
    # if a specific year is selected remove all months option
    if ("All Months" %in% sel && length(sel) >1) {
      updateSelectizeInput(
        session,
        "month",
        selected = setdiff(sel, "All Months")
      )
    }
    #if nothing is selected, revert to "All Months"
    if (length(sel) ==0){
      updateSelectizeInput(
        session,
        "month",
        selected = "All Months"
      )
    }
  })
  
  # 4 Facility type mutual exclusivity
  observeEvent(input$facility_type, ignoreInit = TRUE, {
    sel <- input$facility_type
    
    if (is.null(sel)) return() 
    # if a specific facility type is selected remove all facilities option
    if ("All Facilities" %in% sel && length(sel) >1) {
      updateSelectizeInput(
        session,
        "facility_type",
        selected = setdiff(sel, "All Facilities")
      )
    }
    #if nothing is selected, revert to "All Facilities"
    if (length(sel) ==0){
      updateSelectizeInput(
        session,
        "facility_type",
        selected = "All Facilities"
      )
    }
  })
  
  # -----------------------------
  # Reactive dataset
  filtered_data_main <- reactive({
    req(input$region, input$zone, input$year, input$month, input$facility_type)
    df <- hmis
    
    if (!"All Regions" %in% input$region)
      df <- df |> filter(region %in% input$region)
    
    if (!"All Zones" %in% input$zone)
      df <- df |> filter(zone %in% input$zone)
    
    if (!"All Years" %in% input$year)
      df <- df |> filter(greg_year_ui %in% input$year)
    
    if (!"All Months" %in% input$month)
      df <- df |> filter(greg_month %in% input$month)
    
    if (!"All Facilities" %in% input$facility_type)
      df <- df |> filter(facility_type %in% input$facility_type)
    
    df
  })
  
  prepared_data <- reactive({
    req(filtered_data_main())
    
    df <- filtered_data_main() %>%
      select(-population) %>%   
      mutate(
        region = as.character(region),
        zone   = as.character(zone)
      )
    
    pop_join <- zone_population %>%
      select(region, zone, greg_year_num, population)
    
    df <- df %>%
      left_join(pop_join, by = c("region", "zone", "greg_year_num"))
    
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
    
    total_tested <- prepared_data() |>
      filter(data_type == "tested") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_tested)
  })
  
  output$vb_confirmed <- renderText({
    
    total_confirmed <- prepared_data() |>
      filter(data_type == "positives") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_confirmed)
  })
  
  
  
  output$vb_allcases <- renderText({
    total_cases <- prepared_data() |>
      filter(data_type %in% c("positives", "clinical")) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_cases)
  })
  
  
  output$vb_death <- renderText({
    total_deaths <- prepared_data() |>
      filter(
        !data_type %in% c("clinical", "tested", "positives"),
        department == "IPD",
        outcome == "Mortality"
      ) |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
    
    kpi_value(total_deaths)
  })
  
  
  # Tests & Positivity Trend 
  output$plot_test_positivity <- renderPlotly({
    
    df_all <- prepared_data()
    
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
          marker = list(size = 4),
          yaxis = "y2",
          line = list(width = 2),
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
          line = list(width = 2),
          marker = list(
            #color = "black",
            size = 4
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
        xaxis = list(title = "",
                     tickangle = -45
        )
      )
  })
  
  
  output$plot_species_proportion <- renderPlotly({
    
    df_raw <- prepared_data() |>
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
  
  
  # -----------------------------
  # -----------------------------
  # API / Parasitic Incidence Plot
  # -----------------------------
  output$plot_incidence <- renderPlotly({
    
    df <- prepared_data() %>%
      mutate(facility_type = "All Facilities") # to only work for all facilities filter
    
    # -------------------------
    # 1️⃣ Prepare base dataset (cases)
    # -------------------------
    df_cases <- df %>%
      filter(data_type %in% c("tested", "positives", "clinical")) %>%
      group_by(region, zone, greg_year_num, greg_date, month_year, data_type) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = data_type, values_from = value, values_fill = 0) %>%
      mutate(
        tpr = ifelse(tested > 0, positives / tested, 0),
        adjusted_positive = positives + clinical * tpr
      )
    
    # -------------------------
    # 2️⃣ Geography logic
    # -------------------------
    if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
      geo_var <- "zone"
      df_cases <- df_cases %>% filter(zone %in% input$zone)
    } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
      geo_var <- "region"
      df_cases <- df_cases %>% filter(region %in% input$region)
    } else {
      geo_var <- "region"
      df_cases <- df_cases %>% mutate(region = "National")
    }
    
    # -------------------------
    # 3️⃣ Population aggregation for selected geography
    # -------------------------
    pop_geo <- df %>%
      select(region, zone, greg_year_num, population) %>%
      distinct() %>%
      mutate(population = as.numeric(population))
    
    if (geo_var == "zone") {
      pop_geo <- pop_geo %>%
        group_by(zone, greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
      pop_geo <- pop_geo %>%
        group_by(region, greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    } else {
      # National
      pop_geo <- pop_geo %>%
        group_by(greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
        mutate(region = "National")
    }
    
    # -------------------------
    # 4️⃣ Aggregated / Yearly view
    # -------------------------
    if (input$api_view == "Aggregated") {
      
      df_yearly <- df_cases %>%
        group_by(.data[[geo_var]], greg_year_num) %>%
        summarise(
          total_cases = sum(adjusted_positive, na.rm = TRUE),
          months_available = n_distinct(greg_date),
          .groups = "drop"
        ) %>%
        left_join(pop_geo, by = c(geo_var, "greg_year_num")) %>%
        mutate(
          annualized_cases = ifelse(
            months_available < 12,
            total_cases * (12 / months_available),
            total_cases
          ),
          population = as.numeric(population),
          annualized_cases = as.numeric(annualized_cases)
        )
      
      # Aggregate across selected years
      plot_df <- df_yearly %>%
        mutate(api_yearly = annualized_cases / population * 1000) %>%
        group_by(.data[[geo_var]]) %>%
        summarise(
          api = mean(api_yearly, na.rm = TRUE),
          .groups = "drop"
        )|>
        filter(!is.na(api), is.finite(api))
      
      # Use plot_df for plotting
      p <- ggplot(plot_df, aes_string(
        x = paste0("reorder(", geo_var, ", api)"),
        y = "api",
        text = "paste0(round(api,1), ' per 1,000')"
      )) +
        geom_col(fill = "cyan4") +
        coord_flip() +
        labs(x = "", y = "API per 1,000") +
        theme_minimal()
      
    } else {
      # -------------------------
      # 5️⃣ Monthly view
      # -------------------------
      df_monthly <- df_cases %>%
        group_by(.data[[geo_var]], greg_date, month_year, greg_year_num) %>%
        summarise(
          monthly_cases = sum(adjusted_positive, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(pop_geo, by = c(geo_var, "greg_year_num")) %>%
        mutate(
          api = (monthly_cases * 12 / population) * 1000
        ) %>%
        filter(!is.na(api), is.finite(api))
      
      date_range <- range(df_monthly$greg_date, na.rm = TRUE)
      n_months <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 30
      dtick_val <- if (n_months <= 12) "M1" else if (n_months <= 24) "M3" else if (n_months <= 60) "M6" else "M12"
      tick_format_val <- if (n_months > 36) "%Y" else "%b %Y"
      
      p <- plot_ly(
        df_monthly,
        x = ~greg_date,
        y = ~api,
        color = as.formula(paste0("~", geo_var)),
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 4),
        text = ~paste0(round(api,1), " per 1,000"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "API per 1,000"),
          xaxis = list(
            title = "",
            tickformat = tick_format_val,
            dtick = dtick_val,
            tickangle = -45
          )
        )
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  #------------------------
  # 2. Mortality Plot with Toggle
  output$plot_death_rate <- renderPlotly({
    
    df <- prepared_data() %>%
      mutate(facility_type = "All Facilities") # to only work for all facilities filter
    
    # -------------------------
    # 1️⃣ Prepare base dataset (deaths)
    # -------------------------
    df_deaths <- df %>%
      filter(data_type %in% c("pf_conf" ,"pv_conf", "pm_conf","po_conf","mixed_conf"),
             department == "IPD",
             outcome == "Mortality") %>%
      group_by(region, zone, greg_year_num, greg_date, month_year) %>%
      summarise(total_death = sum(value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df_deaths) == 0) {
      return(no_data_plot("No mortality data for selected inputs"))
    }
    
    # -------------------------
    # 2️⃣ Geography logic
    # -------------------------
    if (!is.null(input$zone) && length(input$zone) > 0 && !"All Zones" %in% input$zone) {
      geo_var <- "zone"
      df_deaths <- df_deaths %>% filter(zone %in% input$zone)
    } else if (!is.null(input$region) && length(input$region) > 0 && !"All Regions" %in% input$region) {
      geo_var <- "region"
      df_deaths <- df_deaths %>% filter(region %in% input$region)
    } else {
      geo_var <- "region"
      df_deaths <- df_deaths %>% mutate(region = "National")
    }
    
    if (nrow(df_deaths) == 0) {
      return(no_data_plot("No mortality data after geography filtering"))
    }
    
    # -------------------------
    # 3️⃣ Population aggregation for selected geography
    # -------------------------
    pop_geo <- df %>%
      select(region, zone, greg_year_num, population) %>%
      distinct() %>%
      mutate(population = as.numeric(population))
    
    if (geo_var == "zone") {
      pop_geo <- pop_geo %>%
        group_by(zone, greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    } else if (!is.null(input$region) && length(input$region) > 0 && !"All Regions" %in% input$region) {
      pop_geo <- pop_geo %>%
        group_by(region, greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
    } else {
      # National
      pop_geo <- pop_geo %>%
        group_by(greg_year_num) %>%
        summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
        mutate(region = "National")
    }
    
    # -------------------------
    # 4️⃣ Aggregated / Yearly view
    # -------------------------
    if (input$death_view == "Aggregated") {
      
      df_yearly <- df_deaths %>%
        group_by(.data[[geo_var]], greg_year_num) %>%
        summarise(
          total_deaths = sum(total_death, na.rm = TRUE),
          months_available = n_distinct(greg_date),
          .groups = "drop"
        ) %>%
        left_join(pop_geo, by = c(geo_var, "greg_year_num")) %>%
        mutate(
          annualized_deaths = ifelse(
            months_available < 12,
            total_deaths * (12 / months_available),
            total_deaths
          ),
          population = as.numeric(population),
          annualized_deaths = as.numeric(annualized_deaths)
        )
      
      if (nrow(df_yearly) == 0) {
        return(no_data_plot("No mortality data after joining population"))
      }
      
      # Aggregate across selected years
      plot_df <- df_yearly %>%
        mutate(
          mortality_yearly = annualized_deaths / (population * 0.69) * 100000
        ) %>%
        group_by(.data[[geo_var]]) %>%
        summarise(
          mortality_per_100k = mean(mortality_yearly, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(mortality_per_100k), is.finite(mortality_per_100k))
      
      # Plot
      p <- ggplot(plot_df, aes_string(
        x = paste0("reorder(", geo_var, ", mortality_per_100k)"),
        y = "mortality_per_100k",
        text = "paste0(round(mortality_per_100k,1), ' per 100,000')"
      )) +
        geom_col(fill = "cyan4") +
        coord_flip() +
        labs(x = "", y = "Mortality per 100,000") +
        theme_minimal()
      
    } else {
      # -------------------------
      # 5️⃣ Monthly view
      # -------------------------
      df_monthly <- df_deaths %>%
        group_by(.data[[geo_var]], greg_date, month_year, greg_year_num) %>%
        summarise(
          monthly_deaths = sum(total_death, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(pop_geo, by = c(geo_var, "greg_year_num")) %>%
        mutate(
          mortality_per_100k = (monthly_deaths * 12 / (population * 0.69)) * 100000
        ) %>%
        filter(!is.na(mortality_per_100k), is.finite(mortality_per_100k))
      
      if (nrow(df_monthly) == 0) {
        return(no_data_plot("No monthly mortality data after joining population"))
      }
      
      date_range <- range(df_monthly$greg_date, na.rm = TRUE)
      n_months <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 30
      dtick_val <- if (n_months <= 12) "M1" else if (n_months <= 24) "M3" else if (n_months <= 60) "M6" else "M12"
      tick_format_val <- if (n_months > 36) "%Y" else "%b %Y"
      
      p <- plot_ly(
        df_monthly,
        x = ~greg_date,
        y = ~mortality_per_100k,
        color = as.formula(paste0("~", geo_var)),
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 4),
        text = ~paste0(round(mortality_per_100k,1), " per 100,000"),
        hoverinfo = "text"
      ) %>%
        layout(
          yaxis = list(title = "Mortality per 100,000"),
          xaxis = list(
            title = "",
            tickformat = tick_format_val,
            dtick = dtick_val,
            tickangle = -45
          )
        )
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_completeness <- renderPlotly({
    
    df <- prepared_data() %>%
      filter(greg_year_num >= 2022)
    
    # Keep only needed data types
    df <- df |> 
      filter(data_type %in% c("actual_reports", "expected_reports"))
    
    # ---- No data check ----
    total_expected <- df |> 
      filter(data_type == "expected_reports") |> 
      summarise(total = sum(value, na.rm = TRUE)) |> 
      pull(total)
    
    if (nrow(df) == 0 || is.na(total_expected) || total_expected == 0) {
      return(no_data_plot())
    }
    
    # =========================
    # AGGREGATED VIEW (GAUGE)
    # =========================
    if (input$completeness_view == "Aggregate") {
      
      actual <- df |> 
        filter(data_type == "actual_reports") |> 
        summarise(val = sum(value, na.rm = TRUE)) |> 
        pull(val)
      
      expected <- df |> 
        filter(data_type == "expected_reports") |> 
        summarise(val = sum(value, na.rm = TRUE)) |> 
        pull(val)
      
      value <- (actual / expected) * 100
      
      p <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        number = list(suffix = "%"),
        gauge = list(
          axis = list(range = c(0, 100)),
          bar = list(color = "cyan4", thickness = 0.5),
          steps = list(
            list(range = c(0, 80), color = "#f4a59c"),
            list(range = c(80, 89), color = "#fcd581"),
            list(range = c(90, 100), color = "#90c7b1")
          ),
          threshold = list(
            value = 90,
            line = list(color = "black", width = 2)
          )
        )
      )
      
      return(p)
    }
    
    # =========================
    # BY MONTH (MULTI-LINE)
    # =========================
    
    # ---- Determine geography ----
    if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
      geo_var <- "zone"
      df <- df |> filter(zone %in% input$zone)
      
    } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
      geo_var <- "region"
      df <- df |> filter(region %in% input$region)
      
    } else {
      geo_var <- "region"
      df <- df |> mutate(region = "National")
    }
    
    # ---- Aggregate ----
    df_trend <- df |>
      group_by(greg_date, !!sym(geo_var), data_type) |>
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |>
      mutate(
        completeness = ifelse(expected_reports > 0,
                              (actual_reports / expected_reports) * 100,
                              NA)
      ) |>
      arrange(greg_date)
    
    # ---- Dynamic x-axis scaling ----
    date_range <- range(df_trend$greg_date, na.rm = TRUE)
    n_months <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 30
    
    dtick_val <- if (n_months <= 12) {
      "M1"
    } else if (n_months <= 24) {
      "M3"
    } else if (n_months <= 60) {
      "M6"
    } else {
      "M12"
    }
    
    tick_format_val <- if (n_months > 36) "%Y" else "%b %Y"
    
    # ---- Plot ----
    p <- plot_ly(
      df_trend,
      x = ~greg_date,
      y = ~completeness,
      color = ~get(geo_var),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4),
      # Hover text showing Month-Year and completeness
      text = ~paste0(format(greg_date, "%b %Y"), ": ", round(completeness, 1), "%"),
      hoverinfo = "text"
    ) |>
      layout(
        yaxis = list(title = "% Completeness", range = c(0, 100)),
        xaxis = list(
          title = "",
          tickformat = tick_format_val,
          tickangle = -45,
          dtick = dtick_val
        ),
        shapes = list(
          list(
            type = "line",
            x0 = min(df_trend$greg_date, na.rm = TRUE),
            x1 = max(df_trend$greg_date, na.rm = TRUE),
            y0 = 90,
            y1 = 90,
            line = list(dash = "dash", color = "black")
          )
        )
      )
    
    p
  })
  
  
  output$plot_timeliness <- renderPlotly({
    
    df <- prepared_data() %>%
      filter(greg_year_num >= 2022)
    
    # Keep only needed data types
    df <- df |> 
      filter(data_type %in% c("actual_reports_ontime", "expected_reports"))
    
    # ---- No data check ----
    total_expected <- df |> 
      filter(data_type == "expected_reports") |> 
      summarise(total = sum(value, na.rm = TRUE)) |> 
      pull(total)
    
    if (nrow(df) == 0 || is.na(total_expected) || total_expected == 0) {
      return(no_data_plot())
    }
    
    # =========================
    # AGGREGATED VIEW (GAUGE)
    # =========================
    if (input$timeliness_view == "Aggregate") {
      
      actual <- df |> 
        filter(data_type == "actual_reports_ontime") |> 
        summarise(val = sum(value, na.rm = TRUE)) |> 
        pull(val)
      
      expected <- df |> 
        filter(data_type == "expected_reports") |> 
        summarise(val = sum(value, na.rm = TRUE)) |> 
        pull(val)
      
      value <- (actual / expected) * 100
      
      p <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        number = list(suffix = "%"),
        gauge = list(
          axis = list(range = c(0, 100)),
          bar = list(color = "cyan4", thickness = 0.5),
          steps = list(
            list(range = c(0, 80), color = "#f4a59c"),
            list(range = c(80, 89), color = "#fcd581"),
            list(range = c(90, 100), color = "#90c7b1")
          ),
          threshold = list(
            value = 90,
            line = list(color = "black", width = 2)
          )
        )
      )
      
      return(p)
    }
    
    # =========================
    # BY MONTH (MULTI-LINE)
    # =========================
    
    # ---- Determine geography ----
    if (!is.null(input$zone) && !"All Zones" %in% input$zone) {
      geo_var <- "zone"
      df <- df |> filter(zone %in% input$zone)
      
    } else if (!is.null(input$region) && !"All Regions" %in% input$region) {
      geo_var <- "region"
      df <- df |> filter(region %in% input$region)
      
    } else {
      geo_var <- "region"
      df <- df |> mutate(region = "National")
    }
    
    # ---- Aggregate ----
    df_trend <- df |>
      group_by(greg_date, !!sym(geo_var), data_type) |>
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(names_from = data_type, values_from = value, values_fill = 0) |>
      mutate(
        timeliness = ifelse(expected_reports > 0,
                            (actual_reports_ontime / expected_reports) * 100,
                            NA)
      ) |>
      arrange(greg_date)
    
    # ---- Dynamic x-axis scaling ----
    date_range <- range(df_trend$greg_date, na.rm = TRUE)
    n_months <- as.numeric(difftime(date_range[2], date_range[1], units = "days")) / 30
    
    dtick_val <- if (n_months <= 12) {
      "M1"
    } else if (n_months <= 24) {
      "M3"
    } else if (n_months <= 60) {
      "M6"
    } else {
      "M12"
    }
    
    tick_format_val <- if (n_months > 36) "%Y" else "%b %Y"
    
    # ---- Plot ----
    p <- plot_ly(
      df_trend,
      x = ~greg_date,
      y = ~timeliness,
      color = ~get(geo_var),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 4),
      # Hover text: show Month-Year and value
      text = ~paste0(format(greg_date, "%b %Y"), ": ", round(timeliness, 1), "%"),
      hoverinfo = "text"
    ) |>
      layout(
        yaxis = list(title = "% Timeliness", range = c(0, 100)),
        xaxis = list(
          title = "",
          tickformat = tick_format_val,
          tickangle = -45,
          dtick = dtick_val
        ),
        shapes = list(
          list(
            type = "line",
            x0 = min(df_trend$greg_date, na.rm = TRUE),
            x1 = max(df_trend$greg_date, na.rm = TRUE),
            y0 = 90,
            y1 = 90,
            line = list(dash = "dash", color = "black")
          )
        )
      )
    
    p
  })
  
}

# --- Run App ---
shinyApp(ui = ui, server = server)

