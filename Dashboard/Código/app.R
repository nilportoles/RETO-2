# ================================================================
# Dashboard CRONOS-2 Wave 4 — Salud mental y bienestar
# Lee: Datos/Base de datos depurada/cron2_w4_clean.rds
# ================================================================

suppressPackageStartupMessages({
  library(shiny); library(tidyverse); library(plotly); library(DT)
  library(leaflet); library(sf); library(rnaturalearth); library(rnaturalearthdata)
})

wmean <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    ok <- is.finite(x) & is.finite(w)
  } else {
    ok <- !(is.na(x) | is.na(w))
  }
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) return(mean(x, na.rm = TRUE))
  sum(x * w) / sw
}

datos <- readRDS("~/Desktop/M8/RETO-2/Datos/Base de datos depurada/cron2_w4_clean.rds")

ui <- fluidPage(
  titlePanel("CRONOS-2 Wave 4 — Salud mental y bienestar"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("countries", "Países",
                     choices = sort(unique(datos$cntry_iso2)),
                     multiple = TRUE),
      checkboxGroupInput("sex", "Sexo",
                         choices = c("Male","Female"),
                         selected = c("Male","Female")),
      checkboxGroupInput("ages", "Edad",
                         choices = levels(droplevels(datos$age_band)),
                         selected = levels(droplevels(datos$age_band))),
      hr(),
      selectInput("construct", "Constructos",
                  choices = c("Afecto positivo"="positive_affect_mean",
                              "Distrés"="distress_mean"),
                  selected="distress_mean"),
      helpText("Medias ponderadas por c2weight.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Descriptivo",
                 uiOutput("map_title"),
                 leafletOutput("map_europe", height="520px"),
                 br(),
                 h4("Tabla resumen por país"),
                 DTOutput("table_desc")
        ),
        tabPanel("Comparaciones",
                 h4("Comparación entre países"),
                 plotlyOutput("cmp_countries", height="420px"),
                 fluidRow(
                   column(6, h4("Por sexo"), plotlyOutput("cmp_sex", height="360px")),
                   column(6, h4("Por edad"), plotlyOutput("cmp_age", height="360px"))
                 )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  

  d_filt <- reactive({
    d <- datos
    if (length(input$countries)) d <- d %>% filter(cntry_iso2 %in% input$countries)
    if (length(input$sex))       d <- d %>% filter(gndr_lab %in% input$sex)
    if (length(input$ages))      d <- d %>% filter(!is.na(age_band) & age_band %in% input$ages)
    d
  })
  
  output$map_title <- renderUI({
    lbl <- if (input$construct == "distress_mean") "Distrés" else "Afecto positivo"
    tags$h4(sprintf("Mapa — %s", lbl))
  })
  
  output$map_europe <- renderLeaflet({
    var <- input$construct
    lbl <- if (var == "distress_mean") "Distrés" else "Afecto positivo"
    
    d <- d_filt() %>%
      group_by(cntry_iso2) %>%
      summarise(val = wmean(.data[[var]], c2weight), .groups="drop")
    
    eu <- rnaturalearth::ne_countries(scale="medium", returnclass="sf")
    m  <- eu %>% mutate(iso2 = iso_a2) %>%
      select(iso2, name_long, geometry) %>%
      left_join(d, by = c("iso2"="cntry_iso2"))
    
    pal <- colorNumeric("YlOrRd", domain = m$val, na.color = "#f0f0f0")
    
    leaflet(m) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(val),
        color = "#666", weight = 0.7, fillOpacity = 0.8,
        label = ~paste0(name_long, ": ",
                        ifelse(is.na(val), "NA", sprintf("%.2f", val))),
        layerId = ~iso2,
        highlightOptions = highlightOptions(weight = 2, color = "#333", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = ~val,
                title = paste0(lbl, " (media)"), opacity = 1)
  })
  
  output$table_desc <- renderDT({
    d_filt() %>%
      group_by(cntry_iso2) %>%
      summarise(
        `Afecto positivo` = wmean(positive_affect_mean, c2weight),
        `Distrés`         = wmean(distress_mean,        c2weight),
        .groups="drop"
      ) %>%
      arrange(desc(`Distrés`)) %>%
      datatable(options = list(pageLength = 10))
  })
  
  output$cmp_countries <- renderPlotly({
    var <- input$construct
    d <- d_filt() %>%
      group_by(cntry_iso2) %>%
      summarise(media = wmean(.data[[var]], c2weight), .groups="drop") %>%
      arrange(desc(media))
    validate(need(any(is.finite(d$media)), "Sin datos suficientes para dibujar este gráfico."))
    p <- ggplot(d, aes(x = reorder(cntry_iso2, media), y = media)) +
      geom_col() + coord_flip() +
      labs(x = "País", y = "Media ponderada (1–4)")
    ggplotly(p)
  })
  
  output$cmp_sex <- renderPlotly({
    var <- input$construct
    d <- d_filt() %>%
      filter(!is.na(gndr_lab)) %>%
      group_by(gndr_lab) %>%
      summarise(media = wmean(.data[[var]], c2weight), .groups="drop")
    validate(need(any(is.finite(d$media)), "Sin datos suficientes para dibujar este gráfico."))
    p <- ggplot(d, aes(x = gndr_lab, y = media, fill = gndr_lab)) +
      geom_col() + labs(x = "Sexo", y = "Media ponderada (1–4)")
    ggplotly(p)
  })
  
  output$cmp_age <- renderPlotly({
    var <- input$construct
    d <- d_filt() %>%
      filter(!is.na(age_band)) %>%
      group_by(age_band) %>%
      summarise(media = wmean(.data[[var]], c2weight), .groups="drop")
    validate(need(any(is.finite(d$media)), "Sin datos suficientes para dibujar este gráfico."))
    p <- ggplot(d, aes(x = age_band, y = media, fill = age_band)) +
      geom_col() + labs(x = "Edad", y = "Media ponderada (1–4)")
    ggplotly(p)
  })
}

shinyApp(ui, server)
