a# Load libraries ----------------------------------------------------------
library(pacman)
p_load(terra, sf, fs, tidyverse, glue, shiny, leaflet, rmapshaper, geodata, bslib, bsicons, shinyjs, shinyWidgets)

sf::sf_use_s2(FALSE)
g <- gc(reset = T)
options(scipen = 999, warn = -1)

# Base data ---------------------------------------------------------------
# bra1_sf <- geodata::world(resolution = 1, path = './tmpr')
bra1_sf <- terra::vect('./www/geo.gpkg')

# Raster data -------------------------------------------------------------
cffe <- terra::rast(glue('./www/rf_coffee_world.tif'))
coco <- terra::rast(glue('./www/rf_cocoa_world.tif'))

# Renombrar las capas internamente
names(cffe) <- c("Current", "Future", "Impact")
names(coco) <- c("Current", "Future", "Impact")


# Class -------------------------------------------------------------------
levels(cffe[[1]]) <- data.frame(value = 1:5, Current = c('Unsuitable', 'Arabica', 'Robusta', 'Limitations', 'Mixed'))
levels(cffe[[2]]) <- data.frame(value = 1:5, Future = c('Unsuitable', 'Arabica', 'Robusta', 'Limitations', 'Mixed'))
levels(cffe[[3]]) <- data.frame(value = 0:4, Impact = c('Unsuitable','Incremental adaptation','Systemic adaptation', 'Transform','Opportunities'))

levels(coco[[1]]) <- data.frame(value = 1:8, Current =  c('Unsuitable','Hot - Very dry','Hot - Dry','Very hot - Wet', 'Cold - Very wet','Cool - Very dry','Limitations','Mixed'))
levels(coco[[2]]) <- data.frame(value = 1:8, Future =  c('Unsuitable','Hot - Very dry','Hot - Dry','Very hot - Wet', 'Cold - Very wet','Cool - Very dry','Limitations','Mixed'))
levels(coco[[3]]) <- data.frame(value = 0:4, Impact = c('Unsuitable','Incremental adaptation','Systemic adaptation', 'Transform','Opportunities'))

# Colors ------------------------------------------------------------------
clrs.cco <- c("Unsuitable" = "#F2F2F2", 
              "Hot - Very dry" = "#FFF59D",
              "Hot - Dry" = "#FFCC80", 
              "Very hot - Wet" = "#FFAB91",
              "Cold - Very wet" = "#90CAF9", 
              "Cool - Very dry" = "#B2EBF2",
              "Limitations" = "#D9D9D9", 
              "Mixed" = "#FFFFC0")

clrs.cff <- c("Unsuitable" = "#F5F5F5", 
              "Arabica" = "#3DA153",
              "Robusta" = "#676E29", 
              "Limitations" = "#D9D9D9",
              "Mixed" = "#FFFFC0")

clrs.adp <- c("Unsuitable"             = "#EEEEEE",
              "Incremental adaptation" = "#82B382",
              "Systemic adaptation"    = "#E6D621",
              "Transform"              = "#CC5B54",
              "Opportunities"          = "#5F8C51")

# Diccionario para mapear UI con los nombres internos de las capas
escenario_map <- c("Baseline" = "Current", "Futuro" = "Future", "Impacto" = "Impact")

# ========================================================================= #
# SHINY UI                                                                  #
# ========================================================================= #
ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      /* Color de la barra superior */
      .bslib-page-title { background-color: #798B8C !important; color: white !important; padding: 15px 20px !important; margin: 0 !important; font-size: 18px;}
      .bslib-sidebar-layout > .navbar { display: none; }
      
      /* Estilo del radioGroupButtons vertical */
      .btn-group-vertical { width: 100%; box-shadow: none; }
      .btn-group-vertical > .btn { border-color: #3B7B50; color: #3B7B50; background-color: #F8F9FA; border-radius: 0; }
      .btn-group-vertical > .btn:first-child { border-top-left-radius: 4px; border-top-right-radius: 4px; }
      .btn-group-vertical > .btn:last-child { border-bottom-left-radius: 4px; border-bottom-right-radius: 4px; }
      .btn-group-vertical > .btn.active { background-color: #3B7B50 !important; color: white !important; border-color: #3B7B50 !important; }
      
      /* Títulos de secciones en el sidebar */
      .sidebar-title { font-weight: bold; color: #555; margin-bottom: 10px; font-size: 14px; display: flex; align-items: center; gap: 8px;}
      
      /* Separador */
      hr { border-top: 1px solid #ccc; margin: 20px 0; }
    "))
  ),
  
  
  # Este es el título que se ve DENTRO de la página (con el ícono)
  title = HTML("<i class='bi bi-geo-alt-fill'></i> Aptitud Climática de Cultivos — World"),
  
  # --- NUEVO: Este es el título que se ve en la PESTAÑA del navegador ---
  window_title = "Aclimatar-World",
  
  # title = HTML("<i class='bi bi-geo-alt-fill'></i> Aptitud Climática de Cultivos — Brasil"),
  theme = bs_theme(version = 5, bootswatch = "flatly", bg = "#FFFFFF", fg = "#333333"),
  
  sidebar = sidebar(
    width = 320,
    bg = "#F4F6F6", 
    
    # 1. Selección de Cultivo
    div(class = "sidebar-title", HTML("<i class='bi bi-flower1'></i> Cultivo")),
    radioButtons("cultivo", label = NULL, 
                 choiceNames = list(
                   HTML("🍫 Cacao"),
                   HTML("☕ Café")
                 ),
                 choiceValues = c("Cacao", "Café"), 
                 selected = "Café"),
    
    hr(),
    
    # 2. Selección de Capa (Visualización)
    div(class = "sidebar-title", HTML("<i class='bi bi-layers'></i> Visualizar capa")),
    radioGroupButtons(
      inputId = "escenario",
      label = NULL,
      choices = c("Baseline", "Futuro", "Impacto"),
      selected = "Baseline",
      direction = "vertical",
      justified = TRUE
    ),
    
    hr(),
    
    # --- NUEVO: BÚSQUEDA POR COORDENADAS ---
    div(class = "sidebar-title", HTML("<i class='bi bi-search'></i> Buscar Coordenadas")),
    layout_columns(
      col_widths = c(6, 6),
      numericInput("in_lon", label = "Longitud", value = NULL, step = 0.1),
      numericInput("in_lat", label = "Latitud", value = NULL, step = 0.1)
    ),
    actionButton("btn_buscar", "Buscar Píxel", class = "btn-success", style = "width: 100%; font-weight: bold; margin-bottom: 10px;"),
    # ---------------------------------------
    
    hr(),
    
    # 3. Leyenda Dinámica en el Sidebar
    div(class = "sidebar-title", HTML("<i class='bi bi-palette'></i> Leyenda")),
    uiOutput("leyenda_sidebar")
  ),
  
  card(
    full_screen = TRUE,
    class = "p-0",
    leafletOutput("mapa", height = "100%")
  )
)

# ========================================================================= #
# SHINY SERVER                                                              #
# ========================================================================= #
server <- function(input, output, session) {
  
  r_base <- reactive({
    if (input$cultivo == "Café") return(cffe)
    return(coco)
  })
  
  r_activa <- reactive({
    r <- r_base()
    layer_name <- escenario_map[[input$escenario]]
    return(r[[layer_name]])
  })
  
  map_setup <- reactive({
    escenario <- input$escenario
    cultivo <- input$cultivo
    
    if (escenario == "Impacto") {
      pal_colors <- clrs.adp
      dominio <- 0:4
    } else {
      if (cultivo == "Café") {
        pal_colors <- clrs.cff
        dominio <- 1:5
      } else {
        pal_colors <- clrs.cco
        dominio <- 1:8
      }
    }
    
    pal_func <- colorFactor(palette = unname(pal_colors), domain = dominio, na.color = "transparent")
    
    list(
      pal_func = pal_func,
      colors = unname(pal_colors),
      labels = names(pal_colors)
    )
  })
  
  output$leyenda_sidebar <- renderUI({
    req(map_setup())
    setup <- map_setup()
    
    items <- mapply(function(col, lab) {
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 6px;",
        tags$div(style = sprintf("width: 16px; height: 16px; background-color: %s; border: 1px solid #bbb; margin-right: 10px; border-radius: 3px;", col)),
        tags$span(lab, style = "font-size: 13px; color: #444;")
      )
    }, setup$colors, setup$labels, SIMPLIFY = FALSE)
    
    tags$div(items)
  })
  
  output$mapa <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      addPolygons(data = bra1_sf, color = "#444444", weight = 1, fillOpacity = 0, group = "Base") |> 
      setView(lng = -55, lat = -10, zoom = 4) |> 
      addLayersControl(overlayGroups = c("Base"), options = layersControlOptions(collapsed = FALSE))
  })
  
  observe({
    req(r_activa(), map_setup())
    setup <- map_setup()
    
    leafletProxy("mapa") |> 
      clearImages() |> 
      addRasterImage(
        r_activa(),
        colors = setup$pal_func,
        opacity = 0.8,
        project = TRUE,
        method = "ngb",
        group = "RasterLayer"
      )
  })
  
  # --- FUNCIÓN CENTRALIZADA PARA MOSTRAR EL POPUP ---
  mostrar_popup <- function(lng, lat, centrar_mapa = FALSE) {
    pt_df <- data.frame(lon = lng, lat = lat)
    pt_vect <- terra::vect(pt_df, geom = c("lon", "lat"), crs = "EPSG:4326")
    
    val <- terra::extract(r_base(), pt_vect, factors = TRUE)
    
    # Validación: Si no hay datos (cayó en el océano o fuera de Brasil), avisar al usuario
    if(is.na(val$Current[1]) && is.na(val$Future[1]) && is.na(val$Impact[1])) {
      showNotification("Las coordenadas ingresadas no contienen datos para este cultivo.", type = "warning")
      return(NULL) 
    }
    
    clrs_crop <- if(input$cultivo == "Café") clrs.cff else clrs.cco
    
    cat_curr <- as.character(val$Current[1])
    cat_fut  <- as.character(val$Future[1])
    cat_imp  <- as.character(val$Impact[1])
    
    col_curr <- clrs_crop[cat_curr]
    col_fut  <- clrs_crop[cat_fut]
    col_imp  <- clrs.adp[cat_imp]
    
    pill_html <- function(color, texto) {
      if(is.na(texto)) return("<span style='color:#999; font-style:italic;'>Sin datos</span>")
      border <- ifelse(toupper(color) %in% c("#FFFFFF", "#F2F2F2", "#F5F5F5", "#EEEEEE"), "1px solid #ccc", "1px solid transparent")
      
      paste0(
        "<span style='display:inline-block; background-color:", color, 
        "; border:", border, "; border-radius:12px; padding: 2px 12px; ",
        "font-size:12px; color: #222; text-align:center; min-width:80px;'>", 
        texto, "</span>"
      )
    }
    
    icon_title <- ifelse(input$cultivo == "Café", "☕", "🍫")
    
    popup_content <- paste0(
      "<div style='font-family: Arial, sans-serif; min-width: 180px;'>",
      "<div style='font-size:15px; font-weight:bold; color:#2c3e50; margin-bottom: 8px;'>", icon_title, " ", input$cultivo, "</div>",
      "<hr style='margin: 8px 0; border-top: 1px solid #eee;'>",
      
      "<div style='display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;'>",
      "<span style='color:#666; font-size:13px;'>Baseline</span>", pill_html(col_curr, cat_curr),
      "</div>",
      
      "<div style='display:flex; justify-content:space-between; align-items:center; margin-bottom:6px;'>",
      "<span style='color:#666; font-size:13px;'>Futuro</span>", pill_html(col_fut, cat_fut),
      "</div>",
      
      "<div style='display:flex; justify-content:space-between; align-items:center;'>",
      "<span style='color:#666; font-size:13px;'>Impacto</span>", pill_html(col_imp, cat_imp),
      "</div>",
      
      "</div>"
    )
    
    proxy <- leafletProxy("mapa") |> clearPopups() |> addPopups(lng = lng, lat = lat, popup = popup_content)
    
    # Si la búsqueda fue manual, hacemos un pequeño zoom a la zona
    if(centrar_mapa) {
      proxy |> setView(lng = lng, lat = lat, zoom = 6)
    }
  }
  
  # EVENTO 1: Clic en el mapa
  observeEvent(input$mapa_click, {
    click <- input$mapa_click
    req(click)
    
    # --- NUEVO: Actualizar los inputs del sidebar ---
    # Usamos round(..., 4) para que no se vea un número larguísimo de decimales en el cuadro
    updateNumericInput(session, "in_lon", value = round(click$lng, 4))
    updateNumericInput(session, "in_lat", value = round(click$lat, 4))
    # ------------------------------------------------
    
    mostrar_popup(click$lng, click$lat, centrar_mapa = FALSE)
  })
  
  # EVENTO 2: Clic en el botón de búsqueda
  observeEvent(input$btn_buscar, {
    req(input$in_lon, input$in_lat) # Asegura que haya valores escritos
    mostrar_popup(input$in_lon, input$in_lat, centrar_mapa = TRUE)
  })
}

shinyApp(ui, server)
