

### Renderización del mapa y gráficos

function(input, output, session){
  
  output$mapa_estados <- renderLeaflet({
    
    leaflet() |> 
      addProviderTiles(provider = providers$CartoDB.DarkMatter) |> 
      setView( lat=43, lng=-90 , zoom=4) |> 
      addCircleMarkers(~Longitude, ~Latitude, 
                       data = df,
                       fillColor = ~paleta(metrica_fin), fillOpacity = 0.7, color="white", radius=~metrica_fin*22, stroke=FALSE,
                       label = tooltip_texto,
                       labelOptions = labelOptions( 
                         style = list("font-weight" = "normal", 
                                      padding = "3px 8px"), 
                         textsize = "13px", 
                         direction = "auto")
      ) 
    
  })
  
  valor_fil <- reactive({
    
    df |> 
      filter(State_Abb %in% input$estado)
    
  })
  
  
  texto_reac <- reactive({
    
    req(valor_fil())
    
    paste0(
      "Ciudad: ", valor_fil()$City, "<br/>",
      "Cantidad total de empresas: ", valor_fil()$total_industrias, "<br/>",
      "Calif. de las características de mercado (normalizadas): ", round(valor_fil()$proper_tot, digits = 2)*100, "<br/>",
      "<strong>Calificación final: </strong>", round(valor_fil()$metrica_fin, digits = 2)*100,"%", "<br/>") |> 
      lapply(htmltools::HTML)
    
  })
  
  
  observeEvent(input$estado,{
    
    leafletProxy("mapa_estados") |> 
      clearMarkers() |> 
      addCircleMarkers(~Longitude, ~Latitude, 
                       data = valor_fil(),
                       fillColor = ~paleta(metrica_fin), 
                       fillOpacity = 0.7, color="white", 
                       radius=~metrica_fin*22, stroke=FALSE,
                       label = texto_reac(),
                       labelOptions = labelOptions( 
                         style = list("font-weight" = "normal", 
                                      padding = "3px 8px"), 
                         textsize = "13px", 
                         direction = "auto")
      )
    
    
  }, ignoreInit = TRUE)
  
  
  bandas_reac <- reactive({
    
    bounds |> 
      filter(STATE_ABBR %in% input$estado) |> 
      sf::st_bbox(bounds$geometry) |> 
      as.numeric()
    
  })
  
  
  observe({
    
    #req(bandas_reac())
    
    leafletProxy("mapa_estados") |> 
      flyToBounds(bandas_reac()[1],
                  bandas_reac()[2],
                  bandas_reac()[3],
                  bandas_reac()[4])
    
  })
  
  output$grafico_correlacion <- renderEcharts4r({
    
    df |> 
      select(.data[[input$var1]], .data[[input$var2]]) |> 
      setNames(c("var_1", "var_2") ) |> 
      e_charts(var_1, dispose = FALSE) |> 
      e_scatter(var_2, name = "Observaciones") |> 
      e_lm(var_2 ~ var_1, name = "Correlación") |> 
      e_theme("auritus") |> 
      e_color(color = c("#ff8e2b","#c92c2c")) |> 
      e_legend(FALSE) |> 
      e_title( "Relación observada entre variables", 
               left = "center",
               textStyle = list(
                 color = "gray",
                 fontSize = 14
               )
      ) |>
      e_x_axis(
        nameLocation = "center", 
        splitArea = list(show = FALSE),
        axisLabel = list(margin = 3),
        axisPointer = list(
          show = TRUE, 
          lineStyle = list(
            color = "#999999",
            width = 0.75,
            type = "dotted"
          )
        )
      ) |> 
      e_y_axis(
        nameLocation = "center", 
        splitArea = list(show = FALSE),
        axisLabel = list(margin = 0),
        axisPointer = list(
          show = TRUE, 
          lineStyle = list(
            color = "#999999",
            width = 0.75,
            type = "dotted"
          )
        )
      )
    
    
  })
  
  
  p_input_n <- reactive({
    
    names(df[5:12]) |> 
      as.data.frame() |> 
      setNames("opciones_nom") |> 
      filter(opciones_nom != input$var1 )
    
  })
  
  
  observeEvent(input$var1,{
    
    req(p_input_n())
    
    updatePickerInput(
      session, "var2",
      choices = p_input_n()$opciones_nom
      
    )
    
  })
  
  
  
  output$grafico_barras <- renderEcharts4r({
    
    df |> 
      filter(State_Abb %in% input$estado) |> 
      arrange(desc(metrica_fin)) |> 
      top_n(10,metrica_fin) |>
      e_charts(City, dispose = FALSE) |> 
      e_bar(metrica_fin, name = "Calificación por ciudad") |> 
      e_tooltip(trigger = "axis",
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12),
                formatter = e_tooltip_pointer_formatter("percent",
                                                        digits = 2)
      ) |> 
      e_theme("auritus") |> 
      e_color(color = "#a13333") |> 
      e_y_axis(show = FALSE) |> 
      e_legend(FALSE) |> 
      e_title("Calificación por ciudad", 
              left = "center",
              textStyle = list(
                color = "gray", 
                fontSize = 14
              )
      )
    
    
    
  })
  
  output$grafico_barras_media <- renderEcharts4r({
    
    df |> 
      filter(State_Abb %in% input$estado) |> 
      group_by(State_Abb) |> 
      summarise(media = mean(metrica_fin)) |> 
      arrange(desc(media)) |> 
      top_n(10,media) |> 
      e_charts(State_Abb, dispose = FALSE) |> 
      e_bar(media,name = "Calificación promedio") |> 
      e_tooltip(trigger = "axis",
                textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12),
                formatter = e_tooltip_pointer_formatter("percent",
                                                        digits = 2)
      ) |> 
      e_color(color = "#a13333") |> 
      e_theme("auritus") |> 
      e_y_axis(show = FALSE) |> 
      e_legend(FALSE) |> 
      e_title("Calificación media por estados", 
              left = "center",
              textStyle = list(
                color = "gray", 
                fontSize = 14
              )
      )
    
  })
  
  output$tabla_fil <- renderReactable({
    
    df |> 
      filter(State_Abb %in% input$estado) |> 
      select(City, State_Abb,
             total_industrias, metrica_fin) |> 
      reactable(
        language = reactableLang(
          noData = "Sin datos seleccionados",
          pageInfo = "{rowStart}\u2013{rowEnd} de {rows} obs.",
          pagePrevious = "\u276e",
          pageNext = "\u276f",
          pageNumbers = "{page} de {pages}",
        ),
        compact = TRUE,
        theme = reactableTheme(backgroundColor = "transparent",
                               color = "white"
        ),
        columns = list(
          metrica_fin = colDef(name = "Calificación", format  = 
                                 colFormat(percent = TRUE,
                                           digits = 2)
          ),
          total_industrias = colDef(name = "Industrias totales"),
          City = colDef(name = "Ciudad",
                        
                        cell = function(value, index) {
                          estado <- df$State_Abb[index]
                          #species <- if (!is.na(species)) species else "Unknown"
                          div(
                            div(style = list(fontWeight = 600), value),
                            div(style = list(fontSize = 12), estado)
                          )
                        }
                        
          ),
          State_Abb = colDef(show = FALSE)
          
        ),
        paginationType = "simple",
        # searchable = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        defaultPageSize = 7
        
      ) 
    
    
  })
  
  
}
