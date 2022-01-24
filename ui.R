

## Vista del usuario

fluidPage(
  title = "¿En dónde debería expandir sus operaciones una empresa?",
  style="color: #6b6b6b;",
  setBackgroundColor("#262626"),
  column(width = 3, align = "center",
         br(),
         div(style="display: inline-block;vertical-align:top; width: 130px;",
             img(src="logo_small_b.png", height = '108px', width = '100px')),
         br(),
         br(),
         pickerInput("estado", 
                     "Estado a seleccionar:",
                     choices = unique(df$State_Abb),
                     selected = unique(df$State_Abb),
                     multiple = TRUE,
                     options = list(
                       `actions-box` = TRUE,
                       `live-search`=TRUE)
         ),hr(),
         pickerInput("var1", "Primer variable a correlacionar",
                     choices = names(df[5:12]),
                     selected = names(df[5])
         ),
         pickerInput("var2", "Segunda variable a correlacionar",
                     choices = names(df[5:12]),
                     selected = names(df[6])
         ),hr(),
         reactableOutput("tabla_fil")
  ),
  column(width = 6,
         leafletOutput("mapa_estados",
                       width = "100%",
                       height = 900)
  ),
  column(width = 3,
         br(),
         echarts4rOutput("grafico_correlacion", width = 300, height = 280),
         echarts4rOutput("grafico_barras", width = 300, height = 280),
         echarts4rOutput("grafico_barras_media", width = 300, height = 280)
         
         )
  
)

