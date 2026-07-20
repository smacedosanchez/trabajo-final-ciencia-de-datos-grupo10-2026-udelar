library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

# --- Carga y preparación de datos (mismo procesamiento que el .qmd) ---
datos_limpio <- read_csv("datos_limpios.csv", show_col_types = FALSE) %>%
  mutate(
    Sexo = factor(Sexo, levels = c(0, 1), labels = c("Mujer", "Varón")),
    Zona = factor(if_else(Departamento == "MONTEVIDEO", "Montevideo", "Interior"),
                  levels = c("Montevideo", "Interior")),
    INSE_categoria = factor(INSE.Cat, levels = 0:6,
                            labels = c("B-", "B+", "M-", "M", "M+", "A-", "A+"),
                            ordered = TRUE),
    Aislamiento = factor(Tipo.aislamiento.recod, levels = 0:4,
                         labels = c("Retornó a rutina normal", "Aislamiento parcial",
                                    "Comenzó a salir por trabajo", "No realiza aislamiento",
                                    "Aislamiento total")),
    Cambio_actividad = Fisica.ahora - Fisica.antes,
    Cambio_actividad_categoria = case_when(
      is.na(Cambio_actividad) ~ NA_character_,
      Cambio_actividad < 0 ~ "Disminuyó",
      Cambio_actividad == 0 ~ "Se mantuvo",
      Cambio_actividad > 0 ~ "Aumentó"
    ),
    Cambio_actividad_categoria = factor(Cambio_actividad_categoria,
                                        levels = c("Disminuyó", "Se mantuvo", "Aumentó")),
    
    BDI_categoria = factor(if_else(BDI.Cat == 0, 0, 1), levels = c(0, 1),
                           labels = c("Sin depresión", "Depresión")),
    STAI_E_categoria = factor(STAI.E.Cat, levels = c(0, 1),
                              labels = c("Normal", "Clínica")),
    STAI_R_categoria = factor(STAI.R.Cat, levels = c(0, 1),
                              labels = c("Normal", "Clínica")),
    HS_categoria = factor(HS.Cat, levels = c(0, 1, 2, 3),
                          labels = c("Ninguna", "Leve", "Moderada", "Alta"),
                          ordered = TRUE)
  )

indicadores <- c("Depresión (BDI)" = "BDI_categoria",
                 "Ansiedad estado (STAI-E)" = "STAI_E_categoria",
                 "Ansiedad rasgo (STAI-R)" = "STAI_R_categoria",
                 "Desesperanza (HS)" = "HS_categoria")

agrupadores <- c("Sexo" = "Sexo",
                 "Zona geográfica" = "Zona",
                 "Nivel socioeconómico (INSE)" = "INSE_categoria",
                 "Modalidad de aislamiento" = "Aislamiento",
                 "Cambio en actividad física" = "Cambio_actividad_categoria")

# --- UI ---
ui <- dashboardPage(
  
  dashboardHeader(title = "Salud mental durante COVID-19",
                  titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    selectInput("indicador", "Indicador de salud mental:",
                choices = indicadores),
    selectInput("agrupador", "Cruzar con:",
                choices = agrupadores),
    uiOutput("selector_grupos"),
    sidebarMenu(
      menuItem("Comparación por grupo", tabName = "comparacion", icon = icon("chart-bar")),
      menuItem("Resumen y distribución", tabName = "resumen", icon = icon("table"))
    )
  ),
  
  dashboardBody(
      tags$head(
        tags$style(HTML("
        .main-header .logo {
          font-size: 18px;
          white-space: nowrap;
          overflow: visible;
        }
      "))
      ),
    
    tabItems(
      tabItem(tabName = "comparacion",
              fluidRow(
                box(width = 12, title = "Proporción del indicador en los grupos seleccionados",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("barras", height = "500px"))
              )
      ),
      
      tabItem(tabName = "resumen",
              fluidRow(
                box(width = 6, title = "Tabla resumen grupo vs indicador",
                    status = "primary", solidHeader = TRUE,
                    tableOutput("tabla_resumen")),
                box(width = 6, title = "Distribución general del indicador",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("distribucion_general", height = "400px"))
              )
      )
    )
  )
)

# --- Server ---
server <- function(input, output) {
  
  # Selector dinámico: los checkboxes cambian según la variable de agrupación elegida
  output$selector_grupos <- renderUI({
    niveles <- datos_limpio %>%
      pull(all_of(input$agrupador)) %>%
      levels()
    
    checkboxGroupInput("grupos_seleccionados", "Grupos a mostrar:",
                       choices = niveles, selected = niveles)
  })
  
  datos_filtrados <- reactive({
    req(input$grupos_seleccionados)
    
    datos_limpio %>%
      select(grupo = all_of(input$agrupador), categoria = all_of(input$indicador)) %>%
      filter(!is.na(grupo), !is.na(categoria),
             grupo %in% input$grupos_seleccionados) %>%
      mutate(grupo = fct_drop(grupo))
  })
  
  output$barras <- renderPlotly({
    p <- datos_filtrados() %>%
      count(grupo, categoria) %>%
      group_by(grupo) %>%
      mutate(proporcion = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = grupo, y = proporcion, fill = categoria)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = names(agrupadores)[agrupadores == input$agrupador],
           y = "Proporción de participantes",
           fill = names(indicadores)[indicadores == input$indicador]) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    ggplotly(p)
  })
  
  # Tabla centrada en los grupos: una fila por grupo, columnas = categorías del indicador
  output$tabla_resumen <- renderTable({
    base <- datos_filtrados()
    
    totales <- base %>%
      count(grupo, name = "Total")
    
    base %>%
      count(grupo, categoria) %>%
      group_by(grupo) %>%
      mutate(Porcentaje = round(100 * n / sum(n), 1)) %>%
      ungroup() %>%
      select(grupo, categoria, Porcentaje) %>%
      pivot_wider(names_from = categoria, values_from = Porcentaje, values_fill = 0) %>%
      left_join(totales, by = "grupo") %>%
      rename(!!names(agrupadores)[agrupadores == input$agrupador] := grupo)
  })
  
  output$distribucion_general <- renderPlotly({
    p <- datos_limpio %>%
      select(categoria = all_of(input$indicador)) %>%
      filter(!is.na(categoria)) %>%
      count(categoria) %>%
      mutate(Porcentaje = round(100 * n / sum(n), 1)) %>%
      ggplot(aes(x = categoria, y = n, fill = categoria)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.3) +
      labs(x = names(indicadores)[indicadores == input$indicador],
           y = "Cantidad de participantes") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui, server)