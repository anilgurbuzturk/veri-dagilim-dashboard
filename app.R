# ============================================================
# app.R — Veri Dağılım Dashboard'u (v3)
# ============================================================

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(plotly)
library(reactable)
library(writexl)
library(tidyr)
library(purrr)
library(scales)

source("R/helpers.R")

# ── UI ────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Veri Dağılım Dashboard'u",
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    base_font  = font_google("Inter")
  ),
  
  # value_box küçülme sorununu CSS ile sabitle
  tags$head(tags$style(HTML("
    .bslib-value-box { min-height: 120px !important; }
    .bslib-value-box .value-box-value { font-size: 2rem !important; }
    .bslib-value-box .value-box-title { font-size: 0.9rem !important; }
  "))),
  
  sidebar = sidebar(
    width = 300,
    fileInput("file", "Excel Dosyası Yükle",
              accept = c(".xlsx", ".xls")),
    uiOutput("sheet_ui"),
    hr(),
    h6("Çapraz Tablo Ayarları", class = "text-muted"),
    uiOutput("row_var_ui"),
    uiOutput("col_var_ui"),
    hr(),
    h6("İstatistik Sayfası İçin", class = "text-muted"),
    uiOutput("num_var_ui"),
    hr(),
    downloadButton("download_clean", "Temiz Veriyi İndir (.xlsx)",
                   class = "btn-success w-100")
  ),
  
  layout_columns(
    col_widths = c(12),
    navset_card_tab(
      title = "Analiz Paneli",
      
      # ── Sekme 1: Özet ──
      nav_panel("Özet",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  value_box("Satır Sayısı", textOutput("n_rows"),
                            showcase = icon("table-list"), theme = "primary"),
                  value_box("Sütun Sayısı", textOutput("n_cols"),
                            showcase = icon("table-columns"), theme = "info"),
                  value_box("Eksik Değer (%)", textOutput("pct_na"),
                            showcase = icon("circle-exclamation"), theme = "warning")
                ),
                card(card_header("Temizleme Raporu"),
                     verbatimTextOutput("clean_log"))
      ),
      
      # ── Sekme 2: Çapraz Tablo (adet + yüzde) ──
      nav_panel("Çapraz Tablo",
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header("Adet"),
                       reactableOutput("xtab_count")),
                  card(card_header("Satır Yüzdesi (%)"),
                       reactableOutput("xtab_pct"))
                )
      ),
      
      # ── Sekme 3: Detaylı İstatistik ──
      nav_panel("Detaylı İstatistik",
                card(
                  card_header(textOutput("stat_title_mean")),
                  reactableOutput("stat_mean")
                ),
                card(
                  card_header(textOutput("stat_title_median")),
                  reactableOutput("stat_median")
                ),
                card(
                  card_header(textOutput("stat_title_sd")),
                  reactableOutput("stat_sd")
                ),
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header(textOutput("stat_title_min")),
                       reactableOutput("stat_min")),
                  card(card_header(textOutput("stat_title_max")),
                       reactableOutput("stat_max"))
                )
      ),
      
      # ── Sekme 4: Görseller ──
      nav_panel("Görseller",
                card(card_header("Isı Haritası (Adet)"),
                     plotlyOutput("heatmap_plot", height = "420px")),
                card(card_header("Yığılmış Yüzde Bar"),
                     plotlyOutput("stacked_bar", height = "400px"))
      ),
      
      # ── Sekme 5: Veri Tablosu ──
      nav_panel("Veri Tablosu",
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "py-1",
                    span("İnteraktif Tablo", class = "fw-bold"),
                    span("— sağ alttaki", icon("expand"), "ile tam ekran yapabilirsiniz",
                         class = "text-muted small ms-2")
                  ),
                  reactableOutput("data_table")
                )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ---- Veri okuma & temizleme ----
  sheets <- reactive({
    req(input$file)
    excel_sheets(input$file$datapath)
  })
  
  raw_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath, sheet = input$sheet %||% 1)
  })
  
  clean_data <- reactive({
    req(input$file)
    clean_excel(input$file$datapath, sheet = input$sheet %||% 1)
  })
  
  # ---- Dinamik UI ----
  output$sheet_ui <- renderUI({
    req(sheets())
    selectInput("sheet", "Sayfa", choices = sheets())
  })
  
  cat_vars <- reactive(clean_data() |> select(where(~ is.character(.) | is.factor(.))) |> names())
  num_vars <- reactive(clean_data() |> select(where(is.numeric)) |> names())
  
  output$row_var_ui <- renderUI({
    req(length(cat_vars()) > 0)
    selectInput("row_var", "Satır Değişkeni", choices = cat_vars())
  })
  output$col_var_ui <- renderUI({
    req(length(cat_vars()) > 1)
    selectInput("col_var", "Sütun Değişkeni",
                choices  = cat_vars(),
                selected = cat_vars()[min(2, length(cat_vars()))])
  })
  output$num_var_ui <- renderUI({
    req(length(num_vars()) > 0)
    selectInput("num_var", "Sayısal Değişken", choices = num_vars())
  })
  
  # ---- Özet kutular ----
  output$n_rows <- renderText(nrow(clean_data()))
  output$n_cols <- renderText(ncol(clean_data()))
  output$pct_na <- renderText(sprintf("%.1f%%", mean(is.na(clean_data())) * 100))
  
  output$clean_log <- renderPrint({
    cat(cleaning_report(raw_data(), clean_data()))
  })
  
  # ---- Çapraz tablo stilleyici ----
  xtab_style <- function(df) {
    first_col <- names(df)[1]
    reactable(df,
              compact     = TRUE,
              bordered    = TRUE,
              striped     = TRUE,
              sortable    = TRUE,
              resizable   = TRUE,
              defaultColDef = colDef(
                align       = "center",
                headerStyle = list(fontWeight = "bold", background = "#f0f4f8",
                                   fontSize = "0.85rem")
              ),
              columns = stats::setNames(
                list(colDef(
                  align = "left",
                  style = function(value) {
                    if (value == "TOPLAM") {
                      list(fontWeight = "bold", background = "#e8f0fe")
                    } else {
                      list(fontWeight = "bold")
                    }
                  }
                )),
                first_col
              ),
              rowStyle = function(index) {
                if (index == nrow(df)) {
                  list(fontWeight = "bold", background = "#e8f0fe")
                }
              }
    )
  }
  
  # ---- Çapraz Tablo sekmesi ----
  output$xtab_count <- renderReactable({
    req(input$row_var, input$col_var)
    crosstab_count(clean_data(), input$row_var, input$col_var) |> xtab_style()
  })
  
  output$xtab_pct <- renderReactable({
    req(input$row_var, input$col_var)
    crosstab_pct(clean_data(), input$row_var, input$col_var) |> xtab_style()
  })
  
  # ---- Detaylı İstatistik sekmesi ----
  # Başlıklar
  output$stat_title_mean   <- renderText(paste0("Ortalama — ", input$num_var))
  output$stat_title_median <- renderText(paste0("Medyan — ", input$num_var))
  output$stat_title_sd     <- renderText(paste0("Standart Sapma — ", input$num_var))
  output$stat_title_min    <- renderText(paste0("Min — ", input$num_var))
  output$stat_title_max    <- renderText(paste0("Max — ", input$num_var))
  
  # Tablolar
  output$stat_mean <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, mean, "Ortalama") |> xtab_style()
  })
  
  output$stat_median <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, median, "Medyan") |> xtab_style()
  })
  
  output$stat_sd <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, sd, "Std Sapma") |> xtab_style()
  })
  
  output$stat_min <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, min, "Min") |> xtab_style()
  })
  
  output$stat_max <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, max, "Max") |> xtab_style()
  })
  
  # ---- Görseller ----
  output$heatmap_plot <- renderPlotly({
    req(input$row_var, input$col_var)
    ggplotly(build_heatmap(clean_data(), input$row_var, input$col_var))
  })
  
  output$stacked_bar <- renderPlotly({
    req(input$row_var, input$col_var)
    ggplotly(build_stacked_pct_bar(clean_data(), input$row_var, input$col_var))
  })
  
  # ---- Veri tablosu (compact header + full_screen) ----
  output$data_table <- renderReactable({
    reactable(
      clean_data(),
      searchable      = TRUE,
      filterable      = TRUE,
      sortable        = TRUE,
      resizable       = TRUE,
      compact         = TRUE,
      bordered        = TRUE,
      striped         = TRUE,
      highlight       = TRUE,
      defaultPageSize = 25,
      paginationType  = "jump",
      style           = list(fontSize = "0.82rem"),
      defaultColDef   = colDef(
        headerStyle = list(
          fontSize   = "0.78rem",
          fontWeight = "bold",
          padding    = "4px 8px"
        ),
        style = list(padding = "2px 8px")
      )
    )
  })
  
  # ---- Export ----
  output$download_clean <- downloadHandler(
    filename = function() paste0("temiz_veri_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) write_xlsx(clean_data(), file)
  )
}

shinyApp(ui, server)
