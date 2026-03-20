# ============================================================
# app.R — Veri Dağılım Dashboard'u (v4)
# Opsiyonel çapraz tablo + ağırlık değişkeni desteği
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
library(scales)

source("R/helpers.R")

# ── UI ────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Veri Dağılım Dashboard'u",
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   base_font = font_google("Inter")),
  
  tags$head(tags$style(HTML("
    .bslib-value-box { min-height: 120px !important; }
    .bslib-value-box .value-box-value { font-size: 2rem !important; }
    .bslib-value-box .value-box-title { font-size: 0.9rem !important; }
    .tab-pane { overflow-y: auto !important; max-height: none !important; }
    .tab-pane .card { min-height: fit-content !important; overflow: visible !important; }
    .tab-pane .card .card-body { overflow: visible !important; }
    .reactable { min-height: 0 !important; }
  "))),
  
  sidebar = sidebar(
    width = 300,
    fileInput("file", "Excel Dosyası Yükle", accept = c(".xlsx", ".xls")),
    uiOutput("sheet_ui"),
    hr(),
    h6("Değişken Seçimi", class = "text-muted"),
    uiOutput("num_var_ui"),
    uiOutput("wt_var_ui"),
    hr(),
    h6("Çapraz Tablo (opsiyonel)", class = "text-muted"),
    uiOutput("row_var_ui"),
    uiOutput("col_var_ui"),
    hr(),
    downloadButton("download_clean", "Temiz Veriyi İndir (.xlsx)",
                   class = "btn-success w-100")
  ),
  
  layout_columns(
    col_widths = c(12),
    navset_card_tab(
      title = "Analiz Paneli",
      
      # ── Özet ──
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
      
      # ── Çapraz Tablo ──
      nav_panel("Çapraz Tablo",
                uiOutput("xtab_ui")
      ),
      
      # ── Detaylı İstatistik ──
      nav_panel("Detaylı İstatistik",
                uiOutput("stats_ui")
      ),
      
      # ── Görseller ──
      nav_panel("Görseller",
                uiOutput("visuals_ui")
      ),
      
      # ── Veri Tablosu ──
      nav_panel("Veri Tablosu",
                card(
                  full_screen = TRUE,
                  card_header(
                    class = "py-1",
                    span("İnteraktif Tablo", class = "fw-bold"),
                    span(HTML("&mdash; sağ alttaki "), icon("expand"),
                         " ile tam ekran yapabilirsiniz",
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
  
  # ---- Sabitler ----
  NONE <- NONE_LABEL
  
  # ---- Veri okuma & temizleme ----
  sheets <- reactive({ req(input$file); excel_sheets(input$file$datapath) })
  
  raw_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath, sheet = input$sheet %||% 1)
  })
  
  clean_data <- reactive({
    req(input$file)
    clean_excel(input$file$datapath, sheet = input$sheet %||% 1)
  })
  
  # ---- Yardımcı: seçilen değer "Seçilmedi" mi? ----
  sel <- function(val) { !is.null(val) && val != NONE }
  
  # aktif ağırlık
  active_wt <- reactive({ if (sel(input$wt_var)) input$wt_var else NULL })
  
  # ---- Dinamik UI ----
  output$sheet_ui <- renderUI({
    req(sheets())
    selectInput("sheet", "Sayfa", choices = sheets())
  })
  
  cat_vars <- reactive(clean_data() |> select(where(~ is.character(.) | is.factor(.))) |> names())
  num_vars <- reactive(clean_data() |> select(where(is.numeric)) |> names())
  
  output$num_var_ui <- renderUI({
    req(length(num_vars()) > 0)
    selectInput("num_var", "Sayısal Değişken", choices = num_vars())
  })
  
  output$wt_var_ui <- renderUI({
    choices <- c(NONE, num_vars())
    selectInput("wt_var", "Ağırlık Değişkeni (opsiyonel)", choices = choices, selected = NONE)
  })
  
  output$row_var_ui <- renderUI({
    choices <- c(NONE, cat_vars())
    selectInput("row_var", "Satır Değişkeni", choices = choices, selected = NONE)
  })
  
  output$col_var_ui <- renderUI({
    choices <- c(NONE, cat_vars())
    default <- if (length(cat_vars()) > 1) cat_vars()[2] else NONE
    selectInput("col_var", "Sütun Değişkeni", choices = choices, selected = default)
  })
  
  # ---- Özet kutular ----
  output$n_rows <- renderText(nrow(clean_data()))
  output$n_cols <- renderText(ncol(clean_data()))
  output$pct_na <- renderText(sprintf("%.1f%%", mean(is.na(clean_data())) * 100))
  output$clean_log <- renderPrint(cat(cleaning_report(raw_data(), clean_data())))
  
  # ---- Reactable stil ----
  xtab_style <- function(df) {
    first_col <- names(df)[1]
    reactable(df,
              compact = TRUE, bordered = TRUE, striped = TRUE,
              sortable = TRUE, resizable = TRUE,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(fontWeight = "bold", background = "#f0f4f8", fontSize = "0.85rem")
              ),
              columns = stats::setNames(
                list(colDef(align = "left",
                            style = function(value) {
                              if (identical(as.character(value), "TOPLAM"))
                                list(fontWeight = "bold", background = "#e8f0fe")
                              else list(fontWeight = "bold")
                            }
                )), first_col
              ),
              rowStyle = function(index) {
                if (index == nrow(df)) list(fontWeight = "bold", background = "#e8f0fe")
              }
    )
  }
  
  # ============================================================
  # ÇAPRAZ TABLO SEKMESİ
  # ============================================================
  output$xtab_ui <- renderUI({
    if (!sel(input$row_var) || !sel(input$col_var)) {
      card(
        card_header("Bilgi"),
        div(class = "p-4 text-muted",
            icon("info-circle", class = "me-2"),
            "Çapraz tablo görmek için sol panelden ",
            tags$strong("Satır"), " ve ", tags$strong("Sütun"),
            " değişkenlerini seçin.")
      )
    } else {
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Adet"), reactableOutput("xtab_count")),
          card(card_header("Satır Yüzdesi (%)"), reactableOutput("xtab_pct"))
        )
      )
    }
  })
  
  output$xtab_count <- renderReactable({
    req(sel(input$row_var), sel(input$col_var))
    crosstab_count(clean_data(), input$row_var, input$col_var, active_wt()) |> xtab_style()
  })
  
  output$xtab_pct <- renderReactable({
    req(sel(input$row_var), sel(input$col_var))
    crosstab_pct(clean_data(), input$row_var, input$col_var, active_wt()) |> xtab_style()
  })
  
  # ============================================================
  # DETAYLI İSTATİSTİK SEKMESİ
  # ============================================================
  output$stats_ui <- renderUI({
    req(input$num_var)
    has_row <- sel(input$row_var)
    has_col <- sel(input$col_var)
    wt_label <- if (sel(input$wt_var)) paste0(" (ağırlık: ", input$wt_var, ")") else ""
    
    if (!has_row && !has_col) {
      # ── Grupsuz: basit istatistik ──
      card(
        card_header(paste0("Özet İstatistik — ", input$num_var, wt_label)),
        reactableOutput("stat_simple")
      )
    } else if (has_row && !has_col) {
      # ── Tek boyutlu: gruba göre ──
      card(
        card_header(paste0("Gruba Göre İstatistik — ", input$num_var,
                           " × ", input$row_var, wt_label)),
        reactableOutput("stat_grouped")
      )
    } else {
      # ── İki boyutlu: çapraz ──
      tagList(
        card(card_header(paste0("Ortalama — ", input$num_var, wt_label)),
             reactableOutput("stat_mean")),
        card(card_header(paste0("Medyan — ", input$num_var, wt_label)),
             reactableOutput("stat_median")),
        card(card_header(paste0("Std Sapma — ", input$num_var, wt_label)),
             reactableOutput("stat_sd")),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header(paste0("Min — ", input$num_var)),
               reactableOutput("stat_min")),
          card(card_header(paste0("Max — ", input$num_var)),
               reactableOutput("stat_max"))
        )
      )
    }
  })
  
  # Basit istatistik
  output$stat_simple <- renderReactable({
    req(input$num_var)
    simple_stats(clean_data(), input$num_var, active_wt()) |>
      reactable(compact = TRUE, bordered = TRUE, striped = TRUE, fullWidth = FALSE,
                columns = list(Deger = colDef(align = "right")))
  })
  
  # Tek boyutlu gruplu
  output$stat_grouped <- renderReactable({
    req(input$num_var, sel(input$row_var))
    grouped_stats(clean_data(), input$num_var, input$row_var, active_wt()) |> xtab_style()
  })
  
  # Çapraz istatistikler
  output$stat_mean <- renderReactable({
    req(input$num_var, sel(input$row_var), sel(input$col_var))
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, w_mean, active_wt()) |> xtab_style()
  })
  output$stat_median <- renderReactable({
    req(input$num_var, sel(input$row_var), sel(input$col_var))
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, w_median, active_wt()) |> xtab_style()
  })
  output$stat_sd <- renderReactable({
    req(input$num_var, sel(input$row_var), sel(input$col_var))
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, w_sd, active_wt()) |> xtab_style()
  })
  output$stat_min <- renderReactable({
    req(input$num_var, sel(input$row_var), sel(input$col_var))
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, function(x, w) min(x, na.rm = TRUE), active_wt()) |> xtab_style()
  })
  output$stat_max <- renderReactable({
    req(input$num_var, sel(input$row_var), sel(input$col_var))
    crosstab_stat(clean_data(), input$row_var, input$col_var,
                  input$num_var, function(x, w) max(x, na.rm = TRUE), active_wt()) |> xtab_style()
  })
  
  # ============================================================
  # GÖRSELLER SEKMESİ
  # ============================================================
  output$visuals_ui <- renderUI({
    if (!sel(input$row_var) || !sel(input$col_var)) {
      card(
        card_header("Bilgi"),
        div(class = "p-4 text-muted",
            icon("info-circle", class = "me-2"),
            "Görseller için ", tags$strong("Satır"), " ve ",
            tags$strong("Sütun"), " değişkenlerini seçin.")
      )
    } else {
      tagList(
        card(card_header("Isı Haritası (Adet)"),
             plotlyOutput("heatmap_plot", height = "420px")),
        card(card_header("Yığılmış Yüzde Bar"),
             plotlyOutput("stacked_bar", height = "400px"))
      )
    }
  })
  
  output$heatmap_plot <- renderPlotly({
    req(sel(input$row_var), sel(input$col_var))
    ggplotly(build_heatmap(clean_data(), input$row_var, input$col_var))
  })
  output$stacked_bar <- renderPlotly({
    req(sel(input$row_var), sel(input$col_var))
    ggplotly(build_stacked_pct_bar(clean_data(), input$row_var, input$col_var))
  })
  
  # ============================================================
  # VERİ TABLOSU
  # ============================================================
  output$data_table <- renderReactable({
    reactable(clean_data(),
              searchable = TRUE, filterable = TRUE, sortable = TRUE,
              resizable = TRUE, compact = TRUE, bordered = TRUE,
              striped = TRUE, highlight = TRUE,
              defaultPageSize = 25, paginationType = "jump",
              style = list(fontSize = "0.82rem"),
              defaultColDef = colDef(
                headerStyle = list(fontSize = "0.78rem", fontWeight = "bold", padding = "4px 8px"),
                style = list(padding = "2px 8px")
              )
    )
  })
  
  # ============================================================
  # EXPORT
  # ============================================================
  output$download_clean <- downloadHandler(
    filename = function() paste0("temiz_veri_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) write_xlsx(clean_data(), file)
  )
}

shinyApp(ui, server)
