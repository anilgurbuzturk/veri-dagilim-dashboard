# ============================================================
# app.R — Veri Dağılım Dashboard'u (v2 – Çapraz Tablo Odaklı)
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

  sidebar = sidebar(
    width = 300,
    fileInput("file", "Excel Dosyası Yükle",
              accept = c(".xlsx", ".xls")),
    uiOutput("sheet_ui"),
    hr(),
    h6("Çapraz Tablo Ayarları", class = "text-muted"),
    uiOutput("row_var_ui"),
    uiOutput("col_var_ui"),
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
          value_box("Satır",   textOutput("n_rows"),
                    showcase = icon("table-list"), theme = "primary"),
          value_box("Sütun",   textOutput("n_cols"),
                    showcase = icon("table-columns"), theme = "info"),
          value_box("Eksik %", textOutput("pct_na"),
                    showcase = icon("circle-exclamation"), theme = "warning")
        ),
        card(card_header("Temizleme Raporu"),
             verbatimTextOutput("clean_log"))
      ),

      # ── Sekme 2: Çapraz Tablolar ──
      nav_panel("Çapraz Tablo",
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Adet (Satır × Sütun)"),
               reactableOutput("xtab_count")),
          card(card_header("Satır Yüzdesi (%)"),
               reactableOutput("xtab_pct"))
        ),
        card(
          card_header(textOutput("xtab_mean_title")),
          reactableOutput("xtab_mean")
        )
      ),

      # ── Sekme 3: Görseller ──
      nav_panel("Görseller",
        layout_columns(
          col_widths = c(12),
          card(card_header("Isı Haritası (Adet)"),
               plotlyOutput("heatmap_plot", height = "400px"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Gruplu Bar"),
               plotlyOutput("grouped_bar", height = "400px")),
          card(card_header("Yığılmış Yüzde Bar"),
               plotlyOutput("stacked_bar", height = "400px"))
        )
      ),

      # ── Sekme 4: Sayısal Özet ──
      nav_panel("Sayısal Özet",
        card(
          card_header("Tüm Sayısal Değişkenler — Özet İstatistikler"),
          reactableOutput("num_summary")
        )
      ),

      # ── Sekme 5: Veri Tablosu ──
      nav_panel("Veri Tablosu",
        card(
          card_header("İnteraktif Tablo"),
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

  cat_vars <- reactive({
    clean_data() |> select(where(~ is.character(.) | is.factor(.))) |> names()
  })
  num_vars <- reactive({
    clean_data() |> select(where(is.numeric)) |> names()
  })

  output$row_var_ui <- renderUI({
    req(length(cat_vars()) > 0)
    selectInput("row_var", "Satır Değişkeni", choices = cat_vars())
  })
  output$col_var_ui <- renderUI({
    req(length(cat_vars()) > 1)
    # varsayılan olarak ikinci kategorik değişkeni seç
    selectInput("col_var", "Sütun Değişkeni",
                choices  = cat_vars(),
                selected = cat_vars()[min(2, length(cat_vars()))])
  })
  output$num_var_ui <- renderUI({
    req(length(num_vars()) > 0)
    selectInput("num_var", "Ortalama Hesaplanacak Değişken", choices = num_vars())
  })

  # ---- Özet kutular ----
  output$n_rows <- renderText(nrow(clean_data()))
  output$n_cols <- renderText(ncol(clean_data()))
  output$pct_na <- renderText(sprintf("%.1f%%", mean(is.na(clean_data())) * 100))

  output$clean_log <- renderPrint({
    cat(cleaning_report(raw_data(), clean_data()))
  })

  # ---- Çapraz tablolar ----
  xtab_style <- function(df) {
    reactable(df,
              compact = TRUE, bordered = TRUE, striped = TRUE,
              sortable = TRUE, resizable = TRUE,
              defaultColDef = colDef(
                align = "center",
                headerStyle = list(fontWeight = "bold", background = "#f0f4f8")
              ),
              columns = setNames(
                list(colDef(align = "left", style = list(fontWeight = "bold"))),
                names(df)[1]
              )
    )
  }

  output$xtab_count <- renderReactable({
    req(input$row_var, input$col_var)
    crosstab_count(clean_data(), input$row_var, input$col_var) |> xtab_style()
  })

  output$xtab_pct <- renderReactable({
    req(input$row_var, input$col_var)
    crosstab_pct(clean_data(), input$row_var, input$col_var) |> xtab_style()
  })

  output$xtab_mean_title <- renderText({
    req(input$num_var)
    paste0("Ortalama: ", input$num_var, " (Satır × Sütun)")
  })

  output$xtab_mean <- renderReactable({
    req(input$row_var, input$col_var, input$num_var)
    crosstab_mean(clean_data(), input$row_var, input$col_var, input$num_var) |>
      xtab_style()
  })

  # ---- Görseller ----
  output$heatmap_plot <- renderPlotly({
    req(input$row_var, input$col_var)
    ggplotly(build_heatmap(clean_data(), input$row_var, input$col_var),
             tooltip = c("x", "y", "fill"))
  })

  output$grouped_bar <- renderPlotly({
    req(input$row_var, input$col_var)
    ggplotly(build_grouped_bar(clean_data(), input$row_var, input$col_var),
             tooltip = c("x", "y", "fill"))
  })

  output$stacked_bar <- renderPlotly({
    req(input$row_var, input$col_var)
    ggplotly(build_stacked_pct_bar(clean_data(), input$row_var, input$col_var),
             tooltip = c("x", "y", "fill"))
  })

  # ---- Sayısal özet ----
  output$num_summary <- renderReactable({
    numeric_summary_all(clean_data()) |>
      reactable(
        compact   = TRUE,
        bordered  = TRUE,
        striped   = TRUE,
        sortable  = TRUE,
        resizable = TRUE,
        defaultColDef = colDef(
          align = "right",
          headerStyle = list(fontWeight = "bold", background = "#f0f4f8")
        ),
        columns = list(
          Degisken = colDef(align = "left", style = list(fontWeight = "bold"))
        )
      )
  })

  # ---- Veri tablosu ----
  output$data_table <- renderReactable({
    reactable(clean_data(),
              searchable = TRUE, filterable = TRUE, sortable = TRUE,
              resizable  = TRUE, compact = TRUE, bordered = TRUE,
              striped = TRUE, highlight = TRUE,
              defaultPageSize = 20, paginationType = "jump")
  })

  # ---- Export ----
  output$download_clean <- downloadHandler(
    filename = function() paste0("temiz_veri_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content  = function(file) write_xlsx(clean_data(), file)
  )
}

# ── Çalıştır ──────────────────────────────────────────────────
shinyApp(ui, server)
