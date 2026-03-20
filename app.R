# ============================================================
# Veri Dağılım Dashboard'u
# Paketler: readxl, shiny, bslib, ggplot2, plotly,
#           reactable, dplyr, janitor, writexl
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

# ── UI ────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = "Veri Dağılım Dashboard'u",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  
  # -- Sidebar: yükleme & kontroller --
  sidebar = sidebar(
    width = 300,
    fileInput("file", "Excel Dosyası Yükle",
              accept = c(".xlsx", ".xls")),
    uiOutput("sheet_ui"),
    hr(),
    uiOutput("num_var_ui"),
    uiOutput("cat_var_ui"),
    hr(),
    sliderInput("bins", "Histogram Kutu Sayısı", 10, 100, 30, step = 5),
    hr(),
    downloadButton("download_clean", "Temiz Veriyi İndir (.xlsx)",
                   class = "btn-success w-100")
  ),
  
  # -- Ana panel: kartlar --
  layout_columns(
    col_widths = c(12),
    navset_card_tab(
      title = "Genel Bakış",
      
      # Sekme 1: Özet
      nav_panel("Özet",
                layout_columns(
                  col_widths = c(4, 4, 4),
                  value_box("Satır Sayısı",    textOutput("n_rows"),
                            showcase = icon("table-list"), theme = "primary"),
                  value_box("Sütun Sayısı",    textOutput("n_cols"),
                            showcase = icon("table-columns"), theme = "info"),
                  value_box("Eksik Değer (%)", textOutput("pct_na"),
                            showcase = icon("circle-exclamation"), theme = "warning")
                ),
                card(
                  card_header("Temizleme Raporu"),
                  verbatimTextOutput("clean_log")
                )
      ),
      
      # Sekme 2: Sayısal dağılım
      nav_panel("Sayısal Dağılım",
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header("Histogram"),
                       plotlyOutput("hist_plot", height = "400px")),
                  card(card_header("Boxplot"),
                       plotlyOutput("box_plot",  height = "400px"))
                ),
                card(
                  card_header("Özet İstatistikler"),
                  reactableOutput("num_summary")
                )
      ),
      
      # Sekme 3: Kategorik dağılım
      nav_panel("Kategorik Dağılım",
                layout_columns(
                  col_widths = c(6, 6),
                  card(card_header("Bar Grafiği"),
                       plotlyOutput("bar_plot",  height = "400px")),
                  card(card_header("Frekans Tablosu"),
                       reactableOutput("freq_table"))
                )
      ),
      
      # Sekme 4: Veri tablosu
      nav_panel("Veri Tablosu",
                card(
                  card_header("İnteraktif Tablo (tüm veriler)"),
                  reactableOutput("data_table")
                )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ---------- Dosya okuma & temizleme ----------
  raw_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath, sheet = input$sheet %||% 1)
  })
  
  sheets <- reactive({
    req(input$file)
    excel_sheets(input$file$datapath)
  })
  
  clean_data <- reactive({
    df <- raw_data()
    df %>%
      clean_names() %>%                        
      remove_empty(which = c("rows", "cols")) %>%
      mutate(across(where(is.character), trimws))
  })
  
  # ---------- Dinamik UI elemanları ----------
  output$sheet_ui <- renderUI({
    req(sheets())
    selectInput("sheet", "Sayfa Seçin", choices = sheets())
  })
  
  num_vars <- reactive({
    clean_data() %>% select(where(is.numeric)) %>% names()
  })
  
  cat_vars <- reactive({
    clean_data() %>% select(where(~ is.character(.) | is.factor(.))) %>% names()
  })
  
  output$num_var_ui <- renderUI({
    req(length(num_vars()) > 0)
    selectInput("num_var", "Sayısal Değişken", choices = num_vars())
  })
  
  output$cat_var_ui <- renderUI({
    req(length(cat_vars()) > 0)
    selectInput("cat_var", "Kategorik Değişken", choices = cat_vars())
  })
  
  # ---------- Özet kutular ----------
  output$n_rows <- renderText(nrow(clean_data()))
  output$n_cols <- renderText(ncol(clean_data()))
  output$pct_na <- renderText({
    pct <- mean(is.na(clean_data())) * 100
    sprintf("%.1f%%", pct)
  })
  
  output$clean_log <- renderPrint({
    raw <- raw_data()
    cln <- clean_data()
    cat("Orijinal boyut :", nrow(raw), "x", ncol(raw), "\n")
    cat("Temiz boyut    :", nrow(cln), "x", ncol(cln), "\n")
    cat("Silinen satır  :", nrow(raw) - nrow(cln), "\n")
    cat("Silinen sütun  :", ncol(raw) - ncol(cln), "\n")
    cat("Kalan NA sayısı:", sum(is.na(cln)), "\n")
    cat("\nSütun isimleri:\n")
    cat(paste(" ", names(cln), collapse = "\n"))
  })
  
  # ---------- Sayısal grafikler ----------
  output$hist_plot <- renderPlotly({
    req(input$num_var)
    p <- ggplot(clean_data(), aes(x = .data[[input$num_var]])) +
      geom_histogram(bins = input$bins, fill = "#2c3e50", color = "white") +
      labs(x = input$num_var, y = "Frekans") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$box_plot <- renderPlotly({
    req(input$num_var)
    p <- ggplot(clean_data(), aes(y = .data[[input$num_var]])) +
      geom_boxplot(fill = "#18bc9c", color = "#2c3e50", width = 0.4) +
      labs(y = input$num_var) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
  output$num_summary <- renderReactable({
    req(input$num_var)
    vals <- clean_data()[[input$num_var]]
    tibble(
      İstatistik = c("Ortalama", "Medyan", "Std Sapma",
                     "Min", "Q1", "Q3", "Max", "NA Sayısı"),
      Değer = c(
        round(mean(vals, na.rm = TRUE), 2),
        round(median(vals, na.rm = TRUE), 2),
        round(sd(vals, na.rm = TRUE), 2),
        round(min(vals, na.rm = TRUE), 2),
        round(quantile(vals, 0.25, na.rm = TRUE), 2),
        round(quantile(vals, 0.75, na.rm = TRUE), 2),
        round(max(vals, na.rm = TRUE), 2),
        sum(is.na(vals))
      )
    ) %>%
      reactable(
        compact   = TRUE,
        bordered  = TRUE,
        striped   = TRUE,
        fullWidth = FALSE,
        columns   = list(Değer = colDef(align = "right"))
      )
  })
  
  # ---------- Kategorik grafikler ----------
  output$bar_plot <- renderPlotly({
    req(input$cat_var)
    df_freq <- clean_data() %>%
      count(.data[[input$cat_var]], sort = TRUE) %>%
      head(20)
    
    p <- ggplot(df_freq, aes(
      x = reorder(.data[[input$cat_var]], n), y = n)) +
      geom_col(fill = "#e74c3c") +
      coord_flip() +
      labs(x = input$cat_var, y = "Frekans") +
      theme_minimal(base_size = 13)
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$freq_table <- renderReactable({
    req(input$cat_var)
    clean_data() %>%
      count(.data[[input$cat_var]], sort = TRUE, name = "Frekans") %>%
      mutate(Oran = sprintf("%.1f%%", Frekans / sum(Frekans) * 100)) %>%
      reactable(
        searchable = TRUE,
        compact     = TRUE,
        bordered    = TRUE,
        striped     = TRUE,
        defaultPageSize = 10,
        columns = list(
          Frekans = colDef(
            align = "right",
            cell  = function(value, index) {
              width <- paste0(value / max(.) * 100, "%")
              bar   <- div(style = list(
                background  = "#e74c3c33",
                width       = width,
                height      = "18px",
                borderRadius = "3px"
              ))
              div(style = list(display = "flex", alignItems = "center", gap = "8px"),
                  span(value), bar)
            }
          )
        )
      )
  })
  
  # ---------- Veri tablosu ----------
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
      defaultPageSize = 15,
      paginationType  = "jump"
    )
  })
  
  # ---------- Export ----------
  output$download_clean <- downloadHandler(
    filename = function() {
      paste0("temiz_veri_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      write_xlsx(clean_data(), file)
    }
  )
}

# ── Çalıştır ──────────────────────────────────────────────────
shinyApp(ui, server)