# ============================================================
# R/helpers.R
# Yardımcı fonksiyonlar — app.R tarafından source() ile yüklenir
# ============================================================

#' Excel verisini oku ve otomatik temizle
#' @param path Excel dosya yolu
#' @param sheet Sayfa adı veya numarası
#' @return Temizlenmiş tibble
clean_excel <- function(path, sheet = 1) {
  readxl::read_excel(path, sheet = sheet) |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    dplyr::mutate(dplyr::across(where(is.character), trimws))
}

#' Temizleme öncesi-sonrası rapor metni üret
#' @param raw Orijinal veri
#' @param clean Temizlenmiş veri
#' @return Karakter vektörü (cat ile basılabilir)
cleaning_report <- function(raw, clean) {
  paste0(
    "Orijinal boyut : ", nrow(raw), " x ", ncol(raw), "\n",
    "Temiz boyut    : ", nrow(clean), " x ", ncol(clean), "\n",
    "Silinen satir  : ", nrow(raw) - nrow(clean), "\n",
    "Silinen sutun  : ", ncol(raw) - ncol(clean), "\n",
    "Kalan NA sayisi: ", sum(is.na(clean)), "\n",
    "\nSutun isimleri:\n",
    paste("  ", names(clean), collapse = "\n")
  )
}

#' Sayısal değişken için özet istatistik tibble'ı
#' @param vals Sayısal vektör
#' @return Tek sütunlu tibble
numeric_summary <- function(vals) {
  dplyr::tibble(
    Istatistik = c("Ortalama", "Medyan", "Std Sapma",
                   "Min", "Q1", "Q3", "Max", "NA Sayisi"),
    Deger = c(
      round(mean(vals, na.rm = TRUE), 2),
      round(median(vals, na.rm = TRUE), 2),
      round(sd(vals, na.rm = TRUE), 2),
      round(min(vals, na.rm = TRUE), 2),
      round(quantile(vals, 0.25, na.rm = TRUE), 2),
      round(quantile(vals, 0.75, na.rm = TRUE), 2),
      round(max(vals, na.rm = TRUE), 2),
      sum(is.na(vals))
    )
  )
}

#' Kategorik değişken frekans tablosu
#' @param df Data frame
#' @param var_name Sütun adı (string)
#' @return Frekans + oran tibble'ı
freq_table <- function(df, var_name) {
  df |>
    dplyr::count(.data[[var_name]], sort = TRUE, name = "Frekans") |>
    dplyr::mutate(Oran = sprintf("%.1f%%", Frekans / sum(Frekans) * 100))
}

# ── Grafik oluşturucular ──────────────────────────────────────

#' Histogram (ggplot nesnesi döner, plotly'e çevrilmez)
build_histogram <- function(df, var_name, bins = 30) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +
    ggplot2::geom_histogram(bins = bins, fill = "#2c3e50", color = "white") +
    ggplot2::labs(x = var_name, y = "Frekans") +
    ggplot2::theme_minimal(base_size = 13)
}

#' Boxplot
build_boxplot <- function(df, var_name) {
  ggplot2::ggplot(df, ggplot2::aes(y = .data[[var_name]])) +
    ggplot2::geom_boxplot(fill = "#18bc9c", color = "#2c3e50", width = 0.4) +
    ggplot2::labs(y = var_name) +
    ggplot2::theme_minimal(base_size = 13)
}

#' Bar chart (ilk 20 kategori)
build_bar <- function(df, var_name) {
  df_freq <- df |>
    dplyr::count(.data[[var_name]], sort = TRUE) |>
    utils::head(20)
  
  ggplot2::ggplot(df_freq,
                  ggplot2::aes(x = reorder(.data[[var_name]], n), y = n)) +
    ggplot2::geom_col(fill = "#e74c3c") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = var_name, y = "Frekans") +
    ggplot2::theme_minimal(base_size = 13)
}
