# ============================================================
# R/helpers.R — Yardımcı fonksiyonlar (v2 – çapraz tablo odaklı)
# ============================================================

#' Excel verisini oku ve otomatik temizle
clean_excel <- function(path, sheet = 1) {
  readxl::read_excel(path, sheet = sheet) |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    dplyr::mutate(dplyr::across(where(is.character), trimws))
}

#' Temizleme raporu
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

#' Çapraz tablo: adet
crosstab_count <- function(df, row_var, col_var) {
  tbl <- table(df[[row_var]], df[[col_var]], useNA = "no")
  res <- as.data.frame.matrix(tbl) |>
    tibble::rownames_to_column(var = row_var)
  # toplam satırı ekle
  totals <- colSums(tbl)
  total_row <- c("TOPLAM", as.character(totals))
  names(total_row) <- names(res)
  res <- rbind(res, total_row)
  # toplam sütunu ekle
  numeric_cols <- setdiff(names(res), row_var)
  res[numeric_cols] <- lapply(res[numeric_cols], as.numeric)
  res$TOPLAM <- rowSums(res[numeric_cols])
  res
}

#' Çapraz tablo: satır yüzdesi
crosstab_pct <- function(df, row_var, col_var) {
  tbl <- table(df[[row_var]], df[[col_var]], useNA = "no")
  pct <- prop.table(tbl, margin = 1) * 100
  as.data.frame.matrix(round(pct, 1)) |>
    tibble::rownames_to_column(var = row_var)
}

#' Çapraz tablo: sayısal değişkenin ortalaması
crosstab_mean <- function(df, row_var, col_var, num_var) {
  agg <- stats::aggregate(
    stats::as.formula(paste(num_var, "~", row_var, "+", col_var)),
    data = df, FUN = mean, na.rm = TRUE
  )
  wide <- tidyr::pivot_wider(
    agg,
    names_from  = dplyr::all_of(col_var),
    values_from = dplyr::all_of(num_var)
  )
  wide |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
}

#' Tüm sayısal değişkenler için özet istatistikler
numeric_summary_all <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) == 0) return(dplyr::tibble(Degisken = character()))

  purrr::map_dfr(num_cols, function(col) {
    vals <- df[[col]]
    dplyr::tibble(
      Degisken  = col,
      N         = sum(!is.na(vals)),
      NA_Sayisi = sum(is.na(vals)),
      Ortalama  = round(mean(vals, na.rm = TRUE), 2),
      Medyan    = round(median(vals, na.rm = TRUE), 2),
      Std_Sapma = round(sd(vals, na.rm = TRUE), 2),
      Min       = round(min(vals, na.rm = TRUE), 2),
      Q1        = round(quantile(vals, 0.25, na.rm = TRUE), 2),
      Q3        = round(quantile(vals, 0.75, na.rm = TRUE), 2),
      Max       = round(max(vals, na.rm = TRUE), 2)
    )
  })
}

#' Çapraz tablo ısı haritası
build_heatmap <- function(df, row_var, col_var, fill_label = "Adet") {
  tbl <- as.data.frame(table(df[[row_var]], df[[col_var]], useNA = "no"))
  names(tbl) <- c("Satir", "Sutun", "Deger")

  ggplot2::ggplot(tbl, ggplot2::aes(x = Sutun, y = Satir, fill = Deger)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = Deger),
                       size = 5, fontface = "bold", color = "white") +
    ggplot2::scale_fill_gradient(low = "#64b5f6", high = "#0d47a1",
                                 name = fill_label) +
    ggplot2::labs(x = col_var, y = row_var) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(size = 12),
      legend.position  = "right"
    )
}

#' Gruplu bar chart
build_grouped_bar <- function(df, row_var, col_var) {
  tbl <- as.data.frame(table(df[[row_var]], df[[col_var]], useNA = "no"))
  names(tbl) <- c("Satir", "Sutun", "Adet")

  ggplot2::ggplot(tbl, ggplot2::aes(x = Satir, y = Adet, fill = Sutun)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = col_var) +
    ggplot2::labs(x = row_var, y = "Adet") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "top"
    )
}

#' Yığılmış yüzde bar chart
build_stacked_pct_bar <- function(df, row_var, col_var) {
  tbl <- as.data.frame(table(df[[row_var]], df[[col_var]], useNA = "no"))
  names(tbl) <- c("Satir", "Sutun", "Adet")

  ggplot2::ggplot(tbl, ggplot2::aes(x = Satir, y = Adet, fill = Sutun)) +
    ggplot2::geom_col(position = "fill", width = 0.7) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_fill_brewer(palette = "Set2", name = col_var) +
    ggplot2::labs(x = row_var, y = "Oran") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "top"
    )
}
