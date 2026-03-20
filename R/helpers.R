# ============================================================
# R/helpers.R — v3
# ============================================================

clean_excel <- function(path, sheet = 1) {
  readxl::read_excel(path, sheet = sheet) |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    dplyr::mutate(dplyr::across(where(is.character), trimws))
}

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

# ── Çapraz Tablolar ──────────────────────────────────────────

crosstab_count <- function(df, row_var, col_var) {
  tbl <- table(df[[row_var]], df[[col_var]], useNA = "no")
  res <- as.data.frame.matrix(tbl) |>
    tibble::rownames_to_column(var = row_var)
  # TOPLAM sütunu
  numeric_cols <- setdiff(names(res), row_var)
  res$TOPLAM <- rowSums(res[numeric_cols])
  # TOPLAM satırı (şablon yaklaşımı)
  total_row <- res[1, , drop = FALSE]
  total_row[1, 1] <- "TOPLAM"
  for (nm in c(numeric_cols, "TOPLAM")) {
    total_row[1, nm] <- sum(res[[nm]])
  }
  rbind(res, total_row)
}

crosstab_pct <- function(df, row_var, col_var) {
  tbl <- table(df[[row_var]], df[[col_var]], useNA = "no")
  pct <- prop.table(tbl, margin = 1) * 100
  res <- as.data.frame.matrix(round(pct, 1)) |>
    tibble::rownames_to_column(var = row_var)
  # TOPLAM satırı: genel dağılım yüzdesi
  col_totals <- colSums(tbl)
  total_pct  <- round(col_totals / sum(col_totals) * 100, 1)
  total_row  <- res[1, , drop = FALSE]  # şablon satır al
  total_row[1, 1] <- "TOPLAM"
  for (i in seq_along(total_pct)) {
    total_row[1, i + 1] <- total_pct[i]
  }
  rbind(res, total_row)
}

# ── Detaylı Çapraz İstatistik ────────────────────────────────

crosstab_stat <- function(df, row_var, col_var, num_var, fun, fun_name) {
  agg <- stats::aggregate(
    stats::as.formula(paste(num_var, "~", row_var, "+", col_var)),
    data = df, FUN = fun, na.rm = TRUE
  )
  wide <- tidyr::pivot_wider(
    agg,
    names_from  = dplyr::all_of(col_var),
    values_from = dplyr::all_of(num_var)
  )
  # TOPLAM sütunu: grup bazında istatistik
  grp_agg <- stats::aggregate(
    stats::as.formula(paste(num_var, "~", row_var)),
    data = df, FUN = fun, na.rm = TRUE
  )
  names(grp_agg)[2] <- "TOPLAM"
  wide <- dplyr::left_join(wide, grp_agg, by = row_var)
  wide <- wide |> dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 2)))
  # TOPLAM satırı: sütun bazında istatistik (şablon yaklaşımı)
  col_agg <- stats::aggregate(
    stats::as.formula(paste(num_var, "~", col_var)),
    data = df, FUN = fun, na.rm = TRUE
  )
  total_vals <- stats::setNames(col_agg[[2]], col_agg[[1]])
  overall    <- fun(df[[num_var]], na.rm = TRUE)
  total_row  <- wide[1, , drop = FALSE]
  total_row[1, 1] <- "TOPLAM"
  for (nm in names(wide)[-1]) {
    if (nm == "TOPLAM") {
      total_row[1, nm] <- round(overall, 2)
    } else {
      total_row[1, nm] <- round(total_vals[nm], 2)
    }
  }
  rbind(wide, total_row)
}

# ── Görseller ─────────────────────────────────────────────────

build_heatmap <- function(df, row_var, col_var) {
  tbl <- as.data.frame(table(df[[row_var]], df[[col_var]], useNA = "no"))
  names(tbl) <- c("Satir", "Sutun", "Deger")
  
  ggplot2::ggplot(tbl, ggplot2::aes(x = Sutun, y = Satir, fill = Deger)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = Deger),
                       size = 5, fontface = "bold", color = "white") +
    ggplot2::scale_fill_gradient(low = "#64b5f6", high = "#0d47a1",
                                 name = "Adet") +
    ggplot2::labs(x = col_var, y = row_var) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid      = ggplot2::element_blank(),
      axis.text       = ggplot2::element_text(size = 12),
      legend.position = "right"
    )
}

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