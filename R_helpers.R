# ============================================================
# R/helpers.R — v4 (opsiyonel çapraz + ağırlık desteği)
# ============================================================

NONE_LABEL <- "\u2014 Se\u00e7ilmedi \u2014"

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

# ── Ağırlıklı istatistik fonksiyonları ───────────────────────

w_mean <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) { ok <- ok & !is.na(w); x <- x[ok]; w <- w[ok]; sum(x * w) / sum(w) }
  else mean(x[ok])
}

w_median <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) {
    ok <- ok & !is.na(w); x <- x[ok]; w <- w[ok]
    ord <- order(x); x <- x[ord]; w <- w[ord]
    cs <- cumsum(w); x[which(cs >= sum(w) / 2)[1]]
  } else median(x[ok])
}

w_sd <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) {
    ok <- ok & !is.na(w); x <- x[ok]; w <- w[ok]
    mu <- sum(x * w) / sum(w)
    sqrt(sum(w * (x - mu)^2) / (sum(w) - 1))
  } else sd(x[ok])
}

w_n <- function(x, w = NULL) {
  ok <- !is.na(x)
  if (!is.null(w)) { ok <- ok & !is.na(w); round(sum(w[ok]), 1) }
  else sum(ok)
}

# ── Basit (tek değişken) istatistik ──────────────────────────

simple_stats <- function(df, num_var, wt_var = NULL) {
  x <- df[[num_var]]
  w <- if (!is.null(wt_var)) df[[wt_var]] else NULL
  dplyr::tibble(
    Istatistik = c("N", "Ortalama", "Medyan", "Std Sapma", "Min", "Q1", "Q3", "Max"),
    Deger = c(
      w_n(x, w),
      round(w_mean(x, w), 2),
      round(w_median(x, w), 2),
      round(w_sd(x, w), 2),
      round(min(x, na.rm = TRUE), 2),
      round(quantile(x, 0.25, na.rm = TRUE), 2),
      round(quantile(x, 0.75, na.rm = TRUE), 2),
      round(max(x, na.rm = TRUE), 2)
    )
  )
}

# ── Tek boyutlu gruplu istatistik ────────────────────────────

grouped_stats <- function(df, num_var, group_var, wt_var = NULL) {
  groups <- sort(unique(df[[group_var]]))
  rows <- lapply(groups, function(g) {
    sub <- df[df[[group_var]] == g, , drop = FALSE]
    x <- sub[[num_var]]
    w <- if (!is.null(wt_var)) sub[[wt_var]] else NULL
    data.frame(
      Grup      = g,
      N         = w_n(x, w),
      Ortalama  = round(w_mean(x, w), 2),
      Medyan    = round(w_median(x, w), 2),
      Std_Sapma = round(w_sd(x, w), 2),
      Min       = round(min(x, na.rm = TRUE), 2),
      Max       = round(max(x, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
  })
  res <- do.call(rbind, rows)
  # TOPLAM satırı
  x_all <- df[[num_var]]
  w_all <- if (!is.null(wt_var)) df[[wt_var]] else NULL
  total <- data.frame(
    Grup      = "TOPLAM",
    N         = w_n(x_all, w_all),
    Ortalama  = round(w_mean(x_all, w_all), 2),
    Medyan    = round(w_median(x_all, w_all), 2),
    Std_Sapma = round(w_sd(x_all, w_all), 2),
    Min       = round(min(x_all, na.rm = TRUE), 2),
    Max       = round(max(x_all, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
  names(res)[1] <- group_var
  names(total)[1] <- group_var
  rbind(res, total)
}

# ── Çapraz tablo istatistik ──────────────────────────────────

crosstab_stat <- function(df, row_var, col_var, num_var, fun, wt_var = NULL) {
  rows_vals <- sort(unique(df[[row_var]]))
  cols_vals <- sort(unique(df[[col_var]]))

  mat <- matrix(NA_real_, nrow = length(rows_vals), ncol = length(cols_vals))
  for (i in seq_along(rows_vals)) {
    for (j in seq_along(cols_vals)) {
      sub <- df[df[[row_var]] == rows_vals[i] & df[[col_var]] == cols_vals[j], , drop = FALSE]
      if (nrow(sub) > 0) {
        x <- sub[[num_var]]
        w <- if (!is.null(wt_var)) sub[[wt_var]] else NULL
        mat[i, j] <- round(fun(x, w), 2)
      }
    }
  }
  res <- as.data.frame(mat)
  names(res) <- cols_vals
  res <- cbind(setNames(data.frame(rows_vals, stringsAsFactors = FALSE), row_var), res)

  # TOPLAM sütunu (satır bazında)
  res$TOPLAM <- sapply(rows_vals, function(rv) {
    sub <- df[df[[row_var]] == rv, , drop = FALSE]
    x <- sub[[num_var]]; w <- if (!is.null(wt_var)) sub[[wt_var]] else NULL
    round(fun(x, w), 2)
  })

  # TOPLAM satırı (sütun bazında)
  total_row <- res[1, , drop = FALSE]
  total_row[1, 1] <- "TOPLAM"
  for (cv in cols_vals) {
    sub <- df[df[[col_var]] == cv, , drop = FALSE]
    x <- sub[[num_var]]; w <- if (!is.null(wt_var)) sub[[wt_var]] else NULL
    total_row[1, cv] <- round(fun(x, w), 2)
  }
  # genel toplam
  x_all <- df[[num_var]]; w_all <- if (!is.null(wt_var)) df[[wt_var]] else NULL
  total_row[1, "TOPLAM"] <- round(fun(x_all, w_all), 2)
  rbind(res, total_row)
}

# ── Çapraz tablo: adet ───────────────────────────────────────

crosstab_count <- function(df, row_var, col_var, wt_var = NULL) {
  if (is.null(wt_var)) {
    tbl <- table(df[[row_var]], df[[col_var]], useNA = "no")
    res <- as.data.frame.matrix(tbl) |>
      tibble::rownames_to_column(var = row_var)
    numeric_cols <- setdiff(names(res), row_var)
    res$TOPLAM <- rowSums(res[numeric_cols])
  } else {
    # ağırlıklı frekans
    agg <- stats::aggregate(
      stats::as.formula(paste(wt_var, "~", row_var, "+", col_var)),
      data = df, FUN = sum, na.rm = TRUE
    )
    wide <- tidyr::pivot_wider(agg, names_from = dplyr::all_of(col_var),
                               values_from = dplyr::all_of(wt_var), values_fill = 0)
    res <- as.data.frame(wide)
    numeric_cols <- setdiff(names(res), row_var)
    res[numeric_cols] <- lapply(res[numeric_cols], function(x) round(x, 1))
    res$TOPLAM <- round(rowSums(res[numeric_cols]), 1)
  }
  total_row <- res[1, , drop = FALSE]
  total_row[1, 1] <- "TOPLAM"
  for (nm in c(numeric_cols, "TOPLAM")) total_row[1, nm] <- sum(res[[nm]])
  rbind(res, total_row)
}

# ── Çapraz tablo: satır yüzdesi ──────────────────────────────

crosstab_pct <- function(df, row_var, col_var, wt_var = NULL) {
  ct <- crosstab_count(df, row_var, col_var, wt_var)
  # son satır (TOPLAM) dahil yüzdeye çevir
  numeric_cols <- setdiff(names(ct), c(row_var, "TOPLAM"))
  for (i in seq_len(nrow(ct))) {
    row_total <- ct$TOPLAM[i]
    if (row_total > 0) {
      for (nm in numeric_cols) {
        ct[i, nm] <- round(ct[i, nm] / row_total * 100, 1)
      }
    }
  }
  ct$TOPLAM <- NULL
  ct
}

# ── Görseller ─────────────────────────────────────────────────

build_heatmap <- function(df, row_var, col_var) {
  tbl <- as.data.frame(table(df[[row_var]], df[[col_var]], useNA = "no"))
  names(tbl) <- c("Satir", "Sutun", "Deger")
  ggplot2::ggplot(tbl, ggplot2::aes(x = Sutun, y = Satir, fill = Deger)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = Deger), size = 5, fontface = "bold", color = "white") +
    ggplot2::scale_fill_gradient(low = "#64b5f6", high = "#0d47a1", name = "Adet") +
    ggplot2::labs(x = col_var, y = row_var) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 12), legend.position = "right")
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
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   legend.position = "top")
}
