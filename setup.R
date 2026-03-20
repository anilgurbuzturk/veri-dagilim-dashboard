# ============================================================
# setup.R — renv sabitleme + rsconnect manifest üretimi
# Kullanım: RStudio'da proje açıkken source("setup.R")
# ============================================================

cat("── 1/3 rsconnect paketi kontrol ─────────────────────────\n")
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

cat("── 2/3 renv başlatılıyor ────────────────────────────────\n")
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# renv::init() projedeki library() çağrılarını tarar,
# renv.lock dosyasını oluşturur ve renv/library'ye kopyalar.
renv::init()

cat("\n✓ renv.lock oluşturuldu.\n")
cat("  Paket ekler/çıkarırsan: renv::snapshot()\n\n")

cat("── 3/3 manifest.json üretiliyor ─────────────────────────\n")
rsconnect::writeManifest()

cat("\n✓ manifest.json oluşturuldu.\n")
cat("  Artık git add + commit + push yapabilirsin.\n")
cat("  Posit Connect'te 'From Git Repository' ile deploy et.\n")
