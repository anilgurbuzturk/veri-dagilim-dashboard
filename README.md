# Veri Dağılım Dashboard'u

Excel dosyasındaki verilerin dağılımlarını interaktif olarak inceleyen R Shiny dashboard'u.

## Özellikler

- **Excel yükle** → otomatik `clean_names()` + boş satır/sütun temizliği
- **Sayısal dağılım** → histogram (ayarlanabilir bin) + boxplot + özet istatistikler
- **Kategorik dağılım** → bar chart (top 20) + frekans/oran tablosu
- **Veri tablosu** → filtrelenebilir, sıralanabilir, aranabilir
- **Export** → temizlenmiş veriyi `.xlsx` olarak indir

## Proje Yapısı

```
my-shiny-dashboard/
├── app.R              # UI + server (ana giriş noktası)
├── R/
│   └── helpers.R      # Temizleme, istatistik, grafik fonksiyonları
├── www/               # Statik dosyalar (logo, CSS vb.)
├── data/              # Örnek Excel dosyaları
├── setup.R            # renv başlatma + manifest üretme scripti
├── renv.lock          # (setup.R çalıştıktan sonra oluşur)
└── manifest.json      # (setup.R çalıştıktan sonra oluşur)
```

## Kurulum

### 1. Paketleri kur (hızlı başlangıç)

```r
install.packages(c(
  "readxl", "shiny", "bslib",
  "ggplot2", "plotly", "reactable",
  "dplyr", "janitor", "writexl"
))
```

### 2. Lokalde çalıştır

```r
shiny::runApp()
```

### 3. renv ile sabitle + manifest üret (deploy için)

```r
source("setup.R")
```

Bu script sırasıyla:
1. `renv::init()` → `renv.lock` oluşturur
2. `rsconnect::writeManifest()` → `manifest.json` oluşturur

## Posit Connect'e Deploy

1. GitHub'a push et
2. Posit Connect → **New Content → From Git Repository**
3. Repo URL'sini yapıştır, branch: `main`, root: `/`
4. Deploy et → URL'ni al

> **Private repo?** Posit Connect'te Git hesabı / SSH key / GitHub App entegrasyonu gerekir.

## Paketler

| Paket | Amaç |
|---|---|
| `readxl` | Excel okuma |
| `shiny` + `bslib` | Dashboard altyapısı + Bootstrap 5 teması |
| `ggplot2` + `plotly` | Statik → interaktif grafikler |
| `reactable` | İnteraktif tablolar |
| `dplyr` + `janitor` | Veri temizleme / dönüştürme |
| `writexl` | Temiz veriyi Excel'e export |
