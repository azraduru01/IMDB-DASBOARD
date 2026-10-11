# IMDB Dashboard kurulum notları

Mevcut README.md R uygulama kodu içeriyordu. Aynı kod app.R dosyasına da eklendi; README korunmuştur.

Gereksinimler: R, tercihen RStudio; shiny, shinydashboard, tidyverse, readxl, plotly, DT paketleri.

R konsolunda:

```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "readxl", "plotly", "DT"))
shiny::runApp(".")
```

Önce çalışma dizinini proje köküne alın ve özgün IMDB_TABLE.xlsx dosyasını bu klasöre yerleştirin. Dosya GitHub deposunda bulunmuyor; yalnızca kodu indirerek uygulama çalışmaz. Uygulama Excel'in ilk satırını atlayarak okur ve kodda belirtilen kolonları bekler. Rastgele bir veri dosyasıyla değiştirmeyin.

Veri dosyasını ve kullanılan R/paket sürümlerini ayrıca yedekleyin. Veri paylaşım iznini doğrulamadan herkese açık depoya yüklemeyin. Kurulum, R sözdizimi ve çalıştırma bu denetimde test edilmedi; paket sürümleri henüz kilitlenmedi.
