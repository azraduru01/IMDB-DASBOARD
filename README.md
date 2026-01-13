library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(plotly)
library(DT)

# -----------------------------
# VERİYİ OKU
# -----------------------------
mydata <- read_excel("IMDB_TABLE.xlsx", skip = 1) %>%
  mutate(
    startYear = as.integer(startYear),
    rank = suppressWarnings(as.integer(rank)),
    averageRating = as.numeric(averageRating),
    numVotes = as.numeric(numVotes),
    runtimeMinutes = as.numeric(runtimeMinutes),
    genres = as.character(genres),
    directors = as.character(directors),
    writers = as.character(writers),
    primaryTitle = as.character(primaryTitle),
    Title_IMDb_Link = as.character(Title_IMDb_Link)
  )

file_date <- format(file.mtime("IMDB_TABLE.xlsx"), "%Y-%m-%d")
kaggle_url <- "https://www.kaggle.com/datasets/tiagoadrianunes/imdb-top-5000-movies"

genre_choices <- mydata %>%
  mutate(genres = ifelse(is.na(genres), "", genres)) %>%
  separate_rows(genres, sep = ",\\s*") %>%
  filter(genres != "") %>%
  distinct(genres) %>%
  arrange(genres) %>%
  pull(genres)

var_list <- tibble(
  Değişken = names(mydata),
  Tür = sapply(mydata, function(x) {
    if (is.numeric(x)) "Sayısal (numeric)"
    else if (is.integer(x)) "Sayısal (integer)"
    else "Kategorik / Metinsel"
  })
)

# -----------------------------
# UI
# -----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "IMDB Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-bar")),
      menuItem("Tablolar", tabName = "tables", icon = icon("table")),
      menuItem("References", tabName = "refs", icon = icon("book"))
    ),
    hr(),
    
    sliderInput(
      "year", "Yıl aralığı (startYear):",
      min = min(mydata$startYear, na.rm = TRUE),
      max = max(mydata$startYear, na.rm = TRUE),
      value = c(min(mydata$startYear, na.rm = TRUE), max(mydata$startYear, na.rm = TRUE)),
      step = 1
    ),
    
    sliderInput(
      "rating", "Rating aralığı (averageRating):",
      min = floor(min(mydata$averageRating, na.rm = TRUE) * 10) / 10,
      max = ceiling(max(mydata$averageRating, na.rm = TRUE) * 10) / 10,
      value = c(
        floor(min(mydata$averageRating, na.rm = TRUE) * 10) / 10,
        ceiling(max(mydata$averageRating, na.rm = TRUE) * 10) / 10
      ),
      step = 0.1
    ),
    
    sliderInput(
      "votes", "Oy sayısı aralığı (numVotes):",
      min = min(mydata$numVotes, na.rm = TRUE),
      max = max(mydata$numVotes, na.rm = TRUE),
      value = c(min(mydata$numVotes, na.rm = TRUE), max(mydata$numVotes, na.rm = TRUE))
    ),
    
    sliderInput(
      "runtime", "Süre aralığı (runtimeMinutes):",
      min = min(mydata$runtimeMinutes, na.rm = TRUE),
      max = max(mydata$runtimeMinutes, na.rm = TRUE),
      value = c(min(mydata$runtimeMinutes, na.rm = TRUE), max(mydata$runtimeMinutes, na.rm = TRUE))
    ),
    
    selectizeInput(
      "genre", "Tür seç (genres):",
      choices = genre_choices,
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = "Boş bırakırsan hepsi gelir", maxItems = 10)
    ),
    
    checkboxInput("color_by_genre", "Scatter: genre'a göre renklendir", value = TRUE),
    
    br(),
    actionButton("reset_all", "Tüm filtreleri sıfırla", icon = icon("redo")),
    br(), br(),
    
    tags$div(
      style = "padding:10px; font-size: 12px; color:#fff; opacity:0.9;",
      HTML(paste0(
        "<b>Veri:</b> IMDB_TABLE.xlsx<br>",
        "<b>Kaynak:</b> Kaggle<br>",
        "<b>Dosya tarihi:</b> ", file_date
      ))
    )
  ),
  
  dashboardBody(
    tabItems(
      # -------------------
      # 1) DASHBOARD
      # -------------------
      tabItem(
        tabName = "dash",
        fluidRow(
          valueBoxOutput("kpi_count"),
          valueBoxOutput("kpi_avg_rating"),
          valueBoxOutput("kpi_votes")
        ),
        
        fluidRow(
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Histogram: Average Rating Dağılımı",
            plotOutput("hist_rating", height = 320),
            tags$hr(style="margin:8px 0;"),
            uiOutput("note_hist")   # ✅ açıklama
          ),
          box(
            width = 6, status = "primary", solidHeader = TRUE,
            title = "Etkileşimli Scatter: Votes vs Rating",
            plotlyOutput("scatter_interactive", height = 320),
            tags$hr(style="margin:8px 0;"),
            uiOutput("note_scatter") # ✅ açıklama
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Çoklu Kutu Grafiği: Genre Gruplarına Göre Rating",
            plotOutput("boxplot_genre", height = 420),
            tags$hr(style="margin:8px 0;"),
            uiOutput("note_box")     # ✅ açıklama
          )
        )
      ),
      
      # -------------------
      # 2) TABLOLAR
      # -------------------
      tabItem(
        tabName = "tables",
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Değişken Listesi (İsim + Tür)",
            DTOutput("var_table")
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Veri Seti Özeti (Sayısal Değişkenler)",
            DTOutput("num_summary")
          )
        ),
        
        fluidRow(
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Kategorik Frekans: Genres (Top 15)",
            DTOutput("freq_genre")
          ),
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Kategorik Frekans: Directors (Top 15)",
            DTOutput("freq_director")
          ),
          box(
            width = 4, status = "primary", solidHeader = TRUE,
            title = "Kategorik Frekans: Writers (Top 15)",
            DTOutput("freq_writer")
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Filtrelenmiş Veri Tablosu",
            DTOutput("table_filtered")
          )
        ),
        
        fluidRow(
          box(
            width = 4, status = "warning", solidHeader = TRUE,
            title = "Ayrıştırma",
            selectInput(
              "split_field",
              "Hangi alan ayrıştırılsın?",
              choices = c("genres", "directors", "writers"),
              selected = "genres"
            ),
            helpText("Seçilen alan virgüllere göre satır satır ayrılır.")
          ),
          box(
            width = 8, status = "primary", solidHeader = TRUE,
            title = "Ayrıştırılmış Veri (Long)",
            DTOutput("table_split")
          )
        )
      ),
      
      # -------------------
      # 3) REFERENCES
      # -------------------
      tabItem(
        tabName = "refs",
        fluidRow(
          box(
            width = 12, status = "primary", solidHeader = TRUE,
            title = "Üst Veri Paneli",
            fluidRow(
              column(6, textInput("meta_dataset", "Öğrenci Adı", value = "Azra Duru Tekinel")),
              column(6, textInput("meta_author", "Veri setinin yazarı (varsa)", value = "Tiago Adrian Nunes (Kaggle)"))
            ),
            fluidRow(
              column(6, textInput("meta_student", "Öğrenci numarası", value = "2307071132")),
              column(6, dateInput("meta_date", "Tarih", value = Sys.Date()))
            ),
            hr(),
            uiOutput("meta_preview")
          )
        ),
        
        fluidRow(
          box(
            width = 12, status = "warning", solidHeader = TRUE,
            title = "Kaynakça / References",
            tags$ul(
              tags$li(HTML(paste0("Kaggle veri seti: <a href='", kaggle_url, "' target='_blank'>", kaggle_url, "</a>"))),
              tags$li("Paketler: shiny, shinydashboard, tidyverse (dplyr, tidyr, ggplot2), plotly, DT, readxl"),
              tags$li("Not: Bu dashboard, EDA (Exploratory Data Analysis) amacıyla hazırlanmıştır.")
            )
          )
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- mydata %>%
      filter(
        startYear >= input$year[1], startYear <= input$year[2],
        averageRating >= input$rating[1], averageRating <= input$rating[2],
        numVotes >= input$votes[1], numVotes <= input$votes[2],
        runtimeMinutes >= input$runtime[1], runtimeMinutes <= input$runtime[2]
      )
    
    if (!is.null(input$genre) && length(input$genre) > 0) {
      pattern <- paste0("(^|,\\s*)(", paste(input$genre, collapse = "|"), ")(,|$)")
      df <- df %>% filter(str_detect(genres, regex(pattern)))
    }
    df
  })
  
  # RESET
  observeEvent(input$reset_all, {
    updateSliderInput(session, "year",
                      value = c(min(mydata$startYear, na.rm = TRUE), max(mydata$startYear, na.rm = TRUE))
    )
    updateSliderInput(session, "rating",
                      value = c(
                        floor(min(mydata$averageRating, na.rm = TRUE) * 10) / 10,
                        ceiling(max(mydata$averageRating, na.rm = TRUE) * 10) / 10
                      )
    )
    updateSliderInput(session, "votes",
                      value = c(min(mydata$numVotes, na.rm = TRUE), max(mydata$numVotes, na.rm = TRUE))
    )
    updateSliderInput(session, "runtime",
                      value = c(min(mydata$runtimeMinutes, na.rm = TRUE), max(mydata$runtimeMinutes, na.rm = TRUE))
    )
    updateSelectizeInput(session, "genre", selected = character(0))
    updateCheckboxInput(session, "color_by_genre", value = TRUE)
  })
  
  # KPI
  output$kpi_count <- renderValueBox({
    valueBox(nrow(filtered_data()), "Kayıt sayısı", icon = icon("database"))
  })
  output$kpi_avg_rating <- renderValueBox({
    avg <- mean(filtered_data()$averageRating, na.rm = TRUE)
    valueBox(round(avg, 2), "Ortalama rating", icon = icon("star"))
  })
  output$kpi_votes <- renderValueBox({
    v <- sum(filtered_data()$numVotes, na.rm = TRUE)
    valueBox(format(v, big.mark = ","), "Toplam oy", icon = icon("thumbs-up"))
  })
  
  # Histogram
  output$hist_rating <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Bu filtrelerde veri yok."))
    ggplot(df, aes(x = averageRating)) +
      geom_histogram(bins = 20) +
      labs(x = "Average Rating", y = "Frekans") +
      theme_minimal()
  })
  
  # Scatter
  output$scatter_interactive <- renderPlotly({
    df <- filtered_data() %>% filter(!is.na(numVotes), !is.na(averageRating))
    validate(need(nrow(df) > 0, "Scatter için veri yok."))
    
    df <- df %>%
      mutate(first_genre = ifelse(
        is.na(genres) | genres == "",
        NA_character_,
        str_split(genres, ",\\s*", simplify = TRUE)[, 1]
      ))
    
    if (isTRUE(input$color_by_genre)) {
      p <- ggplot(df, aes(
        x = numVotes, y = averageRating, color = first_genre,
        text = paste0(
          "Title: ", primaryTitle,
          "<br>Year: ", startYear,
          "<br>Rating: ", averageRating,
          "<br>Votes: ", numVotes,
          "<br>Genre: ", first_genre
        )
      )) +
        geom_point(alpha = 0.7) +
        labs(x = "Number of Votes (log)", y = "Average Rating", color = "Genre") +
        theme_minimal()
    } else {
      p <- ggplot(df, aes(
        x = numVotes, y = averageRating,
        text = paste0(
          "Title: ", primaryTitle,
          "<br>Year: ", startYear,
          "<br>Rating: ", averageRating,
          "<br>Votes: ", numVotes
        )
      )) +
        geom_point(alpha = 0.7) +
        labs(x = "Number of Votes (log)", y = "Average Rating") +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(type = "log"))
  })
  
  # Boxplot
  output$boxplot_genre <- renderPlot({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Bu filtrelerde veri yok."))
    
    selected_genres <- input$genre
    if (is.null(selected_genres) || length(selected_genres) == 0) {
      selected_genres <- df %>%
        mutate(genres = ifelse(is.na(genres), "", genres)) %>%
        separate_rows(genres, sep = ",\\s*") %>%
        filter(genres != "") %>%
        count(genres, sort = TRUE) %>%
        slice_head(n = 6) %>%
        pull(genres)
    }
    
    box_df <- df %>%
      mutate(genres = ifelse(is.na(genres), "", genres)) %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres %in% selected_genres) %>%
      filter(!is.na(averageRating))
    
    validate(need(nrow(box_df) > 0, "Boxplot için uygun veri yok."))
    
    ggplot(box_df, aes(x = genres, y = averageRating, fill = genres)) +
      geom_boxplot(outlier.alpha = 0.3) +
      labs(x = "Genre", y = "Average Rating") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  # GRAFİK ALTI KISA AÇIKLAMALAR 
  output$note_hist <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(tags$div(style="color:#777;", "Bu filtrelerde veri yok."))
    
    m <- mean(df$averageRating, na.rm = TRUE)
    med <- median(df$averageRating, na.rm = TRUE)
    
    skew_note <- if (m > med) {
      "Ortalama medyandan büyük → dağılım sağa doğru kuyruklu olabilir."
    } else if (m < med) {
      "Ortalama medyandan küçük → dağılım sola doğru kuyruklu olabilir."
    } else {
      "Ortalama ve medyan benzer → dağılım daha dengeli görünüyor."
    }
    
    tags$div(
      tags$b("Kısa yorum: "),
      tags$ul(
        tags$li(paste0("Filtre sonrası ", nrow(df), " kayıt var.")),
        tags$li(paste0("Ortalama rating: ", round(m, 2), " | Medyan: ", round(med, 2))),
        tags$li(skew_note),
        tags$li("Bu grafik, rating değerlerinin genel yoğunlaştığı aralığı hızlıca görmeye yarar.")
      )
    )
  })
  
  output$note_scatter <- renderUI({
    df <- filtered_data() %>% filter(!is.na(numVotes), !is.na(averageRating), numVotes > 0)
    if (nrow(df) < 5) return(tags$div(style="color:#777;", "Yorum için yeterli nokta yok (çok az veri)."))
    
    corr <- suppressWarnings(cor(log10(df$numVotes), df$averageRating, use = "complete.obs"))
    
    corr_txt <- if (is.na(corr)) {
      "Korelasyon hesaplanamadı."
    } else if (abs(corr) < 0.2) {
      paste0("log(votes) ile rating arasında zayıf ilişki var (r≈", round(corr, 2), ").")
    } else if (abs(corr) < 0.5) {
      paste0("log(votes) ile rating arasında orta düzey ilişki var (r≈", round(corr, 2), ").")
    } else {
      paste0("log(votes) ile rating arasında güçlü ilişki var (r≈", round(corr, 2), ").")
    }
    
    tags$div(
      tags$b("Kısa yorum: "),
      tags$ul(
        tags$li("X ekseni log ölçeklidir; çok büyük oy sayıları daha okunur olur."),
        tags$li(corr_txt),
        tags$li("Noktaları genre’a göre renklendirmek türler arasında olası kümelenmeleri gösterir.")
      )
    )
  })
  
  output$note_box <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(tags$div(style="color:#777;", "Bu filtrelerde veri yok."))
    
    selected_genres <- input$genre
    if (is.null(selected_genres) || length(selected_genres) == 0) {
      selected_genres <- df %>%
        mutate(genres = ifelse(is.na(genres), "", genres)) %>%
        separate_rows(genres, sep = ",\\s*") %>%
        filter(genres != "") %>%
        count(genres, sort = TRUE) %>%
        slice_head(n = 6) %>%
        pull(genres)
    }
    
    box_df <- df %>%
      mutate(genres = ifelse(is.na(genres), "", genres)) %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres %in% selected_genres) %>%
      filter(!is.na(averageRating))
    
    if (nrow(box_df) == 0) return(tags$div(style="color:#777;", "Seçilen genre'larda boxplot için veri yok."))
    
    med_tbl <- box_df %>%
      group_by(genres) %>%
      summarise(med = median(averageRating, na.rm = TRUE), n = n(), .groups="drop") %>%
      arrange(desc(med))
    
    best <- med_tbl$genres[1]
    best_med <- med_tbl$med[1]
    
    tags$div(
      tags$b("Kısa yorum: "),
      tags$ul(
        tags$li("Kutu grafiği; medyanı, yayılımı (IQR) ve aykırı değerleri aynı anda gösterir."),
        tags$li(paste0("Filtreye göre en yüksek medyan rating: ", best, " (medyan≈", round(best_med, 2), ").")),
        tags$li("Kutu/çubukların genişliği arttıkça, tür içi değişkenlik (dağılım) artar.")
      )
    )
  })
  
  # TABLOLAR
  output$var_table <- renderDT({
    datatable(var_list, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$num_summary <- renderDT({
    df <- filtered_data()
    validate(need(nrow(df) > 0, "Bu filtrelerde veri yok."))
    
    num_vars <- df %>% select(where(is.numeric))
    tbl <- tibble(
      Değişken = names(num_vars),
      Ortalama = sapply(num_vars, \(x) mean(x, na.rm = TRUE)),
      Medyan = sapply(num_vars, \(x) median(x, na.rm = TRUE)),
      Std_Sapma = sapply(num_vars, \(x) sd(x, na.rm = TRUE)),
      Minimum = sapply(num_vars, \(x) min(x, na.rm = TRUE)),
      Maksimum = sapply(num_vars, \(x) max(x, na.rm = TRUE))
    )
    datatable(tbl, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$freq_genre <- renderDT({
    df <- filtered_data() %>%
      mutate(genres = ifelse(is.na(genres), "", genres)) %>%
      separate_rows(genres, sep = ",\\s*") %>%
      filter(genres != "") %>%
      count(genres, sort = TRUE) %>%
      slice_head(n = 15) %>%
      rename(Kategori = genres, Frekans = n)
    
    datatable(df, options = list(dom = "t", pageLength = 15))
  })
  
  output$freq_director <- renderDT({
    df <- filtered_data() %>%
      mutate(directors = ifelse(is.na(directors), "", directors)) %>%
      separate_rows(directors, sep = ",\\s*") %>%
      filter(directors != "") %>%
      count(directors, sort = TRUE) %>%
      slice_head(n = 15) %>%
      rename(Kategori = directors, Frekans = n)
    
    datatable(df, options = list(dom = "t", pageLength = 15))
  })
  
  output$freq_writer <- renderDT({
    df <- filtered_data() %>%
      mutate(writers = ifelse(is.na(writers), "", writers)) %>%
      separate_rows(writers, sep = ",\\s*") %>%
      filter(writers != "") %>%
      count(writers, sort = TRUE) %>%
      slice_head(n = 15) %>%
      rename(Kategori = writers, Frekans = n)
    
    datatable(df, options = list(dom = "t", pageLength = 15))
  })
  
  output$table_filtered <- renderDT({
    df <- filtered_data() %>%
      select(rank, primaryTitle, startYear, runtimeMinutes, averageRating, numVotes, directors, writers, genres)
    datatable(df, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  output$table_split <- renderDT({
    df <- filtered_data()
    field <- input$split_field
    
    split_df <- df %>%
      mutate(tmp = .data[[field]]) %>%
      mutate(tmp = ifelse(is.na(tmp), "", tmp)) %>%
      separate_rows(tmp, sep = ",\\s*") %>%
      filter(tmp != "") %>%
      transmute(primaryTitle, startYear, averageRating, numVotes, kategori = tmp) %>%
      arrange(desc(averageRating))
    
    datatable(split_df, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  output$meta_preview <- renderUI({
    tags$div(
      tags$h4("Özet (Preview)"),
      tags$p(tags$b("Öğrenci Adı: "), input$meta_dataset),
      tags$p(tags$b("Veri seti yazarı: "), input$meta_author),
      tags$p(tags$b("Öğrenci numarası: "), ifelse(nchar(input$meta_student) == 0, "-", input$meta_student)),
      tags$p(tags$b("Tarih: "), as.character(input$meta_date)),
      tags$p(tags$b("Kaynak : "), tags$a(href = kaggle_url, target = "_blank", kaggle_url))
    )
  })
}

shinyApp(ui, server)
