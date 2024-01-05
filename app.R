library(shiny)
library(shinythemes)

# Memuat Data
# Membuat dataset berdasarkan data yang diberikan
dataset <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)
# Mengonversi kolom-kolom ke tipe data numerik
dataset$x1 <- as.numeric(dataset$x1)
dataset$x2 <- as.numeric(dataset$x2)
dataset$x3 <- as.numeric(dataset$x3)
dataset$x4 <- as.numeric(dataset$x4)
dataset$x5 <- as.numeric(dataset$x5)
dataset$y <- as.numeric(dataset$y)

# Membuat model regresi linier berganda
sales_model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = dataset)

# Menghitung R-squared
r_squared <- summary(sales_model)$r.squared * 100  # Mengonversi R-squared menjadi persen

# Fungsi interpretasi model
get_model_interpretation <- function() {
  interpretation <- "Dalam model regresi linier berganda ini, setiap penambahan satu unit pada 'Jumlah Pengunjung Website per Bulan' diperkirakan akan meningkatkan penjualan sebesar koefisien yang terkait. Hal yang sama berlaku untuk variabel lainnya. 'Jumlah Transaksi Bulanan', 'Rata-rata Jumlah Item per Transaksi', 'Peringkat Kepuasan Pelanggan', dan 'Jumlah Iklan Online per Bulan' juga memiliki pengaruh signifikan terhadap penjualan. Dengan ini, diharapkan dapat mengoptimalkan strategi pemasaran, meningkatkan kepuasan pelanggan, serta mengoptimalkan jumlah pengunjung dan transaksi untuk meningkatkan penjualan secara efektif."
  interpretation
}

# UI untuk Shiny Dashboard
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        /* Menyesuaikan teks dalam tag div dengan class justified-text menjadi rata kanan-kiri */
        .justified-text {
          text-align: justify;
        }
        /* Memberikan gaya teks tebal pada tag strong */
        strong {
          font-weight: bold;
        }
        /* Mengatur ukuran font untuk teks di tag strong */
        .model-accuracy {
          font-size: 20px; /* Atur ukuran font sesuai kebutuhan */
        }
        /* Mengatur gaya teks untuk output hasil prediksi */
        .predicted-result {
          font-size: 25px; /* Sesuaikan ukuran font sesuai kebutuhan */
          font-weight: bold; /* Teks dalam tag akan tebal */
        }
        /* Gaya untuk pemisah */
        .separator {
          margin-top: 20px; /* Atur jarak atas */
          border-top: 2px solid #ccc; /* Atur garis pemisah */
          padding-top: 20px; /* Atur ruang atas garis pemisah */
        }
      ")
    )
  ),
  theme = shinytheme("flatly"),  # Menggunakan tema "flatly"
  navbarPage(
    "Dashboard",
    tabPanel(
      "Model",
      fluidRow(
        column(
          width = 6,
          h4("Koefisien Model Regresi"),
          tableOutput("model_coefficients")
        ),
        column(
          width = 6,
          div(class = "separator"), # Garis pemisah
          fluidRow(
            column(
              width = 12,
              h4("Akurasi Model"),
              uiOutput("model_accuracy")  # Output R-squared dipindahkan di samping koefisien
            ),
            column(
              width = 12,
              h4("Interpretasi Model Regresi"),
              div(
                get_model_interpretation(),
                class = "justified-text"  # Menambahkan kelas CSS untuk rata kanan-kiri
              )
            )
          )
        )
      ),
      div(class = "separator"), # Garis pemisah
      fluidRow(
        column(
          width = 12,
          h4("Hasil Regresi Linear Berganda"),
          tableOutput("regression_result")
        )
      )
    ),
    tabPanel(
      "Prediksi",
      fluidRow(
        column(
          width = 6,
          h4("Masukkan Data Prediksi"),
          numericInput(
            "x1_input",
            "Jumlah Pengunjung Website per Bulan:",
            value = 200000
          ),
          numericInput(
            "x2_input",
            "Jumlah Transaksi Bulanan:",
            value = 10000
          ),
          numericInput(
            "x3_input",
            "Rata-rata Jumlah Item per Transaksi:",
            value = 5,
            step = 0.1
          ),
          sliderInput(
            "x4_input",
            "Peringkat Kepuasan Pelanggan:",
            min = 1,
            max = 10,
            value = 8,
            step = 0.1
          ),
          numericInput(
            "x5_input",
            "Jumlah Iklan Online per Bulan:",
            value = 30000
          )
        ),
        column(
          width = 6,
          div(class = "separator"), # Garis pemisah
          h4("Hasil Prediksi Tabel"),
          tableOutput("predicted_table"),  # Mengubah verbatimTextOutput menjadi tableOutput
          br(),
          h4("Hasil Prediksi"),
          htmlOutput("predicted_result")  # Mengubah verbatimTextOutput menjadi htmlOutput
        )
      )
    )
  )
)

# Server untuk Shiny Dashboard
server <- function(input, output) {
  output$model_coefficients <- renderTable({
    coefficients <- summary(sales_model)$coefficients
    coefficients
  })
  
  output$model_interpretation <- renderUI({
    # Menampilkan interpretasi model dengan tag div untuk menerapkan gaya CSS
    div(
      get_model_interpretation(),
      class = "justified-text"  # Menambahkan kelas CSS untuk rata kanan-kiri
    )
  })
  
  output$regression_result <- renderTable({
    # Menampilkan hasil regresi linear berganda
    predicted <- predict(sales_model, newdata = dataset)
    actual_predicted <- cbind(dataset, Predicted = predicted)
    actual_predicted
  })
  
  output$model_accuracy <- renderUI({
    # Output R-squared dihilangkan, dan angka akurasi model diberi gaya teks yang diinginkan
    strong(paste(round(r_squared, 2), "%"), class = "model-accuracy")
  })
  
  output$predicted_table <- renderTable({
    # Menampilkan hasil prediksi dalam tabel terpisah
    data.frame(
      Variabel = c(
        "Jumlah Pengunjung Website",
        "Jumlah Transaksi",
        "Rata-rata Item per Transaksi",
        "Peringkat Kepuasan Pelanggan",
        "Jumlah Iklan Online"
      ),
      "Nilai" = c(
        input$x1_input,
        input$x2_input,
        input$x3_input,
        input$x4_input,
        input$x5_input
      )
    )
  })
  
  output$predicted_result <- renderUI({
    # Menampilkan hasil prediksi dengan gaya teks yang diinginkan
    predicted <- predict(
      sales_model,
      newdata = data.frame(
        x1 = input$x1_input,
        x2 = input$x2_input,
        x3 = input$x3_input,
        x4 = input$x4_input,
        x5 = input$x5_input
      )
    )
    
    HTML(paste("<span class='predicted-result'><strong>", round(predicted, 2), "</strong></span>"))
  })
}

shinyApp(ui = ui, server = server)

