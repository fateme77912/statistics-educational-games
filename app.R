library(shiny)

# =========================================================
# 1) نام بازی‌ها
# شماره‌ها فقط شناسه داخلی هستند و به دانش‌آموز نمایش داده نمی‌شوند.
# =========================================================

game_titles <- c(
  "0"  = "حافظه آماری",
  "1"  = "محاسبه میانگین",
  "2"  = "محاسبه میانه",
  "3"  = "محاسبه مد",
  "4"  = "محاسبه دامنه تغییرات",
  "5"  = "میانگین، میانه، مد و دامنه",
  "6"  = "تفسیر نمودارهای آماری",
  "7"  = "یافتن عدد گمشده",
  "8"  = "تفسیر نمودار جعبه‌ای",
  "9"  = "احتمال رویدادهای ساده",
  "10" = "احتمال رویدادهای متمم و هم‌پوشان",
  "11" = "احتمال رویدادهای مستقل و وابسته",
  "12" = "رویدادهای مرکب و تعداد حالت‌ها",
  "13" = "تشخیص نمونه‌های تصادفی، نماینده و سوگیرانه",
  "14" = "نمادگذاری جایگشت و ترکیب",
  "15" = "اصل شمارش",
  "16" = "جایگشت‌ها",
  "17" = "فاکتوریل",
  "18" = "تشخیص رویدادهای مستقل و وابسته",
  "19" = "پیش‌بینی بر اساس داده‌ها",
  "20" = "مسئله‌های احتمال",
  "21" = "مسئله‌های پیش‌بینی و برآورد",
  "22" = "تقسیم فاکتوریل‌ها"
)


# =========================================================
# 2) درخت طبقه‌بندی بازی‌ها
# پایه → بخش آموزشی → شناسه بازی‌ها
# =========================================================

game_tree <- list(

  "پنجم دبستان" = list(
    "آمار و تحلیل داده‌ها" = c("0", "1")
  ),

  "ششم دبستان" = list(
    "آمار و تحلیل داده‌ها" = c("0", "1", "2", "3"),
    "احتمال و رویدادها" = c("9")
  ),

  "هفتم" = list(
    "آمار و تحلیل داده‌ها" = c("0", "1", "2", "3", "4", "5", "6", "7"),
    "احتمال و رویدادها" = c("9")
  ),

  "هشتم" = list(
    "آمار و تحلیل داده‌ها" = c("0", "1", "2", "3", "4", "5", "6", "7"),
    "احتمال و رویدادها" = c("9", "12")
  ),

  "نهم" = list(
    "آمار و تحلیل داده‌ها" = c("0", "1", "2", "3", "4", "5", "6", "7"),
    "احتمال و رویدادها" = c("9", "12")
  ),

  "دهم تجربی" = list(
    "آمار و تحلیل داده‌ها" = c("13"),
    "احتمال و رویدادها" = c("10", "12", "20"),
    "شمارش، ترکیب و پیش‌بینی" = c("14", "15", "16", "17", "22")
  ),

  "دهم ریاضی" = list(
    "آمار و تحلیل داده‌ها" = c("13"),
    "احتمال و رویدادها" = c("10", "12", "20"),
    "شمارش، ترکیب و پیش‌بینی" = c("14", "15", "16", "17", "22")
  ),

  "دهم انسانی" = list(
    "آمار و تحلیل داده‌ها" = c("13")
  ),

  "یازدهم تجربی" = list(
    "آمار و تحلیل داده‌ها" = c("1", "2", "4", "5", "6", "13"),
    "احتمال و رویدادها" = c("10", "11", "18", "20")
  ),

  "یازدهم ریاضی" = list(
    "آمار و تحلیل داده‌ها" = c("1", "2", "4", "5", "6", "8", "13", "21"),
    "احتمال و رویدادها" = c("10", "11", "18", "20"),
    "شمارش، ترکیب و پیش‌بینی" = c("14", "15", "16", "17", "22")
  ),

  "یازدهم انسانی" = list(
    "احتمال و رویدادها" = c("19"),
    "شمارش، ترکیب و پیش‌بینی" = c("21")
  ),

  "دوازدهم تجربی" = list(
    "احتمال و رویدادها" = c("20")
  ),

  "دوازدهم ریاضی" = list(
    "احتمال و رویدادها" = c("20")
  ),

  "دوازدهم انسانی" = list(
    "شمارش، ترکیب و پیش‌بینی" = c("14", "15", "16", "17", "22")
  )
)


# =========================================================
# 3) رابط کاربری
# =========================================================

ui <- fluidPage(

  tags$head(

    # فونت وزیرمتن
    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://cdn.jsdelivr.net/gh/rastikerdar/",
        "vazirmatn@v33.003/Vazirmatn-font-face.css"
      )
    ),

    tags$style(HTML("
      body {
        direction: rtl;
        text-align: right;
        font-family: Vazirmatn, sans-serif;
        background-color: #f3f8fc;
        color: #1e293b;
      }

      .main-container {
        max-width: 1100px;
        margin: 25px auto;
        padding: 25px;
      }

      .main-title {
        text-align: center;
        color: #124d87;
        font-size: 29px;
        font-weight: 800;
        margin-bottom: 10px;
      }

      .subtitle {
        text-align: center;
        color: #52677d;
        font-size: 15px;
        margin-bottom: 30px;
      }

      .selection-box {
        background-color: #ffffff;
        border-radius: 16px;
        box-shadow: 0 3px 14px rgba(20, 65, 100, 0.10);
        padding: 20px 25px;
        margin-bottom: 25px;
      }

      .section-title {
        color: #124d87;
        font-size: 21px;
        font-weight: 700;
        margin: 25px 0 15px 0;
      }

      .game-card {
        background: #ffffff;
        border-radius: 15px;
        box-shadow: 0 3px 12px rgba(20, 65, 100, 0.12);
        padding: 20px;
        margin-bottom: 18px;
        min-height: 145px;
        border-right: 5px solid #1976d2;
      }

      .game-card-title {
        color: #123e6a;
        font-size: 18px;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 12px;
      }

      .game-card-text {
        color: #60758a;
        font-size: 14px;
        margin-bottom: 18px;
      }

      .btn-game {
        background-color: #1976d2;
        color: #ffffff !important;
        border: none;
        border-radius: 9px;
        padding: 9px 22px;
        font-family: Vazirmatn, sans-serif;
        font-size: 14px;
        text-decoration: none;
        display: inline-block;
      }

      .btn-game:hover {
        background-color: #105fae;
        color: #ffffff !important;
        text-decoration: none;
      }

      .form-control, .selectize-input {
        font-family: Vazirmatn, sans-serif;
        border-radius: 8px;
      }

      label {
        color: #174a78;
        font-weight: 700;
        margin-bottom: 8px;
      }

      .no-game {
        background-color: #fff8e8;
        color: #8a5b00;
        padding: 18px;
        border-radius: 12px;
        font-size: 15px;
      }
    "))
  ),

  div(
    class = "main-container",

    h1("بازی‌های آموزشی آمار و احتمال", class = "main-title"),

    p(
      "ابتدا پایه تحصیلی و سپس بخش آموزشی موردنظر خود را انتخاب کنید.",
      class = "subtitle"
    ),

    div(
      class = "selection-box",

      fluidRow(

        column(
          width = 6,

          selectInput(
            inputId = "grade",
            label = "۱. پایه و رشته تحصیلی را انتخاب کنید:",
            choices = names(game_tree),
            selected = "هفتم"
          )
        ),

        column(
          width = 6,

          selectInput(
            inputId = "topic",
            label = "۲. بخش آموزشی را انتخاب کنید:",
            choices = NULL
          )
        )
      )
    ),

    h3("بازی‌های پیشنهادی", class = "section-title"),

    fluidRow(
      uiOutput("games_ui")
    )
  )
)


# =========================================================
# 4) منطق سرور
# =========================================================

server <- function(input, output, session) {

  # با تغییر پایه، فقط بخش‌های موجود برای همان پایه نشان داده می‌شوند
  observeEvent(input$grade, {

    available_topics <- names(game_tree[[input$grade]])

    updateSelectInput(
      session = session,
      inputId = "topic",
      choices = available_topics,
      selected = available_topics[1]
    )

  }, ignoreInit = FALSE)


  # نمایش کارت بازی‌ها براساس پایه و بخش انتخاب‌شده
  output$games_ui <- renderUI({

    req(input$grade, input$topic)

    game_ids <- game_tree[[input$grade]][[input$topic]]

    if (length(game_ids) == 0) {
      return(
        div(
          class = "no-game",
          "در حال حاضر بازی مرتبطی برای این بخش ثبت نشده است."
        )
      )
    }

    tagList(

      lapply(game_ids, function(game_id) {

        column(
          width = 6,

          div(
            class = "game-card",

            # شماره بازی اصلاً نمایش داده نمی‌شود
            h4(
              game_titles[[game_id]],
              class = "game-card-title"
            ),

            p(
              "برای شروع، دکمه ورود به بازی را انتخاب کنید.",
              class = "game-card-text"
            ),

            actionButton(
              inputId = paste0("open_game_", game_id),
              label = "ورود به بازی",
              class = "btn-game",
              onclick = paste0(
                "Shiny.setInputValue('selected_game', '",
                game_id,
                "', {priority: 'event'});"
              )
            )
          )
        )
      })
    )
  })


  # =======================================================
  # 5) انتخاب بازی
  # فعلاً با انتخاب هر بازی، نام آن به کاربر نمایش داده می‌شود.
  # در این قسمت باید کد بازکردن یا اجرای بازی واقعی خودتان را بگذارید.
  # =======================================================

  observeEvent(input$selected_game, {

    selected_id <- input$selected_game
    selected_title <- game_titles[[selected_id]]

    showModal(
      modalDialog(
        title = selected_title,

        tags$p(
          paste0("بازی «", selected_title, "» انتخاب شد.")
        ),

        tags$p(
          "در این بخش، کد اجرای بازی یا انتقال به صفحه بازی را قرار دهید."
        ),

        easyClose = TRUE,

        footer = modalButton("بازگشت")
      )
    )

  })
}


# =========================================================
# 6) اجرای برنامه
# =========================================================

shinyApp(ui = ui, server = server)
