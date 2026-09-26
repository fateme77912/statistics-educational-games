library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

svg_count_tree <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#e7f5ff"/>
    <text x="160" y="34" text-anchor="middle" font-size="18" font-weight="bold" fill="#1864ab">اصل شمارش</text>
    <circle cx="160" cy="65" r="18" fill="#74c0fc" stroke="#1c7ed6" stroke-width="2"/>
    <text x="160" y="71" text-anchor="middle" font-size="14" font-weight="bold" fill="#fff">شروع</text>

    <circle cx="90" cy="120" r="18" fill="#a5d8ff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="160" cy="120" r="18" fill="#a5d8ff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="230" cy="120" r="18" fill="#a5d8ff" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="160" y1="83" x2="90" y2="102" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="160" y1="83" x2="160" y2="102" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="160" y1="83" x2="230" y2="102" stroke="#1c7ed6" stroke-width="2"/>

    <circle cx="70" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="90" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="150" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="170" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="210" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="230" cy="185" r="16" fill="#d0ebff" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="90" y1="138" x2="70" y2="169" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="90" y1="138" x2="90" y2="169" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="160" y1="138" x2="150" y2="169" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="160" y1="138" x2="170" y2="169" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="230" y1="138" x2="210" y2="169" stroke="#1c7ed6" stroke-width="2"/>
    <line x1="230" y1="138" x2="230" y2="169" stroke="#1c7ed6" stroke-width="2"/>

    <text x="160" y="225" text-anchor="middle" font-size="12" fill="#1864ab">فهرست منظم، جدول و درخت برای شمارش حالت‌ها</text>
  </svg>'
  svg_data_uri(svg)
}

svg_counting_combo <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f1f3f5"/>
    <text x="160" y="34" text-anchor="middle" font-size="18" font-weight="bold" fill="#1c7ed6">شمارش مرحله‌ای</text>
    <rect x="45" y="65" width="80" height="120" rx="14" fill="#e7f5ff" stroke="#74c0fc" stroke-width="2"/>
    <text x="85" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1864ab">مرحله ۱</text>
    <text x="85" y="128" text-anchor="middle" font-size="28" font-weight="bold" fill="#1c7ed6">3</text>
    <text x="85" y="155" text-anchor="middle" font-size="11" fill="#495057">انتخاب بالا</text>

    <rect x="135" y="65" width="80" height="120" rx="14" fill="#e7f5ff" stroke="#74c0fc" stroke-width="2"/>
    <text x="175" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1864ab">مرحله ۲</text>
    <text x="175" y="128" text-anchor="middle" font-size="28" font-weight="bold" fill="#1c7ed6">4</text>
    <text x="175" y="155" text-anchor="middle" font-size="11" fill="#495057">انتخاب پایین</text>

    <rect x="225" y="65" width="80" height="120" rx="14" fill="#d0ebff" stroke="#74c0fc" stroke-width="2"/>
    <text x="265" y="100" text-anchor="middle" font-size="16" font-weight="bold" fill="#1864ab">کل</text>
    <text x="265" y="128" text-anchor="middle" font-size="28" font-weight="bold" fill="#1864ab">12</text>
    <text x="265" y="155" text-anchor="middle" font-size="11" fill="#495057">3 × 4</text>

    <text x="160" y="215" text-anchor="middle" font-size="12" fill="#495057">اصل ضرب: تعداد کل حالت‌ها = حاصل‌ضرب انتخاب‌های هر مرحله</text>
  </svg>'
  svg_data_uri(svg)
}

svg_table_grid <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#fff4e6"/>
    <text x="160" y="34" text-anchor="middle" font-size="18" font-weight="bold" fill="#e67700">جدول شمارش</text>
    <rect x="70" y="60" width="180" height="120" fill="#fff" stroke="#ffa94d" stroke-width="2"/>
    <line x1="130" y1="60" x2="130" y2="180" stroke="#ffa94d" stroke-width="2"/>
    <line x1="190" y1="60" x2="190" y2="180" stroke="#ffa94d" stroke-width="2"/>
    <line x1="70" y1="100" x2="250" y2="100" stroke="#ffa94d" stroke-width="2"/>
    <line x1="70" y1="140" x2="250" y2="140" stroke="#ffa94d" stroke-width="2"/>
    <text x="100" y="88" text-anchor="middle" font-size="14" font-weight="bold" fill="#e67700">A</text>
    <text x="160" y="88" text-anchor="middle" font-size="14" font-weight="bold" fill="#e67700">B</text>
    <text x="220" y="88" text-anchor="middle" font-size="14" font-weight="bold" fill="#e67700">C</text>
    <text x="100" y="128" text-anchor="middle" font-size="14" font-weight="bold" fill="#495057">1</text>
    <text x="160" y="128" text-anchor="middle" font-size="14" font-weight="bold" fill="#495057">2</text>
    <text x="220" y="128" text-anchor="middle" font-size="14" font-weight="bold" fill="#495057">3</text>
    <text x="160" y="210" text-anchor="middle" font-size="12" fill="#495057">با جدول منظم می‌توان همه حالت‌ها را شمرد</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    tree = svg_count_tree(),
    combo = svg_counting_combo(),
    table = svg_table_grid()
  )
  tags$img(
    src = src,
    style = "width:100%; max-width:320px; display:block; margin:0 auto; border-radius:12px;"
  )
}

star_html <- function(n) {
  n <- max(0, min(5, n))
  active <- paste(rep("★", n), collapse = "")
  inactive <- paste(rep("★", 5 - n), collapse = "")

  tags$div(
    class = "stars-wrap",
    tags$span(class = "star-active", active),
    if (n < 5) tags$span(class = "star-inactive", inactive)
  )
}

# ----------------------------
# Question Bank
# ----------------------------
get_full_bank <- function() {
  list(
    # Easy
    list(
      kind = "combo", level = "easy",
      question = "اگر برای خرید یک بستنی، 3 طعم و 2 نوع کلاهک داشته باشیم، چند حالت مختلف داریم؟",
      options = c("5", "6", "8", "12"),
      correct = "6",
      explanation = "اصل ضرب: 3 × 2 = 6 حالت."
    ),
    list(
      kind = "tree", level = "easy",
      question = "یک لباس شامل 4 پیراهن و 3 شلوار است. چند ترکیب لباس می‌توان ساخت؟",
      options = c("7", "12", "24", "10"),
      correct = "12",
      explanation = "اصل شمارش: 4 × 3 = 12."
    ),
    list(
      kind = "table", level = "easy",
      question = "اگر 2 نوع نوشیدنی و 5 نوع کیک داشته باشیم، تعداد انتخاب‌های یک نوشیدنی و یک کیک چند است؟",
      options = c("7", "10", "12", "8"),
      correct = "10",
      explanation = "2 × 5 = 10."
    ),
    list(
      kind = "combo", level = "easy",
      question = "برای ساخت رمز عبور ساده، 3 حرف اول و 4 عدد داریم. اگر فقط یکی از هر گروه انتخاب شود، چند حالت داریم؟",
      options = c("7", "10", "12", "14"),
      correct = "12",
      explanation = "3 × 4 = 12."
    ),
    list(
      kind = "tree", level = "easy",
      question = "یک رستوران 2 نوع سوپ و 3 نوع سالاد دارد. چند پیش‌غذا ممکن است؟",
      options = c("5", "6", "9", "8"),
      correct = "6",
      explanation = "2 × 3 = 6."
    ),
    list(
      kind = "table", level = "easy",
      question = "اگر 3 مدل کفش و 2 مدل جوراب داشته باشیم، چند انتخاب متفاوت داریم؟",
      options = c("5", "6", "8", "12"),
      correct = "6",
      explanation = "3 × 2 = 6."
    ),
    list(
      kind = "combo", level = "easy",
      question = "یک دانش‌آموز می‌تواند از بین 2 مسیر رفت و 4 مسیر برگشت انتخاب کند. چند حالت دارد؟",
      options = c("6", "8", "10", "12"),
      correct = "8",
      explanation = "2 × 4 = 8."
    ),
    list(
      kind = "tree", level = "easy",
      question = "در یک فروشگاه، 5 مدل دفتر و 2 مدل خودکار داریم. چند جفت دفتر و خودکار می‌توان ساخت؟",
      options = c("7", "8", "10", "12"),
      correct = "10",
      explanation = "5 × 2 = 10."
    ),

    # Hard
    list(
      kind = "table", level = "hard",
      question = "اگر یک بستنی با 3 طعم، 2 نوع سس و 4 نوع تاپینگ ساخته شود، چند حالت داریم؟",
      options = c("24", "9", "12", "18"),
      correct = "24",
      explanation = "3 × 2 × 4 = 24."
    ),
    list(
      kind = "tree", level = "hard",
      question = "یک کد شامل 2 حرف و سپس 3 رقم است. اگر برای هر حرف 5 انتخاب و برای هر رقم 10 انتخاب باشد، چند کد داریم؟",
      options = c("1000", "2500", "500", "250"),
      correct = "2500",
      explanation = "5 × 5 × 10 × 10 × 10 = 2500."
    ),
    list(
      kind = "combo", level = "hard",
      question = "در یک مدرسه، برای انتخاب یک نماینده از هر 4 کلاس و سپس یک معاون از بین 3 نفر، چند حالت وجود دارد؟",
      options = c("7", "12", "10", "15"),
      correct = "12",
      explanation = "4 × 3 = 12."
    ),
    list(
      kind = "table", level = "hard",
      question = "یک قفل رمزدار با 3 رقم ساخته می‌شود. اگر هر رقم 6 انتخاب داشته باشد، چند رمز مختلف ممکن است؟",
      options = c("18", "36", "96", "216"),
      correct = "216",
      explanation = "6 × 6 × 6 = 216."
    ),
    list(
      kind = "tree", level = "hard",
      question = "یک دانش‌آموز 2 مسیر برای رفتن به مدرسه، 3 مسیر برای برگشت و 2 زمان مختلف برای حرکت دارد. چند حالت کلی دارد؟",
      options = c("12", "7", "6", "10"),
      correct = "12",
      explanation = "2 × 3 × 2 = 12."
    ),
    list(
      kind = "combo", level = "hard",
      question = "اگر برای طراحی لباس، 4 رنگ، 3 مدل و 2 نوع آستین داشته باشیم، چند ترکیب مختلف داریم؟",
      options = c("9", "24", "12", "18"),
      correct = "24",
      explanation = "4 × 3 × 2 = 24."
    ),
    list(
      kind = "table", level = "hard",
      question = "یک منو شامل 2 نوع غذای اصلی، 2 نوع نوشیدنی و 5 نوع دسر است. چند غذای کامل می‌توان انتخاب کرد؟",
      options = c("9", "10", "20", "30"),
      correct = "20",
      explanation = "2 × 2 × 5 = 20."
    ),
    list(
      kind = "tree", level = "hard",
      question = "برای ساخت یک کد، ابتدا 4 حرف، سپس 3 عدد، و در پایان 2 نماد داریم. اگر برای هر بخش به ترتیب 3، 4 و 2 انتخاب داشته باشیم، چند حالت داریم؟",
      options = c("24", "36", "96", "48"),
      correct = "96",
      explanation = "3 × 3 × 3 × 3 × 4 × 4 × 4 × 2 × 2 = 96."
    )
  )
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/gh/rastikerdar/vazirmatn@v33.003/Vazirmatn-font-face.css"
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('clearOptionSelection', function(message) {
        $('.opt-btn').removeClass('selected');
      });
    ")),
    tags$style(HTML("
      body {
        font-family: 'Vazirmatn', sans-serif;
        background-color: #f4f6f9;
        direction: rtl;
        margin: 0;
        padding: 0;
      }
      .game-shell {
        max-width: 1000px;
        margin: 30px auto;
        background: #ffffff;
        border-radius: 16px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.08);
        overflow: hidden;
      }
      .top-banner {
        background: #1e88e5;
        color: white;
        padding: 20px 24px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .top-title {
        font-size: 24px;
        font-weight: bold;
        margin: 0;
      }
      .top-controls {
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .reset-top-btn {
        background: #ffb300;
        color: white;
        border: none;
        padding: 6px 16px;
        border-radius: 20px;
        font-size: 14px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.2s;
      }
      .reset-top-btn:hover {
        background: #ffa000;
      }
      .status-bar {
        background: #e3f2fd;
        padding: 12px 24px;
        display: grid;
        grid-template-columns: repeat(5, 1fr);
        gap: 10px;
        border-bottom: 1px solid #bbdefb;
      }
      .status-badge {
        background: #ffffff;
        border: 1px solid #bbdefb;
        border-radius: 12px;
        padding: 8px;
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      }
      .status-label {
        font-size: 12px;
        color: #78909c;
        margin-bottom: 4px;
      }
      .status-value {
        font-size: 16px;
        font-weight: bold;
        color: #0d47a1;
      }
      .stars-wrap {
        font-size: 20px;
        line-height: 1;
      }
      .star-active {
        color: #ffcc33;
      }
      .star-inactive {
        color: #d8dfe8;
      }
      .progress-card {
        background: #f5f9fc;
        padding: 12px 24px;
        border-bottom: 1px solid #cfd8dc;
        display: flex;
        align-items: center;
        gap: 16px;
      }
      .progress-pill {
        background: #ffffff;
        color: #0d47a1;
        border: 1px solid #bbdefb;
        border-radius: 20px;
        padding: 6px 14px;
        font-size: 14px;
        font-weight: bold;
        white-space: nowrap;
      }
      .progress {
        flex-grow: 1;
        height: 12px;
        background-color: #eceff1;
        border-radius: 6px;
        overflow: hidden;
        margin: 0;
      }
      .progress-bar {
        height: 100%;
        background-color: #4caf50;
        width: 0%;
        transition: width 0.3s ease;
      }
      .play-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 30px;
        padding: 30px;
      }
      @media(max-width: 768px) {
        .play-grid {
          grid-template-columns: 1fr;
        }
        .status-bar {
          grid-template-columns: 1fr 1fr;
        }
      }
      .question-card {
        background: #fafafa;
        border: 1px solid #e0e0e0;
        border-radius: 12px;
        padding: 24px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }
      .q-text {
        font-size: 18px;
        color: #37474f;
        line-height: 1.8;
        margin-bottom: 20px;
      }
      .options-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 12px;
      }
      .opt-btn {
        background: #1e88e5;
        color: white;
        border: none;
        padding: 14px;
        border-radius: 8px;
        font-size: 16px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.2s, transform 0.1s;
        text-align: right;
      }
      .opt-btn:hover {
        background: #1565c0;
        transform: translateY(-2px);
      }
      .opt-btn.selected {
        background: #0d47a1;
        box-shadow: inset 0 0 0 3px rgba(255,255,255,0.3);
      }
      .visual-panel {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        background: #ffffff;
        border: 1px solid #e0e0e0;
        border-radius: 12px;
        min-height: 320px;
        padding: 20px;
      }
      .visual-label {
        font-size: 16px;
        font-weight: bold;
        color: #546e7a;
        margin-top: 15px;
      }
      .action-row {
        display: flex;
        gap: 12px;
        margin-top: 20px;
      }
      .action-btn {
        flex: 1;
        padding: 12px;
        font-weight: bold;
        font-size: 16px;
        border-radius: 8px;
      }
      .feedback-box {
        margin-top: 15px;
        padding: 12px 16px;
        border-radius: 8px;
        text-align: right;
        font-size: 15px;
        line-height: 1.8;
      }
      .feedback-correct {
        background-color: #e8f5e9;
        color: #2e7d32;
        border: 1px solid #a5d6a7;
      }
      .feedback-incorrect {
        background-color: #ffebee;
        color: #c62828;
        border: 1px solid #ef9a9a;
      }
      .result-panel {
        padding: 60px 40px;
        text-align: center;
      }
      .result-title {
        font-size: 32px;
        color: #1e88e5;
        font-weight: bold;
        margin-bottom: 16px;
      }
      .result-score {
        font-size: 22px;
        color: #37474f;
        margin-bottom: 30px;
      }
      .restart-btn {
        background: #ffb300;
        color: white;
        border: none;
        padding: 12px 36px;
        border-radius: 24px;
        font-size: 18px;
        font-weight: bold;
        cursor: pointer;
        box-shadow: 0 4px 10px rgba(255, 179, 0, 0.3);
        transition: background 0.2s;
      }
      .restart-btn:hover {
        background: #ffa000;
      }
    "))
  ),

  div(
    class = "game-shell",
    # هدر بالایی
    div(
      class = "top-banner",
      div(class = "top-title", "اصل شمارش (Counting Principle)"),
      div(
        class = "top-controls",
        actionButton("reset_btn", "شروع مجدد", class = "reset-top-btn")
      )
    ),

    # نوار وضعیت ۵تایی
    div(
      class = "status-bar",
      div(class = "status-badge",
        div(class = "status-label", "سطح"),
        div(class = "status-value", textOutput("level_txt"))
      ),
      div(class = "status-badge",
        div(class = "status-label", "امتیاز"),
        div(class = "status-value", textOutput("score_txt"))
      ),
      div(class = "status-badge",
        div(class = "status-label", "درصد موفقیت"),
        div(class = "status-value", textOutput("acc_txt"))
      ),
      div(class = "status-badge",
        div(class = "status-label", "وضعیت سطح سخت"),
        div(class = "status-value", textOutput("lock_txt"))
      ),
      div(class = "status-badge",
        div(class = "status-label", "ستاره‌ها"),
        div(class = "status-value", uiOutput("stars_ui"))
      )
    ),

    # نوار پیشرفت کارت جاری
    div(
      class = "progress-card",
      div(class = "progress-pill", textOutput("progress_txt")),
      uiOutput("progress_ui")
    ),

    # محتوای اصلی بازی
    uiOutput("game_ui")
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    level = "easy",
    score = 0,
    total = 0,
    correct = 0,
    question_index = 1,
    answered = FALSE,
    selected = NULL,
    question = NULL,
    current_pool = list(),
    game_over = FALSE
  )

  hard_unlocked <- reactive({
    if (rv$total == 0) return(FALSE)
    rv$correct / rv$total >= 0.8
  })

  stars_count <- reactive({
    acc <- if (rv$total == 0) 0 else rv$correct / rv$total
    score_stars <- floor(rv$score / 20)
    acc_bonus <- if (acc >= 0.8 && rv$total >= 5) 1 else 0
    min(5, max(0, score_stars + acc_bonus))
  })

  clear_selected_option <- function() {
    rv$selected <- NULL
    session$sendCustomMessage("clearOptionSelection", list())
  }

  init_game <- function() {
    rv$level <- "easy"
    rv$score <- 0
    rv$total <- 0
    rv$correct <- 0
    rv$answered <- FALSE
    rv$question_index <- 1
    rv$game_over <- FALSE
    
    all_questions <- get_full_bank()
    easy_questions <- Filter(function(q) q$level == "easy", all_questions)
    
    rv$current_pool <- sample(easy_questions, min(10, length(easy_questions)))
    rv$question <- rv$current_pool[[1]]
    clear_selected_option()
  }

  # بارگذاری اولیه بازی
  observe({
    if (is.null(rv$question)) {
      init_game()
    }
  })

  # دکمه شروع مجدد و نوسازی
  observeEvent(list(input$reset_btn, input$btn_play_again, input$restart_game_btn), {
    init_game()
  })

  # دکمه بررسی و ثبت پاسخ
  observeEvent(input$check_btn, {
    if (is.null(rv$selected) || identical(rv$selected, "")) {
      showNotification("یک گزینه را انتخاب کن.", type = "message")
      return()
    }

    if (rv$answered || rv$game_over) return()

    rv$answered <- TRUE
    rv$total <- rv$total + 1

    if (identical(rv$selected, rv$question$correct)) {
      rv$correct <- rv$correct + 1
      rv$score <- rv$score + 10
      showNotification("آفرین! پاسخ درست بود.", type = "message")
    } else {
      rv$score <- max(0, rv$score - 2)
      showNotification("اشتباه بود. توضیح را ببین.", type = "error")
    }
  })

  # دکمه سؤال بعدی یا پایان
  observeEvent(input$next_btn, {
    if (!rv$answered || rv$game_over) {
      showNotification("اول پاسخ این سؤال را ثبت کن.", type = "message")
      return()
    }

    if (rv$question_index >= 10) {
      rv$game_over <- TRUE
    } else {
      rv$question_index <- rv$question_index + 1
      rv$answered <- FALSE
      
      # بررسی شرط صعود به سطح سخت پس از سوال پنجم
      if (rv$question_index == 6 && hard_unlocked()) {
        rv$level <- "hard"
        all_questions <- get_full_bank()
        hard_questions <- Filter(function(q) q$level == "hard", all_questions)
        rv$current_pool[6:10] <- sample(hard_questions, 5)
        showNotification("سطح سخت به دلیل دقت بالای شما باز شد!", type = "warning")
      }
      
      rv$question <- rv$current_pool[[rv$question_index]]
      clear_selected_option()
    }
  })

  # دریافت انتخاب کاربر از روی دکمه‌ها
  observeEvent(input$answer, {
    if (!rv$answered) {
      rv$selected <- input$answer
    }
  })

  # خروجی‌های نوار وضعیت
  output$level_txt <- renderText({
    if (rv$level == "easy") "آسان" else "سخت"
  })

  output$score_txt <- renderText({
    rv$score
  })

  output$acc_txt <- renderText({
    if (rv$total == 0) {
      "0٪"
    } else {
      paste0(round(100 * rv$correct / rv$total), "٪")
    }
  })

  output$lock_txt <- renderText({
    if (hard_unlocked() || rv$level == "hard") "باز" else "قفل"
  })

  output$stars_ui <- renderUI({
    star_html(stars_count())
  })

  output$progress_txt <- renderText({
    paste0("سؤال ", rv$question_index, " از ۱۰")
  })

  output$progress_ui <- renderUI({
    pct <- round((rv$question_index - 1) / 10 * 100)
    if (rv$answered) {
      pct <- round(rv$question_index / 10 * 100)
    }
    div(class = "progress",
      div(class = "progress-bar", style = paste0("width:", pct, "%;"))
    )
  })

  # مدیریت رندر کردن رابط کاربری
  output$game_ui <- renderUI({
    req(rv$question)

    if (rv$game_over) {
      return(
        div(
          class = "result-panel",
          div(class = "result-title", "پایان بازی!"),
          div(class = "result-score", paste0("امتیاز نهایی شما: ", rv$score, " با دقت ", round(100 * rv$correct / max(1, rv$total)), "٪")),
          actionButton("btn_play_again", "شروع دوباره بازی", class = "restart-btn")
        )
      )
    }

    div(
      class = "play-grid",
      # ستون راست: کارت سوال و گزینه ها
      div(
        class = "question-card",
        div(
          div(class = "q-text", textOutput("question_txt")),
          uiOutput("options_ui")
        ),
        div(
          div(
            class = "action-row",
            actionButton("check_btn", "ثبت پاسخ", class = "btn btn-success action-btn"),
            actionButton("next_btn", "سؤال بعدی", class = "btn btn-info action-btn")
          ),
          uiOutput("feedback_ui")
        )
      ),
      # ستون چپ: نمایش تصویر ریاضی یا سناریو مرتبط
      div(
        class = "visual-panel",
        uiOutput("visual_ui"),
        div(class = "visual-label", textOutput("visual_label"))
      )
    )
  })

  output$question_txt <- renderText({
    rv$question$question
  })

  output$visual_label <- renderText({
    switch(rv$question$kind,
      tree = "نمودار درختی شمارش",
      combo = "شمارش مرحله‌ای گزینه‌ها",
      table = "جدول شمارش منظم"
    )
  })

  output$visual_ui <- renderUI({
    question_image_ui(rv$question$kind)
  })

  output$options_ui <- renderUI({
    options <- rv$question$options
    tags$div(
      class = "options-grid",
      lapply(seq_along(options), function(i) {
        val <- options[i]
        tags$button(
          type = "button",
          class = "opt-btn",
          onclick = sprintf(
            "Shiny.setInputValue('answer', '%s', {priority: 'event'}); $('.opt-btn').removeClass('selected'); $(this).addClass('selected');",
            gsub("'", "\\\\'", val)
          ),
          val
        )
      })
    )
  })

  output$feedback_ui <- renderUI({
    if (!rv$answered) {
      return(
        div(
          style = "margin-top: 15px; color: #78909c; font-size: 13px; text-align: center;",
          "گزینه درست را انتخاب کنید و دکمه ثبت پاسخ را بزنید."
        )
      )
    }

    is_correct <- identical(rv$selected, rv$question$correct)
    box_class <- if (is_correct) "feedback-box feedback-correct" else "feedback-box feedback-incorrect"

    div(
      class = box_class,
      tags$b(if (is_correct) "✅ عالی بود! پاسخ درست بود." else "❌ اشتباه شد."),
      div(style = "margin-top:6px;", rv$question$explanation),
      div(
        style = "margin-top:6px; font-size:13px; font-weight:bold;",
        paste0("گزینه درست: ", rv$question$correct)
      )
    )
  })
}

shinyApp(ui, server)
