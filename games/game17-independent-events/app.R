library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

svg_factorial_rocket <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#e7f5ff"/>
    <text x="160" y="30" text-anchor="middle" font-size="18" font-weight="bold" fill="#1864ab">فاکتوریل</text>

    <path d="M160 55 L185 95 L160 85 L135 95 Z" fill="#4dabf7" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="144" y="85" width="32" height="78" rx="12" fill="#74c0fc" stroke="#1c7ed6" stroke-width="2"/>
    <circle cx="160" cy="110" r="10" fill="#fff" stroke="#1c7ed6" stroke-width="2"/>
    <path d="M144 158 L130 184 L145 182 L160 210 L175 182 L190 184 L176 158 Z" fill="#ffd43b" stroke="#f08c00" stroke-width="2"/>

    <circle cx="82" cy="105" r="18" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <text x="82" y="111" text-anchor="middle" font-size="14" font-weight="bold" fill="#1864ab">3!</text>

    <circle cx="238" cy="105" r="18" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <text x="238" y="111" text-anchor="middle" font-size="14" font-weight="bold" fill="#1864ab">5!</text>

    <circle cx="110" cy="178" r="16" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <text x="110" y="183" text-anchor="middle" font-size="12" font-weight="bold" fill="#1864ab">2!</text>

    <circle cx="210" cy="178" r="16" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <text x="210" y="183" text-anchor="middle" font-size="12" font-weight="bold" fill="#1864ab">4!</text>
  </svg>'
  svg_data_uri(svg)
}

svg_factorial_blocks <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#f1f3f5"/>
    <text x="160" y="30" text-anchor="middle" font-size="18" font-weight="bold" fill="#1c7ed6">n! یعنی ضرب پشت سر هم</text>

    <rect x="50" y="80" width="46" height="46" rx="10" fill="#74c0fc" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="108" y="80" width="46" height="46" rx="10" fill="#4dabf7" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="166" y="80" width="46" height="46" rx="10" fill="#339af0" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="224" y="80" width="46" height="46" rx="10" fill="#228be6" stroke="#1c7ed6" stroke-width="2"/>

    <text x="73" y="108" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">4</text>
    <text x="131" y="108" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">×</text>
    <text x="189" y="108" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">3</text>
    <text x="247" y="108" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">×</text>

    <rect x="108" y="146" width="46" height="46" rx="10" fill="#74c0fc" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="166" y="146" width="46" height="46" rx="10" fill="#4dabf7" stroke="#1c7ed6" stroke-width="2"/>
    <text x="131" y="174" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">2</text>
    <text x="189" y="174" text-anchor="middle" font-size="18" font-weight="bold" fill="#fff">×</text>

    <text x="160" y="224" text-anchor="middle" font-size="13" fill="#495057">مثلاً 4! = 4 × 3 × 2 × 1</text>
  </svg>'
  svg_data_uri(svg)
}

svg_factorial_box <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#fff4e6"/>
    <text x="160" y="30" text-anchor="middle" font-size="18" font-weight="bold" fill="#e67700">فاکتوریل را پیدا کن</text>

    <rect x="65" y="65" width="190" height="120" rx="20" fill="#ffffff" stroke="#ffa94d" stroke-width="3"/>
    <text x="160" y="105" text-anchor="middle" font-size="34" font-weight="bold" fill="#e8590c">6!</text>
    <text x="160" y="142" text-anchor="middle" font-size="14" fill="#495057">یعنی 6 × 5 × 4 × 3 × 2 × 1</text>

    <circle cx="90" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="120" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="150" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="180" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="210" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="240" cy="200" r="12" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    rocket = svg_factorial_rocket(),
    blocks = svg_factorial_blocks(),
    box    = svg_factorial_box()
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
      kind = "blocks", level = "easy",
      question = "مقدار 3! چقدر است؟",
      options = c("3", "6", "9", "12"),
      correct = "6",
      explanation = "3! = 3 × 2 × 1 = 6"
    ),
    list(
      kind = "box", level = "easy",
      question = "مقدار 4! چقدر است؟",
      options = c("12", "18", "24", "30"),
      correct = "24",
      explanation = "4! = 4 × 3 × 2 × 1 = 24"
    ),
    list(
      kind = "rocket", level = "easy",
      question = "مقدار 2! چقدر است؟",
      options = c("1", "2", "3", "4"),
      correct = "2",
      explanation = "2! = 2 × 1 = 2"
    ),
    list(
      kind = "blocks", level = "easy",
      question = "کدام عبارت برابر 5! است؟",
      options = c("5 × 4 × 3 × 2 × 1", "5 × 4 × 3 × 2", "5 × 3 × 2 × 1", "5 + 4 + 3 + 2 + 1"),
      correct = "5 × 4 × 3 × 2 × 1",
      explanation = "فاکتوریل یعنی ضرب اعداد از خود عدد تا 1"
    ),
    list(
      kind = "box", level = "easy",
      question = "مقدار 1! چقدر است؟",
      options = c("0", "1", "2", "3"),
      correct = "1",
      explanation = "1! = 1"
    ),
    list(
      kind = "rocket", level = "easy",
      question = "مقدار 6! چقدر است؟",
      options = c("360", "720", "840", "600"),
      correct = "720",
      explanation = "6! = 6 × 5 × 4 × 3 × 2 × 1 = 720"
    ),
    list(
      kind = "blocks", level = "easy",
      question = "اگر 4! = ؟ ، کدام پاسخ درست است؟",
      options = c("16", "20", "24", "28"),
      correct = "24",
      explanation = "4! = 24"
    ),
    list(
      kind = "box", level = "easy",
      question = "کدام عدد برابر 3! نیست؟",
      options = c("6", "3", "2 × 3", "3 × 2 × 1"),
      correct = "3",
      explanation = "3! = 6 است، نه 3"
    ),

    # Hard
    list(
      kind = "rocket", level = "hard",
      question = "مقدار 7! چقدر است؟",
      options = c("3600", "5040", "7200", "4200"),
      correct = "5040",
      explanation = "7! = 7 × 6 × 5 × 4 × 3 × 2 × 1 = 5040"
    ),
    list(
      kind = "blocks", level = "hard",
      question = "کدام عبارت برابر 6! است؟",
      options = c("6 × 5 × 4 × 3 × 2 × 1", "6 × 5 × 4 × 3 × 2", "6 × 4 × 3 × 2 × 1", "6 + 5 + 4 + 3 + 2 + 1"),
      correct = "6 × 5 × 4 × 3 × 2 × 1",
      explanation = "6! = 6 × 5 × 4 × 3 × 2 × 1"
    ),
    list(
      kind = "box", level = "hard",
      question = "مقدار 5! چقدر است؟",
      options = c("60", "90", "120", "150"),
      correct = "120",
      explanation = "5! = 5 × 4 × 3 × 2 × 1 = 120"
    ),
    list(
      kind = "rocket", level = "hard",
      question = "کدام مقدار درست است؟",
      options = c("2! = 3", "3! = 6", "4! = 18", "5! = 60"),
      correct = "3! = 6",
      explanation = "فقط 3! = 6 درست است"
    ),
    list(
      kind = "blocks", level = "hard",
      question = "مقدار 4! + 1 چقدر است؟",
      options = c("24", "25", "26", "23"),
      correct = "25",
      explanation = "4! = 24، پس 24 + 1 = 25"
    ),
    list(
      kind = "box", level = "hard",
      question = "مقدار 6! ÷ 5! چقدر است؟",
      options = c("5", "6", "30", "1"),
      correct = "6",
      explanation = "6! ÷ 5! = 6"
    ),
    list(
      kind = "rocket", level = "hard",
      question = "اگر n! برای n=7 را حساب کنیم، جواب کدام است؟",
      options = c("5040", "720", "840", "1260"),
      correct = "5040",
      explanation = "7! = 5040"
    ),
    list(
      kind = "blocks", level = "hard",
      question = "کدام عبارت بزرگ‌تر است؟",
      options = c("4!", "3!", "2!", "1!"),
      correct = "4!",
      explanation = "4! = 24 و از بقیه بزرگ‌تر است"
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
      div(class = "top-title", "فاکتوریل‌ها (Factorials)"),
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
    q_num = 1,
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
    rv$q_num <- 1
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

    if (rv$q_num >= 10) {
      rv$game_over <- TRUE
    } else {
      rv$q_num <- rv$q_num + 1
      rv$answered <- FALSE
      
      # بررسی شرط صعود به سطح سخت پس از سوال پنجم
      if (rv$q_num == 6 && hard_unlocked()) {
        rv$level <- "hard"
        all_questions <- get_full_bank()
        hard_questions <- Filter(function(q) q$level == "hard", all_questions)
        rv$current_pool[6:10] <- sample(hard_questions, 5)
        showNotification("سطح سخت به دلیل دقت بالای شما باز شد!", type = "warning")
      }
      
      rv$question <- rv$current_pool[[rv$q_num]]
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
    paste0("سؤال ", rv$q_num, " از ۱۰")
  })

  output$progress_ui <- renderUI({
    pct <- round((rv$q_num - 1) / 10 * 100)
    if (rv$answered) {
      pct <- round(rv$q_num / 10 * 100)
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
      rocket = "نماد فاکتوریل ریاضی",
      blocks = "نمایش ضرب زنجیره‌ای فاکتوریل",
      box    = "محاسبه فاکتوریل درون جعبه"
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
