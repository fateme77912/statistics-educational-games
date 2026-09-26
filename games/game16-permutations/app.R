library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

svg_perm_podium <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#e7f5ff"/>
    <text x="160" y="32" text-anchor="middle" font-size="18" font-weight="bold" fill="#1864ab">جایگشت</text>

    <rect x="82" y="128" width="52" height="58" rx="8" fill="#74c0fc" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="134" y="98" width="52" height="88" rx="8" fill="#4dabf7" stroke="#1c7ed6" stroke-width="2"/>
    <rect x="186" y="150" width="52" height="36" rx="8" fill="#a5d8ff" stroke="#1c7ed6" stroke-width="2"/>

    <text x="160" y="214" text-anchor="middle" font-size="12" fill="#495057">ترتیب مهم است، پس حالت‌ها متفاوت‌اند</text>

    <circle cx="108" cy="112" r="18" fill="#ffd43b" stroke="#f59f00" stroke-width="2"/>
    <text x="108" y="117" text-anchor="middle" font-size="16" font-weight="bold" fill="#fff">1</text>

    <circle cx="160" cy="82" r="18" fill="#ffd43b" stroke="#f59f00" stroke-width="2"/>
    <text x="160" y="87" text-anchor="middle" font-size="16" font-weight="bold" fill="#fff">2</text>

    <circle cx="212" cy="134" r="18" fill="#ffd43b" stroke="#f59f00" stroke-width="2"/>
    <text x="212" y="139" text-anchor="middle" font-size="16" font-weight="bold" fill="#fff">3</text>
  </svg>'
  svg_data_uri(svg)
}

svg_perm_cards <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f1f3f5"/>
    <text x="160" y="32" text-anchor="middle" font-size="18" font-weight="bold" fill="#1c7ed6">ترتیب در انتخاب</text>

    <rect x="60" y="70" width="60" height="90" rx="12" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <rect x="130" y="55" width="60" height="105" rx="12" fill="#fff" stroke="#74c0fc" stroke-width="2"/>
    <rect x="200" y="85" width="60" height="75" rx="12" fill="#fff" stroke="#74c0fc" stroke-width="2"/>

    <text x="90" y="112" text-anchor="middle" font-size="24" font-weight="bold" fill="#1c7ed6">A</text>
    <text x="160" y="102" text-anchor="middle" font-size="24" font-weight="bold" fill="#1c7ed6">B</text>
    <text x="230" y="124" text-anchor="middle" font-size="24" font-weight="bold" fill="#1c7ed6">C</text>

    <line x1="90" y1="160" x2="160" y2="160" stroke="#495057" stroke-width="2" stroke-dasharray="5 4"/>
    <line x1="160" y1="160" x2="230" y2="160" stroke="#495057" stroke-width="2" stroke-dasharray="5 4"/>
    <text x="160" y="194" text-anchor="middle" font-size="12" fill="#495057">A, B, C با ترتیب‌های مختلف</text>
  </svg>'
  svg_data_uri(svg)
}

svg_perm_tree <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#fff4e6"/>
    <text x="160" y="32" text-anchor="middle" font-size="18" font-weight="bold" fill="#e67700">درخت جایگشت</text>

    <circle cx="160" cy="52" r="16" fill="#ffa94d" stroke="#e67700" stroke-width="2"/>
    <text x="160" y="57" text-anchor="middle" font-size="14" font-weight="bold" fill="#fff">شروع</text>

    <circle cx="95" cy="105" r="15" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="160" cy="105" r="15" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <circle cx="225" cy="105" r="15" fill="#ffd8a8" stroke="#e67700" stroke-width="2"/>
    <line x1="160" y1="68" x2="95" y2="92" stroke="#e67700" stroke-width="2"/>
    <line x1="160" y1="68" x2="160" y2="92" stroke="#e67700" stroke-width="2"/>
    <line x1="160" y1="68" x2="225" y2="92" stroke="#e67700" stroke-width="2"/>

    <circle cx="75" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>
    <circle cx="115" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>
    <circle cx="145" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>
    <circle cx="175" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>
    <circle cx="205" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>
    <circle cx="245" cy="165" r="14" fill="#fff3bf" stroke="#e67700" stroke-width="2"/>

    <line x1="95" y1="120" x2="75" y2="151" stroke="#e67700" stroke-width="2"/>
    <line x1="95" y1="120" x2="115" y2="151" stroke="#e67700" stroke-width="2"/>
    <line x1="160" y1="120" x2="145" y2="151" stroke="#e67700" stroke-width="2"/>
    <line x1="160" y1="120" x2="175" y2="151" stroke="#e67700" stroke-width="2"/>
    <line x1="225" y1="120" x2="205" y2="151" stroke="#e67700" stroke-width="2"/>
    <line x1="225" y1="120" x2="245" y2="151" stroke="#e67700" stroke-width="2"/>

    <text x="160" y="214" text-anchor="middle" font-size="12" fill="#495057">هر مرحله انتخاب‌های محدود دارد و ترتیب پاسخ‌ها مهم است</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    podium = svg_perm_podium(),
    cards  = svg_perm_cards(),
    tree   = svg_perm_tree()
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
      kind = "podium", level = "easy",
      question = "اگر 3 نفر برای گرفتن رتبه‌های اول، دوم و سوم باشند، چند ترتیب مختلف برای قرار گرفتن آن‌ها روی سکو وجود دارد؟",
      options = c("3", "6", "9", "12"),
      correct = "6",
      explanation = "3 نفر را می‌توان به 3! = 6 ترتیب مختلف روی سکو قرار داد."
    ),
    list(
      kind = "cards", level = "easy",
      question = "از بین 4 دانش‌آموز، می‌خواهیم یک نفر را به عنوان نفر اول، دوم و سوم انتخاب کنیم. چند ترتیب ممکن است؟",
      options = c("12", "24", "6", "18"),
      correct = "24",
      explanation = "4 × 3 × 2 = 24 چون ترتیب مهم است."
    ),
    list(
      kind = "tree", level = "easy",
      question = "اگر 3 کتاب مختلف را روی قفسه بگذاریم، چند چینش متفاوت داریم؟",
      options = c("3", "4", "6", "9"),
      correct = "6",
      explanation = "3 کتاب را می‌توان به 3! = 6 ترتیب چید."
    ),
    list(
      kind = "podium", level = "easy",
      question = "در یک مسابقه با 4 شرکت‌کننده، چند حالت برای رتبه‌های اول تا سوم وجود دارد؟",
      options = c("12", "18", "24", "6"),
      correct = "24",
      explanation = "4 × 3 × 2 = 24."
    ),
    list(
      kind = "cards", level = "easy",
      question = "3 دوست می‌خواهند پشت سر هم بایستند. چند ترتیب مختلف ممکن است؟",
      options = c("3", "6", "9", "12"),
      correct = "6",
      explanation = "3! = 6."
    ),
    list(
      kind = "tree", level = "easy",
      question = "اگر 5 نفر برای 2 جایگاه اول و دوم رقابت کنند، چند ترتیب ممکن است؟",
      options = c("10", "15", "20", "30"),
      correct = "20",
      explanation = "5 × 4 = 20."
    ),
    list(
      kind = "podium", level = "easy",
      question = "از بین 3 رنگ مختلف، چند ترتیب برای قرار دادن آن‌ها در 3 جایگاه داریم؟",
      options = c("3", "6", "9", "12"),
      correct = "6",
      explanation = "3! = 6."
    ),
    list(
      kind = "cards", level = "easy",
      question = "4 دانش‌آموز روی نیمکت در یک ردیف می‌نشینند. چند حالت برای نشستن وجود دارد؟",
      options = c("8", "12", "24", "16"),
      correct = "24",
      explanation = "4! = 24."
    ),

    # Hard
    list(
      kind = "tree", level = "hard",
      question = "در یک آزمون، 5 نفر برتر باید در رتبه‌های اول تا پنجم قرار بگیرند. چند ترتیب ممکن است؟",
      options = c("120", "60", "24", "100"),
      correct = "120",
      explanation = "5! = 120."
    ),
    list(
      kind = "podium", level = "hard",
      question = "از بین 6 نفر، چند ترتیب برای رتبه‌های اول، دوم و سوم وجود دارد؟",
      options = c("120", "90", "60", "30"),
      correct = "120",
      explanation = "6 × 5 × 4 = 120."
    ),
    list(
      kind = "cards", level = "hard",
      question = "5 کتاب متفاوت را روی یک قفسه می‌چینیم. چند حالت مختلف داریم؟",
      options = c("24", "60", "120", "180"),
      correct = "120",
      explanation = "5! = 120."
    ),
    list(
      kind = "tree", level = "hard",
      question = "اگر 6 نفر بخواهند در 4 جایگاه اول مسابقه قرار بگیرند، چند ترتیب ممکن است؟",
      options = c("360", "240", "180", "120"),
      correct = "360",
      explanation = "6 × 5 × 4 × 3 = 360."
    ),
    list(
      kind = "podium", level = "hard",
      question = "از بین 7 دانش‌آموز، چند ترتیب برای انتخاب 3 نفر اول وجود دارد؟",
      options = c("210", "336", "420", "720"),
      correct = "210",
      explanation = "7 × 6 × 5 = 210."
    ),
    list(
      kind = "cards", level = "hard",
      question = "6 پرچم متفاوت را در یک ردیف قرار می‌دهیم. چند ترتیب مختلف داریم؟",
      options = c("360", "720", "120", "240"),
      correct = "720",
      explanation = "6! = 720."
    ),
    list(
      kind = "tree", level = "hard",
      question = "از بین 5 نفر، چند ترتیب برای قرار گرفتن در 4 صندلی اول وجود دارد؟",
      options = c("120", "60", "24", "90"),
      correct = "120",
      explanation = "5 × 4 × 3 × 2 = 120."
    ),
    list(
      kind = "podium", level = "hard",
      question = "اگر 7 ورزشکار برای سکوهای اول تا سوم رقابت کنند، چند ترتیب ممکن است؟",
      options = c("180", "210", "240", "360"),
      correct = "210",
      explanation = "7 × 6 × 5 = 210."
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
      div(class = "top-title", "جایگشت‌ها (Permutations)"),
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
      podium = "سکوی رتبه‌بندی و جایگاه‌ها",
      cards  = "چینش و ترتیب کارت‌ها",
      tree   = "نمودار درختی جایگشت"
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
