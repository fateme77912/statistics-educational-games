# game19.R
# تشخیص رویدادهای مستقل و وابسته
# مطابق MathGames - Identify Independent and Dependent Events
# Standard: 7.SP.C.8a

library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
}

svg_coin <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#eef7ff"/>
    <circle cx="110" cy="120" r="55" fill="#ffd43b" stroke="#f08c00" stroke-width="4"/>
    <circle cx="210" cy="120" r="55" fill="#ffe8a3" stroke="#f08c00" stroke-width="4"/>
    <text x="110" y="127" text-anchor="middle" font-size="28" font-weight="bold" fill="#b26a00">H</text>
    <text x="210" y="127" text-anchor="middle" font-size="28" font-weight="bold" fill="#b26a00">T</text>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#1864ab">سکه و پرتاب مستقل</text>
    <text x="160" y="210" text-anchor="middle" font-size="14" fill="#495057">هر پرتاب سکه از پرتاب قبلی مستقل است</text>
  </svg>'
  svg_data_uri(svg)
}

svg_dice <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#f8f0fc"/>
    <rect x="70" y="70" width="80" height="80" rx="16" fill="#fff" stroke="#845ef7" stroke-width="4"/>
    <rect x="170" y="70" width="80" height="80" rx="16" fill="#fff" stroke="#845ef7" stroke-width="4"/>
    <circle cx="95" cy="95" r="7" fill="#845ef7"/>
    <circle cx="125" cy="125" r="7" fill="#845ef7"/>
    <circle cx="195" cy="95" r="7" fill="#845ef7"/>
    <circle cx="225" cy="125" r="7" fill="#845ef7"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#5f3dc4">دو تاس و رویداد وابسته</text>
    <text x="160" y="185" text-anchor="middle" font-size="14" fill="#495057">برداشتن بدون جایگذاری می‌تواند وابسته باشد</text>
  </svg>'
  svg_data_uri(svg)
}

svg_balls <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="24" fill="#fff4e6"/>
    <circle cx="90" cy="135" r="22" fill="#ff6b6b"/>
    <circle cx="140" cy="105" r="22" fill="#51cf66"/>
    <circle cx="190" cy="135" r="22" fill="#4dabf7"/>
    <circle cx="240" cy="105" r="22" fill="#ffd43b"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#e67700">انتخاب از کیسه</text>
    <text x="160" y="210" text-anchor="middle" font-size="14" fill="#495057">با جایگذاری = مستقل، بدون جایگذاری = وابسته</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
                coin = svg_coin(),
                dice = svg_dice(),
                balls = svg_balls())
  tags$img(src = src, style = "width:100%; max-width:320px; display:block; margin:0 auto; border-radius:12px;")
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
      kind = "coin", level = "easy",
      question = "اگر یک سکه دو بار پرتاب شود، نتیجه‌ی پرتاب دوم به پرتاب اول وابسته است یا مستقل؟",
      options = c("مستقل", "وابسته", "هر دو", "هیچ کدام"),
      correct = "مستقل",
      explanation = "هر پرتاب سکه نتیجه‌ی قبلی را تغییر نمی‌دهد، پس مستقل است."
    ),
    list(
      kind = "dice", level = "easy",
      question = "اگر دو بار تاس بیندازیم، رویداد دوم نسبت به اول چگونه است؟",
      options = c("مستقل", "وابسته", "غیرممکن", "حتماً برابر"),
      correct = "مستقل",
      explanation = "انداختن دوباره‌ی تاس به نتیجه‌ی قبلی وابسته نیست."
    ),
    list(
      kind = "balls", level = "easy",
      question = "برداشتن یک توپ از کیسه و برنگرداندن آن، معمولاً چه نوع رویدادی است؟",
      options = c("مستقل", "وابسته", "تصادفی نیست", "حتماً برابر"),
      correct = "وابسته",
      explanation = "وقتی توپ برنمی‌گردد، تعداد و احتمال‌ها تغییر می‌کنند؛ پس رویداد وابسته است."
    ),
    list(
      kind = "balls", level = "easy",
      question = "برداشتن یک توپ و سپس برگرداندن آن به کیسه، معمولاً چه نوع رویدادی است؟",
      options = c("مستقل", "وابسته", "نابرابر", "غیرممکن"),
      correct = "مستقل",
      explanation = "با جایگذاری، شرایط کیسه برای انتخاب بعدی تغییر نمی‌کند."
    ),
    list(
      kind = "coin", level = "easy",
      question = "پرتاب سکه‌ی اول، نتیجه‌ی سکه‌ی دوم را تغییر می‌دهد؟",
      options = c("بله", "خیر", "فقط گاهی", "فقط اگر شیر بیاید"),
      correct = "خیر",
      explanation = "پرتاب‌های سکه مستقل هستند."
    ),
    list(
      kind = "dice", level = "easy",
      question = "اگر یک تاس بیندازیم و دوباره همان تاس را بیندازیم، این دو رویداد چه هستند؟",
      options = c("مستقل", "وابسته", "ترکیبی", "ثابت"),
      correct = "مستقل",
      explanation = "هر بار تاس‌ریزی مستقل از بار قبل است."
    ),
    list(
      kind = "balls", level = "easy",
      question = "وقتی از یک کیسه توپ برمی‌داریم و آن را برنمی‌گردانیم، احتمال انتخاب بعدی چه می‌شود؟",
      options = c("تغییر می‌کند", "ثابت می‌ماند", "به صفر می‌رسد", "نصف می‌شود"),
      correct = "تغییر می‌کند",
      explanation = "چون تعداد توپ‌ها عوض می‌شود، احتمال انتخاب بعدی تغییر می‌کند."
    ),
    list(
      kind = "coin", level = "easy",
      question = "دو بار پرتاب سکه: آیا پرتاب اول روی دوم اثر می‌گذارد؟",
      options = c("بله", "خیر", "فقط در بعضی حالت‌ها", "فقط اگر سکه نو باشد"),
      correct = "خیر",
      explanation = "هر پرتاب جداگانه و مستقل است."
    ),

    # Hard
    list(
      kind = "balls", level = "hard",
      question = "در یک کیسه 5 توپ قرمز و 3 توپ آبی داریم. اگر یک توپ برداریم، برنگردانیم و دوباره برداریم، این رویدادها چگونه‌اند؟",
      options = c("مستقل", "وابسته", "مشابه", "نامعلوم"),
      correct = "وابسته",
      explanation = "چون توپ اول برنمی‌گردد، ترکیب کیسه برای انتخاب دوم تغییر می‌کند."
    ),
    list(
      kind = "coin", level = "hard",
      question = "شیر آمدن در پرتاب اول سکه، روی شیر آمدن در پرتاب دوم اثر دارد؟",
      options = c("مستقل", "وابسته", "حتماً اثر دارد", "فقط در سکه‌ی خاص"),
      correct = "مستقل",
      explanation = "نتیجه‌ی پرتاب اول احتمال پرتاب دوم را تغییر نمی‌دهد."
    ),
    list(
      kind = "dice", level = "hard",
      question = "اگر عدد 6 در تاس اول بیاید، احتمال آمدن 6 در تاس دوم تغییر می‌کند؟",
      options = c("بله", "خیر", "فقط بیشتر می‌شود", "فقط کمتر می‌شود"),
      correct = "خیر",
      explanation = "هر تاس‌ریزی مستقل است."
    ),
    list(
      kind = "balls", level = "hard",
      question = "از یک سبد کارت، یک کارت برمی‌داریم، برنمی‌گردانیم و دوباره برمی‌داریم. این مثال از کدام نوع است؟",
      options = c("مستقل", "وابسته", "تصادفی", "ثابت"),
      correct = "وابسته",
      explanation = "بدون جایگذاری یعنی انتخاب دوم به اول وابسته است."
    ),
    list(
      kind = "coin", level = "hard",
      question = "اگر دو نفر هر کدام یک سکه پرتاب کنند، نتیجه‌ی نفر اول روی نفر دوم اثر دارد؟",
      options = c("مستقل", "وابسته", "بستگی دارد", "فقط گاهی"),
      correct = "مستقل",
      explanation = "پرتاب‌های جداگانه‌ی دو نفر مستقل در نظر گرفته می‌شوند."
    ),
    list(
      kind = "dice", level = "hard",
      question = "کدام جمله درست است؟",
      options = c("دو بار پرتاب تاس وابسته است", "برداشتن بدون جایگذاری وابسته است", "پرتاب سکه وابسته است", "همه مستقل‌اند"),
      correct = "برداشتن بدون جایگذاری وابسته است",
      explanation = "انتخاب بدون برگرداندن، وابسته است."
    ),
    list(
      kind = "balls", level = "hard",
      question = "اگر از کیسه‌ای توپ برداریم و بعد آن را دوباره داخل کیسه بگذاریم، این رویدادها چه هستند؟",
      options = c("وابسته", "مستقل", "غیرممکن", "تکراری"),
      correct = "مستقل",
      explanation = "با جایگذاری، شرایط برای انتخاب بعدی مثل قبل می‌ماند."
    ),
    list(
      kind = "coin", level = "hard",
      question = "کدام گزینه وابسته است؟",
      options = c("دو بار پرتاب سکه", "دو بار پرتاب تاس", "برداشتن توپ بدون جایگذاری", "پرتاب یک تاس"),
      correct = "برداشتن توپ بدون جایگذاری",
      explanation = "بدون جایگذاری، احتمال‌های مرحله دوم تغییر می‌کنند."
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
      div(class = "top-title", "تشخیص رویدادهای مستقل و وابسته"),
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

  # کنترل دکمه‌های نوسازی و بازی دوباره
  observeEvent(list(input$reset_btn, input$btn_play_again, input$restart_game_btn), {
    init_game()
  })

  # دکمه ثبت پاسخ
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

  # دکمه مرحله بعد یا پایان کارنامه
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
      
      # بازکردن اتوماتیک سطح سخت پس از سوال پنجم بر اساس معیار دقت
      if (rv$q_num == 6 && hard_unlocked()) {
        rv$level <- "hard"
        all_questions <- get_full_bank()
        hard_questions <- Filter(function(q) q$level == "hard", all_questions)
        rv$current_pool[6:10] <- sample(hard_questions, 5)
        showNotification("سطح سخت به دلیل عملکرد فوق‌العاده شما فعال شد!", type = "warning")
      }
      
      rv$question <- rv$current_pool[[rv$q_num]]
      clear_selected_option()
    }
  })

  # گرفتن گزینه کلیک شده توسط کاربر
  observeEvent(input$answer, {
    if (!rv$answered) {
      rv$selected <- input$answer
    }
  })

  # متغیرهای واکنشی نوار وضعیت
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

  # رندر کردن پویای نمای بازی
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
      # ستون چپ: نمایش بصری رویدادها
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
      coin  = "آزمایش پرتاب سکه",
      dice  = "آزمایش انداختن تاس",
      balls = "آزمایش برداشتن توپ از کیسه"
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
          "یکی از گزینه‌ها را انتخاب کرده و دکمه ثبت پاسخ را بزنید."
        )
      )
    }

    is_correct <- identical(rv$selected, rv$question$correct)
    box_class <- if (is_correct) "feedback-box feedback-correct" else "feedback-box feedback-incorrect"

    div(
      class = box_class,
      tags$b(if (is_correct) "✅ آفرین! پاسخ درست است." else "❌ اشتباه شد."),
      div(style = "margin-top: 6px;", rv$question$explanation),
      div(
        style = "margin-top: 6px; font-size: 13px; font-weight: bold;",
        paste0("گزینه صحیح: ", rv$question$correct)
      )
    )
  })
}

shinyApp(ui, server)
