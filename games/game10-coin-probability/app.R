library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

coin_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <defs>
      <radialGradient id="coinGrad" cx="35%" cy="30%" r="70%">
        <stop offset="0%" stop-color="#fff9d9"/>
        <stop offset="100%" stop-color="#f3c84d"/>
      </radialGradient>
    </defs>
    <rect width="320" height="240" rx="26" fill="#fff8e9"/>
    <circle cx="160" cy="118" r="72" fill="url(#coinGrad)" stroke="#c28c12" stroke-width="10"/>
    <circle cx="160" cy="118" r="54" fill="#ffd965" stroke="#e5ad1f" stroke-width="4"/>
    <text x="160" y="132" text-anchor="middle" font-size="46" font-family="Arial, sans-serif" font-weight="700" fill="#946500">H</text>
    <circle cx="130" cy="86" r="10" fill="rgba(255,255,255,0.35)"/>
    <circle cx="120" cy="76" r="4" fill="rgba(255,255,255,0.35)"/>
  </svg>'
  svg_data_uri(svg)
}

dice_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#eef7ff"/>
    <rect x="92" y="50" width="136" height="136" rx="22" fill="#ffffff" stroke="#2f7be5" stroke-width="8"/>
    <circle cx="126" cy="84" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="84" r="11" fill="#2f7be5"/>
    <circle cx="126" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="160" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="126" cy="152" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="152" r="11" fill="#2f7be5"/>
  </svg>'
  svg_data_uri(svg)
}

spinner_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f2fff7"/>
    <circle cx="160" cy="118" r="74" fill="#ffffff" stroke="#1a9b62" stroke-width="8"/>
    <path d="M160 118 L160 44 A74 74 0 0 1 225 79 Z" fill="#ff6b6b"/>
    <path d="M160 118 L225 79 A74 74 0 0 1 225 157 Z" fill="#ffd43b"/>
    <path d="M160 118 L225 157 A74 74 0 0 1 160 192 Z" fill="#4dabf7"/>
    <path d="M160 118 L160 192 A74 74 0 0 1 95 157 Z" fill="#8cd348"/>
    <path d="M160 118 L95 157 A74 74 0 0 1 95 79 Z" fill="#ffa94d"/>
    <path d="M160 118 L95 79 A74 74 0 0 1 160 44 Z" fill="#b197fc"/>
    <circle cx="160" cy="118" r="12" fill="#1a9b62"/>
    <path d="M160 28 L149 52 L171 52 Z" fill="#ff4d4d"/>
    <text x="190" y="72" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#ffffff">قرمز</text>
    <text x="208" y="122" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#6b5600">زرد</text>
    <text x="190" y="170" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#ffffff">آبی</text>
    <text x="130" y="170" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#ffffff">سبز</text>
    <text x="112" y="122" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#ffffff">نارنجی</text>
    <text x="130" y="72" text-anchor="middle" font-size="16" font-family="Arial" font-weight="700" fill="#ffffff">بنفش</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    coin = coin_svg(),
    dice = dice_svg(),
    spinner = spinner_svg()
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
# Question generation
# ----------------------------
make_question <- function(level = c("easy", "hard")) {
  level <- match.arg(level)

  items <- list()

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "در پرتاب یک سکه سالم، اگر A = «آمدن شیر» باشد، احتمال رخداد مقابل A چقدر است؟",
    options = c("0", "1/4", "1/2", "1"),
    correct = "1/2",
    explanation = "در سکه سالم، احتمال شیر 1/2 است. بنابراین احتمال رخداد مقابل آن نیز 1/2 می‌شود."
  )

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "در پرتاب یک سکه سالم، A = «آمدن خط» است. کدام گزینه رخداد مقابل A را نشان می‌دهد؟",
    options = c("آمدن شیر", "آمدن خط", "آمدن عدد زوج", "هیچ‌کدام"),
    correct = "آمدن شیر",
    explanation = "در سکه، مقابل رخداد «آمدن خط»، رخداد «آمدن شیر» است."
  )

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "دو بار سکه را پرتاب می‌کنیم. A = «بار اول شیر» و B = «بار دوم شیر». این دو رخداد چه رابطه‌ای دارند؟",
    options = c("ناسازگارند", "همپوشان‌اند", "مکمل‌اند", "مساوی‌اند"),
    correct = "همپوشان‌اند",
    explanation = "حالت «شیر، شیر» در هر دو رخداد وجود دارد؛ پس A و B همپوشان‌اند."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در پرتاب یک تاس سالم، اگر A = «آمدن عدد ۴» باشد، احتمال رخداد مقابل A چقدر است؟",
    options = c("1/6", "1/3", "1/2", "5/6"),
    correct = "5/6",
    explanation = "احتمال آمدن یک عدد مشخص 1/6 است. پس احتمال رخداد مقابل آن 5/6 می‌شود."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در پرتاب یک تاس سالم، اگر A = «آمدن عدد زوج» باشد، احتمال رخداد مقابل A چقدر است؟",
    options = c("1/6", "1/3", "1/2", "2/3"),
    correct = "1/2",
    explanation = "اعداد زوج ۲، ۴ و ۶ هستند. رخداد مقابل آن‌ها اعداد فرد ۱، ۳ و ۵ است؛ پس احتمال برابر 3/6 یا 1/2 است."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در پرتاب یک تاس سالم، A = «عدد زوج» و B = «عدد بزرگ‌تر از ۳». این دو رخداد چه رابطه‌ای دارند؟",
    options = c("ناسازگار", "همپوشان", "مکمل", "مساوی"),
    correct = "همپوشان",
    explanation = "A={2,4,6} و B={4,5,6} است. چون ۴ و ۶ در هر دو رخداد هستند، این دو رخداد همپوشان‌اند."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در پرتاب یک تاس سالم، A = «عدد کمتر از ۴» و B = «عدد بیشتر از ۴». این دو رخداد چه رابطه‌ای دارند؟",
    options = c("همپوشان", "ناسازگار", "مکمل کامل", "مساوی"),
    correct = "ناسازگار",
    explanation = "A={1,2,3} و B={5,6} است. هیچ عددی همزمان در هر دو مجموعه نیست؛ پس ناسازگارند."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "گردونه ۶ بخش مساوی دارد. اگر A = «رسیدن به بخش سبز» باشد، احتمال رخداد مقابل A چقدر است؟",
    options = c("1/6", "2/6", "5/6", "1"),
    correct = "5/6",
    explanation = "فقط یک بخش سبز است؛ پس P(A)=1/6 و احتمال رخداد مقابل آن 5/6 است."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "گردونه ۶ بخش مساوی دارد. A = «رسیدن به بخش قرمز» و B = «رسیدن به بخش آبی». این دو رخداد چه رابطه‌ای دارند؟",
    options = c("همپوشان", "ناسازگار", "مکمل", "مساوی"),
    correct = "ناسازگار",
    explanation = "عقربه در هر بار فقط روی یک بخش می‌ایستد؛ بنابراین نمی‌تواند همزمان روی قرمز و آبی باشد."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "گردونه ۶ بخش مساوی دارد. احتمال رسیدن به بخش قرمز یا زرد چقدر است؟",
    options = c("1/6", "2/6", "3/6", "5/6"),
    correct = "2/6",
    explanation = "بخش قرمز یک قسمت و بخش زرد یک قسمت دارد؛ پس احتمال قرمز یا زرد برابر 2/6 است."
  )

  if (level == "hard") {
    items[[length(items) + 1]] <- list(
      kind = "dice",
      question = "در پرتاب یک تاس سالم، A = «عدد اول» و B = «عدد فرد». احتمال A یا B چقدر است؟",
      options = c("3/6", "4/6", "5/6", "1"),
      correct = "4/6",
      explanation = "A={2,3,5} و B={1,3,5} است. اجتماع آن‌ها {1,2,3,5} می‌شود؛ پس احتمال 4/6 است."
    )

    items[[length(items) + 1]] <- list(
      kind = "spinner",
      question = "در گردونه ۶ بخشی، A = «قرمز یا زرد» و B = «زرد یا آبی» است. احتمال A یا B چقدر است؟",
      options = c("2/6", "3/6", "4/6", "5/6"),
      correct = "3/6",
      explanation = "A شامل قرمز و زرد است. B شامل زرد و آبی است. اجتماع آن‌ها قرمز، زرد و آبی می‌شود؛ پس 3/6."
    )

    items[[length(items) + 1]] <- list(
      kind = "coin",
      question = "دو بار سکه را پرتاب می‌کنیم. A = «حداقل یک بار شیر» است. احتمال رخداد مقابل A چقدر است؟",
      options = c("1/4", "1/2", "3/4", "1"),
      correct = "1/4",
      explanation = "رخداد مقابل «حداقل یک بار شیر»، یعنی «هیچ بار شیر نیاید» یا همان خط، خط. احتمال آن 1/4 است."
    )
  }

  q <- sample(items, 1)[[1]]
  q$options <- sample(unique(q$options))
  q
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
      .level-btn {
        background: rgba(255, 255, 255, 0.15);
        color: white;
        border: 1px solid rgba(255, 255, 255, 0.3);
        padding: 6px 16px;
        border-radius: 20px;
        font-size: 14px;
        cursor: pointer;
        transition: all 0.3s;
      }
      .level-btn.active {
        background: #ffffff;
        color: #1e88e5;
        font-weight: bold;
      }
      .level-btn:hover {
        background: rgba(255, 255, 255, 0.3);
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
        grid-template-columns: 1fr 1fr;
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
      div(class = "top-title", "احتمال رخدادهای مقابل و همپوشان"),
      div(
        class = "top-controls",
        actionButton("easy_btn", "سطح آسان", class = "level-btn active"),
        actionButton("hard_btn", "سطح سخت", class = "level-btn"),
        actionButton("reset_btn", "شروع مجدد", class = "reset-top-btn")
      )
    ),

    # نوار وضعیت
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

    # محتوای بازی
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
    question = make_question("easy")
  )

  hard_unlocked <- reactive({
    if (rv$total == 0) return(FALSE)
    rv$correct / rv$total >= 0.8
  })

  stars_count <- reactive({
    acc <- if (rv$total == 0) 0 else rv$correct / rv$total
    score_stars <- floor(rv$score / 25)
    acc_bonus <- if (acc >= 0.8 && rv$total >= 5) 1 else 0
    min(5, max(0, score_stars + acc_bonus))
  })

  clear_selected_option <- function() {
    rv$selected <- NULL
    session$sendCustomMessage("clearOptionSelection", list())
  }

  new_round <- function(level = "easy") {
    rv$level <- level
    rv$score <- 0
    rv$total <- 0
    rv$correct <- 0
    rv$question_index <- 1
    rv$answered <- FALSE
    rv$question <- make_question(level)
    clear_selected_option()
  }

  new_q <- function(level = rv$level) {
    if (rv$question_index >= 10) {
      return()
    }

    rv$question_index <- rv$question_index + 1
    rv$question <- make_question(level)
    rv$answered <- FALSE
    clear_selected_option()
  }

  # تغییر سطح دشواری آسان
  observeEvent(input$easy_btn, {
    new_round("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # تغییر سطح دشواری سخت
  observeEvent(input$hard_btn, {
    if (hard_unlocked()) {
      new_round("hard")
      runjs("$('.level-btn').removeClass('active'); $('#hard_btn').addClass('active');")
    } else {
      showNotification("برای باز شدن سطح سخت باید حداقل 80٪ پاسخ درست داشته باشی.", type = "warning")
    }
  })

  # ریست کردن بازی
  observeEvent(list(input$reset_btn, input$btn_play_again), {
    new_round("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # رفتن به سوال بعدی
  observeEvent(input$next_btn, {
    if (!rv$answered) {
      showNotification("اول پاسخ این سؤال را ثبت کن.", type = "message")
      return()
    }
    new_q(rv$level)
  })

  # بررسی پاسخ ثبت شده
  observeEvent(input$check_btn, {
    if (is.null(rv$selected) || identical(rv$selected, "")) {
      showNotification("یک گزینه را انتخاب کن.", type = "message")
      return()
    }

    if (rv$answered) return()

    rv$answered <- TRUE
    rv$total <- rv$total + 1

    if (identical(rv$selected, rv$question$correct)) {
      rv$correct <- rv$correct + 1
      rv$score <- rv$score + ifelse(rv$level == "easy", 10, 15)
      showNotification("آفرین! پاسخ درست بود.", type = "message")
    } else {
      rv$score <- max(0, rv$score - 2)
      showNotification("اشتباه بود. توضیح را ببین.", type = "error")
    }
  })

  # گوش دادن به کلیک دکمه‌های گزینه‌ها
  observeEvent(input$answer, {
    if (!rv$answered) {
      rv$selected <- input$answer
    }
  })

  # خروجی‌های وضعیت بازی
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
    if (hard_unlocked()) "باز" else "قفل"
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

  output$game_ui <- renderUI({
    req(rv$question_index)

    if (rv$question_index >= 10 && rv$answered) {
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
      # ستون راست: صورت سوال و گزینه‌ها
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
      # ستون چپ: پنل بصری (تصویر تاس/سکه/چرخنده)
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
      coin = "سکه سالم",
      dice = "تاس سالم",
      spinner = "گردونه ۶ بخشی"
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
      tags$b(if (is_correct) "✅ آفرین! پاسخ درست بود." else "❌ اشتباه بود."),
      div(style = "margin-top:6px;", rv$question$explanation),
      div(
        style = "margin-top:6px; font-size:13px; font-weight:bold;",
        paste0("گزینه درست: ", rv$question$correct)
      )
    )
  })
}

shinyApp(ui, server)
