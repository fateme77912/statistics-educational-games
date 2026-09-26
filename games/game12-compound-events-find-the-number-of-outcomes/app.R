library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

spinner_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f4fbff"/>
    <circle cx="160" cy="118" r="78" fill="#ffffff" stroke="#1d9bf0" stroke-width="8"/>
    <circle cx="160" cy="118" r="10" fill="#1d9bf0"/>
    <path d="M160 118 L208 72" stroke="#ff6b6b" stroke-width="8" stroke-linecap="round"/>
    <path d="M160 118 L112 78" stroke="#4dabf7" stroke-width="8" stroke-linecap="round"/>
    <path d="M160 118 L220 120" stroke="#8cd348" stroke-width="8" stroke-linecap="round"/>
    <path d="M160 118 L128 172" stroke="#ffd43b" stroke-width="8" stroke-linecap="round"/>
    <circle cx="208" cy="72" r="12" fill="#ff6b6b"/>
    <circle cx="112" cy="78" r="12" fill="#4dabf7"/>
    <circle cx="220" cy="120" r="12" fill="#8cd348"/>
    <circle cx="128" cy="172" r="12" fill="#ffd43b"/>
  </svg>'
  svg_data_uri(svg)
}

cards_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#fff8f8"/>
    <rect x="88" y="52" width="92" height="124" rx="18" fill="#ffffff" stroke="#ff6b6b" stroke-width="6"/>
    <rect x="140" y="64" width="92" height="124" rx="18" fill="#ffffff" stroke="#1d9bf0" stroke-width="6"/>
    <text x="120" y="120" text-anchor="middle" font-size="44" font-family="Arial, sans-serif" font-weight="700" fill="#ff6b6b">A</text>
    <text x="188" y="132" text-anchor="middle" font-size="44" font-family="Arial, sans-serif" font-weight="700" fill="#1d9bf0">B</text>
  </svg>'
  svg_data_uri(svg)
}

tree_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f8fcff"/>
    <line x1="78" y1="120" x2="160" y2="78" stroke="#1d9bf0" stroke-width="5"/>
    <line x1="78" y1="120" x2="160" y2="162" stroke="#1d9bf0" stroke-width="5"/>
    <line x1="160" y1="78" x2="244" y2="54" stroke="#8cd348" stroke-width="5"/>
    <line x1="160" y1="78" x2="244" y2="102" stroke="#8cd348" stroke-width="5"/>
    <line x1="160" y1="162" x2="244" y2="138" stroke="#ffd43b" stroke-width="5"/>
    <line x1="160" y1="162" x2="244" y2="186" stroke="#ffd43b" stroke-width="5"/>
    <circle cx="78" cy="120" r="12" fill="#1d9bf0"/>
    <circle cx="160" cy="78" r="12" fill="#1d9bf0"/>
    <circle cx="160" cy="162" r="12" fill="#1d9bf0"/>
    <circle cx="244" cy="54" r="12" fill="#8cd348"/>
    <circle cx="244" cy="102" r="12" fill="#8cd348"/>
    <circle cx="244" cy="138" r="12" fill="#ffd43b"/>
    <circle cx="244" cy="186" r="12" fill="#ffd43b"/>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    spinner = spinner_svg(),
    cards = cards_svg(),
    tree = tree_svg()
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
    kind = "spinner",
    question = "یک چرخنده 4 بخشی و یک سکه سالم داریم. چند حالت ممکن برای نتیجه نهایی وجود دارد؟",
    options = c("4", "6", "8", "10"),
    correct = "8",
    explanation = "چرخنده 4 حالت دارد و سکه 2 حالت. برای رویداد مرکب، 4 × 2 = 8 حالت داریم."
  )

  items[[length(items) + 1]] <- list(
    kind = "cards",
    question = "اگر از بین 3 پیراهن و 2 شلوار یک لباس انتخاب کنیم، چند حالت ممکن برای انتخاب داریم؟",
    options = c("5", "6", "8", "9"),
    correct = "6",
    explanation = "برای هر پیراهن 2 شلوار داریم؛ پس 3 × 2 = 6 حالت ممکن است."
  )

  items[[length(items) + 1]] <- list(
    kind = "tree",
    question = "یک انتخاب دو مرحله‌ای داریم: مرحله اول 2 گزینه و مرحله دوم 5 گزینه دارد. تعداد کل حالت‌ها چقدر است؟",
    options = c("7", "8", "10", "12"),
    correct = "10",
    explanation = "در رویداد مرکب، تعداد کل حالت‌ها برابر حاصل‌ضرب تعداد گزینه‌های هر مرحله است: 2 × 5 = 10."
  )

  items[[length(items) + 1]] <- list(
    kind = "tree",
    question = "اگر 4 نوع بستنی و 3 نوع تاپینگ داشته باشیم، چند ترکیب مختلف می‌توان ساخت؟",
    options = c("7", "10", "12", "14"),
    correct = "12",
    explanation = "برای هر نوع بستنی 3 تاپینگ داریم، پس 4 × 3 = 12 ترکیب ممکن است."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "یک سکه و یک تاس سالم را با هم پرتاب می‌کنیم. چند حالت ممکن در فضای نمونه داریم؟",
    options = c("8", "10", "12", "14"),
    correct = "12",
    explanation = "سکه 2 حالت و تاس 6 حالت دارد. بنابراین 2 × 6 = 12 حالت ممکن است."
  )

  items[[length(items) + 1]] <- list(
    kind = "cards",
    question = "در یک رستوران 3 غذای اصلی و 4 نوشیدنی وجود دارد. اگر یک غذا و یک نوشیدنی انتخاب کنیم، چند انتخاب ممکن داریم؟",
    options = c("7", "10", "12", "15"),
    correct = "12",
    explanation = "انتخاب غذا و نوشیدنی یک رویداد مرکب است: 3 × 4 = 12 حالت."
  )

  items[[length(items) + 1]] <- list(
    kind = "tree",
    question = "برای ساخت یک رمز، ابتدا 2 حرف و سپس 3 رقم انتخاب می‌کنیم. چند رمز مختلف می‌توان ساخت؟",
    options = c("5", "6", "8", "10"),
    correct = "6",
    explanation = "2 گزینه برای حرف اول و 3 گزینه برای بخش دوم داریم، پس 2 × 3 = 6 حالت."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "یک چرخنده با 3 بخش رنگی و یک سکه داریم. اگر هر دو را یک بار استفاده کنیم، چند نتیجه ممکن داریم؟",
    options = c("5", "6", "8", "10"),
    correct = "6",
    explanation = "3 حالت برای چرخنده و 2 حالت برای سکه داریم. پس 3 × 2 = 6 حالت."
  )

  items[[length(items) + 1]] <- list(
    kind = "cards",
    question = "یک کفش‌فروشی 2 مدل کفش و 4 رنگ دارد. اگر هر مدل را در هر رنگ داشته باشد، چند گزینه داریم؟",
    options = c("6", "8", "10", "12"),
    correct = "8",
    explanation = "2 مدل و 4 رنگ داریم، پس 2 × 4 = 8 گزینه ممکن است."
  )

  items[[length(items) + 1]] <- list(
    kind = "tree",
    question = "یک انتخاب سه‌مرحله‌ای داریم: 2 گزینه، سپس 2 گزینه، سپس 3 گزینه. تعداد کل حالت‌ها چقدر است؟",
    options = c("7", "10", "12", "14"),
    correct = "12",
    explanation = "برای رویدادهای مرکب چندمرحله‌ای، تعداد حالت‌ها برابر 2 × 2 × 3 = 12 است."
  )

  items[[length(items) + 1]] <- list(
    kind = "spinner",
    question = "چند حالت در فضای نمونه وجود دارد اگر یک تاس سالم و یک سکه سالم را هم‌زمان استفاده کنیم؟",
    options = c("6", "8", "10", "12"),
    correct = "12",
    explanation = "تاس 6 حالت و سکه 2 حالت دارد. بنابراین 6 × 2 = 12 حالت."
  )

  if (level == "hard") {
    items[[length(items) + 1]] <- list(
      kind = "tree",
      question = "اگر 3 مدل تی‌شرت، 4 مدل شلوار و 2 مدل کفش داشته باشیم، چند ترکیب لباس می‌توان ساخت؟",
      options = c("9", "18", "24", "30"),
      correct = "24",
      explanation = "سه انتخاب مستقل داریم: 3 × 4 × 2 = 24 ترکیب ممکن."
    )

    items[[length(items) + 1]] <- list(
      kind = "cards",
      question = "در یک بستنی‌فروشی 4 طعم بستنی، 2 نوع قیف و 3 نوع سس داریم. چند ترکیب مختلف می‌توان ساخت؟",
      options = c("9", "12", "18", "24"),
      correct = "24",
      explanation = "برای هر طعم، 2 قیف و 3 سس داریم: 4 × 2 × 3 = 24 حالت."
    )

    items[[length(items) + 1]] <- list(
      kind = "spinner",
      question = "یک چرخنده 5 بخشی و یک سکه و یک تاس داریم. چند نتیجه ممکن داریم؟",
      options = c("20", "30", "40", "60"),
      correct = "60",
      explanation = "5 حالت برای چرخنده، 2 حالت برای سکه و 6 حالت برای تاس داریم. پس 5 × 2 × 6 = 60."
    )

    items[[length(items) + 1]] <- list(
      kind = "tree",
      question = "برای ساخت یک کد، ابتدا 2 حرف، سپس 10 رقم، سپس 2 علامت انتخاب می‌کنیم. چند کد ممکن است؟",
      options = c("20", "30", "40", "200"),
      correct = "40",
      explanation = "2 × 10 × 2 = 40 حالت ممکن وجود دارد."
    )

    items[[length(items) + 1]] <- list(
      kind = "spinner",
      question = "یک سکه سالم را دو بار و یک تاس سالم را یک بار پرتاب می‌کنیم. چند حالت در فضای نمونه داریم؟",
      options = c("12", "18", "24", "36"),
      correct = "24",
      explanation = "هر سکه 2 حالت دارد، پس دو بار پرتاب سکه 2 × 2 = 4 حالت می‌دهد. با تاس 6 حالت: 4 × 6 = 24."
    )

    items[[length(items) + 1]] <- list(
      kind = "cards",
      question = "برای یک وعده، 2 پیش‌غذا، 3 غذای اصلی و 4 نوشیدنی داریم. چند ترکیب غذا می‌توان ساخت؟",
      options = c("9", "18", "24", "36"),
      correct = "24",
      explanation = "تعداد ترکیب‌ها برابر 2 × 3 × 4 = 24 است."
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
      div(class = "top-title", "یافتن تعداد حالت‌های ممکن در رویدادهای مرکب"),
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

  # تغییر سطح به آسان
  observeEvent(input$easy_btn, {
    new_round("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # تغییر سطح به سخت
  observeEvent(input$hard_btn, {
    if (hard_unlocked()) {
      new_round("hard")
      runjs("$('.level-btn').removeClass('active'); $('#hard_btn').addClass('active');")
    } else {
      showNotification("برای باز شدن سطح سخت باید حداقل 80٪ پاسخ درست داشته باشی.", type = "warning")
    }
  })

  # شروع مجدد بازی
  observeEvent(list(input$reset_btn, input$btn_play_again), {
    new_round("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # سوال بعدی
  observeEvent(input$next_btn, {
    if (!rv$answered) {
      showNotification("اول پاسخ این سؤال را ثبت کن.", type = "message")
      return()
    }
    new_q(rv$level)
  })

  # ثبت پاسخ
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

  # دریافت انتخاب کاربر
  observeEvent(input$answer, {
    if (!rv$answered) {
      rv$selected <- input$answer
    }
  })

  # خروجی‌های وضعیت
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
      # ستون چپ: نمایش بصری تصویر مربوطه
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
      spinner = "چرخنده و سکه",
      cards = "انتخاب ترکیبی",
      tree = "نمودار درختی"
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
          "برای رویدادهای مرکب، معمولاً تعداد حالت‌ها را با ضرب تعداد گزینه‌های هر مرحله پیدا می‌کنیم."
        )
      )
    }

    is_correct <- identical(rv$selected, rv$question$correct)
    box_class <- if (is_correct) "feedback-box feedback-correct" else "feedback-box feedback-incorrect"

    div(
      class = box_class,
      tags$b(if (is_correct) "✅ پاسخ درست!" else "❌ پاسخ نادرست."),
      div(style = "margin-top:6px;", rv$question$explanation),
      div(
        style = "margin-top:6px; font-size:13px; font-weight:bold;",
        paste0("گزینه درست: ", rv$question$correct)
      )
    )
  })
}

shinyApp(ui, server)
