library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

# تصاویر مفهومی عمومی مرتبط با ترتیب (جایگشت) و انتخاب گروهی (ترکیب)
svg_permutation_podium <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f3f0ff"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#6741d9">جایگشت (ترتیب مهم است)</text>
    <!-- سکوی قهرمانی -->
    <rect x="70" y="130" width="50" height="60" fill="#dee2e6" stroke="#495057" stroke-width="2"/>
    <rect x="130" y="100" width="60" height="90" fill="#f1f3f5" stroke="#495057" stroke-width="2"/>
    <rect x="200" y="150" width="50" height="40" fill="#ced4da" stroke="#495057" stroke-width="2"/>
    <text x="95" y="160" text-anchor="middle" font-size="14" font-weight="bold" fill="#495057">۲</text>
    <text x="160" y="130" text-anchor="middle" font-size="18" font-weight="bold" fill="#495057">۱</text>
    <text x="225" y="170" text-anchor="middle" font-size="14" font-weight="bold" fill="#495057">۳</text>
    <text x="160" y="215" text-anchor="middle" font-size="12" fill="#868e96">مدال طلا، نقره و برنز (جایگاه‌های مختلف)</text>
  </svg>'
  svg_data_uri(svg)
}

svg_combination_basket <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#ebfbee"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#2b8a3e">ترکیب (ترتیب مهم نیست)</text>
    <!-- سبد میوه مفهومی -->
    <ellipse cx="160" cy="140" rx="60" ry="30" fill="#ffe3e3" stroke="#ff6b6b" stroke-width="2"/>
    <circle cx="140" cy="125" r="15" fill="#f03e3e"/>
    <circle cx="170" cy="120" r="18" fill="#fab005"/>
    <circle cx="180" cy="135" r="14" fill="#40c057"/>
    <text x="160" y="200" text-anchor="middle" font-size="12" fill="#5c940d">انتخاب میوه‌ها برای سالاد (تنها عضویت مهم است)</text>
  </svg>'
  svg_data_uri(svg)
}

svg_math_notation <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#fff9db"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#e67700">فرمول‌های ریاضی</text>
    <rect x="50" y="70" width="100" height="100" rx="10" fill="#fff" stroke="#ffd43b" stroke-width="2"/>
    <text x="100" y="115" text-anchor="middle" font-size="18" font-weight="bold" fill="#fab005">P(n, r)</text>
    <text x="100" y="145" text-anchor="middle" font-size="11" fill="#495057">n! / (n-r)!</text>
    <rect x="170" y="70" width="100" height="100" rx="10" fill="#fff" stroke="#ffd43b" stroke-width="2"/>
    <text x="220" y="115" text-anchor="middle" font-size="18" font-weight="bold" fill="#fab005">C(n, r)</text>
    <text x="220" y="145" text-anchor="middle" font-size="11" fill="#495057">n! / [r!(n-r)!]</text>
    <text x="160" y="205" text-anchor="middle" font-size="12" fill="#868e96">ابزار محاسبه تعداد کل حالات ممکن</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    podium = svg_permutation_podium(),
    basket = svg_combination_basket(),
    notation = svg_math_notation()
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
# Question Bank (16 Unique Questions)
# ----------------------------
get_full_bank <- function() {
  list(
    # Easy Questions
    list(
      kind = "notation", level = "easy",
      question = "حاصل عبارت P(5, 2) برابر با کدام است؟",
      options = c("10", "20", "60", "120"),
      correct = "20",
      explanation = "فرمول جایگشت: P(5, 2) = 5! / (5-2)! = (5 × 4 × 3!) / 3! = 5 × 4 = 20."
    ),
    list(
      kind = "notation", level = "easy",
      question = "حاصل عبارت C(6, 2) برابر با کدام است؟",
      options = c("15", "30", "12", "36"),
      correct = "15",
      explanation = "فرمول ترکیب: C(6, 2) = 6! / (2! × 4!) = (6 × 5) / 2 = 15."
    ),
    list(
      kind = "podium", level = "easy",
      question = "می‌خواهیم از بین ۱۰ نفر، یک رئیس و یک نایب‌رئیس انتخاب کنیم. کدام فرمول برای این کار مناسب است؟",
      options = c("P(10, 2)", "C(10, 2)", "P(10, 10)", "C(10, 8)"),
      correct = "P(10, 2)",
      explanation = "چون نقش‌ها متفاوت هستند (رئیس و نایب‌رئیس)، ترتیب انتخاب اهمیت دارد؛ بنابراین از جایگشت P(10, 2) استفاده می‌شود."
    ),
    list(
      kind = "basket", level = "easy",
      question = "می‌خواهیم از بین ۸ نفر دانش‌آموز، یک گروه پژوهشی ۳ نفره تشکیل دهیم. کدام فرمول برای محاسبه حالات مناسب است؟",
      options = c("C(8, 3)", "P(8, 3)", "C(8, 5)", "P(8, 5)"),
      correct = "C(8, 3)",
      explanation = "چون اعضای گروه پژوهشی هم‌رتبه هستند و ترتیب در چیدمان آن‌ها اهمیتی ندارد، از فرمول ترکیب C(8, 3) استفاده می‌شود."
    ),
    list(
      kind = "notation", level = "easy",
      question = "مقدار عددی عبارت C(5, 5) چقدر است؟",
      options = c("1", "5", "120", "0"),
      correct = "1",
      explanation = "تعداد روش‌های انتخاب ۵ نفر از ۵ نفر بدون در نظر گرفتن ترتیب، همیشه برابر با ۱ است. فرمول: 5! / (5! × 0!) = 1."
    ),
    list(
      kind = "notation", level = "easy",
      question = "مقدار عددی عبارت P(4, 1) چقدر است؟",
      options = c("4", "24", "1", "12"),
      correct = "4",
      explanation = "تعداد روش‌های انتخاب و چیدمان ۱ شیء از بین ۴ شیء برابر با ۴ است. فرمول: 4! / 3! = 4."
    ),
    list(
      kind = "basket", level = "easy",
      question = "در یک مسابقه نقاشی، می‌خواهیم فقط ۳ نقاشی برتر را بدون رتبه‌بندی مشخص کنیم تا به مرحله بعد بروند. این مسئله مربوط به کدام مفهوم است؟",
      options = c("ترکیب (Combination)", "جایگشت (Permutation)", "اصل جمع", "احتمال شرطی"),
      correct = "ترکیب (Combination)",
      explanation = "چون رتبه‌بندی و ترتیبی بین نقاشی‌های صعودکننده وجود ندارد، با ترکیب سروکار داریم."
    ),
    list(
      kind = "podium", level = "easy",
      question = "در یک مسابقه دو و میدانی با ۸ شرکت‌کننده، مدال‌های طلا، نقره و برنز توزیع می‌شود. این مسئله مربوط به کدام مفهوم است؟",
      options = c("جایگشت (Permutation)", "ترکیب (Combination)", "قانون فاکتوریل ساده", "فضای نمونه تصادفی مستقل"),
      correct = "جایگشت (Permutation)",
      explanation = "چون مدال‌ها متفاوت هستند، ترتیب قرارگیری افراد در سکوها بر نتیجه نهایی تاثیرگذار است و نیاز به جایگشت داریم."
    ),
    
    # Hard Questions
    list(
      kind = "notation", level = "hard",
      question = "حاصل عبارت C(7, 3) + P(4, 2) چقدر است؟",
      options = c("47", "35", "12", "23"),
      correct = "47",
      explanation = "C(7, 3) = 35 و P(4, 2) = 12. حاصل‌جمع آن‌ها: 35 + 12 = 47."
    ),
    list(
      kind = "notation", level = "hard",
      question = "اگر داشته باشیم P(n, 2) = 30، مقدار n چقدر است؟",
      options = c("6", "5", "15", "10"),
      correct = "6",
      explanation = "رابطه P(n, 2) = n(n-1) است. حاصل‌ضرب دو عدد متوالی که ۳۰ می‌شود ۶ و ۵ است. پس n برابر با ۶ است."
    ),
    list(
      kind = "basket", level = "hard",
      question = "در یک جعبه ۵ مهره قرمز و ۴ مهره آبی قرار دارد. می‌خواهیم ۲ مهره قرمز و ۱ مهره آبی انتخاب کنیم. تعداد حالات چقدر است؟",
      options = c("40", "20", "80", "120"),
      correct = "40",
      explanation = "حالات انتخاب قرمز: C(5, 2) = 10. حالات انتخاب آبی: C(4, 1) = 4. طبق اصل ضرب: 10 × 4 = 40."
    ),
    list(
      kind = "podium", level = "hard",
      question = "با حروف کلمه 'MATH' (بدون تکرار حروف) چند کلمه ۴ حرفی می‌توان ساخت؟",
      options = c("24", "12", "16", "4"),
      correct = "24",
      explanation = "جایگشت ۴ شیء متمایز برابر است با P(4, 4) = 4! = 4 × 3 × 2 × 1 = 24."
    ),
    list(
      kind = "notation", level = "hard",
      question = "کدام‌یک از رابطه‌های زیر همواره صحیح است؟",
      options = c("P(n, r) = r! * C(n, r)", "C(n, r) = r! * P(n, r)", "P(n, r) = C(n, r) / r!", "هیچ‌کدام"),
      correct = "P(n, r) = r! * C(n, r)",
      explanation = "جایگشت برابر است با ترکیب ضرب‌در فاکتوریل تعداد انتخاب‌ها؛ زیرا در جایگشت ترتیب اشیاء انتخاب‌شده نیز اعمال می‌شود."
    ),
    list(
      kind = "basket", level = "hard",
      question = "یک مربی فوتسال می‌خواهد از بین ۶ بازیکن آماده، تیم ۵ نفره خود را به زمین بفرستد. او به چند طریق می‌تواند ترکیب اولیه را بچیند؟",
      options = c("6", "30", "120", "720"),
      correct = "6",
      explanation = "چون پست‌ها مشخص نشده و فقط حضور در زمین ملاک است، از ترکیب استفاده می‌کنیم: C(6, 5) = 6."
    ),
    list(
      kind = "notation", level = "hard",
      question = "حاصل نسبت P(8, 3) تقسیم بر C(8, 3) برابر با کدام گزینه است؟",
      options = c("6", "8", "3", "24"),
      correct = "6",
      explanation = "نسبت جایگشت به ترکیب برای r=3 برابر با 3! یا همان ۶ است."
    ),
    list(
      kind = "podium", level = "hard",
      question = "چند عدد ۳ رقمی بدون تکرار ارقام می‌توان با استفاده از ارقام {1, 3, 5, 7, 9} ساخت؟",
      options = c("60", "125", "10", "20"),
      correct = "60",
      explanation = "ترتیب قرارگیری ارقام مهم است. پس از جایگشت استفاده می‌کنیم: P(5, 3) = 5 × 4 × 3 = 60."
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
      div(class = "top-title", "جایگشت و ترکیب (فرمول‌ها و مفاهیم)"),
      div(
        class = "top-controls",
        actionButton("reset_btn", "شروع مجدد", class = "reset-top-btn")
      )
    ),

    # نوار وضعیت ۵تایی مرجع
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
  observeEvent(list(input$reset_btn, input$btn_play_again), {
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
      podium = "جایگشت (ترتیب مهم است)",
      basket = "ترکیب (ترتیب مهم نیست)",
      notation = "فرمول و محاسبات ریاضی"
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
