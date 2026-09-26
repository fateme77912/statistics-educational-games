# game23.R
# بازی ۲۳: تقسیم فاکتوریل‌ها (نسخه ساده‌شده با حالت‌های خاص برای محاسبه ذهنی)

library(shiny)
library(shinyjs)

# ----------------------------
# توابع کمکی
# ----------------------------

star_html <- function(n) {
  n <- max(0, min(5, n))

  tags$div(
    class = "stars-wrap",
    paste0(rep("★", n), collapse = ""),
    tags$span(
      class = "star-inactive",
      paste0(rep("★", 5 - n), collapse = "")
    )
  )
}

option_label_ui <- function(val) {
  if (grepl("/", val, fixed = TRUE)) {
    parts <- strsplit(val, "/", fixed = TRUE)[[1]]

    if (length(parts) == 2) {
      return(
        tags$span(
          class = "fraction-choice",
          tags$span(class = "frac-top", parts[1]),
          tags$span(class = "frac-bar"),
          tags$span(class = "frac-bottom", parts[2])
        )
      )
    }
  }

  tags$span(class = "whole-choice", val)
}

option_buttons_ui <- function(options) {
  tags$div(
    class = "opt-grid",

    lapply(seq_along(options), function(i) {
      val <- options[i]

      tags$button(
        type = "button",
        class = "opt-btn",

        onclick = sprintf(
          "Shiny.setInputValue('answer', '%s', {priority: 'event'});
           $('.opt-btn').removeClass('selected');
           $(this).addClass('selected');",
          val
        ),

        option_label_ui(val)
      )
    })
  )
}

# ----------------------------
# بانک سؤال‌ها
# ----------------------------

get_full_bank <- function() {
  list(
    # --- آسان (تقسیم بر فاکتوریل‌های مشابه، تقسیم بر 1! یا 0!، و تقسیم‌های با اختلاف 1 یا 2) ---
    list(
      level = "easy",
      prompt = "حاصل عبارت زیر را به‌دست آورید (نکته: تقسیم دو فاکتوریل مشابه):",
      expression = "\\frac{4!}{4!}",
      options = c("۱", "۴", "۰", "۲۴"),
      correct = "۱",
      explanation = "تقسیم هر مقدار غیر صفر بر خودش برابر با ۱ است. بنابراین ۴! ÷ ۴! = ۱"
    ),
    
    list(
      level = "easy",
      prompt = "حاصل عبارت زیر کدام است؟ (نکته: تقسیم بر صفر فاکتوریل که 0! = 1 است):",
      expression = "\\frac{3!}{0!}",
      options = c("۶", "۳", "۰", "۱"),
      correct = "۶",
      explanation = "با توجه به اینکه ۰! = ۱ و ۳! = ۶ است، پس: ۶ ÷ ۱ = ۶"
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر را محاسبه کنید (نکته: تقسیم بر یک فاکتوریل):",
      expression = "\\frac{4!}{1!}",
      options = c("۲۴", "۴", "۱۲", "۸"),
      correct = "۲۴",
      explanation = "از آنجا که ۱! = ۱ و ۴! = ۲۴ است، پس: ۲۴ ÷ ۱ = ۲۴"
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{5!}{4!}",
      options = c("۵", "۲۰", "۱", "۱۲۰"),
      correct = "۵",
      explanation = "۵! ÷ ۴! = (۵ × ۴!) ÷ ۴! = ۵"
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر را به‌دست آورید:",
      expression = "\\frac{3!}{2!}",
      options = c("۳", "۶", "۲", "۱"),
      correct = "۳",
      explanation = "۳! ÷ ۲! = (۳ × ۲) ÷ ۲ = ۳"
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر کدام است؟ (نکته: تقسیم دو فاکتوریل مشابه):",
      expression = "\\frac{6!}{6!}",
      options = c("۱", "۶", "۰", "۳۶"),
      correct = "۱",
      explanation = "هر عدد غیر صفر تقسیم بر خودش برابر با ۱ است."
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر را محاسبه کنید (نکته: تقسیم بر صفر فاکتوریل):",
      expression = "\\frac{2!}{0!}",
      options = c("۲", "۰", "۱", "۶"),
      correct = "۲",
      explanation = "با توجه به اینکه ۰! = ۱ و ۲! = ۲ است، پس: ۲ ÷ ۱ = ۲"
    ),

    list(
      level = "easy",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{4!}{2!}",
      options = c("۱۲", "۶", "۲", "۲۴"),
      correct = "۱۲",
      explanation = "۴! ÷ ۲! = (۴ × ۳ × ۲!) ÷ ۲! = ۴ × ۳ = ۱۲"
    ),

    # --- سخت (محاسبات ذهنی با ضرب‌های ۲ یا ۳ عاملی کوچک) ---
    list(
      level = "hard",
      prompt = "حاصل عبارت زیر را به‌دست آورید:",
      expression = "\\frac{5!}{3!}",
      options = c("۲۰", "۱۰", "۱۵", "۵"),
      correct = "۲۰",
      explanation = "۵! ÷ ۳! = ۵ × ۴ = ۲۰"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{6!}{4!}",
      options = c("۳۰", "۱۵", "۲۴", "۱۲"),
      correct = "۳۰",
      explanation = "۶! ÷ ۴! = ۶ × ۵ = ۳۰"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر را محاسبه کنید:",
      expression = "\\frac{7!}{5!}",
      options = c("۴۲", "۳۵", "۲۱", "۱۴"),
      correct = "۴۲",
      explanation = "۷! ÷ ۵! = ۷ × ۶ = ۴۲"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{8!}{6!}",
      options = c("۵۶", "۴۸", "۲۸", "۶۴"),
      correct = "۵۶",
      explanation = "۸! ÷ ۶! = ۸ × ۷ = ۵۶"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر را به‌دست آورید (تقسیم بر فاکتوریل با اختلاف ۳ عدد کوچک):",
      expression = "\\frac{5!}{2!}",
      options = c("۶۰", "۳۰", "۲۰", "۱۲۰"),
      correct = "۶۰",
      explanation = "۵! ÷ ۲! = ۵ × ۴ × ۳ = ۶۰"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{6!}{3!}",
      options = c("۱۲۰", "۶۰", "۳۰", "۳۶"),
      correct = "۱۲۰",
      explanation = "۶! ÷ ۳! = ۶ × ۵ × ۴ = ۱۲۰"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر را محاسبه کنید:",
      expression = "\\frac{4!}{0!}",
      options = c("۲۴", "۰", "۱۲", "۴"),
      correct = "۲۴",
      explanation = "با توجه به اینکه ۰! = ۱ و ۴! = ۲۴ است، پس: ۲۴ ÷ ۱ = ۲۴"
    ),

    list(
      level = "hard",
      prompt = "حاصل عبارت زیر کدام است؟",
      expression = "\\frac{9!}{8!}",
      options = c("۹", "۱", "۷۲", "۰"),
      correct = "۹",
      explanation = "۹! ÷ ۸! = (۹ × ۸!) ÷ ۸! = ۹"
    )
  )
}

# ----------------------------
# رابط کاربری (UI)
# ----------------------------

ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Vazirmatn:wght@400;700;800&display=swap"
    ),

    # اسکریپت ریست دکمه‌ها و فراخوانی مجدد پردازش MathJax بعد از رندر شدن بخش بازی
    tags$script(HTML("
      Shiny.addCustomMessageHandler(
        'clearOptionSelection',
        function(message) {
          $('.opt-btn').removeClass('selected');
        }
      );

      $(document).on('shiny:value', function(event) {
        if (event.target.id === 'game_layout') {
          setTimeout(function() {
            if (typeof MathJax !== 'undefined') {
              MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
            }
          }, 50);
        }
      });
    ")),

    tags$style(HTML("
      * {
        box-sizing: border-box;
      }

      html,
      body {
        min-height: 100%;
      }

      body {
        margin: 0;
        background: #f0f5f9;
        font-family: 'Vazirmatn', sans-serif;
        direction: rtl;
        color: #1e3a5a;
      }

      button,
      input {
        font-family: 'Vazirmatn', sans-serif;
      }

      .game-shell {
        width: calc(100% - 30px);
        max-width: 900px;
        margin: 30px auto;
        background: #ffffff;
        border: 4px solid #dbeafe;
        border-radius: 30px;
        box-shadow: 0 20px 50px rgba(0, 0, 0, 0.10);
        overflow: hidden;
      }

      .game-header {
        position: relative;
        padding: 25px;
        color: #ffffff;
        background: linear-gradient(
          135deg,
          #1864ab 0%,
          #4dabf7 100%
        );
      }

      .header-title {
        padding-left: 100px;
        font-size: 28px;
        font-weight: 800;
        line-height: 1.6;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.20);
      }

      .header-reset {
        position: absolute;
        top: 25px;
        left: 25px;
      }

      .header-reset .btn {
        margin: 0;
        padding: 7px 15px;
        color: #ffffff;
        background: rgba(250, 82, 82, 0.92);
        border: 1px solid rgba(255, 255, 255, 0.50);
        border-radius: 10px;
        font-size: 12px;
        font-weight: 800;
        transition: 0.25s;
      }

      .header-reset .btn:hover,
      .header-reset .btn:focus {
        color: #ffffff;
        background: #fa5252;
        border-color: #ffffff;
        outline: none;
      }

      .status-strip {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 12px;
        margin-top: 20px;
      }

      .status-badge {
        min-width: 0;
        padding: 10px 7px;
        text-align: center;
        background: rgba(255, 255, 255, 0.20);
        border: 1px solid rgba(255, 255, 255, 0.14);
        border-radius: 15px;
      }

      .status-label {
        margin-bottom: 4px;
        font-size: 12px;
        opacity: 0.90;
      }

      .status-value {
        min-height: 26px;
        font-size: 17px;
        font-weight: 700;
        white-space: nowrap;
      }

      .stars-wrap {
        color: #ffd43b;
        font-size: 21px;
        line-height: 1.25;
        white-space: nowrap;
      }

      .star-inactive {
        color: rgba(255, 255, 255, 0.30);
      }

      .main-area {
        padding: 30px;
      }

      .progress-card {
        margin-bottom: 22px;
        padding: 15px 18px;
        background: #f8fbff;
        border: 1px solid #dbeafe;
        border-radius: 18px;
      }

      .progress-info {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 15px;
        margin-bottom: 9px;
        color: #1864ab;
        font-size: 14px;
        font-weight: 800;
      }

      .progress-track {
        width: 100%;
        height: 12px;
        overflow: hidden;
        background: #e7f5ff;
        border-radius: 999px;
      }

      .progress-fill {
        height: 100%;
        background: linear-gradient(
          90deg,
          #1c7ed6,
          #4dabf7
        );
        border-radius: inherit;
        transition: width 0.45s ease;
      }

      .question-card {
        margin-bottom: 25px;
        padding: 25px;
        text-align: center;
        background: #f8fbff;
        border: 2px dashed #a5d8ff;
        border-radius: 20px;
      }

      .question-text {
        color: #1864ab;
        font-size: 22px;
        font-weight: 800;
        line-height: 1.9;
      }

      .factorial-expression {
        min-height: 70px;
        margin-top: 18px;
        padding: 12px;
        direction: ltr;
        text-align: center;
        color: #1864ab;
        font-size: 32px;
        font-weight: 800;
        background: #ffffff;
        border: 2px solid #e7f5ff;
        border-radius: 16px;
      }

      .factorial-expression mjx-container {
        margin: 0 !important;
      }

      .play-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 30px;
        align-items: start;
      }

      .answer-area {
        min-width: 0;
      }

      .opt-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 15px;
      }

      .opt-btn {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100%;
        min-height: 84px;
        padding: 18px;
        direction: ltr;
        color: #1c7ed6;
        background: #ffffff;
        border: 3px solid #e7f5ff;
        border-radius: 18px;
        font-family: 'Vazirmatn', sans-serif;
        font-size: 20px;
        font-weight: 700;
        cursor: pointer;
        transition:
          background 0.25s,
          border-color 0.25s,
          color 0.25s,
          transform 0.25s,
          box-shadow 0.25s;
      }

      .opt-btn:hover {
        color: #1864ab;
        background: #e7f5ff;
        border-color: #339af0;
      }

      .opt-btn.selected {
        color: #ffffff;
        background: #339af0;
        border-color: #1864ab;
        box-shadow: 0 5px 15px rgba(28, 126, 214, 0.22);
        transform: translateY(-3px);
      }

      .fraction-choice {
        display: inline-flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        min-width: 42px;
        line-height: 1.05;
      }

      .frac-top,
      .frac-bottom {
        display: block;
        font-size: 24px;
        font-weight: 800;
      }

      .frac-bar {
        display: block;
        width: 34px;
        margin: 4px 0 3px;
        border-top: 3px solid currentColor;
      }

      .whole-choice {
        font-size: 24px;
        font-weight: 800;
      }

      .action-row {
        display: flex;
        flex-wrap: wrap;
        gap: 15px;
        margin-top: 25px;
      }

      .action-btn {
        min-width: 130px;
        padding: 12px 30px !important;
        border-radius: 15px !important;
        font-family: 'Vazirmatn', sans-serif !important;
        font-size: 18px !important;
        font-weight: 800 !important;
      }

      .primary-action {
        color: #ffffff !important;
        background: #1c7ed6 !important;
        border: none !important;
      }

      .primary-action:hover,
      .primary-action:focus {
        color: #ffffff !important;
        background: #1864ab !important;
      }

      .secondary-action {
        color: #1864ab !important;
        background: #e7f5ff !important;
        border: 2px solid #a5d8ff !important;
      }

      .secondary-action:hover,
      .secondary-action:focus {
        color: #ffffff !important;
        background: #339af0 !important;
        border-color: #339af0 !important;
      }

      .feedback-box {
        margin-top: 25px;
        padding: 20px;
        border-radius: 20px;
        font-size: 18px;
        line-height: 1.9;
      }

      .feedback-title {
        display: block;
        margin-bottom: 5px;
      }

      .f-ok {
        color: #2b8a3e;
        background: #d3f9d8;
        border: 2px solid #b2f2bb;
      }

      .f-bad {
        color: #c92a2a;
        background: #fff5f5;
        border: 2px solid #ffc9c9;
      }

      .result-card {
        padding: 40px 25px;
        text-align: center;
        background: #f8fbff;
        border: 2px dashed #a5d8ff;
        border-radius: 24px;
      }

      .result-title {
        margin-top: 0;
        color: #1864ab;
        font-weight: 800;
        line-height: 1.8;
      }

      .result-details {
        margin: 25px 0;
        font-size: 20px;
        line-height: 2;
      }

      .result-stars .stars-wrap {
        margin: 15px 0;
        color: #ffd43b;
        font-size: 32px;
      }

      .result-stars .star-inactive {
        color: #dbeafe;
      }

      .restart-action {
        color: #ffffff !important;
        background: #1c7ed6 !important;
        border: none !important;
      }

      .restart-action:hover,
      .restart-action:focus {
        color: #ffffff !important;
        background: #1864ab !important;
      }

      @media (max-width: 768px) {
        .game-shell {
          width: calc(100% - 20px);
          margin: 15px auto;
          border-width: 3px;
          border-radius: 24px;
        }

        .game-header {
          padding: 22px 18px;
        }

        .header-title {
          padding-left: 85px;
          font-size: 24px;
        }

        .header-reset {
          top: 22px;
          left: 18px;
        }

        .status-strip {
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 9px;
        }

        .status-badge {
          padding: 9px 5px;
        }

        .main-area {
          padding: 20px;
        }

        .question-card {
          padding: 20px 16px;
        }

        .question-text {
          font-size: 19px;
        }

        .factorial-expression {
          font-size: 28px;
        }
      }

      @media (max-width: 520px) {
        .game-shell {
          width: calc(100% - 12px);
          margin: 8px auto;
          border-width: 2px;
          border-radius: 20px;
        }

        .game-header {
          padding: 18px 12px;
        }

        .header-title {
          padding-left: 0;
          font-size: 21px;
          text-align: center;
        }

        .header-reset {
          position: static;
          margin-top: 12px;
          text-align: center;
        }

        .status-strip {
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 8px;
          margin-top: 16px;
        }

        .status-badge:last-child {
          grid-column: 1 / -1;
        }

        .main-area {
          padding: 14px;
        }

        .progress-card {
          padding: 13px;
        }

        .question-card {
          padding: 18px 13px;
        }

        .question-text {
          font-size: 17px;
          line-height: 1.8;
        }

        .factorial-expression {
          min-height: 62px;
          padding: 9px;
          font-size: 25px;
        }

        .opt-grid {
          gap: 10px;
        }

        .opt-btn {
          min-height: 72px;
          padding: 12px 8px;
          border-width: 2px;
          border-radius: 14px;
        }

        .whole-choice,
        .frac-top,
        .frac-bottom {
          font-size: 21px;
        }

        .action-row {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 10px;
        }

        .action-btn {
          width: 100%;
          min-width: 0;
          padding: 11px 12px !important;
          font-size: 16px !important;
        }

        .feedback-box {
          padding: 16px;
          font-size: 16px;
        }

        .result-card {
          padding: 30px 15px;
        }
      }
    "))
  ),

  withMathJax(),

  div(
    class = "game-shell",

    div(
      class = "game-header",

      div(
        class = "header-title",
        "تقسیم فاکتوریل‌ها"
      ),

      div(
        class = "header-reset",
        actionButton(
          inputId = "reset_btn",
          label = "نوسازی"
        )
      ),

      div(
        class = "status-strip",

        div(
          class = "status-badge",
          div(class = "status-label", "سطح"),
          div(
            class = "status-value",
            textOutput("level_txt", inline = TRUE)
          )
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "امتیاز"),
          div(
            class = "status-value",
            textOutput("score_txt", inline = TRUE)
          )
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "دقت"),
          div(
            class = "status-value",
            textOutput("acc_txt", inline = TRUE)
          )
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "سؤال"),
          div(
            class = "status-value",
            textOutput("q_step_txt", inline = TRUE)
          )
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "ستاره‌ها"),
          uiOutput("stars_ui")
        )
      )
    ),

    div(
      class = "main-area",
      uiOutput("progress_ui"),
      uiOutput("game_layout")
    )
  )
)

# ----------------------------
# بخش سرور (Server)
# ----------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    level = "easy",
    score = 0,
    total = 0,
    correct = 0,
    answered = FALSE,
    q_num = 1,
    question = NULL,
    current_pool = list(),
    game_over = FALSE
  )

  init_game <- function() {
    rv$level <- "easy"
    rv$score <- 0
    rv$total <- 0
    rv$correct <- 0
    rv$answered <- FALSE
    rv$q_num <- 1
    rv$game_over <- FALSE

    all_qs <- get_full_bank()

    rv$current_pool <- sample(
      all_qs,
      min(10, length(all_qs))
    )

    q_data <- rv$current_pool[[1]]
    q_data$options <- sample(q_data$options)

    rv$question <- q_data
    rv$level <- rv$question$level

    session$sendCustomMessage("clearOptionSelection", list())
  }

  observe({
    if (is.null(rv$question)) {
      init_game()
    }
  })

  observeEvent(input$reset_btn, {
    init_game()
  })

  observeEvent(input$check_btn, {
    if (is.null(input$answer) || rv$answered || rv$game_over) return()

    rv$answered <- TRUE
    rv$total <- rv$total + 1

    if (identical(as.character(input$answer), as.character(rv$question$correct))) {
      rv$correct <- rv$correct + 1
      rv$score <- rv$score + 10
    }
  })

  observeEvent(input$next_btn, {
    if (!rv$answered || rv$game_over) return()

    if (rv$q_num >= 10 || rv$q_num >= length(rv$current_pool)) {
      rv$game_over <- TRUE
    } else {
      rv$q_num <- rv$q_num + 1
      rv$answered <- FALSE

      q_data <- rv$current_pool[[rv$q_num]]
      q_data$options <- sample(q_data$options)

      rv$question <- q_data
      rv$level <- rv$question$level

      session$sendCustomMessage("clearOptionSelection", list())
    }
  })

  observeEvent(input$restart_btn, {
    init_game()
  })

  output$q_step_txt <- renderText({
    paste(rv$q_num, "از ۱۰")
  })

  output$level_txt <- renderText({
    if (rv$level == "easy") "آسان" else "سخت"
  })

  output$score_txt <- renderText({
    rv$score
  })

  output$acc_txt <- renderText({
    if (rv$total == 0) {
      "۰٪"
    } else {
      paste0(round(100 * rv$correct / rv$total), "٪")
    }
  })

  output$stars_ui <- renderUI({
    star_html(floor(rv$score / 20))
  })

  output$progress_ui <- renderUI({
    progress_value <- min(
      100,
      round(100 * rv$q_num / 10)
    )

    div(
      class = "progress-card",

      div(
        class = "progress-info",
        tags$span("پیشرفت بازی"),
        tags$span(paste0(rv$q_num, " از ۱۰"))
      ),

      div(
        class = "progress-track",
        div(
          class = "progress-fill",
          style = sprintf(
            "width: %s%%;",
            progress_value
          )
        )
      )
    )
  })

  output$game_layout <- renderUI({

    if (rv$game_over) {
      return(
        div(
          class = "result-card",

          tags$h2(
            class = "result-title",
            "آفرین! مرحلهٔ تقسیم فاکتوریل‌ها تمام شد"
          ),

          div(
            class = "result-details",

            tags$p(
              paste(
                "تعداد پاسخ‌های صحیح:",
                rv$correct,
                "از",
                rv$total
              )
            ),

            tags$p(
              paste(
                "امتیاز نهایی:",
                rv$score
              )
            ),

            div(
              class = "result-stars",
              star_html(floor(rv$score / 20))
            )
          ),

          actionButton(
            inputId = "restart_btn",
            label = "دوباره بازی کن",
            class = "action-btn restart-action"
          )
        )
      )
    }

    tagList(
      div(
        class = "question-card",

        div(
          class = "question-text",
          rv$question$prompt
        ),

        div(
          class = "factorial-expression",
          withMathJax(
            paste0("$$", rv$question$expression, "$$")
          )
        )
      ),

      div(
        class = "play-grid",

        div(
          class = "answer-area",

          option_buttons_ui(rv$question$options),

          div(
            class = "action-row",

            actionButton(
              inputId = "check_btn",
              label = "ثبت پاسخ",
              class = "action-btn primary-action"
            ),

            actionButton(
              inputId = "next_btn",
              label = if (rv$q_num == 10 || rv$q_num >= length(rv$current_pool)) "کارنامه" else "بعدی",
              class = "action-btn secondary-action"
            )
          ),

          uiOutput("feedback_ui")
        )
      )
    )
  })

  output$feedback_ui <- renderUI({
    if (!rv$answered) return(NULL)

    is_correct <- identical(
      as.character(input$answer),
      as.character(rv$question$correct)
    )

    div(
      class = if (is_correct) "feedback-box f-ok" else "feedback-box f-bad",

      tags$b(
        class = "feedback-title",
        if (is_correct) {
          "پاسخ شما درست است."
        } else {
          "پاسخ شما درست نیست."
        }
      ),

      div(rv$question$explanation)
    )
  })
}

shinyApp(ui, server)
