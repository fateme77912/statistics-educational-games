library(shiny)
library(shinyjs)

# ----------------------------
# سناریوهای واقعی برای نمودار جعبه ای
# ----------------------------
boxplot_scenarios <- list(
  list(
    title = "زمان مطالعه دانش آموزان",
    unit = "دقیقه",
    intro = "نمودار جعبه ای زیر زمان مطالعه دانش آموزان را نشان می دهد."
  ),
  list(
    title = "امتیاز آزمون ریاضی",
    unit = "امتیاز",
    intro = "نمودار جعبه ای زیر امتیازهای یک آزمون ریاضی را نشان می دهد."
  ),
  list(
    title = "تعداد صفحات کتاب خوانده شده",
    unit = "صفحه",
    intro = "نمودار جعبه ای زیر تعداد صفحات خوانده شده را نشان می دهد."
  ),
  list(
    title = "زمان دویدن روزانه",
    unit = "دقیقه",
    intro = "نمودار جعبه ای زیر زمان دویدن افراد را نشان می دهد."
  ),
  list(
    title = "قد گیاهان",
    unit = "سانتی متر",
    intro = "نمودار جعبه ای زیر قد چند گیاه را نشان می دهد."
  )
)

compute_five_number <- function(x) {
  as.numeric(fivenum(sort(x)))
}

generate_boxplot_data <- function(level = "easy") {
  n <- if (level == "easy") 7 else 9

  repeat {
    if (level == "easy") {
      base <- sort(sample(seq(6, 26, by = 2), n, replace = FALSE))
    } else {
      base <- sort(sample(seq(8, 48, by = 2), n, replace = FALSE))
    }

    stats <- compute_five_number(base)

    if (length(unique(stats)) >= 4) {
      return(base)
    }
  }
}

build_distractor <- function(correct_answer, candidates_pool) {
  candidates <- unique(candidates_pool[candidates_pool != correct_answer])

  if (length(candidates) > 0) {
    return(sample(candidates, 1))
  }

  wrong_offset <- sample(c(-6, -4, -2, 2, 4, 6), 1)
  wrong_answer <- correct_answer + wrong_offset
  if (wrong_answer == correct_answer) {
    wrong_answer <- correct_answer + 2
  }
  wrong_answer
}

generate_boxplot_question <- function(level = "easy") {
  scenario <- boxplot_scenarios[[sample(seq_along(boxplot_scenarios), 1)]]
  values <- generate_boxplot_data(level)
  stats <- compute_five_number(values)

  names(stats) <- c("min", "q1", "median", "q3", "max")
  range_val <- stats["max"] - stats["min"]
  iqr_val <- stats["q3"] - stats["q1"]

  if (level == "easy") {
    metric <- sample(c("min", "median", "max"), 1)
  } else {
    metric <- sample(c("min", "q1", "median", "q3", "max", "range", "iqr"), 1)
  }

  metric_fa <- switch(
    metric,
    "min" = "کمینه",
    "q1" = "چارک اول",
    "median" = "میانه",
    "q3" = "چارک سوم",
    "max" = "بیشینه",
    "range" = "دامنه تغییرات",
    "iqr" = "فاصله بین چارکی"
  )

  correct_answer <- switch(
    metric,
    "min" = stats["min"],
    "q1" = stats["q1"],
    "median" = stats["median"],
    "q3" = stats["q3"],
    "max" = stats["max"],
    "range" = range_val,
    "iqr" = iqr_val
  )

  distractor_pool <- c(
    stats["min"], stats["q1"], stats["median"], stats["q3"], stats["max"],
    range_val, iqr_val
  )
  wrong_answer <- build_distractor(correct_answer, distractor_pool)
  options <- sample(c(correct_answer, wrong_answer))

  question_text <- paste0(
    scenario$intro, " ",
    "با توجه به این نمودار، <b>", metric_fa, "</b> برابر با کدام گزینه است؟"
  )

  list(
    question = question_text,
    plot_title = scenario$title,
    unit = scenario$unit,
    values = values,
    stats = stats,
    options = options,
    answer = correct_answer
  )
}

draw_boxplot_panel <- function(q_data) {
  stats <- q_data$stats
  x_min <- min(stats["min"], q_data$answer, q_data$options)
  x_max <- max(stats["max"], q_data$answer, q_data$options)
  padding <- max(4, ceiling((x_max - x_min) * 0.15))

  plot(
    NA,
    xlim = c(x_min - padding, x_max + padding),
    ylim = c(0, 1),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )

  axis(1, at = pretty(c(x_min, x_max)), labels = pretty(c(x_min, x_max)), cex.axis = 1.1, col.axis = "#4a6170")
  segments(x_min - padding / 2, 0.5, x_max + padding / 2, 0.5, col = "#a8d7f5", lwd = 2)

  # ویسکرها
  segments(stats["min"], 0.5, stats["q1"], 0.5, col = "#1f95df", lwd = 4)
  segments(stats["q3"], 0.5, stats["max"], 0.5, col = "#1f95df", lwd = 4)

  # کلاهک ها
  segments(stats["min"], 0.38, stats["min"], 0.62, col = "#1f95df", lwd = 4)
  segments(stats["max"], 0.38, stats["max"], 0.62, col = "#1f95df", lwd = 4)

  # جعبه
  rect(stats["q1"], 0.28, stats["q3"], 0.72, col = "#dff3ff", border = "#1f95df", lwd = 4)

  # میانه
  segments(stats["median"], 0.28, stats["median"], 0.72, col = "#ff9f1c", lwd = 4)

  title(main = q_data$plot_title, col.main = "#28536b", font.main = 2, cex.main = 1.2)
  mtext(q_data$unit, side = 1, line = 2.2, col = "#496576", cex = 1)
}

star_html <- function(n) {
  n <- max(0, min(5, n))
  tags$div(
    class = "stars-wrap",
    tags$span(class = "star-active", paste(rep("★", n), collapse = "")),
    if (n < 5) {
      tags$span(class = "star-inactive", paste(rep("★", 5 - n), collapse = ""))
    }
  )
}

# ----------------------------
# رابط کاربری (قالب مرجع)
# ----------------------------
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Vazirmatn:wght@400;500;700;800&display=swap"
    ),
    tags$style(HTML("
      * {
        box-sizing: border-box;
      }

      body {
        margin: 0;
        background: linear-gradient(180deg, #f6fbff 0%, #edf8fe 100%);
        font-family: 'Vazirmatn', Tahoma, sans-serif;
        direction: rtl;
        text-align: right;
        color: #183153;
      }

      button, input {
        font-family: 'Vazirmatn', Tahoma, sans-serif !important;
      }

      .game-shell {
        width: calc(100% - 24px);
        max-width: 1040px;
        margin: 18px auto 26px;
        background: #ffffff;
        border: 2px solid #dceff7;
        border-radius: 26px;
        box-shadow: 0 14px 34px rgba(22, 91, 130, 0.10);
        overflow: hidden;
      }

      .game-header {
        padding: 18px 22px 16px;
        color: #ffffff;
        background: linear-gradient(135deg, #33b4ff 0%, #1698ea 100%);
      }

      .header-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 14px;
        flex-wrap: wrap;
      }

      .header-title {
        font-size: 28px;
        font-weight: 800;
        line-height: 1.4;
      }

      .header-actions {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
      }

      .top-btn {
        min-height: 42px;
        padding: 8px 16px !important;
        border: 0 !important;
        border-radius: 999px !important;
        font-weight: 800 !important;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.10);
      }

      .btn-easy {
        color: #ffffff !important;
        background: #8cd348 !important;
      }

      .btn-hard {
        color: #6b4b00 !important;
        background: #ffd43b !important;
      }

      .btn-reset {
        color: #ffffff !important;
        background: #ff6b6b !important;
      }

      .status-strip {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 10px;
        margin-top: 14px;
      }

      .status-badge {
        min-width: 0;
        min-height: 64px;
        padding: 9px 8px;
        text-align: center;
        background: rgba(255, 255, 255, 0.92);
        border: 1px solid rgba(255, 255, 255, 0.95);
        border-radius: 18px;
        box-shadow: 0 6px 14px rgba(8, 91, 145, 0.12);
      }

      .status-label {
        margin-bottom: 2px;
        color: #5f7a96;
        font-size: 12px;
        font-weight: 700;
      }

      .status-value {
        color: #0f3b6d;
        font-size: 16px;
        font-weight: 800;
        line-height: 1.5;
        overflow-wrap: anywhere;
      }

      .stars-wrap {
        direction: ltr;
        min-height: 30px;
        color: #ffcc33;
        font-size: 28px;
        line-height: 1;
        text-align: center;
        white-space: nowrap;
      }

      .star-active {
        color: #ffcc33;
      }

      .star-inactive {
        color: #d8dfe8;
      }

      .main-area {
        padding: 18px;
      }

      .progress-card {
        padding: 12px 16px;
        margin-bottom: 14px;
        background: #f3fbfe;
        border: 2px solid #dceff7;
        border-radius: 20px;
      }

      .progress-row {
        display: flex;
        align-items: center;
        gap: 12px;
      }

      .progress-pill {
        flex: 0 0 auto;
        padding: 6px 13px;
        color: #145388;
        font-size: 13px;
        font-weight: 800;
        white-space: nowrap;
        background: #ffffff;
        border: 1px solid #d5ecfb;
        border-radius: 999px;
      }

      .game-progress {
        flex: 1;
        min-width: 0;
        height: 16px;
        overflow: hidden;
        background: #e4f3fa;
        border-radius: 999px;
      }

      .game-progress-bar {
        height: 100%;
        background: linear-gradient(90deg, #8cd348 0%, #4dabf7 100%);
        border-radius: 999px;
        transition: width 0.25s ease;
      }

      .question-card {
        padding: 18px 22px;
        margin-bottom: 14px;
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
      }

      .question-text {
        margin: 0;
        color: #0f3b6d;
        font-size: 19px;
        font-weight: 700;
        line-height: 2;
        text-align: center;
      }

      .question-text b {
        color: #087bbf;
        font-weight: 800;
      }

      .play-grid {
        display: grid;
        grid-template-columns: minmax(0, 58fr) minmax(0, 42fr);
        gap: 14px;
        align-items: stretch;
      }

      .game-card, .answer-card {
        min-width: 0;
        min-height: 360px;
        padding: 18px;
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
      }

      .visual-label {
        margin: 4px 0 14px;
        color: #0f3b6d;
        font-size: 15px;
        font-weight: 800;
        text-align: center;
      }

      .plot-frame {
        background: #ffffff;
        border: 2px solid #bfe3fb;
        border-radius: 15px;
        padding: 12px;
      }

      .answer-card {
        display: flex;
        flex-direction: column;
        justify-content: center;
        background: linear-gradient(180deg, #ffffff 0%, #f8fdff 100%);
      }

      .option-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px;
        width: 100%;
        max-width: 440px;
        margin: 4px auto 0;
      }

      .option-btn {
        width: 100%;
        min-width: 0;
        min-height: 62px;
        padding: 10px !important;
        color: #ffffff !important;
        font-size: 24px !important;
        font-weight: 800 !important;
        background: linear-gradient(180deg, #3eb7ff 0%, #1e9bec 100%) !important;
        border: 0 !important;
        border-radius: 14px !important;
        box-shadow: inset 0 -3px 0 rgba(0, 0, 0, 0.12), 0 4px 8px rgba(30, 155, 236, 0.16);
        transition: transform 0.15s ease, filter 0.15s ease;
      }

      .option-btn:hover, .option-btn:focus {
        color: #ffffff !important;
        filter: brightness(1.04);
        transform: translateY(-1px);
      }

      .option-btn:disabled {
        cursor: not-allowed;
        filter: grayscale(0.35);
        opacity: 0.65;
        transform: none;
      }

      .feedback-ok, .feedback-bad, .hint-box {
        width: 100%;
        padding: 12px 14px;
        margin-top: 16px;
        font-size: 14px;
        line-height: 1.9;
        text-align: center;
        border-radius: 14px;
      }

      .feedback-ok {
        color: #1f6b2d;
        background: #effcf1;
        border: 1px solid #8cd348;
      }

      .feedback-bad {
        color: #8a4b00;
        background: #fff6e6;
        border: 1px solid #ffa94d;
      }

      .hint-box {
        color: #60748b;
        background: #f8fdff;
        border: 1px dashed #d5ecfb;
      }

      .result-box {
        padding: 30px 22px;
        text-align: center;
        background: linear-gradient(180deg, #ffffff 0%, #f3fbfe 100%);
        border: 2px solid #dceff7;
        border-radius: 22px;
      }

      .result-title {
        margin-bottom: 12px;
        color: #2094de;
        font-size: 29px;
        font-weight: 800;
      }

      .result-text {
        margin-bottom: 10px;
        color: #3d5a6f;
        font-size: 20px;
      }

      .result-score {
        margin-bottom: 16px;
        color: #21a769;
        font-size: 28px;
        font-weight: 800;
      }

      .final-btn {
        min-width: 170px;
        min-height: 44px;
        padding: 9px 18px !important;
        color: #ffffff !important;
        font-weight: 800 !important;
        background: #2cab59 !important;
        border: 0 !important;
        border-radius: 14px !important;
      }

      @media (max-width: 900px) {
        .status-strip {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .status-badge:last-child {
          grid-column: 1 / -1;
        }
        .play-grid {
          grid-template-columns: 1fr;
        }
        .game-card, .answer-card {
          min-height: auto;
        }
      }

      @media (max-width: 600px) {
        .game-shell {
          width: calc(100% - 20px);
          margin: 10px auto;
          border-radius: 20px;
        }
        .game-header {
          padding: 16px 14px;
        }
        .header-title {
          width: 100%;
          font-size: 22px;
          text-align: center;
        }
        .header-actions {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          width: 100%;
        }
        .btn-reset {
          grid-column: 1 / -1;
        }
        .top-btn {
          width: 100%;
          padding: 8px !important;
        }
        .status-strip {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .status-badge:last-child {
          grid-column: 1 / -1;
        }
        .main-area {
          padding: 12px;
        }
        .progress-row {
          align-items: stretch;
          flex-direction: column;
        }
        .game-progress {
          flex: none;
          width: 100%;
        }
        .question-card {
          padding: 14px;
        }
        .question-text {
          font-size: 17px;
        }
        .game-card, .answer-card {
          padding: 13px;
        }
        .option-grid {
          grid-template-columns: 1fr;
        }
        .option-btn {
          min-height: 54px;
          font-size: 21px !important;
        }
      }
    "))
  ),

  div(
    class = "game-shell",

    div(
      class = "game-header",

      div(
        class = "header-top",

        div(
          class = "header-title",
          "تفسیر نمودار جعبه‌ای"
        ),

        div(
          class = "header-actions",
          actionButton("btn_easy", "سطح آسان", class = "top-btn btn-easy"),
          actionButton("btn_hard", "سطح سخت", class = "top-btn btn-hard"),
          actionButton("btn_reset", "شروع مجدد", class = "top-btn btn-reset")
        )
      ),

      div(
        class = "status-strip",

        div(
          class = "status-badge",
          div(class = "status-label", "سطح"),
          div(class = "status-value", textOutput("level_txt"))
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "پاسخ‌های درست"),
          div(class = "status-value", textOutput("score_txt"))
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "درصد موفقیت"),
          div(class = "status-value", textOutput("accuracy_txt"))
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "وضعیت سطح سخت"),
          div(class = "status-value", textOutput("lock_txt"))
        ),

        div(
          class = "status-badge",
          div(class = "status-label", "ستاره‌ها"),
          div(class = "status-value", uiOutput("stars_ui"))
        )
      )
    ),

    div(
      class = "main-area",
      uiOutput("game_content")
    )
  )
)

# ----------------------------
# منطق سرور
# ----------------------------
server <- function(input, output, session) {
  state <- reactiveValues(
    level = "easy",
    hard_unlocked = FALSE,
    question_idx = 1,
    correct_count = 0,
    current_q = NULL,
    feedback = NULL,
    answered = FALSE
  )

  load_question <- function() {
    state$answered <- FALSE
    state$feedback <- NULL
    state$current_q <- generate_boxplot_question(state$level)
  }

  observe({
    if (is.null(state$current_q)) {
      load_question()
    }
  })

  observeEvent(input$btn_easy, {
    state$level <- "easy"
    state$question_idx <- 1
    state$correct_count <- 0
    state$feedback <- NULL
    load_question()
  })

  observeEvent(input$btn_hard, {
    if (state$hard_unlocked) {
      state$level <- "hard"
      state$question_idx <- 1
      state$correct_count <- 0
      state$feedback <- NULL
      load_question()
    }
  })

  observeEvent(input$btn_reset, {
    state$question_idx <- 1
    state$correct_count <- 0
    state$feedback <- NULL
    load_question()
  })

  output$level_txt <- renderText({
    if (state$level == "easy") "آسان" else "سخت"
  })

  output$score_txt <- renderText({
    paste0(state$correct_count, " از ۱۰")
  })

  output$accuracy_txt <- renderText({
    if (state$question_idx > 10) {
      answered_count <- 10
    } else if (state$answered) {
      answered_count <- state$question_idx
    } else {
      answered_count <- state$question_idx - 1
    }

    if (answered_count <= 0) {
      return("۰٪")
    }

    paste0(round(100 * state$correct_count / answered_count), "٪")
  })

  output$lock_txt <- renderText({
    if (state$hard_unlocked) "باز" else "قفل"
  })

  output$stars_ui <- renderUI({
    num_stars <- floor(state$correct_count / 2)
    star_html(num_stars)
  })

  handle_answer <- function(chosen_val) {
    if (state$answered || is.null(state$current_q) || state$question_idx > 10) {
      return()
    }

    correct_ans <- state$current_q$answer

    if (abs(chosen_val - correct_ans) < 0.05) {
      state$feedback <- list(
        type = "correct",
        text = "آفرین! پاسخ شما درست است."
      )
      state$correct_count <- state$correct_count + 1
    } else {
      state$feedback <- list(
        type = "incorrect",
        text = paste0("پاسخ نادرست است. جواب درست: ", correct_ans)
      )
    }

    state$answered <- TRUE

    delay(2500, {
      if (state$question_idx < 10) {
        state$question_idx <- state$question_idx + 1
        load_question()
      } else {
        if (state$level == "easy" && state$correct_count >= 8) {
          state$hard_unlocked <- TRUE
        }
        state$question_idx <- 11
      }
    })
  }

  observeEvent(input$btn_opt1, {
    req(state$current_q)
    handle_answer(state$current_q$options[1])
  })

  observeEvent(input$btn_opt2, {
    req(state$current_q)
    handle_answer(state$current_q$options[2])
  })

  output$boxplot_ui <- renderPlot({
    req(state$current_q)
    par(family = "sans", mar = c(4, 2, 4, 2))
    draw_boxplot_panel(state$current_q)
  }, res = 110)

  output$game_content <- renderUI({
    if (state$question_idx > 10) {
      percentage <- round((state$correct_count / 10) * 100)

      return(
        div(
          class = "result-box",

          div(class = "result-title", "پایان بازی"),

          div(
            class = "result-text",
            paste0("تعداد پاسخ‌های درست شما: ", state$correct_count, " از ۱۰")
          ),

          div(
            class = "result-score",
            paste0("امتیاز شما: ", percentage, "٪")
          ),

          if (state$level == "easy" && state$correct_count >= 8) {
            div(
              class = "feedback-ok",
              "تبریک! سطح سخت برای شما باز شد."
            )
          } else if (state$level == "easy") {
            div(
              class = "feedback-bad",
              "برای باز شدن سطح سخت باید حداقل ۸ پاسخ درست داشته باشید."
            )
          },

          br(),

          actionButton("btn_play_again", "بازی مجدد", class = "final-btn")
        )
      )
    }

    req(state$current_q)
    q_data <- state$current_q
    progress_width <- (state$question_idx - 1) * 10

    tagList(
      div(
        class = "progress-card",

        div(
          class = "progress-row",

          div(
            class = "progress-pill",
            paste0("سؤال ", state$question_idx, " از ۱۰")
          ),

          div(
            class = "game-progress",

            div(
              class = "game-progress-bar",
              style = paste0("width: ", progress_width, "%;")
            )
          )
        )
      ),

      div(
        class = "question-card",

        div(
          class = "question-text",
          HTML(q_data$question)
        )
      ),

      div(
        class = "play-grid",

        div(
          class = "game-card",

          div(
            class = "visual-label",
            "نمودار جعبه‌ای"
          ),

          div(
            class = "plot-frame",
            plotOutput("boxplot_ui", height = "320px")
          )
        ),

        div(
          class = "answer-card",

          div(
            class = "visual-label",
            "یکی از گزینه‌های زیر را انتخاب کنید"
          ),

          div(
            class = "option-grid",

            actionButton(
              "btn_opt1",
              as.character(q_data$options[1]),
              class = "option-btn",
              disabled = if (state$answered) "disabled" else NULL
            ),

            actionButton(
              "btn_opt2",
              as.character(q_data$options[2]),
              class = "option-btn",
              disabled = if (state$answered) "disabled" else NULL
            )
          ),

          if (is.null(state$feedback)) {
            div(
              class = "hint-box",
              "برای ثبت پاسخ، یکی از دو گزینه را انتخاب کنید."
            )
          } else if (state$feedback$type == "correct") {
            div(
              class = "feedback-ok",
              state$feedback$text
            )
          } else {
            div(
              class = "feedback-bad",
              state$feedback$text
            )
          }
        )
      )
    )
  })

  observeEvent(input$btn_play_again, {
    state$question_idx <- 1
    state$correct_count <- 0
    state$feedback <- NULL
    load_question()
  })
}

shinyApp(ui = ui, server = server)
