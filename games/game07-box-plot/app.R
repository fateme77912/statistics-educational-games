library(shiny)
library(shinyjs)

# ----------------------------
# سناریوهای تولید سؤال
# ----------------------------
scenarios <- list(
  list(
    title = "ساعات مطالعه هفتگی",
    col1 = "روز",
    col2 = "ساعت مطالعه",
    items = c(
      "شنبه", "یکشنبه", "دوشنبه", "سه شنبه",
      "چهارشنبه", "پنجشنبه", "جمعه"
    )
  ),
  list(
    title = "دمای هوای شهر",
    col1 = "روز",
    col2 = "دما (درجه)",
    items = c(
      "شنبه", "یکشنبه", "دوشنبه", "سه شنبه",
      "چهارشنبه", "پنجشنبه", "جمعه"
    )
  ),
  list(
    title = "کتاب های خوانده شده",
    col1 = "نام دانش آموز",
    col2 = "تعداد کتاب",
    items = c("علی", "سارا", "رضا", "مریم", "امیر", "مینا", "پویا")
  ),
  list(
    title = "امتیاز بازی های رایانه ای",
    col1 = "مرحله",
    col2 = "امتیاز کسب شده",
    items = c(
      "مرحله 1", "مرحله 2", "مرحله 3", "مرحله 4",
      "مرحله 5", "مرحله 6", "مرحله 7"
    )
  ),
  list(
    title = "زمان دویدن روزانه",
    col1 = "روز",
    col2 = "زمان (دقیقه)",
    items = c(
      "شنبه", "یکشنبه", "دوشنبه", "سه شنبه",
      "چهارشنبه", "پنجشنبه", "جمعه"
    )
  )
)

get_mode_value <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  modes[1]
}

# ----------------------------
# تولید سؤال عدد گمشده
# ----------------------------
generate_missing_number_question <- function(level = "easy") {
  scenario <- scenarios[[sample(seq_along(scenarios), 1)]]
  n <- if (level == "easy") 5 else 7
  selected_items <- scenario$items[1:n]

  if (level == "easy") {
    avg <- sample(4:10, 1)
    values <- rep(avg, n)
    adjustments <- sample(c(-2, -1, 0, 1, 2), n, replace = TRUE)
    values <- values + adjustments
    values[values < 1] <- 1
  } else {
    values <- sample(5:30, n, replace = TRUE)
  }

  missing_idx <- sample(1:n, 1)
  correct_answer <- values[missing_idx]

  metric <- sample(c("mean", "median", "mode", "range"), 1)

  metric_val <- switch(
    metric,
    "mean"   = mean(values),
    "median" = median(values),
    "mode"   = get_mode_value(values),
    "range"  = max(values) - min(values)
  )

  metric_fa <- switch(
    metric,
    "mean"   = "میانگین",
    "median" = "میانه",
    "mode"   = "مد",
    "range"  = "دامنه تغییرات"
  )

  display_values <- as.character(values)
  display_values[missing_idx] <- "؟"

  wrong_offset <- sample(c(-3, -2, -1, 1, 2, 3), 1)
  wrong_answer <- correct_answer + wrong_offset

  if (wrong_answer < 1) {
    wrong_answer <- correct_answer + 4
  }

  if (wrong_answer == correct_answer) {
    wrong_answer <- correct_answer + 2
  }

  options <- sample(c(correct_answer, wrong_answer))

  question_text <- paste0(
    "در جدول زیر مقدار مربوط به <b>«",
    selected_items[missing_idx],
    "»</b> گم شده است. اگر بدانیم <b>",
    metric_fa,
    "</b> کل داده‌های این جدول برابر با <b>",
    round(metric_val, 1),
    "</b> است، مقدار گمشده را پیدا کنید."
  )

  list(
    question = question_text,
    table_title = scenario$title,
    col1 = scenario$col1,
    col2 = scenario$col2,
    items = selected_items,
    display_values = display_values,
    options = options,
    answer = correct_answer
  )
}

star_html <- function(n) {
  n <- max(0, min(5, n))

  tags$div(
    class = "stars-wrap",
    tags$span(
      class = "star-active",
      paste(rep("★", n), collapse = "")
    ),
    if (n < 5) {
      tags$span(
        class = "star-inactive",
        paste(rep("★", 5 - n), collapse = "")
      )
    }
  )
}

# ----------------------------
# رابط کاربری
# ----------------------------
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    tags$link(
      rel = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Vazirmatn:wght@400;500;700;800&display=swap"
      )
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

      button,
      input {
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
        grid-template-columns: minmax(0, 55fr) minmax(0, 45fr);
        gap: 14px;
        align-items: stretch;
      }

      .game-card,
      .answer-card {
        min-width: 0;
        min-height: 340px;
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

      .table-wrap {
        width: 100%;
        overflow-x: auto;
        border-radius: 15px;
      }

      .mathgames-table {
        width: 100%;
        border-spacing: 0;
        border-collapse: separate;
        overflow: hidden;
        background: #ffffff;
        border: 2px solid #bfe3fb;
        border-radius: 15px;
      }

      .mathgames-table th {
        padding: 10px;
        color: #ffffff;
        font-size: 15px;
        font-weight: 800;
        text-align: center;
        background: #5abfff;
        border-left: 1px dashed #a8d7f5;
      }

      .mathgames-table th:last-child {
        border-left: 0;
      }

      .mathgames-table td {
        padding: 10px;
        color: #3a5568;
        font-size: 15px;
        font-weight: 500;
        text-align: center;
        background: #ffffff;
        border-top: 1px dashed #bfe3fb;
        border-left: 1px dashed #bfe3fb;
      }

      .mathgames-table td:last-child {
        border-left: 0;
      }

      .mathgames-table td.missing-value {
        color: #d9485f;
        font-size: 20px;
        font-weight: 800;
        background: #fff3f5;
      }

      .table-title-header {
        font-size: 16px !important;
        background: linear-gradient(180deg, #2aa6f2 0%, #198dd8 100%) !important;
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
        box-shadow:
          inset 0 -3px 0 rgba(0, 0, 0, 0.12),
          0 4px 8px rgba(30, 155, 236, 0.16);
        transition: transform 0.15s ease, filter 0.15s ease;
      }

      .option-btn:hover,
      .option-btn:focus {
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

      .feedback-ok,
      .feedback-bad,
      .hint-box {
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

        .game-card,
        .answer-card {
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
          white-space: normal;
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

        .game-card,
        .answer-card {
          padding: 13px;
        }

        .option-grid {
          grid-template-columns: 1fr;
        }

        .option-btn {
          min-height: 54px;
          font-size: 21px !important;
        }

        .mathgames-table th,
        .mathgames-table td {
          padding: 8px 6px;
          font-size: 13px;
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
          "پیدا کردن عدد گمشده"
        ),

        div(
          class = "header-actions",
          actionButton(
            "btn_easy",
            "سطح آسان",
            class = "top-btn btn-easy"
          ),
          actionButton(
            "btn_hard",
            "سطح سخت",
            class = "top-btn btn-hard"
          ),
          actionButton(
            "btn_reset",
            "شروع مجدد",
            class = "top-btn btn-reset"
          )
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
    state$current_q <- generate_missing_number_question(state$level)
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

    paste0(
      round(100 * state$correct_count / answered_count),
      "٪"
    )
  })

  output$lock_txt <- renderText({
    if (state$hard_unlocked) "باز" else "قفل"
  })

  output$stars_ui <- renderUI({
    num_stars <- floor(state$correct_count / 2)
    star_html(num_stars)
  })

  handle_answer <- function(chosen_val) {
    if (
      state$answered ||
      is.null(state$current_q) ||
      state$question_idx > 10
    ) {
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
        text = paste0(
          "پاسخ نادرست است. جواب درست: ",
          correct_ans
        )
      )
    }

    state$answered <- TRUE

    delay(2500, {
      if (state$question_idx < 10) {
        state$question_idx <- state$question_idx + 1
        load_question()
      } else {
        if (
          state$level == "easy" &&
          state$correct_count >= 8
        ) {
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

  output$game_content <- renderUI({
    if (state$question_idx > 10) {
      percentage <- round(
        (state$correct_count / 10) * 100
      )

      return(
        div(
          class = "result-box",

          div(
            class = "result-title",
            "پایان بازی"
          ),

          div(
            class = "result-text",
            paste0(
              "تعداد پاسخ‌های درست شما: ",
              state$correct_count,
              " از ۱۰"
            )
          ),

          div(
            class = "result-score",
            paste0(
              "امتیاز شما: ",
              percentage,
              "٪"
            )
          ),

          if (
            state$level == "easy" &&
            state$correct_count >= 8
          ) {
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

          actionButton(
            "btn_play_again",
            "بازی مجدد",
            class = "final-btn"
          )
        )
      )
    }

    req(state$current_q)
    q_data <- state$current_q

    table_rows <- lapply(
      seq_along(q_data$items),
      function(i) {
        value_class <- if (
          q_data$display_values[i] == "؟"
        ) {
          "missing-value"
        } else {
          NULL
        }

        tags$tr(
          tags$td(q_data$items[i]),
          tags$td(
            class = value_class,
            q_data$display_values[i]
          )
        )
      }
    )

    progress_width <- (state$question_idx - 1) * 10

    tagList(
      div(
        class = "progress-card",

        div(
          class = "progress-row",

          div(
            class = "progress-pill",
            paste0(
              "سؤال ",
              state$question_idx,
              " از ۱۰"
            )
          ),

          div(
            class = "game-progress",

            div(
              class = "game-progress-bar",
              style = paste0(
                "width: ",
                progress_width,
                "%;"
              )
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
            "جدول داده‌ها"
          ),

          div(
            class = "table-wrap",

            tags$table(
              class = "mathgames-table",

              tags$thead(
                tags$tr(
                  tags$th(
                    colspan = 2,
                    class = "table-title-header",
                    q_data$table_title
                  )
                ),

                tags$tr(
                  tags$th(q_data$col1),
                  tags$th(q_data$col2)
                )
              ),

              tags$tbody(table_rows)
            )
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
