# app.R
# تفسیر جدول برای محاسبات آماری

library(shiny)
library(shinyjs)

# ----------------------------
# 1) سناریوهای مختلف برای تولید سوالات واقعی
# ----------------------------
scenarios <- list(
  list(
    title = "ساعات مطالعه هفتگی",
    col1 = "روز",
    col2 = "ساعت مطالعه",
    items = c("شنبه", "یکشنبه", "دوشنبه", "سه شنبه", "چهارشنبه", "پنجشنبه", "جمعه")
  ),
  list(
    title = "دمای هوای شهر",
    col1 = "روز",
    col2 = "دما (درجه)",
    items = c("شنبه", "یکشنبه", "دوشنبه", "سه شنبه", "چهارشنبه", "پنجشنبه", "جمعه")
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
    items = c("مرحله 1", "مرحله 2", "مرحله 3", "مرحله 4", "مرحله 5", "مرحله 6", "مرحله 7")
  ),
  list(
    title = "زمان دویدن روزانه",
    col1 = "روز",
    col2 = "زمان (دقیقه)",
    items = c("شنبه", "یکشنبه", "دوشنبه", "سه شنبه", "چهارشنبه", "پنجشنبه", "جمعه")
  )
)

get_mode_value <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  modes <- ux[tab == max(tab)]
  modes[1]
}

make_question_table <- list(
  easy = function() {
    scenario <- scenarios[[sample(seq_along(scenarios), 1)]]
    n <- 5
    selected_items <- scenario$items[1:n]

    avg <- sample(4:10, 1)
    values <- rep(avg, n)
    adjustments <- sample(c(-2, -1, 0, 1, 2), n, replace = TRUE)
    values <- values + adjustments
    values[values < 1] <- 1

    metric <- sample(c("mean", "median", "mode", "range"), 1)

    ans_val <- switch(
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

    question_text <- paste0(
      "جدول زیر اطلاعات مربوط به «", scenario$title,
      "» را نشان می‌دهد. <b>", metric_fa,
      "</b> این داده‌ها را به دست آورید."
    )

    list(
      question = question_text,
      table_title = scenario$title,
      col1 = scenario$col1,
      col2 = scenario$col2,
      items = selected_items,
      values = values,
      answer = round(ans_val, 1)
    )
  },

  hard = function() {
    scenario <- scenarios[[sample(seq_along(scenarios), 1)]]
    n <- 7
    selected_items <- scenario$items[1:n]
    values <- sample(5:30, n, replace = TRUE)

    metric <- sample(c("mean", "median", "mode", "range"), 1)

    ans_val <- switch(
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

    question_text <- paste0(
      "جدول زیر اطلاعات مربوط به «", scenario$title,
      "» را نشان می‌دهد. <b>", metric_fa,
      "</b> این داده‌ها را به دست آورید. (در صورت نیاز پاسخ را تا یک رقم اعشار گرد کنید)"
    )

    list(
      question = question_text,
      table_title = scenario$title,
      col1 = scenario$col1,
      col2 = scenario$col2,
      items = selected_items,
      values = values,
      answer = round(ans_val, 1)
    )
  }
)

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
# 2) UI (قالب مرجع ظاهری)
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
      body {
        margin: 0;
        background: linear-gradient(180deg, #f6fbff 0%, #edf8fe 100%);
        font-family: 'Vazirmatn', Tahoma, sans-serif;
        direction: rtl;
        color: #183153;
      }

      .game-shell {
        max-width: 1040px;
        margin: 18px auto 26px auto;
        background: #ffffff;
        border: 2px solid #dceff7;
        border-radius: 26px;
        box-shadow: 0 14px 34px rgba(22, 91, 130, 0.10);
        overflow: hidden;
      }

      .game-header {
        background: linear-gradient(135deg, #33b4ff 0%, #1698ea 100%);
        color: #ffffff;
        padding: 18px 22px 16px 22px;
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
        line-height: 1.35;
      }

      .header-actions {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
      }

      .top-btn {
        border: none !important;
        border-radius: 999px !important;
        font-family: 'Vazirmatn', Tahoma, sans-serif !important;
        font-weight: 800 !important;
        padding: 8px 16px !important;
        box-shadow: 0 4px 10px rgba(0,0,0,0.10);
      }

      .btn-easy {
        background: #8cd348 !important;
        color: #ffffff !important;
      }

      .btn-hard {
        background: #ffd43b !important;
        color: #6b4b00 !important;
      }

      .btn-reset {
        background: #ff6b6b !important;
        color: #ffffff !important;
      }

      .status-strip {
        display: grid;
        grid-template-columns: repeat(5, 1fr);
        gap: 10px;
        margin-top: 14px;
      }

      .status-badge {
        background: rgba(255,255,255,0.92);
        color: #145388;
        border: 1px solid rgba(255,255,255,0.95);
        border-radius: 18px;
        padding: 9px 10px;
        text-align: center;
        box-shadow: 0 6px 14px rgba(8, 91, 145, 0.12);
        min-height: 58px;
      }

      .status-label {
        font-size: 12px;
        font-weight: 700;
        color: #5f7a96;
        margin-bottom: 2px;
      }

      .status-value {
        font-size: 16px;
        font-weight: 800;
        color: #0f3b6d;
        line-height: 1.5;
      }

      .stars-wrap {
        font-size: 30px;
        line-height: 1;
        letter-spacing: 1px;
        text-align: center;
        text-shadow: 0 1px 0 rgba(0,0,0,0.10);
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
        background: #f3fbfe;
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 12px 16px;
        margin-bottom: 14px;
      }

      .progress-row {
        display: flex;
        align-items: center;
        gap: 12px;
        flex-wrap: wrap;
      }

      .progress-pill {
        background: #ffffff;
        color: #145388;
        border: 1px solid #d5ecfb;
        border-radius: 999px;
        padding: 6px 13px;
        font-size: 13px;
        font-weight: 800;
        white-space: nowrap;
      }

      .progress {
        flex: 1;
        min-width: 180px;
        height: 16px;
        border-radius: 999px;
        overflow: hidden;
        background: #e4f3fa;
        margin: 0;
      }

      .progress-bar {
        height: 100%;
        background: linear-gradient(90deg, #8cd348, #4dabf7);
        border-radius: 999px;
      }

      .question-card {
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 18px 22px;
        margin-bottom: 14px;
      }

      .question-text {
        font-size: 20px;
        font-weight: 800;
        color: #0f3b6d;
        line-height: 1.95;
        text-align: center;
        margin: 0;
      }

      .question-sub {
        text-align: center;
        color: #5f7a96;
        font-size: 13px;
        margin-top: 8px;
      }

      .play-grid {
        display: grid;
        grid-template-columns: 55% 45%;
        gap: 14px;
        align-items: stretch;
      }

      .game-card,
      .answer-card {
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 18px;
        min-height: 320px;
      }

      .answer-card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fdff 100%);
      }

      .visual-label {
        text-align: center;
        color: #0f3b6d;
        font-size: 15px;
        font-weight: 800;
        margin-top: 4px;
        margin-bottom: 14px;
      }

      .mathgames-table {
        width: 100%;
        border-collapse: separate;
        border-spacing: 0;
        border: 2px solid #bfe3fb;
        border-radius: 15px;
        overflow: hidden;
        background: #ffffff;
      }

      .mathgames-table th {
        background: #5abfff;
        color: #ffffff;
        text-align: center;
        padding: 10px;
        font-size: 15px;
        font-weight: 800;
        border-left: 1px dashed #a8d7f5;
      }

      .mathgames-table th:last-child {
        border-left: none;
      }

      .mathgames-table td {
        background: #ffffff;
        color: #3a5568;
        text-align: center;
        padding: 10px;
        font-size: 15px;
        border-top: 1px dashed #bfe3fb;
        border-left: 1px dashed #bfe3fb;
      }

      .mathgames-table td:last-child {
        border-left: none;
      }

      .table-title-header {
        background: linear-gradient(180deg, #2aa6f2 0%, #198dd8 100%) !important;
        font-size: 16px !important;
      }

      .answer-input input {
        width: 100% !important;
        height: 52px !important;
        border: 2px solid #d5ecfb !important;
        border-radius: 14px !important;
        text-align: center !important;
        font-family: 'Vazirmatn', Tahoma, sans-serif !important;
        font-size: 22px !important;
        font-weight: 700 !important;
      }

      .action-row {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        margin-top: 14px;
      }

      .action-btn {
        border-radius: 14px !important;
        font-family: 'Vazirmatn', Tahoma, sans-serif !important;
        font-weight: 800 !important;
        padding: 8px 16px !important;
        flex: 1;
      }

      .feedback-ok {
        background: #effcf1;
        border: 1px solid #8cd348;
        color: #1f6b2d;
        border-radius: 14px;
        padding: 12px 14px;
        margin-top: 14px;
        font-size: 14px;
        line-height: 1.85;
        text-align: center;
      }

      .feedback-bad {
        background: #fff6e6;
        border: 1px solid #ffa94d;
        color: #8a4b00;
        border-radius: 14px;
        padding: 12px 14px;
        margin-top: 14px;
        font-size: 14px;
        line-height: 1.85;
        text-align: center;
      }

      .hint-box {
        color: #60748b;
        font-size: 13px;
        line-height: 1.8;
        background: #f8fdff;
        border: 1px dashed #d5ecfb;
        border-radius: 14px;
        padding: 10px 12px;
        margin-top: 12px;
        text-align: center;
      }

      .result-box {
        text-align: center;
        background: linear-gradient(180deg, #ffffff 0%, #f3fbfe 100%);
        border: 2px solid #dceff7;
        border-radius: 22px;
        padding: 30px 22px;
      }

      .result-title {
        font-size: 29px;
        font-weight: 800;
        color: #2094de;
        margin-bottom: 12px;
      }

      .result-text {
        font-size: 20px;
        color: #3d5a6f;
        margin-bottom: 10px;
      }

      .result-score {
        font-size: 28px;
        font-weight: 800;
        color: #21a769;
        margin-bottom: 16px;
      }

      @media (max-width: 900px) {
        .status-strip {
          grid-template-columns: repeat(2, 1fr);
        }

        .play-grid {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 600px) {
        .game-shell {
          margin: 10px;
          border-radius: 20px;
        }

        .header-title {
          font-size: 23px;
        }

        .question-text {
          font-size: 18px;
        }

        .status-strip {
          grid-template-columns: 1fr;
        }

        .header-actions {
          width: 100%;
        }

        .top-btn {
          flex: 1;
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
        div(class = "header-title", "تفسیر جدول برای محاسبات آماری"),

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
          div(class = "status-value", textOutput("acc_txt"))
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
      uiOutput("game_layout")
    )
  )
)

# ----------------------------
# 3) Server
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

    if (state$level == "easy") {
      state$current_q <- make_question_table$easy()
    } else {
      state$current_q <- make_question_table$hard()
    }
  }

  # شروع اولیه بازی
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
    updateTextInput(session, "user_answer", value = "")
  })

  observeEvent(input$btn_hard, {
    if (state$hard_unlocked) {
      state$level <- "hard"
      state$question_idx <- 1
      state$correct_count <- 0
      state$feedback <- NULL
      load_question()
      updateTextInput(session, "user_answer", value = "")
    } else {
      showNotification(
        "برای باز کردن سطح سخت باید حداقل ۸۰٪ امتیاز در سطح آسان کسب کنید.",
        type = "warning"
      )
    }
  })

  observeEvent(input$btn_reset, {
    state$question_idx <- 1
    state$correct_count <- 0
    state$feedback <- NULL
    load_question()
    updateTextInput(session, "user_answer", value = "")
  })

  # خروجی‌های وضعیت (Status strip)
  output$level_txt <- renderText({
    if (state$level == "easy") "آسان" else "سخت"
  })

  output$score_txt <- renderText({
    paste0(state$correct_count, " از 10")
  })

  output$acc_txt <- renderText({
    done <- min(state$question_idx, 10)
    if (state$question_idx > 10) done <- 10
    if (done == 1 && is.null(state$feedback)) {
      "0٪"
    } else {
      # محاسبه بر اساس تعداد سوالاتی که پاسخ داده شده‌اند
      answered_count <- if (is.null(state$feedback)) done - 1 else done
      if (answered_count == 0) {
        "0٪"
      } else {
        paste0(round(100 * state$correct_count / answered_count), "٪")
      }
    }
  })

  output$lock_txt <- renderText({
    if (state$hard_unlocked) "باز" else "قفل"
  })

  output$stars_ui <- renderUI({
    # هر دو پاسخ صحیح، ۱ ستاره
    num_stars <- floor(state$correct_count / 2)
    star_html(num_stars)
  })

  # بررسی پاسخ کاربر
  observeEvent(input$btn_submit, {
    if (state$answered || is.null(state$current_q) || state$question_idx > 10) {
      return()
    }

    user_ans <- suppressWarnings(as.numeric(input$user_answer))
    correct_ans <- state$current_q$answer

    if (!is.na(user_ans) && abs(user_ans - correct_ans) < 0.05) {
      state$feedback <- list(
        type = "correct",
        text = "آفرین! پاسخ شما درست است."
      )
      state$correct_count <- state$correct_count + 1
      showNotification("پاسخ صحیح بود!", type = "message")
    } else {
      state$feedback <- list(
        type = "incorrect",
        text = paste0("پاسخ نادرست است. جواب درست: ", correct_ans)
      )
      showNotification("پاسخ اشتباه بود.", type = "error")
    }

    state$answered <- TRUE

    # ورود خودکار به سوال بعدی پس از ۲.۵ ثانیه تأخیر
    delay(2500, {
      if (state$question_idx < 10) {
        state$question_idx <- state$question_idx + 1
        load_question()
        updateTextInput(session, "user_answer", value = "")
      } else {
        if (state$level == "easy" && state$correct_count >= 8) {
          state$hard_unlocked <- TRUE
        }
        state$question_idx <- 11
      }
    })
  })

  # چیدمان پویای بازی و پایان دور
  output$game_layout <- renderUI({
    if (state$question_idx > 10) {
      percentage <- round((state$correct_count / 10) * 100)

      return(
        div(
          class = "result-box",
          div(class = "result-title", "پایان بازی"),
          div(
            class = "result-text",
            paste0("تعداد پاسخ‌های درست شما: ", state$correct_count, " از 10")
          ),
          div(
            class = "result-score",
            paste0("امتیاز شما: ", percentage, "%")
          ),
          if (state$level == "easy" && state$correct_count >= 8) {
            div(
              class = "feedback-ok",
              tags$b("تبریک! سطح سخت برای شما باز شد.")
            )
          } else if (state$level == "easy") {
            div(
              class = "feedback-bad",
              tags$b("برای باز شدن سطح سخت باید حداقل 8 پاسخ درست داشته باشید.")
            )
          },
          br(),
          actionButton("btn_reset", "بازی مجدد", class = "btn btn-success action-btn", style = "max-width:200px;")
        )
      )
    }

    req(state$current_q)
    q_data <- state$current_q

    # تشکیل ردیف‌های جدول سناریو
    table_rows <- lapply(seq_along(q_data$items), function(i) {
      tags$tr(
        tags$td(q_data$items[i]),
        tags$td(q_data$values[i])
      )
    })

    # بدنه اصلی بازی
    tagList(
      div(
        class = "progress-card",
        div(
          class = "progress-row",
          div(
            class = "progress-pill",
            paste0("سوال ", state$question_idx, " از ۱۰")
          ),
          tags$div(
            class = "progress",
            tags$div(
              class = "progress-bar",
              style = paste0("width:", (state$question_idx - 1) * 10, "%;")
            )
          )
        )
      ),

      div(
        class = "question-card",
        div(class = "question-text", HTML(q_data$question))
      ),

      div(
        class = "play-grid",

        # ستون جدول داده‌ها
        div(
          class = "game-card",
          div(class = "visual-label", "جدول داده‌ها"),
          tags$table(
            class = "mathgames-table",
            tags$thead(
              tags$tr(
                tags$th(colspan = 2, class = "table-title-header", q_data$table_title)
              ),
              tags$tr(
                tags$th(q_data$col1),
                tags$th(q_data$col2)
              )
            ),
            tags$tbody(table_rows)
          )
        ),

        # ستون پاسخ‌دهی و بازخورد
        div(
          class = "answer-card",
          div(class = "visual-label", "ثبت پاسخ"),
          div(
            class = "answer-input",
            textInput("user_answer", label = NULL, value = "", placeholder = "پاسخ عددی")
          ),

          div(
            class = "action-row",
            actionButton("btn_submit", "ثبت پاسخ", class = "btn btn-success action-btn")
          ),

          uiOutput("feedback_ui")
        )
      )
    )
  })

  # نمایش بازخورد به صورت مجزا در کارت پاسخ
  output$feedback_ui <- renderUI({
    if (is.null(state$feedback)) {
      return(
        tags$div(
          class = "hint-box",
          "پاسخ خود را بنویسید و روی دکمه ثبت پاسخ کلیک کنید."
        )
      )
    }

    if (state$feedback$type == "correct") {
      tags$div(
        class = "feedback-ok",
        tags$b("پاسخ درست!"),
        div(style = "margin-top:6px;", state$feedback$text)
      )
    } else {
      tags$div(
        class = "feedback-bad",
        tags$b("پاسخ نادرست."),
        div(style = "margin-top:6px;", state$feedback$text)
      )
    }
  })
}

shinyApp(ui = ui, server = server)
