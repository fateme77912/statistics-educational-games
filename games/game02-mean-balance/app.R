# app.R
# محاسبه میانه

library(shiny)

# ----------------------------
# 1) تولید سؤال‌ها و فرمت پاسخ
# ----------------------------
make_question_median <- function(level = 1) {
  if (level == 1) {
    n <- sample(3:5, 1)
    vals <- sample(1:20, n, replace = FALSE)
  } else {
    n <- sample(5:9, 1)

    if (runif(1) < 0.4) {
      center <- sample(10:30, 1)
      spread <- sample(4:12, 1)
      pool <- seq(max(1, center - spread), min(40, center + spread))
      if (length(pool) >= n) {
        vals <- sample(pool, n, replace = FALSE)
      } else {
        vals <- sample(1:40, n, replace = FALSE)
      }
    } else {
      vals <- sample(1:40, n, replace = FALSE)
    }
  }

  sorted_vals <- sort(vals)
  k <- length(sorted_vals)

  if (k %% 2 == 1) {
    ans <- sorted_vals[(k + 1) / 2]
  } else {
    ans <- mean(sorted_vals[c(k / 2, k / 2 + 1)])
  }

  list(
    level = level,
    values = sample(vals),
    answer = ans
  )
}

format_answer <- function(x) {
  if (abs(x - round(x)) < 1e-9) {
    as.character(as.integer(round(x)))
  } else {
    format(round(x, 1), nsmall = 1)
  }
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
# 2) UI (قالب مرجع ظاهری)
# ----------------------------
ui <- fluidPage(
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
        grid-template-columns: 42% 58%;
        gap: 14px;
        align-items: stretch;
      }

      .game-card,
      .answer-card {
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 18px;
        min-height: 270px;
      }

      .answer-card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fdff 100%);
      }

      .visual-label {
        text-align: center;
        color: #0f3b6d;
        font-size: 15px;
        font-weight: 800;
        margin-top: 8px;
      }

      .numbers-box {
        color: #0b4f8a;
        font-size: 27px;
        font-weight: 800;
        line-height: 2;
        text-align: center;
        background: #ffffff;
        border: 1px dashed #a9d7f0;
        border-radius: 16px;
        padding: 18px 12px;
        margin-top: 18px;
      }

      .answer-input input {
        width: 100% !important;
        height: 52px !important;
        border: 2px solid #d5ecfb !important;
        border-radius: 14px !important;
        text-align: center !important;
        font-family: 'Vazirmatn', Tahoma, sans-serif !important;
        font-size: 18px !important;
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
      }

      .rule-box {
        font-size: 14px;
        color: #0c4a6e;
        background: #f0f9ff;
        border: 1px solid #b9e6fe;
        border-radius: 14px;
        padding: 10px 12px;
        text-align: right;
        margin-top: 12px;
        line-height: 1.95;
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
        div(class = "header-title", "محاسبه میانه"),

        div(
          class = "header-actions",
          actionButton("easy_btn", "سطح آسان", class = "top-btn btn-easy"),
          actionButton("hard_btn", "سطح سخت", class = "top-btn btn-hard"),
          actionButton("reset_btn", "شروع مجدد", class = "top-btn btn-reset")
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
          div(class = "status-label", "امتیاز"),
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

      div(
        class = "progress-card",
        div(
          class = "progress-row",
          div(class = "progress-pill", textOutput("progress_txt")),
          uiOutput("progress_ui")
        )
      ),

      div(
        class = "question-card",
        div(class = "question-text", "اعداد زیر را مرتب کنید و میانه را به دست آورید."),
        div(class = "question-sub", "پاسخ خود را (در صورت اعشاری بودن با یک رقم اعشار) وارد کنید.")
      ),

      div(
        class = "play-grid",

        div(
          class = "game-card",
          div(class = "visual-label", "داده‌های سؤال"),
          uiOutput("numbers_ui"),
          div(
            class = "rule-box",
            tags$b("یادآوری:"), tags$br(),
            "• میانه = عدد وسط داده‌ها پس از مرتب‌سازی.", tags$br(),
            "• اگر تعداد داده‌ها فرد باشد، همان عدد وسط میانه است.", tags$br(),
            "• اگر تعداد داده‌ها زوج باشد، میانگین دو عدد وسط را حساب می‌کنیم."
          )
        ),

        div(
          class = "answer-card",
          div(class = "answer-input",
            textInput("ans", label = NULL, placeholder = "پاسخ را بنویسید")
          ),

          div(
            class = "action-row",
            actionButton("check_btn", "ثبت پاسخ", class = "btn btn-success action-btn"),
            actionButton("next_btn", "سؤال بعدی", class = "btn btn-info action-btn")
          ),

          uiOutput("feedback_ui")
        )
      )
    )
  )
)

# ----------------------------
# 3) Server
# ----------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    started = FALSE,
    level = 1,
    q_index = 0,
    questions = list(),
    answers = character(0),
    correct = logical(0),
    current = NULL,
    locked_level2 = TRUE
  )

  new_game <- function(level = 1) {
    rv$started <- TRUE
    rv$level <- level
    rv$q_index <- 1
    rv$questions <- replicate(10, make_question_median(level), simplify = FALSE)
    rv$answers <- rep(NA_character_, 10)
    rv$correct <- rep(NA, 10)
    rv$current <- rv$questions[[1]]

    updateTextInput(session, "ans", value = "")
  }

  stars_count <- reactive({
    if (!rv$started) return(0)

    done <- sum(!is.na(rv$correct))
    if (done < 10) return(0)

    ok <- sum(rv$correct == TRUE, na.rm = TRUE)
    pct <- round(100 * ok / 10)

    if (pct >= 90) {
      5
    } else if (pct >= 80) {
      4
    } else if (pct >= 70) {
      3
    } else if (pct >= 50) {
      2
    } else if (pct >= 30) {
      1
    } else {
      0
    }
  })

  observeEvent(input$easy_btn, {
    new_game(1)
  })

  observeEvent(input$hard_btn, {
    if (rv$locked_level2) {
      showNotification(
        "برای باز کردن مرحله سخت باید حداقل ۸۰٪ امتیاز کسب کنید.",
        type = "warning"
      )
    } else {
      new_game(2)
    }
  })

  observeEvent(input$reset_btn, {
    new_game(rv$level)
  })

  output$level_txt <- renderText({
    if (!rv$started) {
      "—"
    } else if (rv$level == 1) {
      "آسان"
    } else {
      "سخت"
    }
  })

  output$score_txt <- renderText({
    if (!rv$started) return(0)
    sum(rv$correct == TRUE, na.rm = TRUE) * ifelse(rv$level == 1, 10, 15)
  })

  output$acc_txt <- renderText({
    if (!rv$started) return("0٪")

    done <- sum(!is.na(rv$correct))
    ok <- sum(rv$correct == TRUE, na.rm = TRUE)

    if (done == 0) {
      "0٪"
    } else {
      paste0(round(100 * ok / done), "٪")
    }
  })

  output$lock_txt <- renderText({
    if (rv$locked_level2) "قفل" else "باز"
  })

  output$stars_ui <- renderUI({
    star_html(stars_count())
  })

  output$progress_txt <- renderText({
    if (!rv$started) {
      "برای شروع، سطح آسان را انتخاب کن"
    } else {
      paste0("سؤال ", rv$q_index, " از ۱۰")
    }
  })

  output$progress_ui <- renderUI({
    pct <- 0
    if (rv$started) {
      done <- sum(!is.na(rv$correct))
      pct <- round(done / 10 * 100)
    }

    tags$div(
      class = "progress",
      tags$div(
        class = "progress-bar",
        style = paste0("width:", pct, "%;")
      )
    )
  })

  output$numbers_ui <- renderUI({
    if (!rv$started) {
      tags$div(
        class = "hint-box",
        "برای شروع بازی، روی دکمه «سطح آسان» در بالای صفحه بزن."
      )
    } else {
      nums_str <- paste(rv$current$values, collapse = " ، ")
      tags$div(
        class = "numbers-box",
        nums_str
      )
    }
  })

  observeEvent(input$check_btn, {
    req(rv$started)

    i <- rv$q_index
    if (!is.na(rv$correct[i])) {
      return()
    }

    user_input <- trimws(input$ans)
    user_num <- suppressWarnings(as.numeric(user_input))

    true_ans <- rv$current$answer
    is_correct <- !is.na(user_num) && abs(user_num - true_ans) < 1e-9

    rv$answers[i] <- user_input
    rv$correct[i] <- is_correct

    if (is_correct) {
      showNotification("آفرین! پاسخ درست بود.", type = "message")
    } else {
      showNotification("اشتباه بود. توضیح را ببین.", type = "error")
    }
  })

  output$feedback_ui <- renderUI({
    if (!rv$started) {
      return(
        tags$div(
          class = "hint-box",
          "ابتدا سطح آسان را انتخاب کن تا بازی شروع شود."
        )
      )
    }

    i <- rv$q_index
    if (is.na(rv$correct[i])) {
      return(
        tags$div(
          class = "hint-box",
          "پس از وارد کردن پاسخ، روی «ثبت پاسخ» بزن."
        )
      )
    }

    true_ans <- rv$current$answer
    sorted_vals_str <- paste(sort(rv$current$values), collapse = " ، ")

    if (rv$correct[i]) {
      tags$div(
        class = "feedback-ok",
        tags$b("پاسخ درست!"),
        tags$div(
          style = "margin-top:6px;",
          paste0("پاسخ درست: ", format_answer(true_ans))
        )
      )
    } else {
      user_text <- if (is.na(rv$answers[i]) || rv$answers[i] == "") {
        "بدون پاسخ"
      } else {
        rv$answers[i]
      }

      tags$div(
        class = "feedback-bad",
        tags$b("پاسخ نادرست."),
        tags$div(
          style = "margin-top:6px;",
          paste0("پاسخ شما: ", user_text)
        ),
        tags$div(
          style = "margin-top:6px;",
          paste0("داده‌های مرتب‌شده: ", sorted_vals_str)
        ),
        tags$div(
          style = "margin-top:6px;",
          paste0("پاسخ صحیح: ", format_answer(true_ans))
        )
      )
    }
  })

  observeEvent(input$next_btn, {
    req(rv$started)

    i <- rv$q_index
    if (is.na(rv$correct[i])) {
      showNotification("اول پاسخ این سؤال را ثبت کن.", type = "message")
      return()
    }

    if (i < 10) {
      rv$q_index <- rv$q_index + 1
      rv$current <- rv$questions[[rv$q_index]]
      updateTextInput(session, "ans", value = "")
    } else {
      ok <- sum(rv$correct == TRUE, na.rm = TRUE)
      pct <- round(100 * ok / 10)

      if (pct >= 80) {
        rv$locked_level2 <- FALSE
      }

      showNotification(
        "این دور تمام شد. برای دور جدید روی «شروع مجدد» بزن.",
        type = "message"
      )
    }
  })
}

shinyApp(ui = ui, server = server)
