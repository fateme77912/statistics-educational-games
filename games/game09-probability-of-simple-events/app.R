library(shiny)
library(shinyjs)

# -----------------------------
# داده رنگ ها
# -----------------------------
color_data <- list(
  white  = list(hex = "#ffffff", fa = "سفید"),
  yellow = list(hex = "#fff176", fa = "زرد"),
  green  = list(hex = "#8cd348", fa = "سبز"),
  blue   = list(hex = "#4dabf7", fa = "آبی"),
  red    = list(hex = "#ff6b6b", fa = "قرمز"),
  orange = list(hex = "#ffa94d", fa = "نارنجی")
)

available_counts <- c(2, 3, 4, 5, 8, 10)

# -----------------------------
# ساخت درصدهای گزینه ها
# -----------------------------
build_options <- function(correct_prob) {
  base_pool <- c(
    0, 10, 12, 13, 17, 20, 25, 30, 33, 38, 40, 50,
    60, 63, 67, 70, 75, 80, 83, 88, 90, 100
  )

  base_pool <- setdiff(unique(base_pool), correct_prob)

  if (length(base_pool) < 3) {
    base_pool <- setdiff(0:100, correct_prob)
  }

  distractors <- sample(base_pool, 3, replace = FALSE)
  options <- sample(c(correct_prob, distractors), 4, replace = FALSE)
  options
}

# -----------------------------
# تولید سوال متنوع
# -----------------------------
generate_hybrid_question <- function() {
  q_type <- sample(c("spinner", "cards"), 1)
  n_items <- sample(available_counts, 1)

  # رنگ هدف
  target_key <- sample(names(color_data), 1)

  # تعداد رنگ های حاضر در سوال: 2 یا 3
  n_colors_used <- sample(c(2, 3), 1, prob = c(0.7, 0.3))
  other_keys <- sample(setdiff(names(color_data), target_key), n_colors_used - 1)
  used_colors <- c(target_key, other_keys)

  # کنترل تکرار 100%
  allow_all_target <- runif(1) < 0.06

  if (n_items == 2) {
    count_target <- if (allow_all_target) {
      sample(c(1, 2), 1, prob = c(0.9, 0.1))
    } else {
      1
    }
  } else {
    if (allow_all_target) {
      count_target <- sample(1:n_items, 1, prob = c(rep(1, n_items - 1), 0.25))
    } else {
      count_target <- sample(1:(n_items - 1), 1)
    }
  }

  count_target <- min(count_target, n_items)

  items <- character(n_items)
  items[1:count_target] <- target_key

  remaining <- n_items - count_target
  if (remaining > 0) {
    items[(count_target + 1):n_items] <- sample(other_keys, remaining, replace = TRUE)
  }

  items <- sample(items)

  correct_prob <- round(100 * count_target / n_items)

  question_text <- paste0(
    "شما یک مورد را به تصادف انتخاب می‌کنید. احتمال انتخاب رنگ <b>",
    color_data[[target_key]]$fa,
    "</b> چند درصد است؟"
  )

  list(
    type = q_type,
    question = question_text,
    items = items,
    target_key = target_key,
    target_fa = color_data[[target_key]]$fa,
    answer = correct_prob,
    options = build_options(correct_prob)
  )
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/gh/rastikerdar/vazirmatn@v33.003/Vazirmatn-font-face.css"
    ),
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
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-bottom: 1px solid #bbdefb;
      }
      .status-item {
        font-size: 16px;
        color: #0d47a1;
        font-weight: 500;
      }
      .progress-dots {
        display: flex;
        gap: 8px;
      }
      .dot {
        width: 14px;
        height: 14px;
        border-radius: 50%;
        background-color: #dcdcdc;
        display: inline-block;
      }
      .dot.current {
        background-color: #1e88e5;
        transform: scale(1.2);
      }
      .dot.correct {
        background-color: #2e7d32;
      }
      .dot.incorrect {
        background-color: #c62828;
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
        font-size: 18px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.2s, transform 0.1s;
      }
      .opt-btn:hover {
        background: #1565c0;
        transform: translateY(-2px);
      }
      .opt-btn:active {
        transform: translateY(0);
      }
      .visual-panel {
        display: flex;
        justify-content: center;
        align-items: center;
        background: #ffffff;
        border: 1px solid #e0e0e0;
        border-radius: 12px;
        min-height: 320px;
        padding: 20px;
      }
      .card-row {
        display: flex;
        gap: 12px;
        justify-content: center;
        flex-wrap: wrap;
      }
      .math-card {
        width: 70px;
        height: 105px;
        border: 2px solid #455a64;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .spinner-plot-container {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .feedback-box {
        margin-top: 15px;
        padding: 12px;
        border-radius: 8px;
        text-align: center;
        font-size: 16px;
        font-weight: bold;
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
      div(class = "top-title", "آمار و احتمال"),
      div(
        class = "top-controls",
        actionButton("btn_easy", "سطح آسان", class = "level-btn active"),
        actionButton("btn_hard", "سطح سخت", class = "level-btn"),
        actionButton("restart", "شروع مجدد", class = "reset-top-btn")
      )
    ),
    
    # نوار وضعیت
    uiOutput("statusBar"),
    
    # بدنه بازی
    uiOutput("game_ui")
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(
    q = generate_hybrid_question(),
    score = 0,
    q_index = 1,
    feedback = NULL,
    answered = FALSE,
    token = 1,
    level = "آسان",
    history = character(10) # برای ثبت درست/نادرست بودن سوالات
  )

  generate_new_question <- function() {
    rv$q <- generate_hybrid_question()
    rv$feedback <- NULL
    rv$answered <- FALSE
    rv$token <- rv$token + 1
  }

  # تغییر سطح دشواری
  observeEvent(input$btn_easy, {
    rv$level <- "آسان"
    runjs("$('.level-btn').removeClass('active'); $('#btn_easy').addClass('active');")
  })

  observeEvent(input$btn_hard, {
    rv$level <- "سخت"
    runjs("$('.level-btn').removeClass('active'); $('#btn_hard').addClass('active');")
  })

  # نوار وضعیت پویا
  output$statusBar <- renderUI({
    req(rv$q_index <= 10)
    
    dots <- lapply(1:10, function(i) {
      status_class <- "dot"
      if (i == rv$q_index) {
        status_class <- paste(status_class, "current")
      } else if (i < rv$q_index) {
        if (rv$history[i] == "correct") {
          status_class <- paste(status_class, "correct")
        } else {
          status_class <- paste(status_class, "incorrect")
        }
      }
      span(class = status_class)
    })
    
    div(
      class = "status-bar",
      div(class = "status-item", paste0("سوال ", rv$q_index, " از 10")),
      div(class = "progress-dots", dots),
      div(class = "status-item", paste0("امتیاز: ", rv$score))
    )
  })

  # نمایش کارت‌ها یا چرخنده
  output$display_render <- renderUI({
    req(rv$q)

    if (rv$q$type == "cards") {
      div(
        class = "card-row",
        lapply(rv$q$items, function(color_key) {
          div(
            class = "math-card",
            style = paste0(
              "background-color:", color_data[[color_key]]$hex, ";"
            )
          )
        })
      )
    } else {
      div(
        class = "spinner-plot-container",
        plotOutput("spinner_plot", height = "300px", width = "300px")
      )
    }
  })

  # رسم چرخنده
  output$spinner_plot <- renderPlot({
    req(rv$q)
    req(rv$q$type == "spinner")

    n <- length(rv$q$items)
    cols <- sapply(rv$q$items, function(x) color_data[[x]]$hex)

    par(mar = c(0, 0, 0, 0))
    pie(
      rep(1, n),
      col = cols,
      border = "#333333",
      labels = "",
      init.angle = 90,
      radius = 1
    )
    points(0, 0, pch = 21, bg = "#333333", col = "#333333", cex = 2.3)
    arrows(0, 0, 0, 0.8, lwd = 5, col = "#333333", length = 0.1)
  })

  # مدیریت دریافت پاسخ
  observeEvent(input$choice, {
    req(rv$q)

    if (isTRUE(rv$answered)) {
      return()
    }

    user_choice <- suppressWarnings(as.numeric(input$choice))
    correct_answer <- rv$q$answer

    if (!is.na(user_choice) && user_choice == correct_answer) {
      rv$feedback <- div(class = "feedback-box feedback-correct", "✅ عالی! پاسخ شما درست بود.")
      rv$score <- rv$score + 1
      rv$history[rv$q_index] <- "correct"
    } else {
      rv$feedback <- div(
        class = "feedback-box feedback-incorrect",
        paste0("❌ نادرست بود. پاسخ درست: ", correct_answer, "%")
      )
      rv$history[rv$q_index] <- "incorrect"
    }

    rv$answered <- TRUE

    current_index <- isolate(rv$q_index)
    current_token <- isolate(rv$token)

    shinyjs::delay(1600, {
      if (isolate(rv$token) != current_token) {
        return()
      }

      if (current_index < 10) {
        rv$q_index <- current_index + 1
        generate_new_question()
      } else {
        rv$q_index <- 11
      }
    })
  })

  # شروع مجدد بازی
  observeEvent(list(input$restart, input$btn_play_again), {
    rv$score <- 0
    rv$q_index <- 1
    rv$feedback <- NULL
    rv$answered <- FALSE
    rv$token <- rv$token + 1
    rv$history <- character(10)
    rv$q <- generate_hybrid_question()
  })

  # رابط کاربری بخش بازی
  output$game_ui <- renderUI({
    req(rv$q_index)

    if (rv$q_index > 10) {
      return(
        div(
          class = "result-panel",
          div(class = "result-title", "پایان بازی!"),
          div(class = "result-score", paste0("شما به ", rv$score, " سوال از ۱۰ سوال پاسخ درست دادید.")),
          actionButton("btn_play_again", "شروع دوباره بازی", class = "restart-btn")
        )
      )
    }

    div(
      class = "play-grid",
      # ستون راست: صورت سوال و گزینه ها
      div(
        class = "question-card",
        div(
          div(class = "q-text", HTML(rv$q$question)),
          uiOutput("current_feedback")
        ),
        div(
          class = "options-grid",
          lapply(rv$q$options, function(opt) {
            tags$button(
              class = "opt-btn",
              onclick = sprintf(
                "Shiny.setInputValue('choice', %s, {priority: 'event'})",
                opt
              ),
              paste0(opt, "%")
            )
          })
        )
      ),
      # ستون چپ: چرخنده یا کارت ها
      div(
        class = "visual-panel",
        uiOutput("display_render")
      )
    )
  })

  # رندر کردن فیدبک در ستون راست بالای گزینه‌ها
  output$current_feedback <- renderUI({
    rv$feedback
  })
}

shinyApp(ui, server)
