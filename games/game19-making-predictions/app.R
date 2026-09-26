# game20.R
# پیش‌بینی بر اساس احتمال (Making Predictions)
# مطابق MathGames - 7.SP.C.7

library(shiny)
library(shinyjs)

# ----------------------------
# Helpers & SVG Drawings
# ----------------------------
svg_data_uri <- function(svg) {
  paste0("data:image/svg+xml;utf8,", URLencode(svg, reserved = TRUE))
}

# تابع رسم کیسه مهره‌ها به صورت داینامیک
draw_marbles_svg <- function(colors_list) {
  cols <- 4
  marble_elements <- lapply(seq_along(colors_list), function(i) {
    row <- (i - 1) %/% cols
    col <- (i - 1) %% cols
    cx <- 50 + col * 70
    cy <- 60 + row * 70
    color_code <- switch(colors_list[i],
                         "white"  = "#ffffff",
                         "yellow" = "#ffec99",
                         "orange" = "#ffae33",
                         "blue"   = "#a5d8ff",
                         "red"    = "#ff8787",
                         "green"  = "#69db7c",
                         "#ced4da")

    sprintf(
      '<circle cx="%s" cy="%s" r="28" fill="%s" stroke="#343a40" stroke-width="2"/>',
      cx, cy, color_code
    )
  })

  svg <- sprintf(
    '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="260" viewBox="0 0 320 260">
    <rect width="320" height="260" rx="20" fill="#f1f3f5" />
    %s
  </svg>',
    paste(marble_elements, collapse = "")
  )

  svg_data_uri(svg)
}

question_image_ui <- function(colors) {
  tags$img(
    src = draw_marbles_svg(colors),
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
# Question Bank (Enriched)
# ----------------------------
get_full_bank <- function() {
  list(
    # Easy
    list(
      level = "easy",
      marbles = c("yellow","yellow","white","white","orange","white","blue","blue","yellow","blue","red","orange"),
      target_color = "سفید",
      target_key = "white",
      trials = 24,
      options = c("4", "6", "8", "2"),
      correct = "6",
      explanation = "تعداد مهره‌های سفید ۳ عدد از کل ۱۲ مهره است. پس احتمال سفید برابر ۳/۱۲ یا ۱/۴ است. در ۲۴ بار تکرار، بهترین پیش‌بینی ۶ بار است."
    ),
    list(
      level = "easy",
      marbles = c("blue", "yellow", "red", "white"),
      target_color = "قرمز",
      target_key = "red",
      trials = 8,
      options = c("4", "2", "3", "1"),
      correct = "2",
      explanation = "احتمال قرمز ۱ از ۴ است. در ۸ بار تکرار، بهترین پیش‌بینی ۲ بار است."
    ),
    list(
      level = "easy",
      marbles = c("orange", "white", "green", "orange", "orange", "green"),
      target_color = "سبز",
      target_key = "green",
      trials = 15,
      options = c("5", "2", "6", "7"),
      correct = "5",
      explanation = "۲ مهره سبز از ۶ مهره داریم. پس احتمال سبز ۲/۶ یا ۱/۳ است. در ۱۵ بار تکرار، بهترین پیش‌بینی ۵ بار است."
    ),
    list(
      level = "easy",
      marbles = c("white", "orange", "blue", "white", "blue", "blue", "green", "orange"),
      target_color = "سبز",
      target_key = "green",
      trials = 16,
      options = c("3", "0", "2", "5"),
      correct = "2",
      explanation = "۱ مهره سبز از ۸ مهره داریم. پس در ۱۶ بار تکرار، بهترین پیش‌بینی ۲ بار است."
    ),
    list(
      level = "easy",
      marbles = c("orange","red","red","red","red","red","red","red","green","red","red","blue"),
      target_color = "غیر قرمز",
      target_key = "not_red",
      trials = 12,
      options = c("3", "6", "0", "1"),
      correct = "3",
      explanation = "از ۱۲ مهره، ۳ مهره قرمز نیستند. پس احتمال انتخاب مهره غیرقرمز ۳/۱۲ یا ۱/۴ است. در ۱۲ بار تکرار، بهترین پیش‌بینی ۳ بار است."
    ),
    list(
      level = "easy",
      marbles = c("blue","blue","blue","blue","blue","yellow","green","green","red","red","white","white"),
      target_color = "آبی",
      target_key = "blue",
      trials = 6,
      options = c("1", "5", "2", "4"),
      correct = "2",
      explanation = "۵ مهره آبی از ۱۲ مهره داریم. پس احتمال آبی ۵/۱۲ است. در ۶ بار تکرار، بهترین پیش‌بینی نزدیک به ۲ بار است."
    ),

    # Hard
    list(
      level = "hard",
      marbles = c("orange", "blue", "red", "green", "green", "green", "red", "green", "white"),
      target_color = "نارنجی",
      target_key = "orange",
      trials = 18,
      options = c("1", "2", "4", "0"),
      correct = "2",
      explanation = "۱ مهره نارنجی از ۹ مهره داریم. پس احتمال نارنجی ۱/۹ است و در ۱۸ بار تکرار، بهترین پیش‌بینی ۲ بار است."
    ),
    list(
      level = "hard",
      marbles = c("blue", "blue", "blue", "red", "red", "white"),
      target_color = "آبی",
      target_key = "blue",
      trials = 20,
      options = c("5", "10", "15", "8"),
      correct = "10",
      explanation = "۳ مهره آبی از ۶ مهره داریم. پس احتمال آبی ۱/۲ است و در ۲۰ بار تکرار، بهترین پیش‌بینی ۱۰ بار است."
    ),
    list(
      level = "hard",
      marbles = c("red","red","red","red","red","red","red","red","red","red","yellow","red"),
      target_color = "غیر قرمز",
      target_key = "not_red",
      trials = 48,
      options = c("1", "6", "4", "7"),
      correct = "4",
      explanation = "فقط ۱ مهره از ۱۲ مهره قرمز نیست. پس احتمال غیرقرمز ۱/۱۲ است. در ۴۸ بار تکرار، بهترین پیش‌بینی ۴ بار است."
    ),
    list(
      level = "hard",
      marbles = c("green","white","blue","blue","green","blue","blue","yellow","yellow","red","green","yellow"),
      target_color = "آبی",
      target_key = "blue",
      trials = 15,
      options = c("3", "8", "5", "6"),
      correct = "5",
      explanation = "۴ مهره آبی از ۱۲ مهره داریم. پس احتمال آبی ۴/۱۲ یا ۱/۳ است. در ۱۵ بار تکرار، بهترین پیش‌بینی ۵ بار است."
    ),
    list(
      level = "hard",
      marbles = c("red","red","red","green","yellow","red","red","red","white"),
      target_color = "غیر قرمز",
      target_key = "not_red",
      trials = 9,
      options = c("3", "2", "1", "4"),
      correct = "3",
      explanation = "از ۹ مهره، ۳ مهره غیرقرمز هستند. پس احتمال غیرقرمز ۳/۹ یا ۱/۳ است. در ۹ بار تکرار، بهترین پیش‌بینی ۳ بار است."
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
        grid-template-columns: 1.2fr 1fr;
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
        font-size: 18px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.2s, transform 0.1s;
        text-align: center;
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
      div(class = "top-title", "پیش‌بینی بر اساس احتمال (Making Predictions)"),
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
    
    all_qs <- get_full_bank()
    easy_qs <- Filter(function(q) q$level == "easy", all_qs)
    
    rv$current_pool <- sample(easy_qs, min(10, length(easy_qs)))
    rv$question <- rv$current_pool[[1]]
    clear_selected_option()
  }

  # بارگذاری اولیه بازی
  observe({
    if (is.null(rv$question)) {
      init_game()
    }
  })

  # کنترل دکمه‌های بازنشانی
  observeEvent(list(input$reset_btn, input$btn_play_again, input$restart_btn), {
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

    if (identical(as.character(rv$selected), as.character(rv$question$correct))) {
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

    if (rv$q_num >= 10 || rv$q_num >= length(rv$current_pool)) {
      rv$game_over <- TRUE
    } else {
      rv$q_num <- rv$q_num + 1
      rv$answered <- FALSE
      
      # بازکردن اتوماتیک سطح سخت پس از سوال پنجم
      if (rv$q_num == 6 && hard_unlocked()) {
        rv$level <- "hard"
        all_qs <- get_full_bank()
        hard_qs <- Filter(function(q) q$level == "hard", all_qs)
        rv$current_pool[6:10] <- sample(hard_qs, 5)
        showNotification("سطح سخت به دلیل عملکرد عالی شما فعال شد!", type = "warning")
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
      # ستون چپ: نمایش بصری مهره ها
      div(
        class = "visual-panel",
        uiOutput("visual_ui"),
        div(class = "visual-label", textOutput("visual_label"))
      )
    )
  })

  output$question_txt <- renderText({
    sprintf(
      "اگر %d بار مهره‌ای را از کیسه خارج کنیم و مجدداً برگردانیم، بهترین پیش‌بینی برای تعداد دفعات خروج مهره «%s» چیست؟",
      rv$question$trials, rv$question$target_color
    )
  })

  output$visual_label <- renderText({
    "مهره‌های درون کیسه"
  })

  output$visual_ui <- renderUI({
    question_image_ui(rv$question$marbles)
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

    is_correct <- identical(as.character(rv$selected), as.character(rv$question$correct))
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
