# game21.R
# مسائل احتمال ساده (Probability Problems)
# مطابق MathGames - 7.SP.C.5
# ساختار ظاهری و فنی هماهنگ با قالب مرجع و game20.R

library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
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

fraction_box_ui <- function(label_text) {
  div(
    style = "background:#f8fbff; border:2px dashed #a5d8ff; border-radius:20px; padding:35px 22px; text-align:center; min-height: 200px; display: flex; align-items: center; justify-content: center; flex-direction: column;",
    div(
      style = "font-size: 16px; font-weight: bold; color: #546e7a; margin-bottom: 15px;",
      "احتمال وقوع رویداد خواسته شده:"
    ),
    div(
      style = "font-size:38px; font-weight:800; color:#1e88e5; direction:ltr;",
      HTML(sprintf("%s = ?", label_text))
    )
  )
}

# ----------------------------
# Question Bank
# ----------------------------
get_full_bank <- function() {
  list(
    list(
      level = "easy",
      prompt = "امیر ظرفی شامل ۱۷ مهره دارد که ۶ تای آن نقره‌ای است. اگر امیر ظرف را تکان دهد تا یک مهره به طور تصادفی بیرون بیفتد، احتمال اینکه آن مهره نقره‌ای باشد چقدر است؟ پاسخ را به صورت کسر یا عدد صحیح ساده بنویسید.",
      prob_label = "P(silver)",
      options = c("6/17", "11/17", "3/17", "1/17"),
      correct = "6/17",
      explanation = "تعداد حالت‌های مطلوب ۶ مهره نقره‌ای و تعداد کل حالت‌های ممکن ۱۷ مهره است. پس احتمال برابر است با 6/17."
    ),
    list(
      level = "easy",
      prompt = "علی ۶ اسباب‌بازی دارد و فقط ۵ تای آن‌ها را دوست دارد. اگر برادرش به طور تصادفی یکی از اسباب‌بازی‌های او را انتخاب کند، احتمال اینکه اسباب‌بازی انتخاب‌شده مورد علاقه علی باشد چقدر است؟",
      prob_label = "P(like)",
      options = c("5/6", "1/6", "5/12", "1"),
      correct = "5/6",
      explanation = "از ۶ اسباب‌بازی، ۵ تا مورد علاقه علی هستند. پس احتمال انتخاب اسباب‌بازی مورد علاقه برابر 5/6 است."
    ),
    list(
      level = "easy",
      prompt = "کامران ۱۴ نفر در خانواده‌اش دارد که ۹ نفر از آن‌ها زن هستند. اگر او به طور تصادفی یک نفر را انتخاب کند، احتمال اینکه فرد انتخاب‌شده زن باشد چقدر است؟",
      prob_label = "P(female)",
      options = c("9/14", "5/14", "9/5", "1/2"),
      correct = "9/14",
      explanation = "تعداد افراد زن ۹ نفر و کل افراد خانواده ۱۴ نفر هستند. بنابراین احتمال برابر 9/14 است."
    ),
    list(
      level = "easy",
      prompt = "دانیال ۱۶ پیراهن دارد که ۴ تای آن‌ها مشکی است. اگر او به طور تصادفی یک پیراهن انتخاب کند، احتمال اینکه پیراهن مشکی باشد چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(black)",
      options = c("1/4", "4/16", "3/4", "1/2"),
      correct = "1/4",
      explanation = "احتمال اولیه 4/16 است. با ساده کردن کسر، پاسخ نهایی 1/4 می‌شود."
    ),
    list(
      level = "easy",
      prompt = "رضا ۱۷ جفت کفش دارد که ۷ جفت آن ساق‌دار است. اگر او به طور تصادفی یک جفت کفش انتخاب کند، احتمال اینکه کفش انتخاب‌شده ساق‌دار باشد چقدر است؟",
      prob_label = "P(high-tops)",
      options = c("7/17", "10/17", "7/10", "1/2"),
      correct = "7/17",
      explanation = "از ۱۷ جفت کفش، ۷ جفت ساق‌دار هستند. پس احتمال برابر 7/17 است."
    ),
    list(
      level = "hard",
      prompt = "مریم ۲۰ مهره دارد که ۸ تای آن‌ها آبی هستند. اگر یک مهره به طور تصادفی انتخاب شود، احتمال آبی بودن آن چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(blue)",
      options = c("2/5", "8/20", "3/5", "1/5"),
      correct = "2/5",
      explanation = "احتمال اولیه 8/20 است. با ساده کردن صورت و مخرج بر ۴، کسر برابر 2/5 می‌شود."
    ),
    list(
      level = "hard",
      prompt = "سارا ۱۵ مداد دارد که ۵ تای آن‌ها قرمز هستند. اگر او بدون نگاه کردن یک مداد بردارد، احتمال اینکه مداد قرمز باشد چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(red)",
      options = c("1/3", "5/15", "2/3", "1/5"),
      correct = "1/3",
      explanation = "احتمال اولیه 5/15 است و با ساده کردن، پاسخ نهایی 1/3 می‌شود."
    ),
    list(
      level = "hard",
      prompt = "حسین ۲۰ سؤال برای تمرین دارد و جواب ۱۲ سؤال را کاملاً بلد است. اگر معلم یکی از سؤال‌ها را به طور تصادفی انتخاب کند، احتمال اینکه سؤال انتخاب‌شده از سؤال‌های بلدِ حسین باشد چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(known)",
      options = c("3/5", "12/20", "2/5", "1/2"),
      correct = "3/5",
      explanation = "احتمال اولیه 12/20 است. با ساده کردن این کسر، پاسخ 3/5 به دست می‌آید."
    ),
    list(
      level = "hard",
      prompt = "زهرا ۱۲ کارت بازی دارد که ۳ تای آن‌ها طلایی هستند. اگر او یک کارت را به طور تصادفی بردارد، احتمال اینکه کارت طلایی باشد چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(gold)",
      options = c("1/4", "3/12", "1/3", "3/4"),
      correct = "1/4",
      explanation = "احتمال اولیه 3/12 است و با ساده کردن، پاسخ نهایی 1/4 می‌شود."
    ),
    list(
      level = "hard",
      prompt = "محمد ۲۵ شیرینی دارد که ۱۰ تای آن‌ها خامه‌ای هستند. اگر او بدون نگاه کردن یک شیرینی بردارد، احتمال اینکه شیرینی خامه‌ای باشد چقدر است؟ پاسخ را ساده کنید.",
      prob_label = "P(cream)",
      options = c("2/5", "10/25", "3/5", "1/5"),
      correct = "2/5",
      explanation = "احتمال اولیه 10/25 است. با ساده کردن صورت و مخرج بر ۵، پاسخ نهایی 2/5 می‌شود."
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
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 80px;
      }
      .opt-btn:hover {
        background: #1565c0;
        transform: translateY(-2px);
      }
      .opt-btn.selected {
        background: #0d47a1;
        box-shadow: inset 0 0 0 3px rgba(255,255,255,0.3);
      }
      .fraction-choice {
        display: inline-flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        line-height: 1.05;
        min-width: 42px;
        direction: ltr;
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
        border-top: 3px solid currentColor;
        margin: 4px 0 3px 0;
      }
      .whole-choice {
        font-size: 24px;
        font-weight: 800;
        direction: ltr;
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
      div(class = "top-title", "مسائل احتمال ساده (Probability Problems)"),
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
    
    q_data <- rv$current_pool[[1]]
    q_data$options <- sample(q_data$options)
    rv$question <- q_data
    rv$level <- rv$question$level
    clear_selected_option()
  }

  # بارگذاری اولیه بازی
  observe({
    if (is.null(rv$question)) {
      init_game()
    }
  })

  # کنترل دکمه‌های بازنشانی و شروع مجدد
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
      
      # بازکردن اتوماتیک سطح سخت پس از سوال پنجم در صورت عملکرد خوب
      if (rv$q_num == 6 && hard_unlocked()) {
        rv$level <- "hard"
        all_qs <- get_full_bank()
        hard_qs <- Filter(function(q) q$level == "hard", all_qs)
        rv$current_pool[6:10] <- sample(hard_qs, 5)
        showNotification("سطح سخت به دلیل عملکرد عالی شما فعال شد!", type = "warning")
      }
      
      q_data <- rv$current_pool[[rv$q_num]]
      q_data$options <- sample(q_data$options)
      rv$question <- q_data
      rv$level <- rv$question$level
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
            actionButton("next_btn", if (rv$q_num == 10) "کارنامه" else "بعدی", class = "btn btn-info action-btn")
          ),
          uiOutput("feedback_ui")
        )
      ),
      # ستون چپ: نمایش بصری کسر و فرمول احتمال
      div(
        class = "visual-panel",
        uiOutput("visual_ui")
      )
    )
  })

  output$question_txt <- renderText({
    rv$question$prompt
  })

  output$visual_ui <- renderUI({
    fraction_box_ui(rv$question$prob_label)
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
          option_label_ui(val)
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
