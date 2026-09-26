# game22.R
# پیش‌بینی آماری

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
    list(level = "easy", prompt = "اگر دو سکه را ۴ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۱", "۲", "۳", "۴"), correct = "۱", explanation = "احتمال اینکه هر دو سکه شیر بیایند برابر با یک‌چهارم است. یک‌چهارمِ ۴ برابر با ۱ است."),
    list(level = "easy", prompt = "اگر دو سکه را ۸ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۲", "۱", "۳", "۴"), correct = "۲", explanation = "یک‌چهارمِ ۸ برابر با ۲ است. پس انتظار داریم حدود ۲ بار هر دو سکه شیر بیایند."),
    list(level = "easy", prompt = "اگر دو سکه را ۱۲ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۳", "۲", "۴", "۶"), correct = "۳", explanation = "یک‌چهارمِ ۱۲ برابر با ۳ است."),
    list(level = "easy", prompt = "اگر دو سکه را ۱۶ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۴", "۲", "۶", "۸"), correct = "۴", explanation = "احتمال دو شیر برابر با یک‌چهارم است. یک‌چهارمِ ۱۶ برابر با ۴ است."),
    list(level = "easy", prompt = "اگر دو سکه را ۲۰ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۵", "۴", "۸", "۱۰"), correct = "۵", explanation = "یک‌چهارمِ ۲۰ برابر با ۵ است."),
    list(level = "hard", prompt = "اگر دو سکه را ۲۴ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۶", "۴", "۸", "۱۲"), correct = "۶", explanation = "یک‌چهارمِ ۲۴ برابر با ۶ است."),
    list(level = "hard", prompt = "اگر دو سکه را ۲۸ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۷", "۵", "۹", "۱۴"), correct = "۷", explanation = "یک‌چهارمِ ۲۸ برابر با ۷ است."),
    list(level = "hard", prompt = "اگر دو سکه را ۴۰ بار بیندازیم، بهترین پیش‌بینی برای تعداد دفعاتی که هر دو سکه «شیر» می‌آیند چیست؟", options = c("۱۰", "۵", "۱۵", "۲۰"), correct = "۱۰", explanation = "یک‌چهارمِ ۴۰ برابر با ۱۰ است."),
    list(level = "easy", prompt = "در ۱۵ بار چرخاندن یک گردونه، عقربه ۳ بار روی رنگ سبز ایستاده است. با توجه به این داده‌ها، انتظار دارید در ۲۰ چرخش بعدی تقریباً چند بار روی سبز بایستد؟", options = c("۴", "۳", "۵", "۶"), correct = "۴", explanation = "۳ از ۱۵ برابر با یک‌پنجم است. یک‌پنجمِ ۲۰ برابر با ۴ است."),
    list(level = "easy", prompt = "در ۱۰ بار چرخاندن یک گردونه، عقربه ۲ بار روی رنگ قرمز ایستاده است. در ۲۰ چرخش بعدی، تقریباً چند بار انتظار دارید روی قرمز بایستد؟", options = c("۴", "۲", "۵", "۸"), correct = "۴", explanation = "۲ از ۱۰ برابر با یک‌پنجم است. یک‌پنجمِ ۲۰ برابر با ۴ است."),
    list(level = "easy", prompt = "سارا یک تاس را ۱۲ بار انداخت و ۳ بار عدد ۶ آمد. اگر تاس را ۲۰ بار دیگر بیندازد، با استفاده از داده‌های قبلی تقریباً چند بار عدد ۶ می‌آید؟", options = c("۵", "۳", "۴", "۶"), correct = "۵", explanation = "۳ از ۱۲ برابر با یک‌چهارم است. یک‌چهارمِ ۲۰ برابر با ۵ است."),
    list(level = "easy", prompt = "مریم یک سکه را ۱۵ بار انداخت و ۶ بار شیر آمد. اگر سکه را ۲۰ بار دیگر بیندازد، با توجه به این داده‌ها تقریباً چند بار شیر می‌آید؟", options = c("۸", "۶", "۱۰", "۱۲"), correct = "۸", explanation = "۶ از ۱۵ برابر با دوپنجم است. دوپنجمِ ۲۰ برابر با ۸ است."),
    list(level = "hard", prompt = "در ۲۵ بار چرخاندن یک گردونه، عقربه ۵ بار روی رنگ آبی ایستاده است. در ۳۵ چرخش بعدی تقریباً چند بار انتظار دارید روی آبی بایستد؟", options = c("۷", "۵", "۱۰", "۱۴"), correct = "۷", explanation = "۵ از ۲۵ برابر با یک‌پنجم است. یک‌پنجمِ ۳۵ برابر با ۷ است."),
    list(level = "hard", prompt = "علی یک تاس را ۲۰ بار انداخت و ۵ بار عدد ۴ آمد. اگر تاس را ۳۲ بار دیگر بیندازد، تقریباً چند بار عدد ۴ خواهد آمد؟", options = c("۸", "۵", "۱۰", "۱۲"), correct = "۸", explanation = "۵ از ۲۰ برابر با یک‌چهارم است. یک‌چهارمِ ۳۲ برابر با ۸ است."),
    list(level = "hard", prompt = "از ۳۰ بار پرتاب حلقه توسط نیما، ۹ پرتاب موفق بوده است. اگر نیما ۲۰ پرتاب دیگر انجام دهد و همین الگو ادامه یابد، تقریباً چند پرتاب موفق خواهد داشت؟", options = c("۶", "۵", "۸", "۹"), correct = "۶", explanation = "۹ از ۳۰ برابر با سه‌دهم است. سه‌دهمِ ۲۰ برابر با ۶ است."),
    list(level = "hard", prompt = "در ۴۰ بار چرخاندن یک گردونه، عقربه ۱۲ بار روی رنگ زرد ایستاده است. در ۳۰ چرخش بعدی تقریباً چند بار انتظار دارید روی زرد بایستد؟", options = c("۹", "۶", "۱۰", "۱۲"), correct = "۹", explanation = "۱۲ از ۴۰ برابر با سه‌دهم است. سه‌دهمِ ۳۰ برابر با ۹ است.")
  )
}

# ----------------------------
# رابط کاربری
# ----------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Vazirmatn:wght@400;700;800&display=swap"),
    tags$script(HTML("Shiny.addCustomMessageHandler('clearOptionSelection', function(m){ $('.opt-btn').removeClass('selected'); });")),
    tags$style(HTML("
      body { margin: 0; background: #f0f5f9; font-family: 'Vazirmatn', sans-serif; direction: rtl; color: #1e3a5a; }
      .game-shell { max-width: 900px; margin: 30px auto; background: #fff; border-radius: 30px; box-shadow: 0 20px 50px rgba(0,0,0,0.1); overflow: hidden; border: 4px solid #dbeafe; }
      .game-header { background: linear-gradient(135deg, #1864ab 0%, #4dabf7 100%); color: #fff; padding: 25px; }
      .header-title { font-size: 28px; font-weight: 800; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); margin-bottom: 20px; }
      .progress-container { background: #e7f5ff; height: 10px; border-radius: 5px; margin: 0 25px 20px 25px; overflow: hidden; }
      .progress-bar { background: #339af0; height: 100%; transition: width 0.5s ease; }
      .status-strip { display: grid; grid-template-columns: repeat(5, 1fr); gap: 10px; }
      .status-badge { background: rgba(255,255,255,0.2); padding: 10px; border-radius: 12px; text-align: center; }
      .status-label { font-size: 11px; opacity: 0.9; margin-bottom: 2px; }
      .status-value { font-size: 16px; font-weight: 700; }
      .main-area { padding: 30px; }
      .question-card { background: #f8fbff; border: 2px dashed #a5d8ff; border-radius: 20px; padding: 25px; text-align: center; margin-bottom: 25px; }
      .question-text { font-size: 22px; font-weight: 800; color: #1864ab; line-height: 1.9; }
      .play-grid { display: grid; gap: 30px; align-items: start; }
      .opt-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 15px; }
      .opt-btn { width: 100%; border: 3px solid #e7f5ff; border-radius: 18px; padding: 18px; background: #fff; color: #1c7ed6; font-weight: 700; font-size: 20px; transition: 0.3s; cursor: pointer; direction: ltr; display: flex; align-items: center; justify-content: center; min-height: 84px; }
      .opt-btn:hover { background: #e7f5ff; border-color: #339af0; }
      .opt-btn.selected { background: #339af0; color: #fff; border-color: #1864ab; transform: translateY(-3px); box-shadow: 0 5px 15px rgba(0,0,0,0.1); }
      .action-btn { border-radius: 15px !important; font-weight: 800 !important; padding: 12px 30px !important; font-size: 18px !important; }
      .fraction-choice { display: inline-flex; flex-direction: column; align-items: center; justify-content: center; line-height: 1.05; min-width: 42px; }
      .frac-top, .frac-bottom { display: block; font-size: 24px; font-weight: 800; }
      .frac-bar { display: block; width: 34px; border-top: 3px solid currentColor; margin: 4px 0 3px 0; }
      .whole-choice { font-size: 24px; font-weight: 800; }
      .stars-wrap { color: #ffd43b; font-size: 20px; margin-top: -5px; }
      .star-inactive { color: rgba(255,255,255,0.3); }
      .feedback-box { margin-top: 25px; padding: 20px; border-radius: 20px; font-size: 18px; line-height: 1.9; }
      .f-ok { background: #d3f9d8; color: #2b8a3e; border: 2px solid #b2f2bb; }
      .f-bad { background: #fff5f5; color: #c92a2a; border: 2px solid #ffc9c9; }
      @media (max-width: 768px) { .status-strip { grid-template-columns: repeat(3, 1fr); } }
    "))
  ),

  div(class = "game-shell",
    div(class = "game-header",
      div(class = "header-title", "پیش‌بینی آماری"),
      div(class = "status-strip",
        div(class = "status-badge", div(class = "status-label", "سطح"), div(class = "status-value", textOutput("level_txt", inline=T))),
        div(class = "status-badge", div(class = "status-label", "امتیاز"), div(class = "status-value", textOutput("score_txt", inline=T))),
        div(class = "status-badge", div(class = "status-label", "دقت"), div(class = "status-value", textOutput("acc_txt", inline=T))),
        div(class = "status-badge", div(class = "status-label", "ستاره"), uiOutput("stars_ui", inline=T)),
        div(class = "status-badge", actionButton("reset_btn", "نوسازی", style = "padding:2px 8px; font-size:12px; background:#fa5252; color:white; border:none; border-radius:5px; cursor:pointer;"))
      )
    ),
    div(class = "main-area",
      uiOutput("progress_ui"),
      uiOutput("game_layout")
    )
  )
)

# ----------------------------
# سرور
# ----------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(level = "easy", score = 0, total = 0, correct = 0, answered = FALSE, q_num = 1, question = NULL, current_pool = list(), game_over = FALSE)

  init_game <- function() {
    rv$level <- "easy"; rv$score <- 0; rv$total <- 0; rv$correct <- 0; rv$answered <- FALSE; rv$q_num <- 1; rv$game_over <- FALSE
    all_qs <- get_full_bank()
    rv$current_pool <- sample(all_qs, min(10, length(all_qs)))
    q_data <- rv$current_pool[[1]]; q_data$options <- sample(q_data$options)
    rv$question <- q_data; rv$level <- rv$question$level
    session$sendCustomMessage("clearOptionSelection", list())
  }

  observe({ if (is.null(rv$question)) init_game() })
  observeEvent(input$reset_btn, { init_game() })
  
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
    if (rv$q_num >= 10 || rv$q_num >= length(rv$current_pool)) { rv$game_over <- TRUE } else {
      rv$q_num <- rv$q_num + 1; rv$answered <- FALSE
      q_data <- rv$current_pool[[rv$q_num]]; q_data$options <- sample(q_data$options)
      rv$question <- q_data; rv$level <- rv$question$level
      session$sendCustomMessage("clearOptionSelection", list())
    }
  })

  observeEvent(input$restart_btn, { init_game() })

  output$level_txt <- renderText({ if(rv$level == "easy") "آسان" else "سخت" })
  output$score_txt <- renderText({ rv$score })
  output$acc_txt <- renderText({ if(rv$total == 0) "۰٪" else paste0(round(100 * rv$correct / rv$total), "٪") })
  output$stars_ui <- renderUI({ star_html(floor(rv$score / 20)) })
  
  output$progress_ui <- renderUI({
    tags$div(class = "progress-container", tags$div(class = "progress-bar", style = sprintf("width: %d%%;", (rv$q_num / 10) * 100)))
  })

  output$game_layout <- renderUI({
    if (rv$game_over) {
      return(div(style = "text-align:center; padding:40px;",
        tags$h2("پایان بازی", style = "color:#1864ab; font-weight:800;"),
        tags$p(paste("تعداد پاسخ‌های صحیح:", rv$correct, "از ۱۰")),
        tags$p(paste("امتیاز نهایی:", rv$score)),
        actionButton("restart_btn", "شروع مجدد", class = "btn-success action-btn", style = "background:#2b8a3e;color:white;border:none;")
      ))
    }
    tagList(
      div(class = "question-card", div(class = "question-text", rv$question$prompt)),
      div(class = "play-grid",
        div(class = "answer-area",
          option_buttons_ui(rv$question$options),
          div(style = "margin-top:25px; display:flex; gap:15px;",
            actionButton("check_btn", "ثبت پاسخ", class = "btn-primary action-btn", style = "background:#1c7ed6;color:white;border:none;"),
            actionButton("next_btn", if(rv$q_num == 10) "کارنامه" else "بعدی", class = "btn-light action-btn")
          ),
          uiOutput("feedback_ui")
        )
      )
    )
  })

  output$feedback_ui <- renderUI({
    if (!rv$answered) return(NULL)
    is_correct <- identical(as.character(input$answer), as.character(rv$question$correct))
    div(class = if (is_correct) "feedback-box f-ok" else "feedback-box f-bad",
      tags$b(if (is_correct) "پاسخ شما درست است." else "پاسخ شما درست نیست."),
      div(rv$question$explanation)
    )
  })
}

shinyApp(ui, server)
