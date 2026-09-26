library(shiny)
library(shinyjs)

# تعریف ۸ جفت مفهوم آماری (۱۶ کارت)
game_data <- data.frame(
  id = 1:16,
  content = c(
    "میانگین", "Mean",
    "میانه", "Median",
    "مد", "Mode",
    "واریانس", "Variance",
    "انحراف معیار", "Std Deviation",
    "نمودار دایره‌ای", "Pie Chart",
    "دامنه تغییرات", "Range",
    "نمودار ستونی", "Bar Chart"
  ),
  pair_id = rep(1:8, each = 2),
  stringsAsFactors = FALSE
)

# مخلوط کردن اولیه کارت‌ها
set.seed(as.integer(Sys.time()))
game_data <- game_data[sample(nrow(game_data)), ]

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # کتابخانه confetti
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.3/dist/confetti.browser.min.js"),

    tags$style(HTML("
      body {
        background: linear-gradient(to bottom, #89f7fe 0%, #66a6ff 100%);
        min-height: 100vh;
        font-family: 'Tahoma', sans-serif;
        direction: rtl;
        transition: background 1s ease;
      }

      .game-container {
        text-align: center;
        margin-top: 30px;
      }

      .status-text {
        font-size: 20px;
        color: white;
        margin-bottom: 20px;
        text-shadow: 1px 1px 3px rgba(0,0,0,0.5);
      }

      /* دکمه شروع بازی */
      .btn-start {
        font-family: 'Tahoma', sans-serif;
        font-size: 18px;
        font-weight: bold;
        color: white;
        background-color: #ff4757;
        border: none;
        padding: 10px 30px;
        border-radius: 25px;
        cursor: pointer;
        box-shadow: 0 4px 15px rgba(255, 71, 87, 0.4);
        transition: all 0.3s ease;
        margin-bottom: 20px;
      }
      .btn-start:hover {
        background-color: #ff6b81;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(255, 71, 87, 0.6);
      }
      .btn-start:active {
        transform: translateY(1px);
      }

      /* گرید 4 ستونه */
      .card-grid {
        display: grid;
        grid-template-columns: repeat(4, 120px);
        gap: 14px;
        justify-content: center;
        margin-top: 20px;
      }

      /* انیمیشن برد برای کل برد */
      .winner-board {
        animation: boardPulse 1.2s ease-in-out infinite;
      }

      @keyframes boardPulse {
        0%   { transform: scale(1); }
        50%  { transform: scale(1.02); }
        100% { transform: scale(1); }
      }

      /* وضعیت غیرفعال قبل از شروع بازی */
      .grid-disabled {
        pointer-events: none;
        opacity: 0.7;
      }

      .card-link {
        text-decoration: none !important;
      }

      /* کارت شش ضلعی */
      .hex-card {
        width: 110px;
        height: 100px;
        margin: 0 auto;
        background-color: white;
        clip-path: polygon(25% 6%, 75% 6%, 100% 50%, 75% 94%, 25% 94%, 0% 50%);
        -webkit-clip-path: polygon(25% 6%, 75% 6%, 100% 50%, 75% 94%, 25% 94%, 0% 50%);
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        padding: 10px;
        cursor: pointer;
        font-weight: bold;
        border: 2px solid #007bff;
        box-shadow: 2px 2px 6px rgba(0,0,0,0.25);
        transition: transform 0.2s, background-color 0.3s, box-shadow 0.3s;
        line-height: 1.3;
        font-size: 13px;
      }

      .hex-card:hover {
        transform: scale(1.05);
      }

      .hex-card.flipped {
        background-color: #e9ecef;
        border: 3px solid #28a745;
        color: #222;
        background-image: none;
      }

      /* کارت‌های مچ شده: با درخشش زیبا */
      .hex-card.matched {
        background-color: #eefaf1;
        border: 3px solid #28a745;
        color: #222;
        background-image: none;
        box-shadow: 0 0 18px rgba(40, 167, 69, 0.6);
      }

      /* تصویر پس‌زمینه ریاضی و آمار با تم رنگی آبی و سرمه‌ای */
      .hex-card.hidden-content {
        color: transparent;
        background-image: url('https://img.freepik.com/free-vector/math-doodles-illustration-navy-blue-background_53876-118818.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
      }

      /* استایل پنجره برد */
      .modal-content {
        border-radius: 18px;
        border: none;
        box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
      }

      .modal-header {
        background: linear-gradient(135deg, #00c6ff, #0072ff);
        color: white;
        border-top-left-radius: 18px;
        border-top-right-radius: 18px;
      }

      .modal-title {
        width: 100%;
        text-align: center;
        font-weight: bold;
        font-size: 22px;
      }

      .modal-body {
        font-size: 18px;
        text-align: center;
        padding: 30px;
      }

      .btn-restart-modal {
        background-color: #28a745;
        color: white;
        border: none;
        padding: 8px 25px;
        border-radius: 20px;
        font-weight: bold;
        cursor: pointer;
        transition: background-color 0.2s;
      }
      .btn-restart-modal:hover {
        background-color: #218838;
      }
    ")),

    tags$script(HTML("
      // افکت صوتی برنده شدن با Web Audio API (تولید صدا بدون نیاز به فایل صوتی آماده)
      function playWinSound() {
        var context = new (window.AudioContext || window.webkitAudioContext)();
        var notes = [261.63, 329.63, 392.00, 523.25, 659.25, 783.99, 1046.50]; // نت‌های شاد شیپور پیروزی
        notes.forEach(function(freq, index) {
          setTimeout(function() {
            var osc = context.createOscillator();
            var gain = context.createGain();
            osc.connect(gain);
            gain.connect(context.destination);
            osc.frequency.value = freq;
            osc.type = 'triangle';
            gain.gain.setValueAtTime(0.1, context.currentTime);
            gain.gain.exponentialRampToValueAtTime(0.01, context.currentTime + 0.4);
            osc.start(context.currentTime);
            osc.stop(context.currentTime + 0.45);
          }, index * 100);
        });
      }

      Shiny.addCustomMessageHandler('celebrateWin', function(message) {
        // ۱. پخش صدای برد
        playWinSound();

        // ۲. افکت درخشش بدنه صفحه (تغییر تم موقت)
        document.body.style.background = 'linear-gradient(to bottom, #a1c4fd 0%, #c2e9fb 100%)';

        // ۳. بارش متناوب شرشره
        var duration = 4 * 1000;
        var end = Date.now() + duration;

        (function frame() {
          confetti({
            particleCount: 5,
            angle: 60,
            spread: 55,
            origin: { x: 0 }
          });
          confetti({
            particleCount: 5,
            angle: 120,
            spread: 55,
            origin: { x: 1 }
          });

          if (Date.now() < end) {
            requestAnimationFrame(frame);
          }
        }());

        // ۴. افکت حرکت پالس روی برد کارت‌ها
        var board = document.getElementById('game-board');
        if (board) {
          board.classList.add('winner-board');
        }
      });

      Shiny.addCustomMessageHandler('resetWinEffects', function(message) {
        // بازنشانی رنگ پس‌زمینه و حذف کلاس پالس برد در شروع بازی جدید
        document.body.style.background = 'linear-gradient(to bottom, #89f7fe 0%, #66a6ff 100%)';
        var board = document.getElementById('game-board');
        if (board) {
          board.classList.remove('winner-board');
        }
      });
    "))
  ),

  div(class = "game-container",
      h3("بازی حافظه آماری", style = "color: white;"),

      actionButton("start_btn", "شروع بازی", class = "btn-start"),

      div(
        class = "status-text",
        "زمان سپری شده: ",
        textOutput("timer", inline = TRUE),
        " ثانیه"
      ),

      uiOutput("game_board")
  )
)

server <- function(input, output, session) {

  rv <- reactiveValues(
    game_started = FALSE,
    flipped = integer(0),
    matched = integer(0),
    start_time = NULL,
    current_time = 0
  )

  # تابع شروع و تنظیم مجدد بازی
  start_new_game <- function() {
    rv$game_started <- TRUE
    rv$start_time <- Sys.time()
    rv$flipped <- integer(0)
    rv$matched <- integer(0)
    rv$current_time <- 0

    game_data <<- game_data[sample(nrow(game_data)), ]

    updateActionButton(session, "start_btn", label = "شروع مجدد")
    session$sendCustomMessage("resetWinEffects", list())
  }

  observeEvent(input$start_btn, {
    start_new_game()
  })

  # دکمه شروع بازی مجدد از داخل مودال پیروزی
  observeEvent(input$restart_from_modal, {
    removeModal()
    start_new_game()
  })

  observe({
    invalidateLater(1000, session)
    if (rv$game_started && !is.null(rv$start_time)) {
      rv$current_time <- round(difftime(Sys.time(), rv$start_time, units = "secs"))
    }
  })

  output$timer <- renderText({
    rv$current_time
  })

  output$game_board <- renderUI({
    grid_class <- if (rv$game_started) "card-grid" else "card-grid grid-disabled"

    div(
      id = "game-board",
      class = grid_class,
      lapply(1:nrow(game_data), function(i) {
        is_flipped <- i %in% rv$flipped
        is_matched <- i %in% rv$matched

        class_attr <- if (is_matched) {
          "hex-card matched"
        } else if (is_flipped) {
          "hex-card flipped"
        } else {
          "hex-card hidden-content"
        }

        actionLink(
          inputId = paste0("card_", i),
          label = div(class = class_attr, game_data$content[i]),
          class = "card-link",
          onclick = sprintf(
            "Shiny.setInputValue('card_clicked', %d, {priority: 'event'})",
            i
          )
        )
      })
    )
  })

  observeEvent(input$card_clicked, {
    if (!rv$game_started) return()

    idx <- input$card_clicked

    if (idx %in% rv$flipped || idx %in% rv$matched || length(rv$flipped) >= 2) {
      return()
    }

    rv$flipped <- c(rv$flipped, idx)

    if (length(rv$flipped) == 2) {
      card1 <- game_data[rv$flipped[1], ]
      card2 <- game_data[rv$flipped[2], ]

      if (card1$pair_id == card2$pair_id) {
        rv$matched <- c(rv$matched, rv$flipped)
        rv$flipped <- integer(0)

        # پایان بازی و برد بازیکن
        if (length(rv$matched) == nrow(game_data)) {
          rv$game_started <- FALSE

          # ارسال پیام اجرای جشن به مرورگر
          session$sendCustomMessage("celebrateWin", list())

          showModal(
            modalDialog(
              title = "🎉 تبریک! شما برنده شدید 🎉",
              HTML(paste0(
                "<div style='line-height:2; direction: rtl;'>",
                "شما تمامی مفاهیم آماری را با موفقیت پیدا و مطابقت دادید.<br>",
                "⏱️ زمان ثبت شده: <b style='color:#0072ff; font-size: 22px;'>", rv$current_time, " ثانیه</b><br><br>",
                "برای به چالش کشیدن دوباره خود، دکمه زیر را فشار دهید.",
                "</div>"
              )),
              easyClose = TRUE,
              footer = tagList(
                actionButton("restart_from_modal", "بازی مجدد", class = "btn-restart-modal"),
                modalButton("بستن")
              )
            )
          )
        }
      } else {
        delay(1000, {
          rv$flipped <- integer(0)
        })
      }
    }
  })
}

shinyApp(ui, server)
