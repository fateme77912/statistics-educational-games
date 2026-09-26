# game11.R
# بازی 11: احتمال رخدادهای مستقل و وابسته
# ظاهر و ساختار مشابه بازی 10
# منطق آموزشی: Independent and Dependent Events

library(shiny)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

coin_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <defs>
      <radialGradient id="coinGrad" cx="35%" cy="30%" r="70%">
        <stop offset="0%" stop-color="#fff9d9"/>
        <stop offset="100%" stop-color="#f3c84d"/>
      </radialGradient>
    </defs>
    <rect width="320" height="240" rx="26" fill="#fff8e9"/>
    <circle cx="160" cy="118" r="72" fill="url(#coinGrad)" stroke="#c28c12" stroke-width="10"/>
    <circle cx="160" cy="118" r="54" fill="#ffd965" stroke="#e5ad1f" stroke-width="4"/>
    <text x="160" y="132" text-anchor="middle" font-size="46" font-family="Arial, sans-serif" font-weight="700" fill="#946500">H</text>
    <circle cx="130" cy="86" r="10" fill="rgba(255,255,255,0.35)"/>
    <circle cx="120" cy="76" r="4" fill="rgba(255,255,255,0.35)"/>
  </svg>'
  svg_data_uri(svg)
}

dice_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#eef7ff"/>
    <rect x="92" y="50" width="136" height="136" rx="22" fill="#ffffff" stroke="#2f7be5" stroke-width="8"/>
    <circle cx="126" cy="84" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="84" r="11" fill="#2f7be5"/>
    <circle cx="126" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="160" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="118" r="11" fill="#2f7be5"/>
    <circle cx="126" cy="152" r="11" fill="#2f7be5"/>
    <circle cx="194" cy="152" r="11" fill="#2f7be5"/>
  </svg>'
  svg_data_uri(svg)
}

bags_svg <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f4fbff"/>
    <path d="M122 74 C126 52, 194 52, 198 74" fill="none" stroke="#1d9bf0" stroke-width="8" stroke-linecap="round"/>
    <path d="M104 82 Q160 54 216 82 L205 186 Q160 206 115 186 Z" fill="#ffffff" stroke="#1d9bf0" stroke-width="8" />
    <circle cx="137" cy="126" r="12" fill="#ff6b6b"/>
    <circle cx="179" cy="126" r="12" fill="#4dabf7"/>
    <circle cx="160" cy="158" r="12" fill="#8cd348"/>
    <path d="M134 88 Q160 68 186 88" fill="none" stroke="#1d9bf0" stroke-width="6"/>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    coin = coin_svg(),
    dice = dice_svg(),
    bags = bags_svg()
  )

  tags$img(
    src = src,
    style = "width:100%; max-width:320px; display:block; margin:0 auto;"
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

option_buttons_ui <- function(options) {
  tags$div(
    class = "opt-grid",
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
}

# ----------------------------
# Question generation
# ----------------------------
make_question <- function(level = c("easy", "hard")) {
  level <- match.arg(level)

  items <- list()

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "دو بار سکه سالم را پرتاب می‌کنیم. آیا «بار اول شیر» و «بار دوم شیر» رخدادهای مستقلی هستند؟",
    options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "مکمل‌اند", "ناسازگارند"),
    correct = "بله، مستقل‌اند",
    explanation = "نتیجه پرتاب اول روی پرتاب دوم اثری ندارد؛ پس این دو رخداد مستقل‌اند."
  )

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "دو بار سکه سالم را پرتاب می‌کنیم. احتمال آمدن شیر در هر دو بار چقدر است؟",
    options = c("1/2", "1/4", "3/4", "1"),
    correct = "1/4",
    explanation = "برای هر پرتاب، احتمال شیر 1/2 است. چون رخدادها مستقل‌اند، 1/2 × 1/2 = 1/4."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در دو بار پرتاب یک تاس سالم، آیا «بار اول 6» و «بار دوم 6» مستقل هستند؟",
    options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "مکمل‌اند", "همپوشان‌اند"),
    correct = "بله، مستقل‌اند",
    explanation = "هر پرتاب تاس از پرتاب دیگر مستقل است؛ نتیجه بار اول روی بار دوم اثر ندارد."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در دو بار پرتاب یک تاس سالم، احتمال آمدن 6 در هر دو بار چقدر است؟",
    options = c("1/6", "1/12", "1/36", "1/3"),
    correct = "1/36",
    explanation = "احتمال 6 در هر بار 1/6 است. چون رخدادها مستقل‌اند، 1/6 × 1/6 = 1/36."
  )

  items[[length(items) + 1]] <- list(
    kind = "bags",
    question = "از یک کیسه، یک توپ برداشته و بدون جایگذاری، بار دوم هم توپ برمی‌داریم. آیا دو برداشت مستقل‌اند؟",
    options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "مکمل‌اند", "ناسازگارند"),
    correct = "خیر، وابسته‌اند",
    explanation = "چون توپ اول را برنمی‌گردانیم، ترکیب کیسه عوض می‌شود و برداشت دوم به اول وابسته است."
  )

  items[[length(items) + 1]] <- list(
    kind = "bags",
    question = "در یک کیسه 3 توپ قرمز و 2 توپ آبی است. بدون جایگذاری دو توپ برمی‌داریم. احتمال اینکه هر دو توپ قرمز باشند چقدر است؟",
    options = c("3/10", "6/25", "1/5", "1/2"),
    correct = "3/10",
    explanation = "بار اول: 3/5. بعد از برداشتن یک قرمز، بار دوم: 2/4. پس 3/5 × 2/4 = 3/10."
  )

  items[[length(items) + 1]] <- list(
    kind = "bags",
    question = "در یک کیسه 4 توپ سبز و 1 توپ زرد است. اگر یک توپ برداریم و آن را برگردانیم، بار دوم هم توپ برمی‌داریم. آیا دو برداشت مستقل‌اند؟",
    options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "ناسازگارند", "مکمل‌اند"),
    correct = "بله، مستقل‌اند",
    explanation = "چون توپ را برمی‌گردانیم، ترکیب کیسه تغییر نمی‌کند و دو برداشت مستقل‌اند."
  )

  items[[length(items) + 1]] <- list(
    kind = "bags",
    question = "یک کیسه 4 توپ قرمز و 1 توپ آبی دارد. با جایگذاری دو بار برداشت می‌کنیم. احتمال اینکه هر دو بار قرمز باشد چقدر است؟",
    options = c("1/5", "4/25", "2/5", "16/25"),
    correct = "16/25",
    explanation = "در هر بار احتمال قرمز 4/5 است. با جایگذاری، دو برداشت مستقل‌اند: 4/5 × 4/5 = 16/25."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "در پرتاب یک تاس سالم، اگر A = «عدد زوج» و B = «عدد بزرگ‌تر از 4» باشد، این دو رخداد چه رابطه‌ای دارند؟",
    options = c("مستقل‌اند", "وابسته‌اند", "ناسازگارند", "مکمل‌اند"),
    correct = "وابسته‌اند",
    explanation = "وقتی درباره یک پرتاب واحد صحبت می‌کنیم، این رخدادها به هم وابسته به معنای همپوشانی/ارتباط مجموعه‌ای هستند، نه رخدادهای مستقلِ دو مرحله‌ای."
  )

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "دو بار سکه سالم را پرتاب می‌کنیم. احتمال اینکه حداقل یک بار شیر بیاید چقدر است؟",
    options = c("1/4", "1/2", "3/4", "1"),
    correct = "3/4",
    explanation = "مکمل این رخداد «هیچ بار شیر نیاید» است، یعنی خط-خط. احتمال آن 1/4 است، پس جواب 1 - 1/4 = 3/4."
  )

  items[[length(items) + 1]] <- list(
    kind = "coin",
    question = "دو بار سکه سالم را پرتاب می‌کنیم. آیا «بار اول خط» و «بار دوم شیر» مستقل‌اند؟",
    options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "همپوشان‌اند", "مکمل‌اند"),
    correct = "بله، مستقل‌اند",
    explanation = "نتیجه یک پرتاب، نتیجه پرتاب دیگر را تغییر نمی‌دهد؛ پس مستقل‌اند."
  )

  items[[length(items) + 1]] <- list(
    kind = "dice",
    question = "دو بار یک تاس سالم را پرتاب می‌کنیم. احتمال اینکه بار اول 2 و بار دوم 5 بیاید چقدر است؟",
    options = c("1/6", "1/12", "1/18", "1/36"),
    correct = "1/36",
    explanation = "هر رخداد احتمال 1/6 دارد و دو پرتاب مستقل‌اند؛ بنابراین 1/6 × 1/6 = 1/36."
  )

  if (level == "hard") {
    items[[length(items) + 1]] <- list(
      kind = "bags",
      question = "در کیسه‌ای 5 توپ قرمز و 3 توپ آبی است. بدون جایگذاری دو توپ برمی‌داریم. احتمال اینکه اول قرمز و دوم آبی باشد چقدر است؟",
      options = c("15/64", "5/8", "15/56", "3/8"),
      correct = "15/56",
      explanation = "بار اول قرمز: 5/8. بعد از آن 3 آبی از 7 توپ باقی‌مانده داریم، پس 3/7. حاصل ضرب: 5/8 × 3/7 = 15/56."
    )

    items[[length(items) + 1]] <- list(
      kind = "bags",
      question = "در کیسه‌ای 6 توپ سبز و 4 توپ زرد است. یک توپ برمی‌داریم و آن را برنمی‌گردانیم، سپس بار دوم برداشت می‌کنیم. آیا این دو برداشت مستقل‌اند؟",
      options = c("بله، مستقل‌اند", "خیر، وابسته‌اند", "مکمل‌اند", "ناسازگارند"),
      correct = "خیر، وابسته‌اند",
      explanation = "چون بعد از برداشت اول، تعداد توپ‌ها و نسبت رنگ‌ها تغییر می‌کند، برداشت دوم وابسته است."
    )

    items[[length(items) + 1]] <- list(
      kind = "dice",
      question = "دو بار یک تاس سالم را پرتاب می‌کنیم. احتمال اینکه بار اول عدد زوج و بار دوم عدد فرد باشد چقدر است؟",
      options = c("1/6", "1/4", "1/3", "1/2"),
      correct = "1/4",
      explanation = "احتمال زوج در هر بار 1/2 و احتمال فرد در هر بار 1/2 است. دو پرتاب مستقل‌اند، پس 1/2 × 1/2 = 1/4."
    )

    items[[length(items) + 1]] <- list(
      kind = "coin",
      question = "سه بار سکه سالم را پرتاب می‌کنیم. احتمال اینکه هر سه بار شیر بیاید چقدر است؟",
      options = c("1/8", "1/6", "3/8", "1/4"),
      correct = "1/8",
      explanation = "سه پرتاب مستقل داریم و احتمال شیر در هر بار 1/2 است، پس 1/2 × 1/2 × 1/2 = 1/8."
    )

    items[[length(items) + 1]] <- list(
      kind = "bags",
      question = "یک کیسه شامل 2 توپ قرمز و 2 توپ آبی است. با جایگذاری دو بار برداشت می‌کنیم. احتمال اینکه هر دو بار آبی باشد چقدر است؟",
      options = c("1/4", "1/2", "1/8", "3/16"),
      correct = "1/4",
      explanation = "احتمال آبی در هر بار 2/4 = 1/2 است. با جایگذاری، دو برداشت مستقل‌اند: 1/2 × 1/2 = 1/4."
    )
  }

  q <- sample(items, 1)[[1]]
  q$options <- sample(unique(q$options))
  q
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Vazirmatn:wght@400;500;700;800&display=swap"
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('clearOptionSelection', function(message) {
        $('.opt-btn').removeClass('selected');
      });
    ")),
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

      .game-card {
        background: linear-gradient(180deg, #fdfefe 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 16px;
        min-height: 270px;
      }

      .visual-label {
        text-align: center;
        color: #0f3b6d;
        font-size: 15px;
        font-weight: 800;
        margin-top: 8px;
      }

      .answer-card {
        background: linear-gradient(180deg, #ffffff 0%, #f8fdff 100%);
        border: 2px solid #dceff7;
        border-radius: 20px;
        padding: 18px;
        min-height: 270px;
      }

      .opt-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 12px;
        margin-top: 6px;
      }

      .opt-btn {
        width: 100%;
        border: none;
        border-radius: 16px;
        padding: 14px 16px;
        background: linear-gradient(180deg, #42b7ff 0%, #1d9bf0 100%);
        color: #ffffff;
        font-family: 'Vazirmatn', Tahoma, sans-serif;
        font-size: 17px;
        font-weight: 800;
        line-height: 1.8;
        text-align: center;
        box-shadow: 0 6px 14px rgba(29, 155, 240, 0.22);
        transition: all 0.18s ease;
        cursor: pointer;
      }

      .opt-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 10px 18px rgba(29, 155, 240, 0.28);
        background: linear-gradient(180deg, #53beff 0%, #229ff2 100%);
      }

      .opt-btn.selected {
        background: linear-gradient(180deg, #167fca 0%, #0d6fb5 100%);
        box-shadow: inset 0 0 0 3px rgba(255,255,255,0.25), 0 10px 18px rgba(13, 111, 181, 0.28);
        transform: translateY(-1px) scale(0.99);
      }

      .action-row {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        margin-top: 14px;
      }

      .action-btn {
        border-radius: 14px !important;
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

  div(class = "game-shell",

    div(class = "game-header",
      div(class = "header-top",
        div(class = "header-title", "احتمال رخدادهای مستقل و وابسته"),
        div(class = "header-actions",
          actionButton("easy_btn", "سطح آسان", class = "top-btn btn-easy"),
          actionButton("hard_btn", "سطح سخت", class = "top-btn btn-hard"),
          actionButton("reset_btn", "شروع مجدد", class = "top-btn btn-reset")
        )
      ),

      div(class = "status-strip",
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
      )
    ),

    div(class = "main-area",

      div(class = "progress-card",
        div(class = "progress-row",
          div(class = "progress-pill", textOutput("progress_txt")),
          uiOutput("progress_ui")
        )
      ),

      div(class = "question-card",
        div(class = "question-text", textOutput("question_txt")),
        div(class = "question-sub", "گزینه درست را انتخاب کن و روی «ثبت پاسخ» بزن.")
      ),

      div(class = "play-grid",
        div(class = "game-card",
          uiOutput("visual_ui"),
          div(class = "visual-label", textOutput("visual_label"))
        ),

        div(class = "answer-card",
          uiOutput("options_ui"),
          div(class = "action-row",
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
# Server
# ----------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    level = "easy",
    score = 0,
    total = 0,
    correct = 0,
    question_index = 1,
    answered = FALSE,
    selected = NULL,
    question = make_question("easy")
  )

  hard_unlocked <- reactive({
    if (rv$total == 0) return(FALSE)
    rv$correct / rv$total >= 0.8
  })

  stars_count <- reactive({
    acc <- if (rv$total == 0) 0 else rv$correct / rv$total
    score_stars <- floor(rv$score / 25)
    acc_bonus <- if (acc >= 0.8 && rv$total >= 5) 1 else 0
    min(5, max(0, score_stars + acc_bonus))
  })

  clear_selected_option <- function() {
    rv$selected <- NULL
    session$sendCustomMessage("clearOptionSelection", list())
  }

  new_round <- function(level = "easy") {
    rv$level <- level
    rv$score <- 0
    rv$total <- 0
    rv$correct <- 0
    rv$question_index <- 1
    rv$answered <- FALSE
    rv$question <- make_question(level)
    clear_selected_option()
  }

  new_q <- function(level = rv$level) {
    if (rv$question_index >= 10) {
      showNotification("این دور تمام شد. برای دور جدید روی شروع مجدد بزن.", type = "message")
      return()
    }

    rv$question_index <- rv$question_index + 1
    rv$question <- make_question(level)
    rv$answered <- FALSE
    clear_selected_option()
  }

  observeEvent(input$easy_btn, {
    new_round("easy")
  })

  observeEvent(input$hard_btn, {
    if (hard_unlocked()) {
      new_round("hard")
    } else {
      showNotification("برای باز شدن سطح سخت باید حداقل 80٪ پاسخ درست داشته باشی.", type = "warning")
    }
  })

  observeEvent(input$reset_btn, {
    new_round("easy")
  })

  observeEvent(input$next_btn, {
    if (!rv$answered) {
      showNotification("اول پاسخ این سؤال را ثبت کن.", type = "message")
      return()
    }
    new_q(rv$level)
  })

  observeEvent(input$check_btn, {
    if (is.null(input$answer) || identical(input$answer, "")) {
      showNotification("یک گزینه را انتخاب کن.", type = "message")
      return()
    }

    if (rv$answered) return()

    rv$answered <- TRUE
    rv$selected <- input$answer
    rv$total <- rv$total + 1

    if (identical(input$answer, rv$question$correct)) {
      rv$correct <- rv$correct + 1
      rv$score <- rv$score + ifelse(rv$level == "easy", 10, 15)
      showNotification("آفرین! پاسخ درست بود.", type = "message")
    } else {
      rv$score <- max(0, rv$score - 2)
      showNotification("اشتباه بود. توضیح را ببین.", type = "error")
    }
  })

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
    if (hard_unlocked()) {
      "باز"
    } else {
      "قفل"
    }
  })

  output$stars_ui <- renderUI({
    star_html(stars_count())
  })

  output$progress_txt <- renderText({
    paste0("سؤال ", rv$question_index, " از ۱۰")
  })

  output$progress_ui <- renderUI({
    pct <- round((rv$question_index - 1) / 10 * 100)
    if (rv$answered) {
      pct <- round(rv$question_index / 10 * 100)
    }

    tags$div(class = "progress",
      tags$div(class = "progress-bar", style = paste0("width:", pct, "%;"))
    )
  })

  output$question_txt <- renderText({
    rv$question$question
  })

  output$visual_label <- renderText({
    switch(rv$question$kind,
      coin = "سکه سالم",
      dice = "تاس سالم",
      bags = "کیسه توپ‌ها"
    )
  })

  output$visual_ui <- renderUI({
    question_image_ui(rv$question$kind)
  })

  output$options_ui <- renderUI({
    option_buttons_ui(rv$question$options)
  })

  output$feedback_ui <- renderUI({
    if (!rv$answered) {
      return(
        tags$div(
          class = "hint-box",
          "بعد از انتخاب گزینه، روی «ثبت پاسخ» بزن. سپس با «سؤال بعدی» ادامه بده."
        )
      )
    }

    is_correct <- identical(rv$selected, rv$question$correct)
    box_class <- if (is_correct) "feedback-ok" else "feedback-bad"

    tags$div(class = box_class,
      tags$b(if (is_correct) "پاسخ درست!" else "پاسخ نادرست."),
      tags$div(style = "margin-top:6px;", rv$question$explanation),
      tags$div(
        style = "margin-top:6px; font-size:13px; font-weight:700;",
        paste0("گزینه درست: ", rv$question$correct)
      )
    )
  })
}

shinyApp(ui, server)
