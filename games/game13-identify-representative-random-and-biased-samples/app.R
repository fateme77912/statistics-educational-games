library(shiny)
library(shinyjs)

# ----------------------------
# Helpers
# ----------------------------
svg_data_uri <- function(svg) {
  txt <- URLencode(svg, reserved = TRUE)
  paste0("data:image/svg+xml;utf8,", txt)
}

# تصاویر مفهومی عمومی که پاسخ را لو نمی‌دهند
svg_sampling_school <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f4fbff"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#1d9bf0" font-family="Arial, sans-serif">پژوهش در مدرسه</text>
    <!-- کل مدرسه -->
    <rect x="40" y="65" width="100" height="110" rx="10" fill="#e1f5fe" stroke="#0288d1" stroke-width="2"/>
    <text x="90" y="125" text-anchor="middle" font-size="14" fill="#0288d1" font-family="Arial, sans-serif">کل دانش‌آموزان</text>
    <!-- فلش نمونه‌گیری -->
    <path d="M 155 120 L 175 120" stroke="#0072b1" stroke-width="3" fill="none"/>
    <polygon points="175,115 185,120 175,125" fill="#0072b1"/>
    <!-- نمونه -->
    <rect x="190" y="75" width="90" height="90" rx="10" fill="#e8f5e9" stroke="#2e7d32" stroke-width="2"/>
    <text x="235" y="125" text-anchor="middle" font-size="14" fill="#2e7d32" font-family="Arial, sans-serif">گروه نمونه</text>
  </svg>'
  svg_data_uri(svg)
}

svg_sampling_sports <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#fffcf0"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#f08c00" font-family="Arial, sans-serif">نظرسنجی ورزشی / تفریحی</text>
    <!-- نماد توپ یا ورزش -->
    <circle cx="160" cy="115" r="45" fill="#ffe3e3" stroke="#ff6b6b" stroke-width="3"/>
    <line x1="125" y1="90" x2="195" y2="140" stroke="#ff6b6b" stroke-width="2"/>
    <line x1="125" y1="140" x2="195" y2="90" stroke="#ff6b6b" stroke-width="2"/>
    <circle cx="160" cy="115" r="25" fill="#ffffff" stroke="#ff6b6b" stroke-width="2"/>
    <text x="160" y="195" text-anchor="middle" font-size="14" fill="#495057" font-family="Arial, sans-serif">انتخاب گروهی برای بررسی علایق</text>
  </svg>'
  svg_data_uri(svg)
}

svg_sampling_general <- function() {
  svg <- '
  <svg xmlns="http://www.w3.org/2000/svg" width="320" height="240" viewBox="0 0 320 240">
    <rect width="320" height="240" rx="26" fill="#f9fff4"/>
    <text x="160" y="35" text-anchor="middle" font-size="18" font-weight="bold" fill="#2b8a3e" font-family="Arial, sans-serif">روش انتخاب از جامعه</text>
    <!-- المان‌های پراکنده به عنوان کل جامعه -->
    <circle cx="70" cy="90" r="10" fill="#adb5bd"/>
    <circle cx="95" cy="130" r="10" fill="#339af0"/>
    <circle cx="65" cy="160" r="10" fill="#adb5bd"/>
    <circle cx="120" cy="170" r="10" fill="#adb5bd"/>
    <!-- ظرف انتخاب -->
    <rect x="180" y="80" width="80" height="100" rx="8" fill="#f1f3f5" stroke="#495057" stroke-width="2"/>
    <circle cx="205" cy="110" r="10" fill="#339af0"/>
    <circle cx="235" cy="140" r="10" fill="#339af0"/>
    <text x="160" y="210" text-anchor="middle" font-size="13" fill="#868e96" font-family="Arial, sans-serif">چگونه اعضا را وارد سبد نمونه می‌کنیم؟</text>
  </svg>'
  svg_data_uri(svg)
}

question_image_ui <- function(kind) {
  src <- switch(kind,
    school = svg_sampling_school(),
    sports = svg_sampling_sports(),
    general = svg_sampling_general()
  )
  tags$img(
    src = src,
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
# Question Bank (16 Unique Questions)
# ----------------------------
get_full_bank <- function() {
  list(
    # Easy Questions
    list(
      kind = "school", level = "easy",
      question = "اگر از بین کل دانش‌آموزان یک مدرسه، با قرعه‌کشی کامپیوتری و بدون هیچ ترجیحی ۵۰ نفر را انتخاب کنیم، این نمونه چیست؟",
      options = c("نمونه تصادفی", "نمونه معرف", "نمونه اریب", "هیچ‌کدام"),
      correct = "نمونه تصادفی",
      explanation = "چون هر دانش‌آموز شانس کاملاً برابری داشته و هیچ ویژگی خاصی در انتخاب دخالت داده نشده است."
    ),
    list(
      kind = "school", level = "easy",
      question = "اگر در نمونه انتخابی ما، نسبت سال‌اولی‌ها، سال‌دومی‌ها و سال‌سومی‌ها دقیقاً شبیه به نسبت آن‌ها در کل مدرسه باشد، این نمونه چیست؟",
      options = c("نمونه معرف", "نمونه تصادفی", "نمونه اریب", "نمونه نامشخص"),
      correct = "نمونه معرف",
      explanation = "این نمونه معرف است چون ساختار و ویژگی‌های جامعه اصلی را به خوبی بازتاب می‌دهد."
    ),
    list(
      kind = "sports", level = "easy",
      question = "اگر معلمی فقط از دانش‌آموزانی که در حیاط فوتبال بازی می‌کنند درباره ورزش موردعلاقه کل مدرسه بپرسد، این نمونه چیست؟",
      options = c("نمونه اریب", "نمونه معرف", "نمونه تصادفی", "نمونه عادلانه"),
      correct = "نمونه اریب",
      explanation = "این نمونه اریب است چون فقط از گروه خاصی که مشغول ورزش خاصی هستند نظرسنجی شده است."
    ),
    list(
      kind = "general", level = "easy",
      question = "یک کارخانه لامپ‌سازی، از هر جعبه ۱۰۰ تایی به صورت تصادفی ۵ لامپ را برای تست کیفیت برمی‌دارد. این نمونه چیست؟",
      options = c("نمونه تصادفی", "نمونه اریب", "نمونه معرف", "نمونه ناقص"),
      correct = "نمونه تصادفی",
      explanation = "انتخاب لامپ‌ها بدون هیچ الگوی جهت‌داری انجام شده است."
    ),
    list(
      kind = "general", level = "easy",
      question = "کدام روش بهترین راه برای به دست آوردن یک نمونه بدون تعصب و بی‌طرفانه از کل شهروندان است؟",
      options = c("قرعه‌کشی تصادفی کدهای ملی شهروندان", "نظرسنجی از خریداران یک پاساژ لوکس", "مصاحبه با افرادی که ساعت ۸ صبح در ایستگاه مترو هستند", "نظرسنجی تلفنی فقط از صاحبان خودروهای شخصی"),
      correct = "قرعه‌کشی تصادفی کدهای ملی شهروندان",
      explanation = "در این حالت تمام افراد شهر شانس مساوی برای ورود به نمونه دارند."
    ),
    list(
      kind = "sports", level = "easy",
      question = "برای بررسی ساعت خواب شبانه نوجوانان شهر، اگر فقط از اعضای یک باشگاه ورزشی نظرسنجی کنیم، نمونه ما چه ویژگی دارد؟",
      options = c("نمونه اریب است", "نمونه معرف است", "نمونه تصادفی است", "نمونه ایده آل است"),
      correct = "نمونه اریب است",
      explanation = "سبک زندگی ورزشکاران ممکن است با بقیه نوجوانان متفاوت باشد و نمونه جهت‌دار است."
    ),
    list(
      kind = "school", level = "easy",
      question = "اگر از هر کلاس درس در مدرسه به قید قرعه ۲ نفر را انتخاب کنیم تا نظرشان را بپرسیم، این نمونه چیست؟",
      options = c("نمونه معرف", "نمونه اریب", "نمونه نامعتبر", "هیچ‌کدام"),
      correct = "نمونه معرف",
      explanation = "چون از تمام کلاس‌ها نماینده وجود دارد، این نمونه به خوبی جامعه مدرسه را بازنمایی می‌کند."
    ),
    list(
      kind = "general", level = "easy",
      question = "اگر یک سایت خبری نظرسنجی اختیاری بگذارد و فقط کاربران خودش در آن شرکت کنند، این نمونه چیست؟",
      options = c("نمونه اریب", "نمونه معرف", "نمونه تصادفی", "نمونه کامل"),
      correct = "نمونه اریب",
      explanation = "کاربران یک سایت خاص نماینده کل جامعه نیستند و مشارکت اختیاری باعث اریب شدن نتایج می‌شود."
    ),
    
    # Hard Questions
    list(
      kind = "general", level = "hard",
      question = "یک پژوهشگر می‌خواهد نظر مردم درباره طرح جدید شهرداری را بداند. او فقط از افرادی که در ساعت ۱۰ صبح وسط پارک نشسته‌اند پرسش می‌کند. نمونه چگونه است؟",
      options = c("نمونه اریب (سوگیرانه)", "نمونه تصادفی ساده", "نمونه معرف کامل", "نمونه خوشه‌ای استاندارد"),
      correct = "نمونه اریب (سوگیرانه)",
      explanation = "افرادی که ساعت ۱۰ صبح در پارک هستند معمولاً شاغل نیستند یا وقت آزاد دارند و بازتاب‌دهنده نظر کل جامعه نیستند."
    ),
    list(
      kind = "school", level = "hard",
      question = "اگر لیست کل دانش‌آموزان را بر اساس حروف الفبا مرتب کنیم و از هر ۱۰ نفر، نفر دهم را انتخاب کنیم، این چه نوع نمونه‌گیری است؟",
      options = c("نمونه تصادفی (سیستماتیک)", "نمونه اریب", "نمونه غیرمعرف", "نمونه داوطلبانه"),
      correct = "نمونه تصادفی (سیستماتیک)",
      explanation = "اگرچه سیستماتیک است، اما به دلیل شروع تصادفی، نوعی نمونه‌گیری تصادفی و منصفانه به حساب می‌آید."
    ),
    list(
      kind = "sports", level = "hard",
      question = "یک فروشگاه لوازم ورزشی می‌خواهد کیفیت خدمات خود را بسنجد. آن‌ها به صورت تصادفی به خریداران ۱۰ روز گذشته ایمیل می‌زنند. این نمونه چیست؟",
      options = c("نمونه معرف مشتریان", "نمونه اریب کل جامعه شهر", "نمونه تصادفی کل جامعه شهر", "گزینه‌های ۱ و ۲ هر دو صحیح هستند"),
      correct = "گزینه‌های ۱ و ۲ هر دو صحیح هستند",
      explanation = "این نمونه معرف «مشتریان فروشگاه» است، اما نسبت به «کل جامعه شهر» اریب است چون فقط خریداران آنجا را شامل می‌شود."
    ),
    list(
      kind = "general", level = "hard",
      question = "چرا نمونه‌گیری تصادفی ساده (قرعه‌کشی) در آمارهای بزرگ بسیار ارزشمند است؟",
      options = c("چون شانس اریب بودن و سوگیری شخصی را به حداقل می‌رساند", "چون ارزان‌ترین و سریع‌ترین روش است", "چون همیشه پاسخ‌های دقیق‌تری به دست می‌دهد", "چون تعداد نمونه را خود به خود بزرگتر می‌کند"),
      correct = "چون شانس اریب بودن و سوگیری شخصی را به حداقل می‌رساند",
      explanation = "حذف دخالت انسانی در انتخاب، جلوی جهت‌گیری آگاهانه یا ناآگاهانه را می‌گیرد."
    ),
    list(
      kind = "school", level = "hard",
      question = "مدیر مدرسه برای نظرسنجی درباره بوفه، فقط با شورای دانش‌آموزی صحبت می‌کند. این نمونه چه مشکلی دارد؟",
      options = c("نمونه اریب است و نظر دانش‌آموزان عادی را نادیده می‌گیرد", "نمونه معرف کل مدرسه است", "نمونه کاملاً تصادفی است", "مشکلی ندارد و بهترین روش است"),
      correct = "نمونه اریب است و نظر دانش‌آموزان عادی را نادیده می‌گیرد",
      explanation = "اعضای شورا ممکن است سلیقه یا دسترسی‌های متفاوتی با بقیه دانش‌آموزان داشته باشند."
    ),
    list(
      kind = "sports", level = "hard",
      question = "یک مجله خودرو نظرسنجی درباره خرید ماشین برقی منتشر کرده است. چه عاملی باعث می‌شود این نمونه اریب باشد؟",
      options = c("فقط خوانندگان این مجله خاص شانس شرکت دارند", "نمونه‌گیری به روش قرعه‌کشی کشوری نبوده است", "هر دو مورد فوق", "هیچ‌کدام"),
      correct = "هر دو مورد فوق",
      explanation = "محدود شدن به خوانندگان مجله خودرو و عدم استفاده از شیوه انتخاب تصادفی کشوری، نمونه را اریب می‌کند."
    ),
    list(
      kind = "general", level = "hard",
      question = "برای بررسی میانگین درآمد افراد یک شهر، اگر نمونه شامل ۷۰٪ پزشکان و ۳۰٪ کارگران باشد، این نمونه...",
      options = c("اریب است چون معرف ترکیب واقعی جامعه نیست", "معرف است چون از هر دو صنف دارد", "تصادفی است چون هر دو گروه را شامل می‌شود", "هیچ‌کدام"),
      correct = "اریب است چون معرف ترکیب واقعی جامعه نیست",
      explanation = "پزشکان بخش بسیار کوچکی از جامعه واقعی را تشکیل می‌دهند و این توزیع درآمد واقعی جامعه را نشان نمی‌دهد."
    ),
    list(
      kind = "general", level = "hard",
      question = "تفاوت اصلی بین نمونه تصادفی و نمونه معرف چیست؟",
      options = c("تصادفی مربوط به روش انتخاب است و معرف مربوط به شبیه بودن نمونه به جامعه است", "هیچ تفاوتی ندارند و هر دو یک مفهوم هستند", "نمونه تصادفی همیشه اریب است ولی نمونه معرف نه", "نمونه معرف همیشه با تعداد بیشتری انتخاب می‌شود"),
      correct = "تصادفی مربوط به روش انتخاب است و معرف مربوط به شبیه بودن نمونه به جامعه است",
      explanation = "تصادفی بودن روشی است که شانس برابر می‌دهد، در حالی که معرف بودن نشان‌دهنده شباهت ویژگی‌های نمونه با جامعه است."
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
        grid-template-columns: 1fr 1fr;
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
        grid-template-columns: 1fr;
        gap: 12px;
      }
      .opt-btn {
        background: #1e88e5;
        color: white;
        border: none;
        padding: 14px;
        border-radius: 8px;
        font-size: 16px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.2s, transform 0.1s;
        text-align: right;
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
      div(class = "top-title", "نمونه‌های معرف، تصادفی و اریب"),
      div(
        class = "top-controls",
        actionButton("easy_btn", "سطح آسان", class = "level-btn active"),
        actionButton("hard_btn", "سطح سخت", class = "level-btn"),
        actionButton("reset_btn", "شروع مجدد", class = "reset-top-btn")
      )
    ),

    # نوار وضعیت
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
    question_index = 1,
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

  init_game <- function(level = "easy") {
    rv$level <- level
    rv$score <- 0
    rv$total <- 0
    rv$correct <- 0
    rv$question_index <- 1
    rv$answered <- FALSE
    rv$game_over <- FALSE
    
    all_questions <- get_full_bank()
    filtered_questions <- Filter(function(q) q$level == level, all_questions)
    
    rv$current_pool <- sample(filtered_questions, min(10, length(filtered_questions)))
    rv$question <- rv$current_pool[[1]]
    clear_selected_option()
  }

  # شروع خودکار در لود اولیه
  observe({
    if (is.null(rv$question)) {
      init_game("easy")
    }
  })

  # تغییر سطح به آسان
  observeEvent(input$easy_btn, {
    init_game("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # تغییر سطح به سخت
  observeEvent(input$hard_btn, {
    if (hard_unlocked()) {
      init_game("hard")
      runjs("$('.level-btn').removeClass('active'); $('#hard_btn').addClass('active');")
    } else {
      showNotification("برای باز شدن سطح سخت باید حداقل 80٪ پاسخ درست داشته باشی.", type = "warning")
    }
  })

  # شروع مجدد بازی
  observeEvent(list(input$reset_btn, input$btn_play_again), {
    init_game("easy")
    runjs("$('.level-btn').removeClass('active'); $('#easy_btn').addClass('active');")
  })

  # سوال بعدی
  observeEvent(input$next_btn, {
    if (!rv$answered || rv$game_over) return()

    if (rv$question_index >= 10) {
      rv$game_over <- TRUE
    } else {
      rv$question_index <- rv$question_index + 1
      rv$answered <- FALSE
      
      # شرط ارتقای پله‌ای به سطح سخت پس از سوال ۵
      if (rv$question_index == 6 && rv$level == "easy" && (rv$correct / rv$total) >= 0.8) {
        rv$level <- "hard"
        all_questions <- get_full_bank()
        hard_questions <- Filter(function(q) q$level == "hard", all_questions)
        rv$current_pool[6:10] <- sample(hard_questions, 5)
        runjs("$('.level-btn').removeClass('active'); $('#hard_btn').addClass('active');")
      }
      
      rv$question <- rv$current_pool[[rv$question_index]]
      clear_selected_option()
    }
  })

  # ثبت پاسخ
  observeEvent(input$check_btn, {
    if (is.null(rv$selected) || identical(rv$selected, "")) {
      showNotification("یک گزینه را انتخاب کن.", type = "message")
      return()
    }

    if (rv$answered || rv$game_over) return()

    rv$answered <- TRUE
    rv$total <- rv$total + 1

    if (identical(rv$selected, rv$question$correct)) {
      rv$correct <- rv$correct + 1
      rv$score <- rv$score + 10
      showNotification("آفرین! پاسخ درست بود.", type = "message")
    } else {
      rv$score <- max(0, rv$score - 2)
      showNotification("اشتباه بود. توضیح را ببین.", type = "error")
    }
  })

  # دریافت انتخاب کاربر
  observeEvent(input$answer, {
    if (!rv$answered) {
      rv$selected <- input$answer
    }
  })

  # خروجی‌های وضعیت
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
    if (hard_unlocked()) "باز" else "قفل"
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
    div(class = "progress",
      div(class = "progress-bar", style = paste0("width:", pct, "%;"))
    )
  })

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
      # ستون چپ: نمایش بصری تصویر مربوطه
      div(
        class = "visual-panel",
        uiOutput("visual_ui"),
        div(class = "visual-label", textOutput("visual_label"))
      )
    )
  })

  output$question_txt <- renderText({
    req(rv$question)
    rv$question$question
  })

  output$visual_label <- renderText({
    req(rv$question)
    switch(rv$question$kind,
      school = "پژوهش در مدرسه",
      sports = "نظرسنجی ورزشی / تفریحی",
      general = "روش انتخاب از جامعه"
    )
  })

  output$visual_ui <- renderUI({
    req(rv$question)
    question_image_ui(rv$question$kind)
  })

  output$options_ui <- renderUI({
    req(rv$question)
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

    is_correct <- identical(rv$selected, rv$question$correct)
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
