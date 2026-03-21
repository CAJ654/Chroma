library(shiny)
library(curl)
library(jsonlite)

fetch_scheme <- function(hex, mode, count) {
  hex_clean <- gsub("^#", "", hex)
  url <- sprintf("https://www.thecolorapi.com/scheme?hex=%s&mode=%s&count=%d&format=json",
                 hex_clean, mode, count)
  h <- new_handle()
  handle_setopt(h, useragent = "R-Shiny/4.3", timeout = 10L)
  tryCatch({
    resp <- curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) return(NULL)
    fromJSON(rawToChar(resp$content))
  }, error = function(e) NULL)
}

fetch_color_info <- function(hex) {
  hex_clean <- gsub("^#", "", hex)
  url <- sprintf("https://www.thecolorapi.com/id?hex=%s&format=json", hex_clean)
  h <- new_handle()
  handle_setopt(h, useragent = "R-Shiny/4.3", timeout = 10L)
  tryCatch({
    resp <- curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) return(NULL)
    fromJSON(rawToChar(resp$content))
  }, error = function(e) NULL)
}

fetch_ai_palette <- function(model = "default", locked = list()) {
  input_list <- lapply(1:5, function(i) {
    if (i <= length(locked) && !is.null(locked[[i]])) {
      hex <- gsub("^#", "", locked[[i]])
      r <- strtoi(substr(hex, 1, 2), 16L)
      g <- strtoi(substr(hex, 3, 4), 16L)
      b <- strtoi(substr(hex, 5, 6), 16L)
      list(r, g, b)
    } else {
      "N"
    }
  })
  body <- toJSON(list(model = model, input = input_list), auto_unbox = TRUE)
  h <- new_handle()
  handle_setopt(h, post = TRUE, postfields = body, useragent = "R-Shiny/4.3", timeout = 10L)
  handle_setheaders(h, "Content-Type" = "application/json")
  tryCatch({
    resp <- curl_fetch_memory("http://colormind.io/api/", handle = h)
    if (resp$status_code != 200) return(NULL)
    parsed <- fromJSON(rawToChar(resp$content))
    parsed$result
  }, error = function(e) NULL)
}

rgb_to_hex <- function(r, g, b) {
  sprintf("#%02X%02X%02X", as.integer(r), as.integer(g), as.integer(b))
}

text_on_color <- function(hex) {
  hex_clean <- gsub("^#", "", hex)
  r <- strtoi(substr(hex_clean, 1, 2), 16L) / 255
  g <- strtoi(substr(hex_clean, 3, 4), 16L) / 255
  b <- strtoi(substr(hex_clean, 5, 6), 16L) / 255
  lum <- 0.299 * r + 0.587 * g + 0.114 * b
  if (lum > 0.55) "#1a1a2e" else "#ffffff"
}

color_card_ui <- function(hex, name = NULL, rgb_vals = NULL, index = NULL) {
  fg <- text_on_color(hex)
  name_label <- if (!is.null(name) && nchar(name) > 0) name else hex

  tags$div(
    class = "color-card",
    `data-hex` = hex,
    onclick = sprintf("copyToClipboard('%s', this)", hex),
    title = "Click to copy hex",
    tags$div(class = "color-swatch", style = paste0("background:", hex, ";")),
    tags$div(class = "color-info",
      tags$div(class = "color-name", name_label),
      tags$div(class = "color-hex", hex),
      if (!is.null(rgb_vals))
        tags$div(class = "color-rgb",
          sprintf("rgb(%d, %d, %d)", rgb_vals[1], rgb_vals[2], rgb_vals[3]))
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

      * { box-sizing: border-box; }

      body {
        font-family: 'Inter', sans-serif;
        background: #0f0f13;
        color: #e8e8f0;
        margin: 0;
        padding: 0;
        min-height: 100vh;
      }

      .app-header {
        background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
        border-bottom: 1px solid #2a2a3e;
        padding: 22px 40px;
      }

      .app-header h1 {
        margin: 0;
        font-size: 26px;
        font-weight: 700;
        background: linear-gradient(135deg, #a78bfa, #60a5fa);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }

      .app-header p {
        margin: 5px 0 0;
        color: #8888aa;
        font-size: 13px;
      }

      .tab-bar {
        display: flex;
        gap: 4px;
        padding: 0 40px;
        background: #13131f;
        border-bottom: 1px solid #2a2a3e;
      }

      .tab-btn {
        padding: 14px 20px;
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 500;
        color: #8888aa;
        background: none;
        border: none;
        border-bottom: 2px solid transparent;
        cursor: pointer;
        transition: all 0.2s;
        margin-bottom: -1px;
      }

      .tab-btn:hover { color: #e8e8f0; }

      .tab-btn.active {
        color: #a78bfa;
        border-bottom-color: #a78bfa;
      }

      .main-layout {
        display: flex;
        min-height: calc(100vh - 130px);
      }

      .sidebar-panel {
        width: 290px;
        min-width: 290px;
        background: #13131f;
        border-right: 1px solid #2a2a3e;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 18px;
      }

      .content-panel {
        flex: 1;
        padding: 28px 36px;
        overflow-y: auto;
      }

      .control-group { display: flex; flex-direction: column; gap: 7px; }

      .control-label {
        font-size: 11px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #8888aa;
      }

      .color-input-row { display: flex; gap: 8px; align-items: center; }

      input[type='color'] {
        width: 48px;
        height: 42px;
        border: 2px solid #2a2a3e;
        border-radius: 10px;
        background: #1e1e30;
        cursor: pointer;
        padding: 3px;
        flex-shrink: 0;
      }

      input[type='color']:hover { border-color: #a78bfa; }

      .hex-input, .form-select, .form-input {
        background: #1e1e30;
        border: 2px solid #2a2a3e;
        border-radius: 10px;
        color: #e8e8f0;
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        padding: 9px 12px;
        width: 100%;
        outline: none;
        transition: border-color 0.2s;
      }

      .hex-input:focus, .form-select:focus, .form-input:focus {
        border-color: #a78bfa;
      }

      .form-select {
        appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='10' height='10' viewBox='0 0 10 10'%3E%3Cpath fill='%238888aa' d='M5 7L0 2h10z'/%3E%3C/svg%3E\");
        background-repeat: no-repeat;
        background-position: right 12px center;
        padding-right: 32px;
        cursor: pointer;
      }

      .color-preview {
        height: 48px;
        border-radius: 10px;
        border: 2px solid #2a2a3e;
        transition: background 0.2s;
      }

      .generate-btn {
        background: linear-gradient(135deg, #7c3aed, #4f46e5);
        color: white;
        border: none;
        border-radius: 11px;
        padding: 12px 16px;
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 600;
        cursor: pointer;
        width: 100%;
        transition: all 0.2s;
        margin-top: 4px;
      }

      .generate-btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 6px 18px rgba(124, 58, 237, 0.4);
      }

      .generate-btn:disabled { opacity: 0.6; cursor: not-allowed; transform: none; }

      .count-row { display: flex; align-items: center; gap: 8px; }

      .count-btn {
        width: 34px;
        height: 34px;
        border-radius: 8px;
        border: 2px solid #2a2a3e;
        background: #1e1e30;
        color: #e8e8f0;
        font-size: 18px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
        transition: all 0.15s;
        padding: 0;
        line-height: 1;
      }

      .count-btn:hover { border-color: #a78bfa; background: #2a2a3e; }

      .count-display {
        flex: 1;
        text-align: center;
        font-size: 20px;
        font-weight: 700;
        color: #a78bfa;
      }

      .section-title {
        font-size: 16px;
        font-weight: 600;
        color: #e8e8f0;
        margin: 0 0 18px;
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .badge {
        background: #2a2a3e;
        color: #8888aa;
        font-size: 11px;
        font-weight: 500;
        padding: 3px 8px;
        border-radius: 20px;
      }

      .palette-grid {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
      }

      .color-card {
        border-radius: 14px;
        overflow: hidden;
        background: #1a1a2e;
        border: 1px solid #2a2a3e;
        transition: transform 0.2s, box-shadow 0.2s, border-color 0.2s;
        cursor: pointer;
        flex: 1;
        min-width: 110px;
        max-width: 170px;
        user-select: none;
      }

      .color-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 10px 28px rgba(0,0,0,0.4);
        border-color: #3a3a5e;
      }

      .color-card.copied {
        border-color: #a78bfa;
        box-shadow: 0 0 0 2px rgba(167,139,250,0.3);
      }

      .color-swatch { height: 150px; width: 100%; }
      .color-info { padding: 12px; }

      .color-name {
        font-size: 12px;
        font-weight: 600;
        color: #e8e8f0;
        margin-bottom: 5px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .color-hex {
        font-size: 12px;
        font-family: monospace;
        color: #a78bfa;
        font-weight: 500;
        margin-bottom: 3px;
      }

      .color-rgb {
        font-size: 11px;
        color: #6666aa;
        font-family: monospace;
      }

      .hex-strip {
        display: flex;
        gap: 6px;
        flex-wrap: wrap;
        margin-top: 20px;
        padding-top: 20px;
        border-top: 1px solid #2a2a3e;
      }

      .hex-chip {
        background: #1e1e30;
        border: 1px solid #2a2a3e;
        border-radius: 7px;
        padding: 6px 12px;
        font-size: 12px;
        font-family: monospace;
        color: #a78bfa;
        cursor: pointer;
        transition: all 0.15s;
        font-weight: 500;
      }

      .hex-chip:hover { background: #2a2a3e; border-color: #a78bfa; }

      .empty-state {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 280px;
        text-align: center;
        gap: 10px;
      }

      .empty-icon { font-size: 44px; opacity: 0.4; }
      .empty-text { font-size: 15px; font-weight: 500; color: #6666aa; }
      .empty-sub { font-size: 12px; color: #4444aa; }

      .error-box {
        color: #f87171;
        font-size: 13px;
        padding: 12px 16px;
        background: rgba(248, 113, 113, 0.08);
        border: 1px solid rgba(248, 113, 113, 0.2);
        border-radius: 10px;
      }

      .loading-grid { display: flex; gap: 10px; flex-wrap: wrap; }

      .skeleton-card {
        flex: 1;
        min-width: 110px;
        max-width: 170px;
        border-radius: 14px;
        background: #1a1a2e;
        border: 1px solid #2a2a3e;
        overflow: hidden;
      }

      .skeleton-swatch {
        height: 150px;
        background: linear-gradient(90deg, #1e1e30 25%, #2a2a3e 50%, #1e1e30 75%);
        background-size: 200% 100%;
        animation: skeleton-wave 1.5s ease-in-out infinite;
      }

      .skeleton-info { padding: 12px; }

      .skeleton-line {
        height: 10px;
        border-radius: 5px;
        background: linear-gradient(90deg, #1e1e30 25%, #2a2a3e 50%, #1e1e30 75%);
        background-size: 200% 100%;
        animation: skeleton-wave 1.5s ease-in-out infinite;
        margin-bottom: 7px;
      }

      @keyframes skeleton-wave {
        0% { background-position: 200% 0; }
        100% { background-position: -200% 0; }
      }

      .divider {
        height: 1px;
        background: #2a2a3e;
        margin: 4px 0;
      }

      .lock-row {
        display: flex;
        gap: 6px;
        align-items: center;
      }

      .lock-swatch {
        width: 32px;
        height: 32px;
        border-radius: 8px;
        border: 2px solid #2a2a3e;
        flex-shrink: 0;
        cursor: pointer;
        transition: border-color 0.2s;
      }

      .lock-swatch:hover { border-color: #a78bfa; }

      .lock-hex {
        flex: 1;
        background: #1e1e30;
        border: 2px solid #2a2a3e;
        border-radius: 8px;
        color: #a78bfa;
        font-family: monospace;
        font-size: 12px;
        padding: 6px 10px;
        outline: none;
        font-weight: 500;
        transition: border-color 0.2s;
      }

      .lock-hex:focus { border-color: #a78bfa; }

      .lock-toggle {
        width: 28px;
        height: 28px;
        border-radius: 6px;
        border: 2px solid #2a2a3e;
        background: #1e1e30;
        color: #8888aa;
        font-size: 13px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.15s;
        flex-shrink: 0;
        padding: 0;
      }

      .lock-toggle.locked {
        border-color: #a78bfa;
        color: #a78bfa;
        background: rgba(167,139,250,0.1);
      }

      .info-pill {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        background: #1e1e30;
        border: 1px solid #2a2a3e;
        border-radius: 20px;
        padding: 4px 10px;
        font-size: 11px;
        color: #8888aa;
      }

      .info-pill span { color: #a78bfa; font-weight: 600; }

      .toast {
        position: fixed;
        bottom: 24px;
        left: 50%;
        transform: translateX(-50%);
        background: #a78bfa;
        color: #fff;
        padding: 10px 20px;
        border-radius: 8px;
        font-size: 13px;
        font-weight: 600;
        z-index: 9999;
        box-shadow: 0 4px 15px rgba(167,139,250,0.4);
        pointer-events: none;
      }
    "))
  ),

  div(class = "app-header",
    h1("Color Palette Generator"),
    p("Powered by thecolorapi.com \u00b7 colormind.io \u00b7 R + Shiny")
  ),

  div(class = "tab-bar",
    tags$button(id = "tab-scheme", class = "tab-btn active",
      onclick = "switchTab('scheme')", "\U0001F3A8 Color Scheme"),
    tags$button(id = "tab-ai", class = "tab-btn",
      onclick = "switchTab('ai')", "\u2728 AI Palette")
  ),

  div(class = "main-layout",
    div(class = "sidebar-panel",

      # --- Scheme Tab Controls ---
      div(id = "scheme-controls",
        div(class = "control-group",
          div(class = "control-label", "Base Color"),
          div(class = "color-input-row",
            tags$input(type = "color", id = "colorPicker", value = "#7c3aed"),
            tags$input(type = "text", id = "hexInput", class = "hex-input",
              value = "#7C3AED", placeholder = "#7C3AED", maxlength = "7")
          ),
          div(id = "basePreview", class = "color-preview",
            style = "background:#7c3aed;")
        ),
        div(class = "control-group",
          div(class = "control-label", "Scheme Type"),
          tags$select(id = "schemeMode", class = "form-select",
            tags$option(value = "analogic", "Analogic"),
            tags$option(value = "complement", "Complement"),
            tags$option(value = "triad", "Triad"),
            tags$option(value = "quad", "Quad"),
            tags$option(value = "analogic-complement", "Analogic + Complement"),
            tags$option(value = "monochrome", "Monochrome"),
            tags$option(value = "monochrome-dark", "Monochrome Dark"),
            tags$option(value = "monochrome-light", "Monochrome Light")
          )
        ),
        div(class = "control-group",
          div(class = "control-label", "Number of Colors"),
          div(class = "count-row",
            tags$button(class = "count-btn", onclick = "adjustCount(-1)", "\u2212"),
            div(id = "countDisplay", class = "count-display", "5"),
            tags$button(class = "count-btn", onclick = "adjustCount(1)", "+")
          )
        ),
        actionButton("generateScheme", "Generate Scheme",
          class = "generate-btn",
          style = "width:100%;margin-top:4px;")
      ),

      # --- AI Tab Controls ---
      div(id = "ai-controls", style = "display:none;",
        div(class = "control-group",
          div(class = "control-label", "Palette Model"),
          tags$select(id = "aiModel", class = "form-select",
            tags$option(value = "default", "Default"),
            tags$option(value = "ui", "UI Design"),
            tags$option(value = "material", "Material"),
            tags$option(value = "japanese-prints", "Japanese Prints"),
            tags$option(value = "flower-photography", "Flower Photography"),
            tags$option(value = "interior-design", "Interior Design"),
            tags$option(value = "french-Art-Deco", "Art Deco")
          )
        ),
        div(class = "control-group",
          div(class = "control-label", "Lock Colors (optional)"),
          div(class = "control-label",
            style = "text-transform:none;letter-spacing:0;font-size:11px;color:#555577;margin-top:-4px;",
            "Enter hex values to anchor, leave blank to generate"),
          lapply(1:5, function(i) {
            div(class = "lock-row", style = "margin-bottom:6px;",
              div(id = paste0("lockSwatch", i), class = "lock-swatch",
                style = "background:#1e1e30;"),
              tags$input(type = "text", id = paste0("lockHex", i),
                class = "lock-hex", placeholder = paste0("Color ", i),
                maxlength = "7",
                oninput = paste0("updateLockSwatch(", i, ")")),
              tags$button(class = "lock-toggle", id = paste0("lockBtn", i),
                title = "Toggle lock", "\U0001F512",
                onclick = paste0("toggleLock(", i, ")"))
            )
          })
        ),
        actionButton("generateAI", "Generate AI Palette",
          class = "generate-btn",
          style = "width:100%;margin-top:4px;")
      )
    ),

    div(class = "content-panel",
      div(class = "section-title",
        uiOutput("paletteTitleUI")
      ),
      uiOutput("paletteUI"),
      uiOutput("hexStripUI")
    )
  ),

  tags$script(HTML("
    var currentCount = 5;
    var lockedSlots = [false, false, false, false, false];
    var activeTab = 'scheme';

    function switchTab(tab) {
      activeTab = tab;
      document.getElementById('scheme-controls').style.display = tab === 'scheme' ? '' : 'none';
      document.getElementById('ai-controls').style.display = tab === 'ai' ? '' : 'none';
      document.querySelectorAll('.tab-btn').forEach(function(b) { b.classList.remove('active'); });
      document.getElementById('tab-' + tab).classList.add('active');
    }

    function adjustCount(delta) {
      currentCount = Math.max(2, Math.min(10, currentCount + delta));
      document.getElementById('countDisplay').textContent = currentCount;
      Shiny.setInputValue('colorCount', currentCount);
    }

    document.getElementById('colorPicker').addEventListener('input', function(e) {
      var hex = e.target.value.toUpperCase();
      document.getElementById('hexInput').value = hex;
      document.getElementById('basePreview').style.background = hex;
      Shiny.setInputValue('baseHex', hex);
    });

    document.getElementById('hexInput').addEventListener('input', function(e) {
      var val = e.target.value.trim();
      var hex = val.startsWith('#') ? val : '#' + val;
      if (/^#[0-9A-Fa-f]{6}$/.test(hex)) {
        document.getElementById('colorPicker').value = hex;
        document.getElementById('basePreview').style.background = hex;
        Shiny.setInputValue('baseHex', hex.toUpperCase());
      }
    });

    function updateLockSwatch(i) {
      var val = document.getElementById('lockHex' + i).value.trim();
      var hex = val.startsWith('#') ? val : '#' + val;
      var sw = document.getElementById('lockSwatch' + i);
      if (/^#[0-9A-Fa-f]{6}$/.test(hex)) {
        sw.style.background = hex;
        sw.style.borderColor = hex;
      } else {
        sw.style.background = '#1e1e30';
        sw.style.borderColor = '#2a2a3e';
      }
    }

    function toggleLock(i) {
      lockedSlots[i-1] = !lockedSlots[i-1];
      var btn = document.getElementById('lockBtn' + i);
      btn.classList.toggle('locked', lockedSlots[i-1]);
      btn.title = lockedSlots[i-1] ? 'Unlock' : 'Lock';
    }

    function getLocked() {
      var locked = [];
      for (var i = 1; i <= 5; i++) {
        if (lockedSlots[i-1]) {
          var val = document.getElementById('lockHex' + i).value.trim();
          var hex = val.startsWith('#') ? val : '#' + val;
          locked.push(/^#[0-9A-Fa-f]{6}$/.test(hex) ? hex : null);
        } else {
          locked.push(null);
        }
      }
      return locked;
    }

    Shiny.addCustomMessageHandler('setLoading', function(n) {
      var html = '<div class=\"loading-grid\">';
      for (var i = 0; i < n; i++) {
        html += '<div class=\"skeleton-card\">' +
          '<div class=\"skeleton-swatch\"></div>' +
          '<div class=\"skeleton-info\">' +
          '<div class=\"skeleton-line\" style=\"width:80%\"></div>' +
          '<div class=\"skeleton-line\" style=\"width:55%\"></div>' +
          '</div></div>';
      }
      html += '</div>';
      document.getElementById('paletteUI').innerHTML = html;
      document.getElementById('hexStripUI').innerHTML = '';
    });

    Shiny.addCustomMessageHandler('submitAILocked', function(msg) {
      Shiny.setInputValue('lockedColors', getLocked(), {priority: 'event'});
      Shiny.setInputValue('aiModelSel', document.getElementById('aiModel').value, {priority: 'event'});
    });

    function copyToClipboard(hex, el) {
      if (navigator.clipboard) {
        navigator.clipboard.writeText(hex).then(function() {
          el.classList.add('copied');
          setTimeout(function() { el.classList.remove('copied'); }, 1200);
          showToast('Copied ' + hex);
        });
      }
    }

    function showToast(msg) {
      var old = document.querySelector('.toast');
      if (old) old.remove();
      var t = document.createElement('div');
      t.className = 'toast';
      t.textContent = msg;
      document.body.appendChild(t);
      setTimeout(function() {
        t.style.transition = 'opacity 0.3s';
        t.style.opacity = '0';
        setTimeout(function() { t.remove(); }, 300);
      }, 1800);
    }

    // Initialize
    Shiny.setInputValue('baseHex', '#7C3AED');
    Shiny.setInputValue('colorCount', 5);
  "))
)

server <- function(input, output, session) {

  palette_data <- reactiveVal(NULL)
  is_loading  <- reactiveVal(FALSE)
  err_msg     <- reactiveVal(NULL)
  active_mode <- reactiveVal("scheme")

  # --- Generate Color Scheme ---
  observeEvent(input$generateScheme, {
    hex <- input$baseHex
    if (is.null(hex) || !grepl("^#[0-9A-Fa-f]{6}$", hex)) hex <- "#7C3AED"
    mode  <- input$schemeMode
    count <- if (is.null(input$colorCount)) 5L else as.integer(input$colorCount)

    active_mode("scheme")
    is_loading(TRUE)
    err_msg(NULL)
    palette_data(NULL)
    session$sendCustomMessage("setLoading", count)

    data <- fetch_scheme(hex, mode, count)

    is_loading(FALSE)

    if (is.null(data)) {
      err_msg("Could not reach the Color API. Please try again.")
      return()
    }

    colors <- data$colors
    result <- lapply(seq_len(nrow(colors)), function(i) {
      list(
        hex  = colors$hex$value[i],
        name = colors$name$value[i],
        r    = colors$rgb$r[i],
        g    = colors$rgb$g[i],
        b    = colors$rgb$b[i]
      )
    })
    palette_data(list(mode = "scheme", colors = result, scheme_mode = mode,
                      base_hex = hex))
  })

  # --- Generate AI Palette ---
  observeEvent(input$generateAI, {
    session$sendCustomMessage("submitAILocked", list())
  })

  observeEvent(input$lockedColors, {
    active_mode("ai")
    is_loading(TRUE)
    err_msg(NULL)
    palette_data(NULL)
    session$sendCustomMessage("setLoading", 5)

    model  <- if (!is.null(input$aiModelSel)) input$aiModelSel else "default"
    locked <- input$lockedColors

    result_rgb <- fetch_ai_palette(model = model, locked = locked)

    is_loading(FALSE)

    if (is.null(result_rgb)) {
      err_msg("Could not reach the Colormind AI API. Please try again.")
      return()
    }

    colors <- lapply(result_rgb, function(rgb) {
      list(hex = rgb_to_hex(rgb[1], rgb[2], rgb[3]),
           name = NULL, r = rgb[1], g = rgb[2], b = rgb[3])
    })

    palette_data(list(mode = "ai", colors = colors, ai_model = model))
  })

  # --- Palette Title ---
  output$paletteTitleUI <- renderUI({
    pd <- palette_data()
    if (is_loading()) {
      return(tagList(
        "Generating palette",
        tags$span(class = "badge", "loading...")
      ))
    }
    if (!is.null(err_msg())) {
      return(tagList("Palette", tags$span(class = "badge", "error")))
    }
    if (is.null(pd)) {
      return(tagList("Palette", tags$span(class = "badge", "ready")))
    }
    n <- length(pd$colors)
    label <- if (pd$mode == "scheme") pd$scheme_mode else paste("AI \u00b7", pd$ai_model)
    tagList(
      "Palette",
      tags$span(class = "badge", paste(n, "colors \u00b7", label))
    )
  })

  # --- Palette Cards ---
  output$paletteUI <- renderUI({
    if (is_loading()) return(NULL)

    if (!is.null(err_msg())) {
      return(div(class = "error-box", err_msg()))
    }

    pd <- palette_data()

    if (is.null(pd)) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001F3A8"),
        div(class = "empty-text", "Your palette will appear here"),
        div(class = "empty-sub", "Choose a mode and click Generate")
      ))
    }

    cards <- lapply(pd$colors, function(c) {
      color_card_ui(c$hex, c$name, c(c$r, c$g, c$b))
    })

    div(class = "palette-grid", cards)
  })

  # --- Hex Strip ---
  output$hexStripUI <- renderUI({
    pd <- palette_data()
    if (is.null(pd) || is_loading()) return(NULL)

    chips <- lapply(pd$colors, function(c) {
      tags$div(class = "hex-chip",
        onclick = sprintf("copyToClipboard('%s', this)", c$hex),
        c$hex)
    })

    div(class = "hex-strip", chips)
  })
}

shinyApp(ui = ui, server = server)
