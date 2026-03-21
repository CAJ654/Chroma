library(shiny)
library(httr)
library(jsonlite)

fetch_scheme <- function(hex, mode, count) {
  hex_clean <- gsub("^#", "", hex)
  url <- sprintf("https://www.thecolorapi.com/scheme?hex=%s&mode=%s&count=%d&format=json",
                 hex_clean, mode, count)
  tryCatch({
    resp <- httr::GET(url, httr::user_agent("R-Shiny/4.3"), httr::timeout(10))
    if (httr::status_code(resp) != 200) return(NULL)
    fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
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

color_card_ui <- function(hex, name = NULL, rgb_vals = NULL) {
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
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
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

      .main-layout {
        display: flex;
        min-height: calc(100vh - 85px);
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

      .hex-input, .form-select {
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

      .hex-input:focus, .form-select:focus { border-color: #a78bfa; }

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

      .color-rgb { font-size: 11px; color: #6666aa; font-family: monospace; }

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

      .controls-toggle {
        display: none;
        width: 100%;
        background: #1e1e30;
        border: 1px solid #2a2a3e;
        border-radius: 10px;
        color: #a78bfa;
        font-family: 'Inter', sans-serif;
        font-size: 13px;
        font-weight: 600;
        padding: 12px 16px;
        cursor: pointer;
        text-align: left;
        margin-bottom: 2px;
        transition: background 0.2s;
      }

      .controls-toggle:hover { background: #2a2a3e; }

      /* ── Mobile ── */
      @media (max-width: 700px) {
        .app-header { padding: 16px; }
        .app-header h1 { font-size: 20px; }
        .app-header p { font-size: 12px; }

        .main-layout { flex-direction: column; min-height: unset; }

        .sidebar-panel {
          width: 100%;
          min-width: unset;
          border-right: none;
          border-bottom: 1px solid #2a2a3e;
          padding: 14px 14px 16px;
          gap: 14px;
        }

        .controls-toggle { display: flex; align-items: center; justify-content: space-between; }

        .controls-body { overflow: hidden; transition: max-height 0.3s ease; }
        .controls-body.collapsed { max-height: 0 !important; }

        .content-panel { padding: 18px 14px; }

        .palette-grid, .loading-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 10px;
        }

        .color-card, .skeleton-card {
          min-width: unset;
          max-width: unset;
          flex: unset;
        }

        .color-swatch, .skeleton-swatch { height: 110px; }

        .count-btn { width: 42px; height: 42px; font-size: 20px; }
        .generate-btn { padding: 14px 16px; font-size: 14px; }
        input[type='color'] { width: 52px; height: 46px; }
        .hex-input, .form-select { font-size: 14px; padding: 11px 12px; }

        .hex-strip { gap: 8px; margin-top: 16px; padding-top: 16px; }
        .hex-chip { padding: 8px 14px; font-size: 12px; }

        .section-title { font-size: 15px; margin-bottom: 14px; }
      }
    ")))
  ),

  div(class = "app-header",
    h1("Chroma"),
    p("Powered by thecolorapi.com \u00b7 R + Shiny")
  ),

  div(class = "main-layout",
    div(class = "sidebar-panel",

      tags$button(
        class = "controls-toggle",
        id = "controlsToggle",
        onclick = "toggleControls()",
        tags$span("\u2699\ufe0f Controls"),
        tags$span(id = "controlsArrow", "\u25be")
      ),

      div(id = "controlsBody", class = "controls-body",

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

        actionButton("generateScheme", "Generate Palette",
          class = "generate-btn",
          style = "width:100%;margin-top:4px;")

      ) # end controlsBody
    ),

    div(class = "content-panel",
      div(class = "section-title", uiOutput("paletteTitleUI")),
      uiOutput("paletteUI"),
      uiOutput("hexStripUI")
    )
  ),

  tags$script(HTML("
    var currentCount = 5;
    var controlsOpen = true;

    function setControlsHeight() {
      var body = document.getElementById('controlsBody');
      if (!body) return;
      if (controlsOpen) {
        body.style.maxHeight = body.scrollHeight + 'px';
        body.classList.remove('collapsed');
      } else {
        body.style.maxHeight = '0px';
        body.classList.add('collapsed');
      }
    }

    function toggleControls() {
      controlsOpen = !controlsOpen;
      setControlsHeight();
      var arrow = document.getElementById('controlsArrow');
      if (arrow) arrow.textContent = controlsOpen ? '\u25be' : '\u25b8';
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

    Shiny.setInputValue('baseHex', '#7C3AED');
    Shiny.setInputValue('colorCount', 5);
    setTimeout(function() {
      var body = document.getElementById('controlsBody');
      if (body) body.style.maxHeight = body.scrollHeight + 'px';
    }, 100);
  "))
)

server <- function(input, output, session) {

  palette_data <- reactiveVal(NULL)
  is_loading   <- reactiveVal(FALSE)
  err_msg      <- reactiveVal(NULL)

  observeEvent(input$generateScheme, {
    hex   <- input$baseHex
    if (is.null(hex) || !grepl("^#[0-9A-Fa-f]{6}$", hex)) hex <- "#7C3AED"
    mode  <- input$schemeMode
    count <- if (is.null(input$colorCount)) 5L else as.integer(input$colorCount)

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
    palette_data(list(colors = result, scheme_mode = mode, base_hex = hex))
  })

  output$paletteTitleUI <- renderUI({
    if (is_loading()) {
      return(tagList("Palette", tags$span(class = "badge", "loading...")))
    }
    if (!is.null(err_msg())) {
      return(tagList("Palette", tags$span(class = "badge", "error")))
    }
    pd <- palette_data()
    if (is.null(pd)) {
      return(tagList("Palette", tags$span(class = "badge", "ready")))
    }
    tagList(
      "Palette",
      tags$span(class = "badge",
        paste(length(pd$colors), "colors \u00b7", pd$scheme_mode))
    )
  })

  output$paletteUI <- renderUI({
    if (is_loading()) return(NULL)
    if (!is.null(err_msg())) return(div(class = "error-box", err_msg()))

    pd <- palette_data()
    if (is.null(pd)) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001F3A8"),
        div(class = "empty-text", "Your palette will appear here"),
        div(class = "empty-sub", "Pick a color and click Generate Palette")
      ))
    }

    div(class = "palette-grid",
      lapply(pd$colors, function(c) {
        color_card_ui(c$hex, c$name, c(c$r, c$g, c$b))
      })
    )
  })

  output$hexStripUI <- renderUI({
    pd <- palette_data()
    if (is.null(pd) || is_loading()) return(NULL)

    div(class = "hex-strip",
      lapply(pd$colors, function(c) {
        tags$div(class = "hex-chip",
          onclick = sprintf("copyToClipboard('%s', this)", c$hex),
          c$hex)
      })
    )
  })
}

shinyApp(ui = ui, server = server)

if (Sys.getenv("EXPORT_SHINYLIVE") == "true") {
  shinylive::export(".", "site")
}