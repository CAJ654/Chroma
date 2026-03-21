library(shiny)

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
        padding: 24px 40px;
      }

      .app-header h1 {
        margin: 0;
        font-size: 28px;
        font-weight: 700;
        background: linear-gradient(135deg, #a78bfa, #60a5fa);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        letter-spacing: -0.5px;
      }

      .app-header p {
        margin: 6px 0 0;
        color: #8888aa;
        font-size: 14px;
      }

      .main-layout {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 85px);
      }

      .sidebar-panel {
        width: 300px;
        min-width: 300px;
        background: #13131f;
        border-right: 1px solid #2a2a3e;
        padding: 28px 24px;
        display: flex;
        flex-direction: column;
        gap: 20px;
      }

      .content-panel {
        flex: 1;
        padding: 32px 40px;
        overflow-y: auto;
      }

      .control-group {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      .control-label {
        font-size: 12px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #8888aa;
      }

      .color-input-row {
        display: flex;
        gap: 10px;
        align-items: center;
      }

      input[type='color'] {
        width: 52px;
        height: 44px;
        border: 2px solid #2a2a3e;
        border-radius: 10px;
        background: #1e1e30;
        cursor: pointer;
        padding: 3px;
        flex-shrink: 0;
      }

      input[type='color']:hover { border-color: #a78bfa; }

      .hex-input {
        flex: 1;
        background: #1e1e30;
        border: 2px solid #2a2a3e;
        border-radius: 10px;
        color: #e8e8f0;
        font-family: 'Inter', monospace;
        font-size: 14px;
        font-weight: 500;
        padding: 10px 14px;
        transition: border-color 0.2s;
        outline: none;
        width: 100%;
      }

      .hex-input:focus { border-color: #a78bfa; }

      .form-select {
        background: #1e1e30;
        border: 2px solid #2a2a3e;
        border-radius: 10px;
        color: #e8e8f0;
        font-family: 'Inter', sans-serif;
        font-size: 14px;
        padding: 10px 14px;
        width: 100%;
        outline: none;
        cursor: pointer;
        transition: border-color 0.2s;
        appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='%238888aa' d='M6 8L1 3h10z'/%3E%3C/svg%3E\");
        background-repeat: no-repeat;
        background-position: right 14px center;
        padding-right: 36px;
      }

      .form-select:focus { border-color: #a78bfa; }

      .generate-btn {
        background: linear-gradient(135deg, #7c3aed, #4f46e5);
        color: white;
        border: none;
        border-radius: 12px;
        padding: 13px 20px;
        font-family: 'Inter', sans-serif;
        font-size: 14px;
        font-weight: 600;
        cursor: pointer;
        width: 100%;
        transition: all 0.2s;
        letter-spacing: 0.3px;
        margin-top: 8px;
      }

      .generate-btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 8px 20px rgba(124, 58, 237, 0.4);
      }

      .generate-btn:disabled {
        opacity: 0.6;
        cursor: not-allowed;
        transform: none;
      }

      .palette-grid {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        margin-top: 8px;
      }

      .color-card {
        border-radius: 16px;
        overflow: hidden;
        background: #1a1a2e;
        border: 1px solid #2a2a3e;
        transition: transform 0.2s, box-shadow 0.2s;
        cursor: pointer;
        flex: 1;
        min-width: 120px;
        max-width: 180px;
      }

      .color-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 12px 30px rgba(0,0,0,0.4);
        border-color: #3a3a5e;
      }

      .color-swatch { height: 160px; width: 100%; }

      .color-info { padding: 14px; }

      .color-name {
        font-size: 13px;
        font-weight: 600;
        color: #e8e8f0;
        margin-bottom: 6px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .color-hex {
        font-size: 12px;
        font-family: 'Inter', monospace;
        color: #a78bfa;
        font-weight: 500;
        margin-bottom: 4px;
      }

      .color-rgb {
        font-size: 11px;
        color: #6666aa;
        font-family: 'Inter', monospace;
      }

      .section-title {
        font-size: 18px;
        font-weight: 600;
        color: #e8e8f0;
        margin: 0 0 20px;
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .badge {
        background: #2a2a3e;
        color: #8888aa;
        font-size: 11px;
        font-weight: 500;
        padding: 3px 9px;
        border-radius: 20px;
      }

      .copy-bar {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        margin-top: 24px;
        padding-top: 24px;
        border-top: 1px solid #2a2a3e;
      }

      .copy-chip {
        background: #1e1e30;
        border: 1px solid #2a2a3e;
        border-radius: 8px;
        padding: 8px 14px;
        font-size: 12px;
        font-family: 'Inter', monospace;
        color: #a78bfa;
        cursor: pointer;
        transition: all 0.15s;
        font-weight: 500;
      }

      .copy-chip:hover { background: #2a2a3e; border-color: #a78bfa; }

      .empty-state {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 300px;
        text-align: center;
        gap: 12px;
      }

      .empty-icon { font-size: 48px; opacity: 0.5; }
      .empty-text { font-size: 16px; font-weight: 500; color: #6666aa; }
      .empty-sub { font-size: 13px; color: #4444aa; }

      .base-color-preview {
        border-radius: 10px;
        height: 52px;
        width: 100%;
        border: 2px solid #2a2a3e;
        margin-top: 4px;
        transition: background 0.2s;
      }

      .error-msg {
        color: #f87171;
        font-size: 13px;
        padding: 12px 16px;
        background: rgba(248, 113, 113, 0.1);
        border: 1px solid rgba(248, 113, 113, 0.2);
        border-radius: 10px;
      }

      .count-row {
        display: flex;
        align-items: center;
        gap: 10px;
      }

      .count-btn {
        width: 36px;
        height: 36px;
        border-radius: 8px;
        border: 2px solid #2a2a3e;
        background: #1e1e30;
        color: #e8e8f0;
        font-size: 20px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
        transition: all 0.15s;
        line-height: 1;
        padding: 0;
      }

      .count-btn:hover { border-color: #a78bfa; background: #2a2a3e; }

      @keyframes shimmer {
        0% { opacity: 0.4; }
        50% { opacity: 0.7; }
        100% { opacity: 0.4; }
      }

      .shimmer { animation: shimmer 1.5s ease-in-out infinite; }
    "))
  ),

  div(class = "app-header",
    h1("Color Palette Generator"),
    p("Create beautiful palettes from any color using the Color API")
  ),

  div(class = "main-layout",
    div(class = "sidebar-panel",
      div(class = "control-group",
        div(class = "control-label", "Base Color"),
        div(class = "color-input-row",
          tags$input(type = "color", id = "colorPicker", value = "#7c3aed"),
          tags$input(type = "text", id = "hexInput", class = "hex-input",
            value = "#7c3aed", placeholder = "#7c3aed", maxlength = "7")
        ),
        div(id = "basePreview", class = "base-color-preview",
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
          tags$button(id = "decCount", class = "count-btn", "\u2212"),
          div(id = "countDisplay",
            style = "flex:1;text-align:center;font-size:22px;font-weight:700;color:#a78bfa;",
            "5"),
          tags$button(id = "incCount", class = "count-btn", "+")
        )
      ),

      tags$button(id = "generateBtn", class = "generate-btn",
        "Generate Palette")
    ),

    div(class = "content-panel",
      div(class = "section-title",
        "Color Palette",
        tags$span(id = "paletteLabel", class = "badge", "ready")
      ),
      div(id = "paletteContainer",
        div(class = "empty-state",
          div(class = "empty-icon", "\U0001F3A8"),
          div(class = "empty-text", "Your palette will appear here"),
          div(class = "empty-sub", "Pick a color and click Generate Palette")
        )
      ),
      div(id = "hexListContainer", style = "display:none;",
        div(class = "copy-bar", id = "hexList")
      )
    )
  ),

  tags$script(HTML("
    var currentCount = 5;

    document.getElementById('decCount').onclick = function() {
      if (currentCount > 2) {
        currentCount--;
        document.getElementById('countDisplay').textContent = currentCount;
      }
    };

    document.getElementById('incCount').onclick = function() {
      if (currentCount < 10) {
        currentCount++;
        document.getElementById('countDisplay').textContent = currentCount;
      }
    };

    document.getElementById('colorPicker').addEventListener('input', function(e) {
      var hex = e.target.value;
      document.getElementById('hexInput').value = hex;
      document.getElementById('basePreview').style.background = hex;
    });

    document.getElementById('hexInput').addEventListener('input', function(e) {
      var hex = e.target.value.trim();
      if (!hex.startsWith('#')) hex = '#' + hex;
      if (/^#[0-9A-Fa-f]{6}$/.test(hex)) {
        document.getElementById('colorPicker').value = hex;
        document.getElementById('basePreview').style.background = hex;
      }
    });

    document.getElementById('generateBtn').onclick = function() { generatePalette(); };

    function generatePalette() {
      var hex = document.getElementById('hexInput').value.trim();
      if (!hex.startsWith('#')) hex = '#' + hex;
      if (!/^#[0-9A-Fa-f]{6}$/.test(hex)) {
        document.getElementById('paletteContainer').innerHTML =
          '<div class=\"error-msg\">Please enter a valid 6-digit hex color (e.g. #FF5733)</div>';
        return;
      }

      var mode = document.getElementById('schemeMode').value;
      var count = currentCount;
      var hexClean = hex.replace('#', '');
      var btn = document.getElementById('generateBtn');

      btn.textContent = 'Generating...';
      btn.disabled = true;
      document.getElementById('paletteLabel').textContent = 'loading...';

      var skeletons = '';
      for (var i = 0; i < count; i++) {
        skeletons += '<div style=\"flex:1;min-width:120px;max-width:180px;border-radius:16px;background:#1a1a2e;border:1px solid #2a2a3e;overflow:hidden;\">' +
          '<div class=\"shimmer\" style=\"height:160px;background:#2a2a3e;\"></div>' +
          '<div style=\"padding:14px;\">' +
          '<div class=\"shimmer\" style=\"height:12px;background:#2a2a3e;border-radius:4px;margin-bottom:8px;\"></div>' +
          '<div class=\"shimmer\" style=\"height:10px;background:#2a2a3e;border-radius:4px;width:70%;\"></div>' +
          '</div></div>';
      }
      document.getElementById('paletteContainer').innerHTML =
        '<div class=\"palette-grid\">' + skeletons + '</div>';

      fetch('https://www.thecolorapi.com/scheme?hex=' + hexClean + '&mode=' + mode + '&count=' + count)
        .then(function(r) { return r.json(); })
        .then(function(data) {
          btn.textContent = 'Generate Palette';
          btn.disabled = false;

          var colors = data.colors;
          document.getElementById('paletteLabel').textContent = colors.length + ' colors';

          var html = '<div class=\"palette-grid\">';
          var chips = '';

          colors.forEach(function(c) {
            var hexVal = c.hex.value;
            var name = c.name.value;
            var r = c.rgb.r, g = c.rgb.g, b = c.rgb.b;

            html += '<div class=\"color-card\" onclick=\"copyHex(\\'' + hexVal + '\\')\" title=\"Click to copy\">' +
              '<div class=\"color-swatch\" style=\"background:' + hexVal + ';\"></div>' +
              '<div class=\"color-info\">' +
              '<div class=\"color-name\">' + name + '</div>' +
              '<div class=\"color-hex\">' + hexVal + '</div>' +
              '<div class=\"color-rgb\">rgb(' + r + ', ' + g + ', ' + b + ')</div>' +
              '</div></div>';

            chips += '<div class=\"copy-chip\" onclick=\"copyHex(\\'' + hexVal + '\\')\">' + hexVal + '</div>';
          });

          html += '</div>';
          document.getElementById('paletteContainer').innerHTML = html;
          document.getElementById('hexList').innerHTML = chips;
          document.getElementById('hexListContainer').style.display = 'block';
        })
        .catch(function() {
          btn.textContent = 'Generate Palette';
          btn.disabled = false;
          document.getElementById('paletteLabel').textContent = 'error';
          document.getElementById('paletteContainer').innerHTML =
            '<div class=\"error-msg\">Could not reach the Color API. Please check your internet connection and try again.</div>';
          document.getElementById('hexListContainer').style.display = 'none';
        });
    }

    function copyHex(hex) {
      if (navigator.clipboard) {
        navigator.clipboard.writeText(hex).then(function() { showToast('Copied ' + hex); });
      }
    }

    function showToast(msg) {
      var t = document.createElement('div');
      t.textContent = msg;
      t.style.cssText = 'position:fixed;bottom:24px;left:50%;transform:translateX(-50%);background:#a78bfa;color:#fff;padding:10px 20px;border-radius:8px;font-size:13px;font-weight:600;z-index:9999;box-shadow:0 4px 15px rgba(167,139,250,0.4);';
      document.body.appendChild(t);
      setTimeout(function() {
        t.style.transition = 'opacity 0.3s';
        t.style.opacity = '0';
        setTimeout(function() { t.remove(); }, 300);
      }, 1800);
    }
  "))
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
