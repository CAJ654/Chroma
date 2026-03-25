# Color Palette Generator

A fully R-centric Shiny app that generates color palettes using two server-side APIs — all HTTP calls are made from R, not the browser

## Tech Stack

- **R + Shiny** — web framework
- **curl** (R package) — server-side HTTP requests
- **jsonlite** (R package) — JSON parsing
- **thecolorapi.com** — color scheme generation with rich metadata (name, RGB, HSL)
- **colormind.io** — AI/ML-generated color palettes with optional locked colors

## Features

### Color Scheme Tab
- Pick any color via color picker or hex input
- Choose from 8 scheme types: Analogic, Complement, Triad, Quad, Analogic+Complement, Monochrome (3 variants)
- Select 2–10 colors in the palette
- Each card shows color name, hex, and RGB values
- R server fetches and processes all data from thecolorapi.com

### AI Palette Tab
- Choose from 7 Colormind models (Default, UI, Material, Japanese Prints, etc.)
- Optionally lock up to 5 colors as anchors — R sends them to the AI
- AI generates a 5-color palette around locked colors
- R server handles the POST request and parses the response

### Shared Features
- Click any color card or hex chip to copy to clipboard
- Skeleton loading animations while R fetches data
- Toast notifications on copy

## Project Structure

- `app.R` — main Shiny app (UI + server with all API logic in R)
- `setup.R` — package installation helper (not used in production)
- `replit.nix` — Nix environment (includes `rPackages.shiny`)
- System deps: `curl`, `openssl` (for R's curl package)

## Running

Workflow `Start application`:
```
Rscript -e "shiny::runApp('app.R', host='0.0.0.0', port=5000)"
```

Test Deployment 1# Chroma
