# Color Palette Generator

A Shiny web application that generates beautiful color palettes using the free [The Color API](https://www.thecolorapi.com/).

## Tech Stack

- **R + Shiny** — web framework
- **The Color API** — free, no-auth palette generation API (called from the browser via JavaScript fetch)

## Features

- Pick any color via a color picker or hex input
- Choose from 8 scheme types: Analogic, Complement, Triad, Quad, Analogic+Complement, Monochrome (3 variants)
- Choose 2–10 colors in the palette
- Displays color name, hex code, and RGB values for each swatch
- Click any color card or hex chip to copy the hex value to clipboard
- Skeleton loading animation while fetching

## Project Structure

- `app.R` — main Shiny app (UI + server)
- `setup.R` — package installation helper (not used in production; Nix handles shiny)
- `replit.nix` — Nix environment (includes `rPackages.shiny`)

## Running

The workflow `Start application` runs:
```
Rscript -e "shiny::runApp('app.R', host='0.0.0.0', port=5000)"
```
