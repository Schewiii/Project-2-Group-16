# Universal ggThemeAssist

**An Interactive Shiny App for Custom ggplot2 Theming**

---

## Project Overview

**Universal ggThemeAssist** is a Shiny application designed to help R users upload a CSV dataset, select plot geometries (scatter, line, bar, density, histogram), interactively customize **ggplot2** theme parameters (base text size, grid visibility, coordinate flipping, etc.), and persist theme configurations via JSON files.

By combining:

- **Shiny** reactivity
- **ggplot2** theming
- **jsonlite** for JSON‐based persistence
- **colourpicker** for manual color selection

the app enables rapid iteration of plot aesthetics and fosters reproducible, user‐defined styling.

This repository also contains:

- `Final_Report.pdf`  – A detailed write-up of design, implementation, and evaluation
- `Proposal.pdf`  – Initial project proposal
- `Slides.pdf`  – Presentation slides

---

## Features

1. **Upload CSV**: Accepts any valid CSV, filters out rows that are completely `NA`, and trims column names.
2. **Variable Selection**:
   - Choose an **X Variable**.
   - Choose a **Y Variable** (not required for density/histogram).
   - Optionally choose a **Color Variable** (discrete/categorical).
3. **Plot Options**:
   - Geometry: scatter, line, bar, density, histogram
   - Manual color picker (if no color variable is selected)
   - Point size & transparency (alpha)
   - Log-scale toggles for X and Y
4. **Theming**:
   - Base themes: `plotly`, `plotly_white`, `plotly_dark`, `ggplot2`, `seaborn`
   - Base text size (8–24)
   - Show/hide major grid
   - Flip coordinates (horizontal bar plots)
5. **Save & Load Themes**:
   - Save current theme settings (JSON-based) under a user-provided name.
   - Load a saved theme from disk and immediately apply it.
   - Delete unwanted saved themes.
6. **Code Preview & Download**:
   - Optionally view the exact `ggplot2` code that reproduces the current plot.
   - Download the current plot as a PNG.

---

## Folder Structure

- .gitignore
- README.md
- app/
  - Plot_Customization
- saved_themes.json
- Final_Report.pdf
- Proposal.pdf
- Slides.pdf
