library(shiny)
library(ggplot2)
library(jsonlite)
library(colourpicker)

theme_file <- "saved_themes.json"

# ---------------- Helper functions ----------------

save_theme_to_file <- function(name, config) {
  tryCatch({
    themes <- if (file.exists(theme_file)) fromJSON(theme_file) else list()
    themes[[name]] <- config
    write(toJSON(themes, pretty = TRUE), theme_file)
    TRUE
  }, error = function(e) FALSE)
}

load_themes_from_file <- function() {
  tryCatch({
    if (file.exists(theme_file)) fromJSON(theme_file) else list()
  }, error = function(e) list())
}

delete_theme_from_file <- function(name) {
  tryCatch({
    if (file.exists(theme_file)) {
      themes <- fromJSON(theme_file)
      themes[[name]] <- NULL
      write(toJSON(themes, pretty = TRUE), theme_file)
      return(TRUE)
    }
    FALSE
  }, error = function(e) FALSE)
}

apply_theme_config <- function(conf, session) {
  updateSelectInput(session, "theme", selected = conf$base_theme)
  updateSliderInput(session, "base_size", value = conf$base_size)
  updateCheckboxInput(session, "show_grid", value = conf$show_grid)
  updateCheckboxInput(session, "flip", value = conf$flip_coords)
}

# Always pass alpha = input$alpha as a constant (never inside aes()).
generate_geom <- function(input, colorvar, fillcol) {
  if (input$geom == "point") {
    if (colorvar == "none") {
      geom_point(
        size  = input$point_size,
        alpha = input$alpha,
        color = fillcol
      )
    } else {
      geom_point(
        size  = input$point_size,
        alpha = input$alpha
      )
    }
  } else if (input$geom == "line") {
    if (colorvar == "none") {
      geom_line(
        size  = input$point_size,
        alpha = input$alpha,
        color = fillcol
      )
    } else {
      geom_line(
        size  = input$point_size,
        alpha = input$alpha
      )
    }
  } else if (input$geom == "col") {
    # For bars: width = 0.7, position = "stack" (no colorvar) or "dodge" (with colorvar)
    position_type <- if (colorvar == "none") "stack" else "dodge"
    if (colorvar == "none") {
      geom_col(
        width    = 0.7,
        alpha    = input$alpha,
        fill     = fillcol,
        position = position_type
      )
    } else {
      # aes(fill = <colorvar>) already in aes_mapping
      geom_col(
        width    = 0.7,
        alpha    = input$alpha,
        position = position_type
      )
    }
  } else if (input$geom == "density") {
    geom_density(
      alpha = input$alpha,
      fill  = fillcol
    )
  } else if (input$geom == "histogram") {
    geom_histogram(
      alpha = input$alpha,
      fill  = fillcol,
      bins  = 30
    )
  }
}

# ---------------- UI ----------------

ui <- fluidPage(
  titlePanel("Universal ggThemeAssist (No Legend)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      
      # Only show everything below after a file is uploaded:
      conditionalPanel(
        condition = "output.fileUploaded == true",
        
        h4("Variable Selection"),
        uiOutput("var_selectors"),
        
        h4("Plot Options"),
        selectInput(
          "geom", "Geometry",
          choices = c(
            "scatter"   = "point",
            "line"      = "line",
            "bar"       = "col",
            "density"   = "density",
            "histogram" = "histogram"
          )
        ),
        
        # If no color variable is chosen, let user pick a manual color.
        # (We do NOT show any legend-themed UI here.)
        conditionalPanel(
          condition = "input.colorvar == 'none'",
          colourInput("manual_color", "Choose Plot Color", value = "#2C3E50")
        ),
        
        # Always show these:
        sliderInput("point_size", "Point Size", min = 0.5, max = 10, value = 2),
        sliderInput("alpha", "Transparency (alpha)", min = 0.1, max = 1, value = 0.8),
        
        checkboxInput("log_x", "Log Scale X-Axis", FALSE),
        checkboxInput("log_y", "Log Scale Y-Axis", FALSE),
        
        textInput("plot_title", "Plot Title", ""),
        textInput("x_label", "X Axis Label", ""),
        textInput("y_label", "Y Axis Label", ""),
        
        h4("Themes"),
        selectInput(
          "theme", "Base Theme",
          choices = c(
            "plotly"       = "plotly",
            "plotly_white" = "plotly_white",
            "plotly_dark"  = "plotly_dark",
            "ggplot2"      = "ggplot2",
            "seaborn"      = "seaborn"
          ),
          selected = "ggplot2"
        ),
        sliderInput("base_size", "Base Text Size", min = 8, max = 24, value = 12),
        checkboxInput("show_grid", "Show Major Grid", TRUE),
        checkboxInput("flip", "Flip Coordinates", FALSE),
        
        h4("Manage Themes"),
        textInput("theme_name", "Theme Name", placeholder = "e.g., MyDarkStyle"),
        actionButton("save_theme", "Save Current Theme"),
        br(), br(),
        uiOutput("load_theme_ui"),
        
        checkboxInput("show_code", "Show ggplot2 Code", FALSE),
        downloadButton("download_plot", "Download Plot")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.fileUploaded == true",
        plotOutput("themedPlot", height = "500px"),
        tableOutput("data_preview"),
        verbatimTextOutput("data_info"),
        conditionalPanel(
          condition = "input.show_code == true",
          verbatimTextOutput("themeCode")
        )
      ),
      conditionalPanel(
        condition = "output.fileUploaded == false",
        h3(
          "Please upload a CSV file to get started",
          style = "text-align:center; color: #666; margin-top: 100px;"
        )
      )
    )
  )
)

# ---------------- Server ----------------

server <- function(input, output, session) {
  
  # Reactive flag: has a file been uploaded?
  output$fileUploaded <- reactive({ !is.null(input$file) })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Load + clean the CSV
  user_data <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      df <- df[rowSums(is.na(df)) != ncol(df), ]
      names(df) <- trimws(names(df))
      df
    }, error = function(e) {
      showNotification("Error loading file", type = "error")
      NULL
    })
  })
  
  # Dynamically generate X / Y / Color selectors once data is available
  output$var_selectors <- renderUI({
    req(user_data())
    vars <- names(user_data())
    tagList(
      selectInput("xvar", "X Variable", choices = vars),
      conditionalPanel(
        condition = "input.geom !== 'density' && input.geom !== 'histogram'",
        selectInput("yvar", "Y Variable", choices = vars)
      ),
      selectInput(
        "colorvar", "Color Variable (Optional)",
        choices = c("None" = "none", setNames(vars, vars))
      )
    )
  })
  
  # Always render the “Load / Delete Theme” UI, even if there are no saved themes
  output$load_theme_ui <- renderUI({
    saved_themes <- load_themes_from_file()
    if (length(saved_themes) > 0) {
      tagList(
        selectInput(
          "saved_themes", "Load Saved Theme",
          choices = c("Select theme..." = "", names(saved_themes))
        ),
        actionButton("load_theme", "Load Theme"),
        br(), br(),
        actionButton("delete_theme", "Delete Theme")
      )
    } else {
      # No saved themes yet; show a disabled dropdown
      tagList(
        selectInput(
          "saved_themes", "Load Saved Theme",
          choices = c("No saved themes" = "")
        )
        # Buttons are omitted when there’s nothing to load/delete
      )
    }
  })
  
  # Build the underlying ggplot object
  build_plot <- reactive({
    req(input$xvar, user_data())
    df <- user_data()
    
    aes_mapping <- switch(
      input$geom,
      
      "density" = aes_string(x = input$xvar),
      "histogram" = aes_string(x = input$xvar),
      
      "col" = {
        if (input$colorvar != "none") {
          aes_string(x = input$xvar, y = input$yvar, fill = input$colorvar)
        } else {
          aes_string(x = input$xvar, y = input$yvar)
        }
      },
      
      # scatter / line
      {
        if (input$colorvar != "none") {
          aes_string(x = input$xvar, y = input$yvar, color = input$colorvar)
        } else {
          aes_string(x = input$xvar, y = input$yvar)
        }
      }
    )
    
    p <- ggplot(df, aes_mapping)
    p <- p + generate_geom(input, input$colorvar, input$manual_color)
    
    if (input$geom == "col" && input$colorvar != "none") {
      p <- p + scale_fill_brewer(palette = "Set2")
    }
    
    p
  })
  
  # Apply base‐theme, grid, flip, log‐scales, labels, etc.
  themed_plot <- reactive({
    req(build_plot())
    p <- build_plot()
    
    theme_func <- switch(
      input$theme,
      "plotly"       = theme_minimal,
      "plotly_white" = theme_minimal,
      "plotly_dark"  = theme_dark,
      "ggplot2"      = theme_grey,
      "seaborn"      = theme_minimal,
      theme_minimal
    )
    p <- p + theme_func(base_size = input$base_size)
    
    if (!input$show_grid) {
      p <- p + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    }
    
    if (input$theme == "ggplot2") {
      p <- p + theme(
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background  = element_rect(fill = "white")
      )
    } else if (input$theme == "seaborn") {
      p <- p + theme(
        panel.background = element_rect(fill = "#EAEAF2"),
        plot.background  = element_rect(fill = "white")
      )
    } else if (input$theme == "plotly_dark") {
      p <- p + theme(
        panel.background = element_rect(fill = "#2F2F2F"),
        plot.background  = element_rect(fill = "#1E1E1E"),
        text            = element_text(color = "white"),
        axis.text       = element_text(color = "white")
      )
    }
    
    if (input$flip)  p <- p + coord_flip()
    if (input$log_x) p <- p + scale_x_log10()
    if (input$log_y) p <- p + scale_y_log10()
    
    p + labs(
      title = input$plot_title,
      x     = input$x_label,
      y     = input$y_label
    )
  })
  
  output$themedPlot <- renderPlot({ themed_plot() }, res = 96)
  
  output$data_preview <- renderTable({
    head(user_data(), 5)
  }, striped = TRUE)
  
  output$data_info <- renderText({
    df <- user_data()
    paste(
      "Rows: ", nrow(df), "\n",
      "Columns: ", ncol(df), "\n",
      "Numeric columns: ", sum(sapply(df, is.numeric)),
      sep = ""
    )
  })
  
  # Show the ggplot2 code if requested
  output$themeCode <- renderText({
    req(input$xvar)
    
    # 1) Build the aes(...) string
    aes_str <- if (input$geom %in% c("density", "histogram")) {
      paste0("aes(x = ", input$xvar, ")")
    } else if (input$geom == "col" && input$colorvar != "none") {
      paste0(
        "aes(x = ", input$xvar, ", ",
        "y = ", input$yvar, ", ",
        "fill = ", input$colorvar, ")"
      )
    } else if (input$geom == "col" && input$colorvar == "none") {
      paste0("aes(x = ", input$xvar, ", y = ", input$yvar, ")")
    } else {
      # scatter or line
      if (input$colorvar != "none") {
        paste0(
          "aes(x = ", input$xvar, ", ",
          "y = ", input$yvar, ", ",
          "color = ", input$colorvar, ")"
        )
      } else {
        paste0("aes(x = ", input$xvar, ", y = ", input$yvar, ")")
      }
    }
    
    # 2) Build the geom(...) string
    geom_str <- switch(
      input$geom,
      "point" = {
        if (input$colorvar == "none") {
          paste0(
            "geom_point(color = \"", input$manual_color, "\", ",
            "size = ", input$point_size, ", ",
            "alpha = ", input$alpha, ")"
          )
        } else {
          paste0(
            "geom_point(size = ", input$point_size, ", ",
            "alpha = ", input$alpha, ")"
          )
        }
      },
      "line" = {
        if (input$colorvar == "none") {
          paste0(
            "geom_line(color = \"", input$manual_color, "\", ",
            "size = ", input$point_size, ", ",
            "alpha = ", input$alpha, ")"
          )
        } else {
          paste0(
            "geom_line(size = ", input$point_size, ", ",
            "alpha = ", input$alpha, ")"
          )
        }
      },
      "col" = {
        if (input$colorvar == "none") {
          paste0(
            "geom_col(fill = \"", input$manual_color, "\", ",
            "alpha = ", input$alpha, ", ",
            "width = 0.7)"
          )
        } else {
          paste0(
            "geom_col(alpha = ", input$alpha, ", width = 0.7) +\n",
            "  scale_fill_brewer(palette = \"Set2\")"
          )
        }
      },
      "density" = {
        paste0(
          "geom_density(fill = \"", input$manual_color, "\", ",
          "alpha = ", input$alpha, ")"
        )
      },
      # fallback: histogram
      {
        paste0(
          "geom_histogram(fill = \"", input$manual_color, "\", ",
          "alpha = ", input$alpha, ", bins = 30)"
        )
      }
    )
    
    # 3) Build the theme(...) string
    theme_str <- switch(
      input$theme,
      "plotly"       = "theme_minimal",
      "plotly_white" = "theme_minimal",
      "plotly_dark"  = "theme_dark",
      "ggplot2"      = "theme_grey",
      "seaborn"      = "theme_minimal",
      "theme_minimal"
    )
    
    # 4) Build labs(...) if the user typed anything
    labs_args <- c()
    if (nzchar(input$plot_title)) labs_args <- c(labs_args, paste0("title = \"", input$plot_title, "\""))
    if (nzchar(input$x_label))    labs_args <- c(labs_args, paste0("x = \"", input$x_label, "\""))
    if (nzchar(input$y_label))    labs_args <- c(labs_args, paste0("y = \"", input$y_label, "\""))
    labs_str <- if (length(labs_args) > 0) {
      paste0("labs(", paste(labs_args, collapse = ", "), ")")
    } else {
      ""
    }
    
    # 5) Combine all lines into one code snippet
    code_lines <- c(
      "library(ggplot2)",
      "",
      paste0("ggplot(data, ", aes_str, ") +"),
      paste0("  ", geom_str, " +"),
      paste0("  ", theme_str, "(base_size = ", input$base_size, ")")
    )
    
    if (!input$show_grid) {
      code_lines <- c(
        code_lines,
        "  + theme(panel.grid.major = element_blank(),",
        "          panel.grid.minor = element_blank())"
      )
    }
    if (nzchar(labs_str)) {
      code_lines <- c(code_lines, paste0("  + ", labs_str))
    }
    if (input$flip)  code_lines <- c(code_lines, "  + coord_flip()")
    if (input$log_x) code_lines <- c(code_lines, "  + scale_x_log10()")
    if (input$log_y) code_lines <- c(code_lines, "  + scale_y_log10()")
    
    paste(code_lines, collapse = "\n")
  })
  
  # Download handler for PNG
  output$download_plot <- downloadHandler(
    filename = function() "custom_plot.png",
    content = function(file) {
      ggsave(file, plot = themed_plot(), width = 8, height = 6)
    }
  )
  
  # Save / Load / Delete custom themes
  observeEvent(input$save_theme, {
    if (nchar(input$theme_name) > 0) {
      conf <- list(
        base_theme  = input$theme,
        base_size   = input$base_size,
        show_grid   = input$show_grid,
        flip_coords = input$flip
      )
      if (save_theme_to_file(input$theme_name, conf)) {
        showNotification("Theme saved!", type = "message")
        updateTextInput(session, "theme_name", value = "")
      } else {
        showNotification("Error saving theme", type = "error")
      }
    } else {
      showNotification("Please enter a theme name", type = "warning")
    }
  })
  
  observeEvent(input$load_theme, {
    req(input$saved_themes)
    # If the dropdown’s value is empty (""), do nothing.
    if (nzchar(input$saved_themes)) {
      conf <- load_themes_from_file()[[input$saved_themes]]
      if (!is.null(conf)) {
        isolate(apply_theme_config(conf, session))
        showNotification("Theme loaded!", type = "message")
      }
    }
  })
  
  observeEvent(input$delete_theme, {
    req(input$saved_themes)
    # Only delete if actually a saved theme is selected (not the empty choice).
    if (nzchar(input$saved_themes) && delete_theme_from_file(input$saved_themes)) {
      showNotification("Theme deleted", type = "message")
      # After deletion, reset the dropdown to the placeholder
      isolate({ updateSelectInput(session, "saved_themes", selected = "") })
    } else {
      showNotification("Failed to delete theme", type = "error")
    }
  })
}

# ---------------- Run App ----------------

shinyApp(ui, server)
