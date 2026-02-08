server <- function(input, output, session) {
  
  observeEvent(input$download_DCQCmd, {
    card_data <- lapply(names(card_labels), function(id) {
      response <- input[[paste0("check_", id)]]
      comment <- input[[paste0("item_", id, "_comment")]]
      if (!is.null(response)) {
        list(
          id = id,
          label = card_labels[[id]],
          response = response,
          comment = comment
        )
      } else {
        NULL
      }
    })
    card_data <- Filter(Negate(is.null), card_data)
    
    paper_title <- if (input$paper_title == "") "No Title Given" else input$paper_title
    reviewer_name <- if (input$reviewer_name == "") "No Name Given" else input$reviewer_name
    journal_name <- if (input$journal_name == "") "No Journal Given" else input$journal_name
    filename <- paste0("DCQC_Report_", gsub("[^A-Za-z0-9_-]", "_", paper_title), "_", 
                       gsub("[^A-Za-z0-9_-]", "_", reviewer_name), ".html")
    
    html <- paste0(
      '<!DOCTYPE html><html><head><meta charset="UTF-8"><title>DCQC Report</title>',
      '<style>body{font-family:Arial,sans-serif;max-width:800px;margin:40px auto;padding:20px;line-height:1.6}',
      'h1{color:#2c3e50;border-bottom:3px solid #3498db;padding-bottom:10px}',
      'h2{color:#34495e;margin-top:30px;border-bottom:2px solid #95a5a6;padding-bottom:5px}',
      'h3{color:#7f8c8d;margin-top:20px}',
      '.info{background-color:#ecf0f1;padding:15px;border-radius:5px;margin:20px 0}',
      '.info p{margin:5px 0}',
      '.checklist-item{background-color:#f8f9fa;padding:15px;margin:15px 0;border-left:4px solid #3498db;border-radius:3px}',
      '.response{font-weight:bold;color:#2980b9}',
      '.comment{margin-top:10px;font-style:italic;color:#555}',
      'hr{border:none;border-top:2px solid #bdc3c7;margin:30px 0}',
      '.footer{text-align:center;color:#7f8c8d;font-size:0.9em;margin-top:40px}',
      '.download-btn{display:inline-block;padding:10px 20px;background:#3498db;color:white;',
      'text-decoration:none;border-radius:5px;margin:20px 0;cursor:pointer;border:none;font-size:16px}',
      '.download-btn:hover{background:#2980b9}',
      '@media print{.no-print{display:none}}',
      '</style></head><body>',
      '<div class="no-print" style="text-align:center">',
      '<button class="download-btn" onclick="downloadHTML()">💾 Download This Report</button>',
      '</div>',
      '<h1>Data and Code Quality Control Report</h1>',
      '<div class="info">',
      '<p><strong>Paper Title:</strong> ', paper_title, '</p>',
      '<p><strong>Reviewer:</strong> ', reviewer_name, '</p>',
      '<p><strong>Journal:</strong> ', journal_name, '</p>',
      '<p><strong>Date:</strong> ', Sys.Date(), '</p>',
      '</div><hr><h2>Quality Control Checklist</h2>'
    )
    
    for (card in card_data) {
      html <- paste0(html,
                     '<div class="checklist-item"><h3>', card$label, '</h3>',
                     '<p class="response">Response: ', card$response, '</p>'
      )
      if (!is.null(card$comment) && nchar(card$comment) > 0) {
        html <- paste0(html, '<p class="comment">Comment: ', card$comment, '</p>')
      }
      html <- paste0(html, '</div>')
    }
    
    html <- paste0(html,
                   '<hr><div class="footer"><p><em>Report generated using DCQC (SORTEE Guidelines)</em></p></div>',
                   '<script>',
                   'function downloadHTML() {',
                   '  var blob = new Blob([document.documentElement.outerHTML], {type: "text/html"});',
                   '  var url = URL.createObjectURL(blob);',
                   '  var a = document.createElement("a");',
                   '  a.href = url;',
                   '  a.download = "', filename, '";',
                   '  document.body.appendChild(a);',
                   '  a.click();',
                   '  document.body.removeChild(a);',
                   '  URL.revokeObjectURL(url);',
                   '}',
                   '</script>',
                   '</body></html>'
    )
    
    session$sendCustomMessage("openHTML", html)
  })
  data_modal <- shiny::modalDialog(
    easyClose = FALSE,
    footer = NULL,
    size = "l",
    fade = TRUE,
    shiny::div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;",
      shiny::tags$img(src = "circle_black.png", height = "88px", width = "88px", style = "margin-bottom: 20px;"),
      br(),
      textInput("paper_title", tags$b("Paper Title"), placeholder = "Enter paper name", width = "300px"),
      textInput("reviewer_name", tags$b("Reviewer Name"), placeholder = "Enter your name", width = "300px"),
      textInput("journal_name", tags$b("Journal"), placeholder = "Enter journal name", width = "300px"),
      br(),
      br(),
      shinyWidgets::awesomeCheckboxGroup(
        inputId = "stage_checks",
        label = tags$b("Select DCQC Review Stages"),
        choices = paste("Stage", 1:6),
        selected = character(0),
        inline = TRUE,
        status = "success"
      ),
      actionButton("submit_data_modal", "Submit", class = "btn btn-success", style = "margin-top: 20px;")
    )
  )
  shinyjs::hide("stage1_title")
  shinyjs::hide("stage2_title")
  shinyjs::hide("stage3_title")
  shinyjs::hide("stage4_title")
  shinyjs::hide("stage5_title")
  shinyjs::hide("stage6_title")
  shinyjs::hide("comments_1")
  shinyjs::hide("comments_2")
  shinyjs::hide("comments_3")
  shinyjs::hide("comments_4")
  shinyjs::hide("comments_5")
  shinyjs::hide("comments_6")

  shinyjs::hide("1")
  shinyjs::hide("2")
  shinyjs::hide("3")
  shinyjs::hide("4")
  shinyjs::hide("5")
  shinyjs::hide("6")
  shinyjs::hide("7")
  shinyjs::hide("8")
  shinyjs::hide("9")
  shinyjs::hide("10")
  shinyjs::hide("11")
  shinyjs::hide("12")
  shinyjs::hide("13")
  shinyjs::hide("14")
  shinyjs::hide("15")
  shinyjs::hide("1a")
  shinyjs::hide("2a")
  shinyjs::hide("3a")
  shinyjs::hide("4a")
  shinyjs::hide("5a")
  shinyjs::hide("6a")
  shinyjs::hide("review_summary")
  shinyjs::hide("download_DCQCmd")
  shiny::showModal(data_modal)

  shiny::observeEvent(input$submit_data_modal, {
    
    output$paper_title_output <- shiny::renderUI({
      if(input$paper_title == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Title:</b> No Title Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Title:</b>",
        input$paper_title,
        "</p>"
      ))
      )
    })

    output$reviewer_name_output <- shiny::renderUI({
      if(input$reviewer_name == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Name:</b> No Name Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Name:</b>",
        input$reviewer_name,
        "</p>"
      ))
      )
    })

    output$journal_name_output <- shiny::renderUI({
      if(input$journal_name == ""){
        shiny::HTML(paste(
          "<p>",
          "<b>Journal:</b> No Journal Given",
          "</p>"
        ))
      } else(
      shiny::HTML(paste(
        "<p>",
        "<b>Journal:</b>",
        input$journal_name,
        "</p>"
      ))
      )
    })

    if ("Stage 1" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCmd")
      shinyjs::show("stage1_title")
      shinyjs::show("1")
      shinyjs::show("2")
      shinyjs::show("3")
      shinyjs::show("4")
      shinyjs::show("5")
      shinyjs::show("comments_1")
      shinyjs::show("1a")
    }

    if ("Stage 2" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCmd")
      shinyjs::show("stage2_title")
      shinyjs::show("6")
      shinyjs::show("comments_2")
      shinyjs::show("2a")
    }

    if ("Stage 3" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("ddownload_DCQCmd")
      shinyjs::show("stage3_title")
      shinyjs::show("7")
      shinyjs::show("8")
      shinyjs::show("9")
      shinyjs::show("10")
      shinyjs::show("11")
      shinyjs::show("comments_3")
      shinyjs::show("3a")
    }

    if ("Stage 4" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCmd")
      shinyjs::show("stage4_title")
      shinyjs::show("12")
      shinyjs::show("comments_4")
      shinyjs::show("4a")
    }

    if ("Stage 5" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCmd")
      shinyjs::show("stage5_title")
      shinyjs::show("13")
      shinyjs::show("comments_5")
      shinyjs::show("5a")
    }

    if ("Stage 6" %in% input$stage_checks) {
      shiny::removeModal()
      shinyjs::show("review_summary")
      shinyjs::show("download_DCQCmd")
      shinyjs::show("stage6_title")
      shinyjs::show("14")
      shinyjs::show("15")
      shinyjs::show("comments_6")
      shinyjs::show("6a")
    }

    if (length(input$stage_checks) == 0) {
      shinyalert::shinyalert(
        title = "Select a stage to review",
        text = "No stage selected",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
}
