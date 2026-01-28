#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  #Ensure www exists
  out_dir <- app_sys("output")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  shiny::addResourcePath("output", out_dir)
  
  #Reactive to wait until knitting finishes
  html_ready <- reactiveVal(FALSE)
  pdf_ready <- reactiveVal(FALSE)
  
  #Event button to render the markdown survey
  # observeEvent(input$build_survey, {
  #   req(input$request_id)
  #   
  #   rmarkdown::render(input = app_sys("R/survey_template.Rmd"), 
  #                     output_file = file.path(out_dir, "survey_template.html"),
  #                     params = list(id_curr = input$request_id))
  #   html_ready(TRUE)
  #   
  #   #Generate the output that goes to file
  #   output$html_iframe <- renderUI({
  #     shiny::req(html_ready())
  #     tags$iframe(
  #       src = "output/survey_template.html",
  #       width = "100%",
  #       height = "98% !important",
  #       style = "border: none;"
  #       )
  #     })
  # })
  
  output$html_iframe <- renderUI({
    req(html_ready())
    req(file.exists(file.path(out_dir, "survey_template.html")))
    
    tags$iframe(
      src = "output/survey_template.html",
      width = "100%",
      height = "98%",
      style = "border: none;"
    )
  })
  observeEvent(input$build_survey, {
    req(input$request_id)
    
    html_ready(FALSE)
    
    rmarkdown::render(
      input = app_sys("R/survey_template.Rmd"),
      output_file = file.path(out_dir, "survey_template.html"),
      params = list(id_curr = input$request_id),
      quiet = TRUE
    )
    
    html_ready(TRUE)
  })
  
  
  observeEvent(input$generatePDF, {
    shiny::req(html_ready())

    #Paths and pagedown
    html_path <- paste0(out_dir, "/survey_template.html")
    pdf_path <- paste0(out_dir, "/survey_template.pdf")
    # options(chromote.timeout = 60)
    # 
    # pagedown::chrome_print(input = html_path, output = pdf_path)
    wkhtml_cmd <- sprintf("wkhtmltopdf %s %s", shQuote(html_path), shQuote(pdf_path))
    system(wkhtml_cmd)

    # Provide the PDF for download
    showNotification("PDF generated! You can now download it.", type = "message")
  })


  # Download the PDF file
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("Access_IDCURR_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf_path <- file.path(out_dir, "survey_template.pdf")
      req(file.exists(pdf_path))
      file.copy(pdf_path, file)
    }
  )
  
  # #New tab
  # observeEvent(input$new_tab, {
  #   shiny::req(html_ready())
  #   html_path <- file.path(out_dir, "survey_template.html")
  #   session$sendCustomMessage(type = "open_tab", message = paste0("output/survey_template.html"))
  # })
   
}
  
   
