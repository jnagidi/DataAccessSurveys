#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' 


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # start of dashboard
    bs4Dash::dashboardPage(
      title = "ADRC Access Requests",
      fullscreen = TRUE,
      dark = NULL,
      
      header = bs4Dash::dashboardHeader(
        title = tags$a(
          href = 'https://www.uab.edu/',
          target = "_blank",
          tags$img(
            src = 'www/logo-ADC.png',
            width = "100%",
            style = "padding: 8% 10% 2% 10%", 
            alt = 'UAB logo'
          )
        )
      ),
      
      body = bs4Dash::dashboardBody(
        style = "height: 98%",
        uiOutput("html_iframe")
      ),
      
      
      
      sidebar = bs4Dash::dashboardSidebar(
        id = 'sideBarMenu',
        skin = 'dark',
        width = "275px",
        
        bs4Dash::sidebarMenu(
          
          # study select input at top of page
          textInput("request_id", "Enter request", ""),
          actionButton("build_survey", "Build Survey"),
          actionButton("generatePDF", "Generate PDF"),
          downloadButton("downloadPDF", "Download PDF")#,
          # actionButton("new_tab", "New Tab for Printing")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  #Resource path for CSS
  add_resource_path(
    'www', app_sys('app/www/')
  )
  
  #Resource path for Images (e.g. logo in header)
  add_resource_path(
    'img', system.file('app/img/', package = 'ADRCDash')
  )
  
  #Add output path
  add_resource_path(
    'output', app_sys('output')
  )
  
  #Add js
  #golem::add_js_file("custom.js")
  
  #Head tags including favicon
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ADRC Request Surveys'
    ),
    
    # Add here other external resources
    # Initialize use of waiter
    waiter::use_waiter(),
    
    # enable shinybrowser
    shinybrowser::detect()
  )
}
