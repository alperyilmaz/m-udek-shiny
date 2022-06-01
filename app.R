	# mudek-test
library(gt)
library(DBI)
library(shiny)
library(readxl)
library(webshot)
#library(keyring)
library(shinyjs)
library(shinyBS)
library(RSQLite)
library(tidyverse)
library(shinymanager)
library(shinycssloaders)


# admin database!
database_biyomuh <- "biyomuh.db"
database_biyomuh_sabit <- "biyomuh-sabit.db"

database_gidamuh <- "gidamuh.db"
database_gidamuh_sabit <- "gidamuh-sabit.db"

database_kimyamuh <- "kimyamuh.db"
database_kimyamuh_sabit <- "kimyamuh-sabit.db"

database_matmuh_sabit <- "matmuh.db"
database_matmuh <- "matmuh-sabit.db"

database_metalurjimuh <- "metalurjimuh.db"
database_metalurjimuh_sabit <- "metalurjimuh-sabit.db"

# sqlitePath = "/home/zeynep/sqlite/mudek/mudek.db"
# sqlitePath = "mudek-test.db"
options(shiny.maxRequestSize = 30*1024^2)
source("functions.R")

credentials <- data.frame(
  user = c("admin", "user1", "user2", "user3", "user4", "user5"),
  password = c("pass", "pass1", "pass2", "pass3", "pass4", "pass5"),
  # start = c("2019-04-15"),
  # expire = c(NA, "2019-12-31"),
  admin = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  # comment = "Simple and secure authentication mechanism for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# you can use keyring package to set database key
#key_set("R-shinymanager-key", "obiwankenobi")

create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite", # will be created
  # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  passphrase = "passphrase_wihtout_keyring"
)


ui <- fluidPage(
  navbarPage(
    id = "nav",
    title = 'MUDEK APP',
    # TODO we need separate menu for ÇAP students
    navbarMenu('Department Summary',
               tabPanel('Department Summary (ALL)',
                         #downloadButton("export_department_table_all", "Export Department Summary (ALL)"),
                         #tags$hr(),
                         HTML("<center><h3>Department Summary (ALL)</h3></center>"), 
                         shinycssloaders::withSpinner(gt_output('department_table')),
                         tags$hr(),
                         HTML("<center><h3>Department Success Rate (ALL)</h3></center>"),
                         shinycssloaders::withSpinner(gt_output('department_summary_table'))
                         ),
               tabPanel('Department Summary (TR)', 
                         downloadButton("export_department_table_tr", "Export Department Summary (TR)"),
                         HTML("<p>Please print the downloaded html file in landscape layout in Chrome/Firefox</p>"),
                         tags$hr(),
                         HTML("<center><h3>Department Summary (TR)</h3></center>"),
                         shinycssloaders::withSpinner(gt_output('department_table_tr')),
                         tags$hr(),
                         HTML("<center><h3>Department Success Rate (TR)</h3></center>"),
                         shinycssloaders::withSpinner(gt_output('department_summary_table_tr'))
                         ),
               tabPanel('Department Summary (EN)',
                        downloadButton("export_department_table_en", "Export Department Summary (EN)"),
                        HTML("<p>Please print the downloaded html file in landscape layout in Chrome/Firefox</p>"),
                        tags$hr(),
                        HTML("<center><h3>Department Summary (EN)</h3></center>"), 
                        shinycssloaders::withSpinner(gt_output('department_table_en')),
                        tags$hr(),
                        HTML("<center><h3>Department Success Rate (EN)</h3></center>"),
                        shinycssloaders::withSpinner(gt_output('department_summary_table_en'))
                        ),
               tabPanel('Department Summary (ÇAP)', 
                        #downloadButton("export_department_table_cap", "Export Department Summary (ÇAP)"),
                        #tags$hr(),
                        HTML("<center><h3>Department Summary (ÇAP)</h3></center>"),
                        shinycssloaders::withSpinner(gt_output('department_table_cap'))),
               tabPanel('Department PC Matrix',
                        # TODO this button prevents the app from showing gt output. why?
                        #downloadButton("department_table_pc_matriks", "Department PC Matrix"),
                        HTML("<center><h3>Department PC Matrix</h3></center>"),
                        #selectInput("select_term_matrix", "Select Term", choices = get_available_terms()),
                        uiOutput("select_term_matrix_input"),
                        shinycssloaders::withSpinner(gt_output('matrix_table')),
                        tags$hr(),
                        #gt_output('department_table_pc_matriks'),
                        #tags$hr(),
                        #downloadButton("department_table_pc_def", "Department PC Definitions"),
                        HTML("<center><h3>Department PC Definitions</h3></center>"),
                        gt_output('department_table_pc_def'))
   ),  
   tabPanel('Course Summary',
     tabsetPanel(
        tabPanel("Term Summary",
             uiOutput("select_term_course_input"),
             shinycssloaders::withSpinner(gt_output('course_table'))),
        tabPanel("Year Summary",
             uiOutput("select_year_course_input"),
             shinycssloaders::withSpinner(gt_output('course_year_table'))) 
     ) 
   ),
    tabPanel('Student Summary', 
             tabsetPanel(
               tabPanel("Single or All Student",
                 sidebarPanel(
                   uiOutput("student_select_input"),
                   tags$hr(),
                   downloadButton("export_student_report", "Export Student Report"),
                   tags$hr(),
                   downloadButton("export_student_reports_all", "Export All Student Reports"),
                   width = 3
                  ),gt_output('student_table')
               ),
               tabPanel("Batch Student Query",
                 sidebarPanel(
                   textAreaInput("batch_student_list", "Paste student numbers:"),
                   actionButton("batch_student_list_report", "View batch student report", class = "btn-primary"),
                   downloadButton("export_batch_student_report", "Export Batch Student Report"),
                   width = 3
                 ), 
                 # output of panel
                 HTML("<center><h3>Batch Student Report</h3></center>"),
                 HTML("<center style='color:red;'>Student numbers in red are not found in your batch query!</center>"),
                 gt_output('batch_student_list_text')
                 
               )
             )),
    
    tabPanel('Upload',
             sidebarLayout(
               sidebarPanel(
                 uiOutput("user_name"),
                 tags$hr(),
                 uiOutput("user_dept"),
                 selectInput(
                   "select_term",
                   "Term",
                   choices = get_available_terms()
                 ),
                 tags$hr(),
                 fileInput(
                   inputId = "file",
                   label = "Upload your file",
                   multiple = TRUE,
                   accept = c(".xls*")
                 ),
                 radioButtons(
                   "display", "Display Uploaded Files",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"
                 )
               ),
               mainPanel(
                 verbatimTextOutput("parsingError"),
                 shinyjs::useShinyjs(),
                 hidden(
                   div(id = "view_parsing_error_msg_div", style = "display:inline-block",
                       actionButton("view_parsing_error_msg",  "View Error Messages",
                                    icon = icon('exclamation-triangle'))
                   )
                 ),
                 bsModal(id="parsing_error_msg_modal", title = "Error Messages",
                         trigger = "view_parsing_error_msg", size="large",
                         tableOutput("parsing_error_msg")
                 ),
                 uiOutput("uploadedFilesHeader"),
                 tableOutput("uploaded_files"),
                 uiOutput("studentCountsHeader"),
                 DT::dataTableOutput("summary")
               )
             )
    ),
    tabPanel('Parse & Submit',
             uiOutput("fluidrow_parsed_options"),
             withSpinner(DT::dataTableOutput("parsed_table"), hide.ui = FALSE),
             actionButton("previewFiles", "Preview", class = "btn-primary"),
             tags$hr(),
             DT::dataTableOutput("show_preview_table"),
             actionButton("submitFiles", "Submit", class = "btn-primary")

    ),
    
    tabPanel('Submitted Files',
             tags$h3("List of Submitted Files"),
             verbatimTextOutput("submittedFiles"),
             tags$br(),
             tags$h3("Submitted Files Table"),
             shinycssloaders::withSpinner(DT::dataTableOutput("submitted_files_table"))
             # tags$br(),
             # tags$h3("Submission Details"),
             # DT::dataTableOutput("submission_details")
    ),
    tabPanel('Edit',
             navlistPanel(
               tabPanel("Delete Term",
                        uiOutput("select_term_edit_dterm_input"),
                        tags$br(),
                        DT::dataTableOutput("submitted_files_table_edit_dterm"),
                        actionButton("deleteTerm", "Delete Term", class = "btn-primary")
               ),
               tabPanel("Delete Course",
                        uiOutput("select_term_edit_dcourse_input"),
                        uiOutput("select_course_edit_dcourse_input"),
                        tags$br(),
                        DT::dataTableOutput("submitted_files_table_edit_dcourse"),
                        actionButton("deleteCourse", "Delete Course", class = "btn-primary")
                 
               ),
               widths = c(2, 10),
               well = TRUE
             )
    )
 )
)


ui <- secure_app(ui, enable_admin = TRUE)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "database.sqlite",
      # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
      passphrase = "passphrase_wihtout_keyring"
    ),
    timeout = 30
  )
  
  output$user_name <- renderUI({
    tags$b(paste0("Logged in as ", res_auth$user))
  })
  
  userDept <- reactive({
    switch(res_auth$user,
           admin = "Biyomühendislik",
           # admin = c("All",
           #           "Biyomühendislik",
           #           "Gıda Mühendisliği",
           #           "Kimya Mühendisliği",
           #           "Matematik Mühendisliği",
           #           "Metalurji ve Malzeme Mühendisliği"),
           user1 = "Biyomühendislik",
           user2 = "Gıda Mühendisliği",
           user3 = "Kimya Mühendisliği",
           user4 = "Matematik Mühendisliği",
           user5 = "Metalurji ve Malzeme Mühendisliği"
    )
  })
  
  userDB <- reactive({
    switch(res_auth$user,
           admin = database_biyomuh,
           user1 = database_biyomuh,
           user2 = database_gidamuh,
           user3 = database_kimyamuh,
           user4 = database_matmuh,
           user5 = database_metalurjimuh
    )
  })

  userDBsabit <- reactive({
    switch(res_auth$user,
           admin = database_biyomuh_sabit,
           user1 = database_biyomuh_sabit,
           user2 = database_gidamuh_sabit,
           user3 = database_kimyamuh_sabit,
           user4 = database_matmuh_sabit,
           user5 = database_metalurjimuh_sabit
    )
  })
  
  
  output$user_dept <- renderUI({
    selectInput(
      "select_department",
      "Department",
      choices = userDept()
    )
  })
  
  
  inFile_ <- reactive({
    req(input$file)
  })
  
  inFile <- reactive({
    fix_file_names(inFile_())
  })
  
  data <- reactive({
    inFile()$datapath %>%
      purrr::set_names() %>%
      map(~ safe_parse_all_sheets(.x), .id="file")
  })
  
  data_safe <- reactive({
    data() %>%
      map("result") %>%
      compact() %>%
      bind_rows(.id = "file") %>%
      standardize_course_names() 
  })
  
  data_error <- reactive({
    data() %>%
      map("error") %>%
      keep(~!is.null(.x))
  })
  
  
  output$parsingError <- renderPrint({
    validate(
      need(
        length(data_error()) == 0, paste("Parsing failed:", names(data_error()))
      )
    )
  })
  
  
  observeEvent(req(length(data_error()) != 0), shinyjs::show("view_parsing_error_msg_div"))
  observeEvent(input$file, if(length(data_error()) == 0){shinyjs::hide("view_parsing_error_msg_div")})
  
  
  output$parsing_error_msg <- renderTable({
    error_table <- tibble(`File` = names(data_error()),
                          `Error Message` = data_error() %>% map("message") %>% unlist())
    error_table
  }, align = "c", bordered = TRUE, hover = TRUE)
  
  
  output$uploadedFilesHeader <- renderUI(
    if(is.null(input$file)){return()}
    else{tags$h3("Uploaded Files")}
  )
  
  output$studentCountsHeader <- renderUI(
    if(is.null(input$file)){return()}
    else{tags$h3("Student Counts")}
  )
  
  
  output$uploaded_files <- renderTable({
    if(input$display == "head") {
      return(head(inFile()))
    }
    else {
      return(inFile())
    }
  })
  
  output$summary <- DT::renderDataTable(
    DT::datatable({
      student_count(data_safe())
    }, options = list(pageLength=5)
    )
  )
  
  
  output$parsed_table <- DT::renderDataTable(
    DT::datatable({
      data <- data_safe()
      
      if (input$course != "All") {
        data <- data[data$file == input$course,]
      }
      if (input$course_code != "All") {
        data <- data[data$course == input$course_code,]
      }
      if (input$student != "All") {
        data <- data[data$student_no == input$student,]
      }
      
      data
    }, options = list(pageLength=5)
    )
  )
  
  data_long <- reactive({
    data_safe() %>% 
      mutate(department = input$select_department) %>% 
      mutate(term = input$select_term) %>% 
      select(department, term, file, course, method, PC, student_no, score, Puan)
  })
  
  observe({shinyjs::toggleState("previewFiles", input$file)})
  observe({shinyjs::toggleState("submitFiles", input$previewFiles)})
  
  preview_data <- eventReactive(input$previewFiles, {
    req(input$file)
    preview_df <- data_long()  
    preview_df
  })
  
  output$show_preview_table <- DT::renderDataTable(
    DT::datatable({
      preview_data()
    }, options = list(pageLength=5, scrollX = T)
    )
  )

  observe({shinyjs::toggleState("batch_student_list_report", input$batch_student_list)})

  parse_batch_student_list <- eventReactive(input$batch_student_list_report, {
    req(input$batch_student_list)
    input$batch_student_list %>%
      str_split("[, \n\t]+",simplify = T) %>% 
      t() %>% 
      as_tibble() %>% 
      select(student_no=V1) %>%
      # carefully parsing, empty missing student numbers from text area input
      filter(!is.na(student_no)) %>%
      filter(str_length(student_no) >= 8) %>%
      distinct(student_no)
  })

  output$batch_student_list_text <- render_gt(
    expr={ 
      dept_table_sql() %>%
        right_join(parse_batch_student_list(), by="student_no") %>%
        arrange(student_no) %>%
        # find rows which have all NA, i.e the student number is not found!
        mutate(not_found= if_else(if_all(where(is.numeric), is.na), 1, 0)) %>%
        prepare_batch_student_table()
  }, height = px(550)
  )
  
output$export_batch_student_report <- downloadHandler(
    filename = function() {
      # INFO pdf output is a huge table with single header, if printed from html you can have pagination
      paste0(paste(as.integer(as.POSIXct(Sys.time())), "batch-student-report", sep="-") , ".html")
    },
    content = function(file) {
      batch_student_table <- dept_table_sql() %>%
    right_join(parse_batch_student_list(), by="student_no") %>%
    arrange(student_no) %>% 
    # find rows which have all NA, i.e the student number is not found!
    mutate(not_found= if_else(if_all(where(is.numeric), is.na), 1, 0)) %>%
    prepare_batch_student_table()
      # zoom idea taken from https://github.com/rstudio/gt/issues/721#issuecomment-797479922
      #zoom=1 for pdf
      gtsave(batch_student_table, file)
    }
  )
  ### PERSISTENT DATA STORAGE SECTION ###
  
  save_data <- function(data){
    file_name_to_save <- paste0(janitor::make_clean_names(paste(input$select_department, input$select_term)), ".tsv")
    con <- dbConnect(RSQLite::SQLite(), userDB())
    dbWriteTable(conn = con, name = file_name_to_save, value = data, append = TRUE, overwrite = FALSE)
    dbDisconnect(con)
  }
  
  # save_submission_details <- function(){
  #   con <- dbConnect(RSQLite::SQLite(), userDB())
  #   user <- c(res_auth$user)
  #   dept <- c(input$select_department)
  #   term <- c(input$select_term)
  #   time <- c(get_time())
  #   details <- data.frame(user, dept, term, time)
  #   dbWriteTable(conn = con, name = "submission_details.tsv", value = details, append = TRUE)
  #   dbDisconnect(con)
  # }
  
  observeEvent(input$submitFiles, {
    save_data(data_long())
  })
  
  observeEvent(input$submitFiles, {
    showNotification("Submitted!", type = "message")
  })
  
  # observeEvent(input$submitFiles, {
  #   save_submission_details()
  # })
  
  
  output$submittedFiles <- renderPrint({
    input$submitFiles
    input$deleteTerm
    input$deleteCourse
    con <- dbConnect(RSQLite::SQLite(), userDB())
    table_names <- dbListTables(con)
    table_names <- table_names[!table_names %in% c("submission_details.tsv","pc_def","pc_matriks")]
    dbDisconnect(con)
    if(identical(table_names, character(0))){"No files have been submitted."}
    else{table_names}
  })
  
  load_data <- function(){
    con <- dbConnect(RSQLite::SQLite(), userDB())
    table_names <- dbListTables(con)
    table_names <- table_names[!table_names %in% c("submission_details.tsv","pc_def","pc_matriks")]
    tables <- lapply(table_names, dbReadTable, conn = con)
    # tables <- map(table_names, tbl, conn = con) # throws an error!
    merged <- bind_rows(tables)

    con2 <- dbConnect(RSQLite::SQLite(), userDBsabit())
    pc_def <- dbReadTable(conn=con2, "pc_def") %>% as_tibble() %>% select(pc_rank, pc_no) 
    merged <- 
      merged %>% 
      as_tibble() %>% 
      right_join(pc_def, by=c("PC"="pc_no")) %>% 
      mutate(PC=fct_reorder(PC, pc_rank)) %>% 
      select(-pc_rank)
    dbDisconnect(con)
    dbDisconnect(con2)

    #debug
    #saveRDS(merged, "merged_load_data.rds")
    merged
  }

  load_matrix_data <- function(){
    con <- dbConnect(RSQLite::SQLite(), userDBsabit())
    matriks_table <- dbReadTable(conn=con, "pc_matriks")
    pc_def <- dbReadTable(conn=con, "pc_def") %>% as_tibble() %>% select(pc_rank, pc_no)

    merged <- matriks_table %>% 
      as_tibble() %>% 
      right_join(pc_def, by=c("PC"="pc_no")) %>% 
      mutate(PC=fct_reorder(PC, pc_rank)) %>% 
      select(-pc_rank)

    dbDisconnect(con)

    merged  
}
  # load_submission_details <- function(){
  #   con <- dbConnect(RSQLite::SQLite(), userDB())
  #   details_table <- dbReadTable(con, "submission_details.tsv")
  #   dbDisconnect(con)
  #   details_table
  # }
  
  output$submitted_files_table <- DT::renderDataTable(
    DT::datatable({
      input$submitFiles
      input$deleteTerm
      input$deleteCourse
      load_data()
    }, options = list(pageLength = 5, scrollX = T)
    )
  )
  
  # output$submission_details <- DT::renderDataTable(
  #   DT::datatable({
  #     input$submitFiles
  #     load_submission_details()
  #   })
  # )
  
  
  # dept_table_sql <- reactive({
  #   input$submitFiles
  #   con <- dbConnect(RSQLite::SQLite(), userDB())
  #   table_names <- dbListTables(con)
  #   dept_table <- lapply(table_names, dbReadTable, conn = con) %>% 
  #     bind_rows() %>% 
  #     create_table_initial() %>% 
  #     create_department_table()
  #   dbDisconnect(con)
  #   dept_table
  # })
  
  dept_table_sql <- reactive({
    input$submitFiles
    input$deleteTerm
    input$deleteCourse
    dept_table <- load_data() %>%
      create_table_initial() %>%
      create_department_table()
    dept_table
  })
 
  #output$department_table_pc_matriks <- renderText("test")  

  # TODO in department tables we should have ALL PC
  # TODO order of columns for 12b 8a is WRONG
  # TODO the column header should be frozen
  output$department_table <- render_gt(
    expr = {
      dept_table_sql() %>% 
        gt(rowname_col = "student_no") %>%
        fmt_missing(columns = everything(), missing_text = "") %>% 
        #tab_header(
        #  title = md("**Department Report**")
        #) %>%
        tab_stubhead(label = "Student Number") %>% 
        dept_table_gt_options()      
    },
    height = px(550)
  )

output$department_summary_table <- render_gt(
    expr = {
      dept_table_sql() %>% 
        #INFO . is for any thing, we're filtering any thing, i.e everybody
        department_table_summary(".")
    },
    height = px(250)
  )
  
  
  
  output$department_table_en <- render_gt(
    expr = {
      department_table_en_gt <- dept_table_sql() %>% 
        filter_dept_table_sql("[0-9]+[ABCDEF][0-9]+","**Department Report (EN)**") %>%
        dept_table_gt_options()
      #debug
      #saveRDS(department_table_en_gt, "department_table_en_gt.rds")
      #saveRDS(dept_table_sql(), "department_table_sql.rds")
      department_table_en_gt 
    },
    height = px(550)
  )

  output$department_summary_table_en <- render_gt(
    expr = {
      dept_table_sql() %>% 
        department_table_summary("[0-9]+[ABCDEF][0-9]+")
    },
    height = px(250)
  )
  
output$export_department_table_en <- downloadHandler(
    filename = function() {
      # INFO pdf output is a huge table with single header, if printed from html you can have pagination
      paste0(paste(userDept(), "Department Summary EN", sep="-") , ".html")
    },
    content = function(file) {
      gt_dept_table_en <- dept_table_sql() %>%
        filter_dept_table_sql("[0-9]+[ABCDEF][0-9]+","**Department Report (EN)**") %>%
        dept_table_gt_options()

      # zoom idea taken from https://github.com/rstudio/gt/issues/721#issuecomment-797479922
      #zoom=1 for pdf
      gtsave(gt_dept_table_en, file)  
    }
  )
  
  output$department_table_tr <- render_gt(
    expr = {
      dept_table_sql() -> test_table
      # debug
      #saveRDS(test_table,"test_sql_table.rds")
      test_table %>% 
        # TODO unfortunately MatMuh TR is 052 and EN is 058, so ABCDEF wont work!!
        filter_dept_table_sql("[ABCÇDEF]","**Department Report (TR)**", negate=TRUE) %>%
        #filter(str_detect(student_no, "[ABCÇDEF]", negate=TRUE)) %>%
        #gt(rowname_col = "student_no") %>%
        #fmt_missing(columns = everything(), missing_text = "") %>% 
        #tab_header(
        #  title = md("**Department Report (TR)**")
        #) %>%
        #tab_stubhead(label = "Student Number") %>% 
        dept_table_gt_options()       
    },
    height = px(550)
  )

output$department_summary_table_tr <- render_gt(
    expr = {
      dept_table_sql() %>% 
        department_table_summary("[ABCÇDEF]", negate=TRUE)
    },
    height = px(250)
  )

output$export_department_table_tr <- downloadHandler(
    filename = function() {
      # INFO pdf output is a huge table with single header, if printed from html you can have pagination
      paste0(paste(userDept(), "Department Summary TR", sep="-") , ".html")
    },
    content = function(file) {
      gt_dept_table_tr <- dept_table_sql() %>%
        filter_dept_table_sql("[ABCÇDEF]","**Department Report (TR)**", negate=TRUE) %>%
        dept_table_gt_options()
      # zoom idea taken from https://github.com/rstudio/gt/issues/721#issuecomment-797479922
      #zoom=1 for pdf
      gtsave(gt_dept_table_tr, file)
    }
  )
  
  output$department_table_cap <- render_gt(
    expr = {
      dept_table_sql() %>% 
        filter_dept_table_sql("^Ç","**Department Report (ÇAP)**") %>%
        #filter(str_detect(student_no, "^Ç")) %>%
        dept_table_gt_options()       
    },
    height = px(550)
  )
  
  # course_stu_initial_table_sql <- reactive({
  #   input$submitFiles
  #   con <- dbConnect(RSQLite::SQLite(), userDB())
  #   table_names <- dbListTables(con)
  #   init_table <- lapply(table_names, dbReadTable, conn = con) %>% 
  #     bind_rows() %>% 
  #     create_table_initial_plus_by_course()
  #   dbDisconnect(con)
  #   init_table
  # })
  
  
  
  get_terms <- function(){
    if(is_empty(load_data())){return()}
    else{
      load_data() %>% 
        distinct(term)
    }
  }

    get_years <- function(){
    if(is_empty(load_data())){return()}
    else{
      load_data() %>% 
        distinct(term) %>% 
        mutate(year = str_extract(term, "\\d+-\\d+")) %>% 
        distinct(year)

    }
  }
  
  output$select_term_course_input <- renderUI({
    selectInput("select_term_course",
                "Select Term:",
                c(sort(as.character(get_terms()$term))))
    
  })

  output$select_year_course_input <- renderUI({
    selectInput("select_year_course",
                "Select Year:",
                c(sort(as.character(get_years()$year))))
    
  })

  output$select_term_matrix_input <- renderUI({
    selectInput("select_term_matrix",
                "Select Term:",
                get_available_terms())

  })
  
  observeEvent(c(input$submitFiles, input$deleteTerm, input$deleteCourse), {
    updateSelectInput(session,
                      "select_term_course",
                      "Select Term:",
                      choices = c(sort(as.character(get_terms()$term)))
    )
  })

    observeEvent(c(input$submitFiles, input$deleteTerm, input$deleteCourse), {
    updateSelectInput(session,
                      "select_year_course",
                      "Select Year:",
                      choices = c(sort(as.character(get_years()$year)))
    )
  })
  
  course_initial_table_sql <- reactive({
    input$submitFiles
    input$deleteTerm
    input$deleteCourse
    init_table <- load_data() %>%
      filter(term == input$select_term_course) %>% 
      create_table_initial_plus_by_course()
    # DEBUG
    #saveRDS(init_table, "init_table.rds")
    init_table
  })
  
  matrix_initial_table_sql <- reactive({
    init_table <- load_matrix_data() %>%
      filter(term == input$select_term_matrix)

    init_table
  })

  output$course_table <- render_gt(
    expr = {
      course_table <- create_course_table(course_initial_table_sql()) %>% 
        mutate(below_50 = rowSums(. < 50, na.rm = TRUE))
      # DEBUG
      #saveRDS(course_table, "course_table.rds")

      course_table %>%
      prepare_course_table()
    },
    height = px(550)
    
  )

  output$course_year_table <- render_gt(
    expr = {
      load_data() %>% 
        filter(str_detect(term, input$select_year_course)) %>% 
        select(-file) %>% 
        ## TODO load_data() tekrarlar içeriyor, aşağıdaki komut ile distinct hale geldiğinde veri ÇOK azalıyor
        ## TODO purge the database for duplicate entries
        distinct() %>% 
        mutate(yuzluk= (score/Puan) * 100) %>% 
        group_by(PC,course,student_no) %>% 
        summarize(ort_p=mean(yuzluk)) %>% 
        group_by(PC) %>% 
        summarize(`Puan Ortalaması`= round(mean(ort_p, na.rm=T), 2), 
                  `Öğrenci Sayısı`= n(), 
                  `Ders Sayısı`= n_distinct(course)) %>% 
          gt(rowname_col = "PC") %>% 
          cols_align(align = "center") %>% 
          tab_options(
              table.border.top.style = "none",
              # table.border.top.color = "black",
              # table.border.top.width = px(2),
              table.border.bottom.color = "black",
              table.border.bottom.width = px(2),
              table.font.size = px(12),
              heading.border.bottom.color = "black",
              heading.border.bottom.width = px(2),
              column_labels.border.bottom.color = "black",
              column_labels.border.bottom.width= px(2),
              stub.border.color = "black",
              stub.border.width = px(2),
              table_body.border.bottom.color = "black",
              table_body.border.bottom.width = px(2)
            ) %>%
            opt_row_striping() %>%
            tab_style(
              # TODO borders of sticky row is not sticky at all
              # please check https://stackoverflow.com/questions/50361698/border-style-do-not-work-with-sticky-position-element
              # for some css based solution
              style = css(position = "sticky", top = 0),
              locations = list(cells_column_labels(), cells_stubhead())
            ) %>%
            # TODO this does not work, ask somewhere else, is it possible to make borders sticky?
            tab_style(
              # TODO cell_borders(sides = c("top", "bottom"),color = "#BBBBBB",weight = px(1.5),style = "solid")
              style = css(position = "sticky", top = 0),
              locations = list(cells_column_labels(), cells_stubhead())          
            ) %>% 
            # TODO testing if 12px fits to PDF 
            tab_options(table.font.size = px(12))
        

       
    },
    height = px(550)
    
  )

# TODO needs lots of modifications
output$matrix_table <- render_gt(
    expr = {
      matrix_initial_table_sql() %>%
        select(-term, -DersTipi) %>% 
        #select(`Ders Kodu`=Kodu, `Ders Adı`=Ders, everything()) %>% 
        unite(Kodu, Ders, col="Course", sep=" ") %>%
        mutate(PC_var=1) %>% 
        pivot_wider(names_from = PC, values_from = PC_var, names_sort = TRUE) %>% 
        # arrange course names according to 4th (year) and 1st letter and finally ders kodu
        arrange(str_sub(Course,4,4), str_sub(Course,1,1), Course) %>%
        gt(rowname_col = "Course") %>% 
        tab_stubhead(label = "Course") %>%
        fmt_missing(columns = everything(), missing_text = "") %>% 
        dept_table_gt_options()
          },
    height = px(550)

  )
  
  student_initial_table_sql <- reactive({
    input$submitFiles
    input$deleteTerm
    input$deleteCourse
    init_table <- load_data() %>%
      create_table_initial_plus_by_course()
    init_table
  })
  
  output$student_select_input <- renderUI({
    selectInput(
      "select_student",
      "Select student number:",
      multiple = FALSE,
      choices = c(unique(sort(as.character(student_initial_table_sql()$student_no))))
    )
  })
  
  output$student_table <- render_gt(
    expr = {
      student_report()
    },
    height = px(550)
  )
  
  student_report <- reactive({
    create_student_table(student_initial_table_sql(), input$select_student) %>% 
      gt(rowname_col = "course") %>%
      fmt_missing(columns = everything(), missing_text = "") %>% 
      tab_header(
        title = md("**Student Report**"),
        subtitle = md(paste("Student number:", input$select_student))
      ) %>%
      tab_stubhead(label = "Courses") %>% 
      dept_table_gt_options()
  })

  batch_student_report <- reactive({
    create_student_table(student_initial_table_sql(), input$batch_student_list) %>% 
      gt(rowname_col = "course") %>%
      fmt_missing(columns = everything(), missing_text = "") %>% 
      tab_header(
        title = md("**Batch Student Report**")
      ) %>%
      tab_stubhead(label = "Courses") %>% 
      dept_table_gt_options()
  })
  
  output$export_student_report <- downloadHandler(
    filename = function() {
      paste0(paste(userDept(), input$select_student, "Student Report", sep="-") , ".pdf")
    },
    content = function(file) {
      # zoom idea taken from https://github.com/rstudio/gt/issues/721#issuecomment-797479922
      gtsave(student_report(), file, zoom=1)
      # vwidth = 700,
      # vheight = 300
    }
  )
  
  
  
  output$export_student_reports_all <- downloadHandler(
    filename = function() {
      paste0(paste(userDept(), "Student Reports", sep="-") , ".zip")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      files <- c()
      
      for (student in unique(as.character(student_initial_table_sql()$student_no))) {
        name <- paste0(paste(userDept(), student, "Student Report", sep="-") , ".pdf")
        gtsave(
          create_student_table(student_initial_table_sql(), student) %>% 
            gt(rowname_col = "course") %>%
            fmt_missing(columns = everything(), missing_text = "") %>% 
            tab_header(
              title = md("**Student Report**"),
              subtitle = md(paste("Student number:", student))
            ) %>%
            tab_stubhead(label = "Courses") %>% 
            dept_table_gt_options(),
          # zoom idea taken from https://github.com/rstudio/gt/issues/721#issuecomment-797479922
          name, zoom=1
        )
        files <- c(files, name)
      }
      
      zip(file, files)
    }
  )
  
  
  
  
  
  output$fluidrow_parsed_options <- renderUI({
    fluidRow(
      column(4,
             selectInput("course",
                         "Select Course:",
                         c("All",
                           unique(sort(as.character(data_safe()$file)))))
      ),
      column(4,
             selectInput("course_code",
                         "Select Course Code:",
                         c("All",
                           unique(sort(as.character(data_safe()$course)))))
      ),
      column(4,
             selectInput("student",
                         "Select Student Number:",
                         c("All",
                           unique(sort(as.character(data_safe()$student_no)))))
      )
    )
  })
  
  
  
  ### DELETE TERM ###
  
  output$select_term_edit_dterm_input <- renderUI({
    selectInput("select_term_edit_dterm",
                "Select Term:",
                c(sort(as.character(get_terms()$term))))
  })
  
  observeEvent(c(input$submitFiles, input$deleteTerm, input$deleteCourse), {
    updateSelectInput(session,
                      "select_term_edit_dterm",
                      "Select Term:",
                      choices = c(sort(as.character(get_terms()$term)))
    )
  })
  
  
  output$submitted_files_table_edit_dterm <- DT::renderDataTable(
    DT::datatable({
      input$submitFiles
      input$select_term_edit_dterm
      input$deleteTerm
      input$deleteCourse
      con <- dbConnect(RSQLite::SQLite(), userDB())
      table <- dbReadTable(con, paste0(janitor::make_clean_names(paste(userDept(), input$select_term_edit_dterm)), ".tsv"))
      dbDisconnect(con)
      table
    }, options = list(pageLength = 5, scrollX = T)
    )
  )
  
  
  delete_term <- function(){
    con <- dbConnect(RSQLite::SQLite(), userDB())
    dbRemoveTable(con, paste0(janitor::make_clean_names(paste(userDept(), input$select_term_edit_dterm)), ".tsv"))
    dbDisconnect(con)
  }
  
  observe({shinyjs::toggleState("deleteTerm", input$select_term_edit_dterm)})
  
  observeEvent(input$deleteTerm, {
    delete_term()
  })
  
  observeEvent(input$deleteTerm, {
    showNotification("Term is deleted!", type = "message")
  })
  
  
  
  ### DELETE COURSE ###
  
  
  get_course_codes <- function(){
    req(input$select_term_edit_dcourse)
    if(is_empty(load_data())){return()}
    else{
      load_data() %>%
        filter(term == as.character(input$select_term_edit_dcourse)) %>% 
        distinct(course)
    }
  }
  

  output$select_term_edit_dcourse_input <- renderUI({
    selectInput("select_term_edit_dcourse",
                "Select Term:",
                c(sort(as.character(get_terms()$term))))
  })
  
  output$select_course_edit_dcourse_input <- renderUI({
    selectInput("select_course_edit_dcourse",
                "Select Course Code:",
                c(sort(as.character(get_course_codes()$course))))
  })
  
  
  observeEvent(c(input$submitFiles, input$deleteCourse, input$deleteTerm), {
    updateSelectInput(session,
                      "select_term_edit_dcourse",
                      "Select Term:",
                      c(sort(as.character(get_terms()$term))))
    
  })
  
  
  observeEvent(c(input$submitFiles, input$deleteCourse, input$deleteTerm, input$select_term_edit_dcourse), {
    updateSelectInput(session,
                      "select_course_edit_dcourse",
                      "Select Course Code:",
                      c(sort(as.character(get_course_codes()$course)))
    )
  })
  
  
  
  output$submitted_files_table_edit_dcourse <- DT::renderDataTable(
    DT::datatable({
      input$submitFiles
      input$select_term_edit_dcourse
      input$select_course_edit_dcourse
      input$deleteTerm
      input$deleteCourse
      con <- dbConnect(RSQLite::SQLite(), userDB())
      table <- dbReadTable(con, paste0(janitor::make_clean_names(paste(userDept(), input$select_term_edit_dcourse)), ".tsv"))
      dbDisconnect(con)
      table %>% 
        filter(course == as.character(input$select_course_edit_dcourse))
    }, options = list(pageLength = 5, scrollX = T)
    )
  )
  
  
  delete_course <- function(){
    con <- dbConnect(RSQLite::SQLite(), userDB())
    table <- paste0(janitor::make_clean_names(paste(userDept(), input$select_term_edit_dcourse)), ".tsv")
    request <- as.character(input$select_course_edit_dcourse)
    dbExecute(con, paste("DELETE FROM", paste0("'", table, "'"), "WHERE course =", paste0("'", request, "'"), ";"))
    dbDisconnect(con)
  }
  
  observe({shinyjs::toggleState("deleteCourse", input$select_course_edit_dcourse)})
  
  observeEvent(input$deleteCourse, {
    delete_course()
  })
  
  observeEvent(input$deleteCourse, {
    showNotification("Course is deleted!", type = "message")
  })
  
  
  
}

shinyApp(ui = ui, server = server)
