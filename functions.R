parse_all_sheets <- function(file) {

  file %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map_df(
      ~ parse_sheet(file, .x),
      .id = "course")  
  # mutate(instructor= file) %>% 
  # select(instructor, everything())
}

parse_sheet <- function(file, sheet){
  df <- read_excel(file, sheet)  

  methods <- df %>% 
    head(2) %>%
    janitor::remove_empty("cols") %>% 
    rename_with(~ "Ölçme Yöntemi", 
                matches("[ÖöOo][Ll][ÇçCc][Mm][Ee][ \r\n]*[Yy][ÖöOo][Nn][Tt][Ee][Mm][Iıİi] *")) %>% 
    #gather(method, value, -`Ölçme Yöntemi`)
    pivot_longer(-`Ölçme Yöntemi`,
                 names_to = "method", 
                 values_to = "value")  %>% 
    #spread(`Ölçme Yöntemi`, value)
    pivot_wider(names_from = `Ölçme Yöntemi`, 
                values_from = value ) %>% 
    rename_with(~ "PC", matches("[Pp][ \\.]*[ÇçCc][ \\.]*"))

  df %>% 
    filter(row_number() > 2) %>% 
    select(-starts_with("..")) %>% 
    rename_with(~ "Ölçme Yöntemi", 
                matches("[ÖöOo][Ll][ÇçCc][Mm][Ee][ \r\n]*[Yy][ÖöOo][Nn][Tt][Ee][Mm][Iıİi] *")) %>% 
    rename(student_no = `Ölçme Yöntemi`) %>%
    # ARGH!! some Excel cells have scientific notation on!!!
    mutate(student_no = if_else(str_detect(student_no,"^[0-9]\\.[0-9]*E[0-9]"),as.character(as.numeric(student_no)), student_no)) %>%
    mutate(student_no = as.character(student_no)) %>% 
    mutate(student_no=str_trim(student_no, side = "both")) %>%
    # TODO some student numbers have tab character (ex: 1705A054, 1705A055, 1705A703, etc.)
    # department codes
    # biyomuh EN 05A,biyomuh TR 056,kimyamuh EN 05B,kimyamuh TR 051,gidamuh TR 057,metalmuh EN 05C,metalmuh TR 054,matmuh EN 058,matmuh TR 052
    # TODO ideally we should extract numbers with 056 05A, otherwise we'll get foreign student numbers
    mutate(student_no = str_extract(student_no,"Ç*[0-9]{2}05[124678ABC][0-9]+")) %>%
    pivot_longer(-student_no, names_to = "method", values_to = "score") %>% 
    mutate(score = str_trim(score)) %>% 
    left_join(methods, by = c("method")) %>% 
    # let's deal with multiple PC's in a single cell. WARNING should this be allowed in the first place?
    separate_rows(PC,sep = ",") %>% 
    mutate(PC=str_trim(PC)) %>%
    # multiple PC end
    mutate(method = gsub("[[:punct:]]{3}[[:digit:]]", "" , method)) %>% 
    # TODO we need general rule here for 12.1 3a 3.b 10-1
    #mutate(PC = round(as.numeric(PC), digits = 4)) %>%   # for rounding errors
    mutate(PC = as.character(PC)) %>%
    mutate(PC = if_else(str_detect(PC,"\\."),
                        str_extract(PC,"^[0-9]+[ \\._][0-9]"), 
                        PC)) %>% 
    mutate(PC = str_replace(PC, "_","\\.")) %>%
    rename(Puan = starts_with("Puan")) %>%
    mutate(score = as.numeric(score),
           Puan = as.numeric(Puan),
           PC = as.character(PC)) %>% 
    mutate(score = ifelse(is.na(score) & !str_detect(method, regex("Bütünleme", ignore_case = TRUE)), 0, score)) %>% 
    mutate(score = ifelse(score == -1, NA, score)) %>% 
    # exclude Erasmus or Farabi students
    filter(is.na(student_no) | !str_detect(student_no, "^F|^E")) %>%
    # TODO we should warn user about this, rows with very short student numbers are removed
    filter(str_length(student_no) >= 8)  
}


clean_course <- function(course_column){
  #print(paste0("Course name is:",course_column))
  
  course_column %>% 
    str_replace_all("[[:punct:]]", "") %>% 
    toupper() %>% 
    gsub("([[:digit:]]+)\\s*([[:alpha:]]+)", "\\1-\\2", .) %>% 
    gsub("([[:alpha:]]+)\\s*([[:digit:]]+)", "\\1-\\2", .)
}

standardize_course_names <- function(parsed_dataframe){
  #saveRDS(parsed_dataframe,"test.rds")
  parsed_dataframe %>%
    dplyr::mutate(course = clean_course(course))
}

safe_parse_all_sheets <- safely(.f = parse_all_sheets)

student_count <- function(dataframe){
  dataframe %>% 
    group_by(student_no) %>% 
    count(name = "count")
}

# For file names inside Datapath (Shiny)
fix_file_names <- function(file) {
  old_name = file[["datapath"]]
  new_name = file.path(dirname(file[["datapath"]]),
                       file[["name"]])
  file.rename(from = old_name, to = new_name)
  file[["datapath"]] <- new_name
  file
}

create_table_initial <- function(dataframe){
  dataframe %>% 
    group_by(student_no, PC) %>% 
    mutate(pass_fail = case_when(max(score, na.rm = TRUE) == -Inf ~ NA_real_,
                                 max(score, na.rm = TRUE) >= 0.4 ~ 1,
                                 TRUE ~ 0)) %>%
    ungroup()
}

create_department_table <- function(dataframe_initial){
  dataframe_initial  %>% 
    distinct(student_no, PC, pass_fail) %>% 
    #mutate(PC = as.numeric(PC)) %>% 
    spread(PC, pass_fail)
}
# negate is used by department table TR, the negated regex is used.
# default negate is FALSE unless otherwise is requested
filter_dept_table_sql <- function(df, regex, title, negate=FALSE) {
  df %>% 
    # TODO for Eng departments what are the letters?
    # TODO what about ÇAP students which start with Ç
    filter(str_detect(student_no, regex, negate=negate)) %>%
    gt(rowname_col = "student_no") %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    # tab header becomes sticky in PDF so, we omit that 2022.04.20
    #tab_header(
    #  title = md(title)
    #  ) %>%
    tab_stubhead(label = "Student Number")
} 

department_table_summary <- function(df, regex, negate=FALSE){
  
  ones_zeros <- list(
    Success = ~ sum(.x==1, na.rm=T),
    Fail = ~ sum(.x==0, na.rm=T),
    Ratio = ~ sum(.x==1, na.rm=T)/sum(!is.na(.x))
    )
  
  df %>% 
  filter(str_detect(student_no, regex, negate=negate)) %>%
  select(-student_no) %>%  
  summarize(across(everything(), ones_zeros)) %>% 
  pivot_longer(everything(), names_to = "key", values_to = "value") %>% 
  separate(key, into=c("pc","success"), sep = "_") %>% 
  pivot_wider(names_from = pc, values_from = value) %>% 
  gt(rowname_col = "success") %>% 
  tab_stubhead(label = "Success/Fail Cases") %>% 
  cols_align(align = "center") %>% 
  fmt_number(columns=everything(), row = 3, decimals = 2) %>%
  fmt_number(columns=everything(), row = 1:2, decimals = 0) %>%
  tab_source_note(
    source_note = "Ratio is equal to Success cases over total cases"
    ) %>%
  tab_options(
          table.border.top.style = "none",
          table.border.bottom.color = "black",
          table.border.bottom.width = px(2),
          table.font.size = px(14),
          heading.border.bottom.color = "black",
          heading.border.bottom.width = px(2),
          column_labels.border.bottom.color = "black",
          column_labels.border.bottom.width= px(2),
          stub.border.color = "black",
          stub.border.width = px(2),
          table_body.border.bottom.color = "black",
          table_body.border.bottom.width = px(2)
        ) %>%
  opt_row_striping()

}


create_table_initial_plus_by_course <- function(dataframe){
  dataframe %>% 
    group_by(course, student_no, PC) %>% 
    mutate(pass_fail = case_when(max(score/Puan, na.rm = TRUE) == -Inf ~ NA_real_,  
                                 max(score/Puan, na.rm = TRUE) >= 0.4 ~ 1,
                                 TRUE ~ 0)) %>%
    ungroup()
}

create_course_table <- function(dataframe_initial_course){
  
  #debug
  saveRDS(dataframe_initial_course, "dataframe_initial_course.rds")
  
  dataframe_initial_course %>% 
    distinct(course, student_no, PC, pass_fail) %>% 
    group_by(course, PC) %>%
    mutate(pass_fail_perc = round(sum(pass_fail, na.rm = TRUE)/n_distinct(student_no) * 100, digits = 1)) %>%
    ungroup() %>% 
    #mutate(PC = as.numeric(PC)) %>%
    distinct(course, PC, pass_fail_perc) %>% 
    arrange(desc(course)) %>% 
    pivot_wider(names_from = PC, values_from = pass_fail_perc, names_sort = TRUE)
}

prepare_course_table <- function(df){

df %>%
  gt(rowname_col = "course") %>%
        fmt_missing(columns = everything(), missing_text = "") %>% 
        tab_header(
          title = md("**Percentage of Successful Students**"),
          subtitle = md("success criterion: *score >= 40*")
        ) %>%
        tab_stubhead(label = "Courses") %>% 
        tab_spanner(
          label = "PÇ (%)",
          columns = everything()
        ) %>% 
        cols_align(
          align = "center"
        ) %>% 
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
          style = list(cell_fill(color = "#F02241"), #cell_fill(color = "red"),
                       cell_text(color = "white")),
          locations = cells_stub( 
        # TODO these below_50 column names should not be quoted! check examples at 
        # https://search.r-project.org/CRAN/refmans/gt/html/tab_style.html
            rows = "below_50" >= 1)) %>% 
        cols_hide("below_50")

}

create_student_table <- function(dataframe_initial_course, student_number){
  dataframe_initial_course %>%  
    distinct(course, student_no, PC, pass_fail) %>% 
    dplyr::filter(student_no %in% student_number) %>% 
    #mutate(PC = as.numeric(PC)) %>%
    arrange(desc(course)) %>% 
    select(-student_no) %>% 
    pivot_wider(names_from = PC, values_from = pass_fail, names_sort = TRUE)
}

get_time <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

get_available_terms <- function() {
# reverse order is better, the latest term will be selected as default
c("2022-2023 BAHAR", "2022-2023 GÜZ", "2021-2022 BAHAR", "2021-2022 GÜZ",  "2020-2021 BAHAR", "2020-2021 GÜZ", "2019-2020 BAHAR",  "2019-2020 GÜZ","2018-2019 BAHAR", "2018-2019 GÜZ")

}

dept_table_gt_options <- function(data){
data %>% 
  #tab_spanner(
  #        label = "PÇ",
  #        columns = everything()
  #      ) %>% 
        cols_align(
          align = "center"
        ) %>% 
        tab_source_note(
          source_note = "1: passed, 0: failed"
        ) %>% 
        tab_options(
          table.border.top.style = "none",
          table.border.bottom.color = "black",
          table.border.bottom.width = px(2),
          table.font.size = px(14),
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
        tab_options(table.font.size = px(12)) %>%
        # taken from https://www.linkedin.com/pulse/use-font-awesome-gt-tabular-data-yinghui-liu
        text_transform(locations=cells_body(everything()),
                       fn = function(x) {
                         # fa-regular or fa-thin didn't work probably not free
                         dplyr::case_when(x=="1" ~ "<svg width='16px' height='16px' viewBox='0 0 512 512' xmlns='http://www.w3.org/2000/svg'><title>ionicons-v5-e</title><polyline points='416 128 192 384 96 288' style='fill:none;stroke:#000;stroke-linecap:round;stroke-linejoin:round;stroke-width:32px'/></svg>", 
                                          x=="0" ~ "<svg version='1.1' id='Capa_1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' x='0px' y='0px' width='16px' height='16px' 
	 viewBox='0 0 50 50' style='enable-background:new 0 0 50 50;' xml:space='preserve'>
<circle style='fill:#D75A4A;' cx='25' cy='25' r='25'/>
<polyline style='fill:none;stroke:#FFFFFF;stroke-width:2;stroke-linecap:round;stroke-miterlimit:10;' points='16,34 25,25 34,16'/>
<polyline style='fill:none;stroke:#FFFFFF;stroke-width:2;stroke-linecap:round;stroke-miterlimit:10;' points='16,16 25,25 34,34'/></svg> ",
                                          TRUE ~ x)
                       }
        )  
}
prepare_batch_student_table <- function(df){
  df %>%
    gt(rowname_col = "student_no") %>%
    fmt_missing(columns = everything(), missing_text = "") %>%
    tab_stubhead(label = "Student Number") %>%
    dept_table_gt_options() %>%
    tab_style(
      style = list(
        cell_fill(color = "red"),
        cell_text(color = "white")
        ),
      locations = cells_stub(rows= not_found == 1)) %>%
    cols_hide("not_found")
}

determine_dept <- function(student_no){ 
  # TODO this is not elegant, might fail frequently
  # so, the database should have student names without any trailing spaces
  # parsing is done from the end due to CAP students having "Ç" at the beginning
  dept <- str_sub(student_no, start = -6, end = -4)
  result <- case_when(
    dept == "05A" ~ "Bioengineering EN",
    dept == "056" ~ "Bioengineering TR",
    dept == "05B" ~ "Chemical Engineering EN",
    dept == "051" ~ "Chemical Engineering TR",
    dept == "05C" ~ "Metallurgical and Materials Engineering EN",
    dept == "054" ~ "Metallurgical and Materials Engineering TR",
    dept == "058" ~ "Mathematical Engineering EN",
    dept == "052" ~ "Mathematical Engineering TR",
    dept == "057" ~ "Food Engineering TR",
    TRUE ~ "Unknown"
  )
  
  result
  
 }
