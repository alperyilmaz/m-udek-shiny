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
    # TODO some student numbers have tab character (ex: 1705A054, 1705A055, 1705A703, etc.)
    # TODO ideally we should extract numbers with 056 05A, otherwise we'll get foreign student numbers
    mutate(student_no = str_extract(student_no,"Ç*[0-9]+[ABCDEF][0-9]+") %>%
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


create_table_initial_plus_by_course <- function(dataframe){
  dataframe %>% 
    group_by(course, student_no, PC) %>% 
    mutate(pass_fail = case_when(max(score/Puan, na.rm = TRUE) == -Inf ~ NA_real_,  
                                 max(score/Puan, na.rm = TRUE) >= 0.4 ~ 1,
                                 TRUE ~ 0)) %>%
    ungroup()
}

create_course_table <- function(dataframe_initial_course){
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

dept_table_gt_options <- function(data){
data %>% 
  tab_spanner(
          label = "PÇ",
          columns = everything()
        ) %>% 
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
        opt_row_striping()
}
