parse_all_sheets <- function(file) {
  #file <- "Dr_Yeliz Elalmış.xlsx"
  #sheet <- "BYM4081-GR1"
  #sheet <- "BYM4451-GR1"
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
    rename(`Ölçme Yöntemi`= any_of(c("ölcme yöntemi", "ölçme yöntemi", "Ölçme\r\nYöntemi", "Ölçme \r\nYöntemi"))) %>%
    gather(method, value, -`Ölçme Yöntemi`) %>% 
    spread(`Ölçme Yöntemi`, value)
  
  df %>% 
    filter(row_number() > 2) %>% 
    select(-starts_with("..")) %>% 
    rename(`Ölçme Yöntemi` = any_of(c("ölcme yöntemi", "ölçme yöntemi", "Ölçme\r\nYöntemi", "Ölçme \r\nYöntemi"))) %>%
    rename(student_no = `Ölçme Yöntemi`) %>% 
    gather(method, score, -student_no) %>% 
    left_join(methods, by = c("method")) %>% 
    mutate(method = gsub("[[:punct:]]{3}[[:digit:]]", "" , method)) %>%  
    rename(PC = any_of(c("PÇ", "PC", "P.C", "P.C", "P.Ç.", "P.Ç"))) %>% 
    # TODO we need general rule here for 12.1 3a 3.b 10-1
    #mutate(PC = round(as.numeric(PC), digits = 4)) %>%   # for rounding errors
    rename(Puan = starts_with("Puan")) %>%
    mutate(score = as.numeric(score),
           Puan = as.numeric(Puan),
           PC = as.character(PC)) %>% 
    mutate(score = ifelse(is.na(score) & !str_detect(method, regex("Bütünleme", ignore_case = TRUE)), 0, score)) %>% 
    mutate(score = ifelse(score == -1, NA, score)) %>% 
    filter(is.na(student_no) | !str_detect(student_no, "^F|^E"))
}


clean_course <- function(course_column){
  course_column %>% 
    str_replace_all("[[:punct:]]", "") %>% 
    toupper() %>% 
    gsub("([[:digit:]]+)\\s*([[:alpha:]]+)", "\\1-\\2", .) %>% 
    gsub("([[:alpha:]]+)\\s*([[:digit:]]+)", "\\1-\\2", .)
}

standardize_course_names <- function(parsed_dataframe){
  parsed_dataframe %>%
    mutate(course = clean_course(course))
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
    mutate(PC = as.numeric(PC)) %>% 
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
    mutate(PC = as.numeric(PC)) %>%
    distinct(course, PC, pass_fail_perc) %>% 
    arrange(desc(course)) %>% 
    pivot_wider(names_from = PC, values_from = pass_fail_perc, names_sort = TRUE)
}

create_student_table <- function(dataframe_initial_course, student_number){
  dataframe_initial_course %>%  
    distinct(course, student_no, PC, pass_fail) %>% 
    dplyr::filter(student_no %in% student_number) %>% 
    mutate(PC = as.numeric(PC)) %>%
    arrange(desc(course)) %>% 
    select(-student_no) %>% 
    pivot_wider(names_from = PC, values_from = pass_fail, names_sort = TRUE)
}

get_time <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}
