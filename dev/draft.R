files <- list.files(file.path('examples'), full.names = TRUE)
txt <- vapply(files, readr::read_file, character(1))

domain_tasks <- get_prompts(
    'domain',
    lang = 'ru',
    text = txt[1:3],
    domain = c('социолог', 'политолог', 'спелеолог'),
    target = 'Роскомнадзор',
    target_type = 'object'
)
