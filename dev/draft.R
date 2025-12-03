files <- list.files(file.path('examples'), full.names = TRUE)
txt <- vapply(files, readr::read_file, character(1))

prompts <- get_prompts(
    'domain',
    lang = 'ru',
    text = txt[1:3],
    domain = c('социолог', 'политолог', 'спелеолог'),
    target = 'Роскомнадзор',
    target_type = 'object'
)

inputs <- list(
    lang = 'ru',
    texts = txt[1:3],
    domain_roles = c('социолог', 'политолог', 'спелеолог'),
    targets = 'Роскомнадзор',
    target_types = 'object'
)

prepare_expert_chats(inputs, chat_base, expert_role = 'domain')

