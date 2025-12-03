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
    types = 'object',
    target_types = 'объекту'
)

prepare_expert_chats(inputs, chat_base, expert_role = 'domain')

inputs <- list(
    lang = 'ru',
    texts = c(
        'Роскомнадзор хороший',
        'Думаю, это правда: Роскомнадзор защищает пользователей',
        'Роскомндзор плохой'
    ),
    domain_roles = c('социолог', 'политолог', 'спелеолог'),
    targets = c('Роскомнадзор', 'Роскомнадзор защищает пользователей', 'Роскомнадзор'),
    types = c('object', 'statement', 'object'),
    target_types = sapply(c('object', 'statement', 'object'), type_to_term, lang = 'ru')
)

llm_stance(
    c(
        'Роскомнадзор хороший',
        'Думаю, это правда: Роскомнадзор защищает пользователей',
        'Роскомндзор плохой'
    ),
    domain_role = c('социолог', 'политолог', 'спелеолог'),
    target = c('Роскомнадзор', 'Роскомнадзор защищает пользователей', 'Роскомнадзор'),
    type = c('object', 'statement', 'object'),
    lang = 'ru',
    chat_base = chat_base
)
