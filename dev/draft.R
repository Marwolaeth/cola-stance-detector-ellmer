# Multiple Domains ----
llm_stance(
    c(
        'Роскомнадзор хороший',
        'Думаю, это правда: Роскомнадзор защищает пользователей',
        'Роскомндзор плохой'
    ),
    domain_role = c('социолог', 'политолог', 'спелеолог'),
    target = c('Роскомнадзор', 'Роскомнадзор защищает пользователей', 'Роскомнадзор'),
    type = c('object', 'statement', 'object'),
    language = 'ru',
    chat_base = chat_base
)

# Additional Prompts ----
texts <- c(
    '"Не ходите в этот вражеский интырнет, особенно с ВПНами" - сказал депутат, чьи дети живут в Париже и ни в чем себе не отказывают',
    'Эксперты в один голос утверждают: использование VPN - не решение проблем, а огромный риск',
    'Сегодня наш сайт заблокировал Роскомнадзор. Скоро переедем, а пока подберите себе нормальный VPN на всякий случай'
)

res <- llm_stance(
    texts,
    target = 'Использовать VPN опасно',
    chat_base = chat_base,
    type = 'statement',
    language = 'ru',
    domain_role = 'политический обозреватель',
    verbose = TRUE
)

# Multiple chats ----
openrouter_key <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('OPENROUTER_API_KEY')
    ))
}

chat_analysis <- chat_openrouter(
    model = 'amazon/nova-2-lite-v1:free',
    credentials = openrouter_key,
    api_args = list(temperature = 0, max_tokens = 3000)
)

chat_decision <- chat_openrouter(
    # model = 'tngtech/tng-r1t-chimera:free',
    model = 'mistralai/devstral-2512:free',
    credentials = openrouter_key,
    api_args = list(temperature = 0)
)

# see examples.R
indices <- 1:2
# indices <- c(1, 3:11)
res_ru <- llm_stance(
    # text = test_data_ru$text[indices],
    text = c(
        "Роскомнадзор — позор России",
        "Роскомнадзор заблокировал Roblox. Наконец эту помойку прикрыли, спасибо РКН",
        "Роскомнадзор подготовил законопроект"
    ),
    target = 'Роскомнадзор',
    type = 'object',
    scale = 'likert',
    # chat_base = chat_decision,
    chat_base = list(chat_analysis, chat_decision),
    # prompts_dir = 'prompts/custom',
    # verbose = FALSE,
    language = 'ru',
    domain_role = 'политический обозреватель'
)

# Providers ----
## Google ----
chat_decision <- chat_google_gemini(
    model = 'gemini-2.5-flash',
    api_headers = c(task_type = 'CLASSIFICATION')
)

## MWS ----
oc_credentials <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('HUGGINGFACE_API_KEY')
    ))
}

chat_base <- chat_openai_compatible(
    base_url = 'https://router.huggingface.co/v1',
    # name = 'Featherless AI',
    name = 'novita',
    credentials = oc_credentials,
    # model = 'MTSAIR/Cotype-Nano:featherless-ai',
    model = 'deepseek-ai/DeepSeek-V3.2:novita',
    api_args = list(temperature = 0)
)

chat_base$chat('Здравствуйте', echo = 'none')

## Yandex ----
YANDEX_CLOUD_FOLDER = "b1gsm63ta97sfo8jpbgu"
# YANDEX_CLOUD_API_KEY = "<API_key_value>"
YANDEX_CLOUD_MODEL = "aliceai-llm/latest"

ya_credentials <- function() {
    list(Authorization = paste(
        'Api-Key', Sys.getenv('YANDEX_API_KEY')
    ))
}

chat_base <- chat_openai_compatible(
    base_url="https://rest-assistant.api.cloud.yandex.net/v1",
    name = '',
    credentials = ya_credentials,
    # model = glue::glue('gpt://{YANDEX_CLOUD_FOLDER}/{YANDEX_CLOUD_MODEL}'),
    model = 'gpt://b1gsm63ta97sfo8jpbgu/aliceai-llm/latest',
    api_headers = c('OpenAI-Project' = YANDEX_CLOUD_FOLDER)
)

# Unicode ----
cat("\u00B5\n")
cat('\U1F4CA')
cat('\U1F50D')

# Tibbles ----
res_en <- test_data_en |>
    dplyr::mutate(
        role = c('political scientist', 'political scientist', 'policeman')
    ) |>
    llm_stance(
        tweet,
        target,
        target_type,
        role,
        chat_base = list(chat_analysis, chat_decision),
    )
str(res_en)
