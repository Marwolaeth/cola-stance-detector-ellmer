### AI GENERATED ###
# Установка ellmer (если необходимо)
# remotes::install_github('tidyverse/ellmer')

library(ellmer)
library(glue)
# library(stringr) # Для удобной работы со строками

# --- Настройка базового LLM ----
# Предполагается, что у вас настроен ключ API (например, переменная среды OPENAI_API_KEY)

# Создаем базовый объект чата.
# Мы будем клонировать его для каждого агента, чтобы избежать загрязнения истории.
# ВНИМАНИЕ: Для сложных задач, как COLA, рекомендуется использовать более мощные модели (GPT-4).
openrouter_key <- function() {
    list(Authorization = paste(
        'Bearer', Sys.getenv('OPENROUTER_API_KEY')
    ))
}

chat_base <- chat_openrouter(
    model = 'openai/gpt-oss-20b:free',
    credentials = openrouter_key,
    api_args = list(temperature = 0)
)

# Схема анализа ----
type_stance <- type_enum(
    values = c('Positive', 'Negative', 'Neutral'),
    description = "Окончательно определённая позиция автора по отношению к цели."
)

type_analysis <- type_object(
    stance = type_stance,
    explanation = type_string(
        description = 'Поясните ваше решение о классификации позиции автора в одном абзаце.'
    )
)

# Служебные функции ----
type_to_term <- function(type = c('object', 'statement')) {
    type <- match.arg(type, c('object', 'statement'), several.ok = FALSE)
    
    switch(
        type,
        object = 'объекту',
        statement = 'утверждению'
    )
}

get_prompts <- function(role, ...) {
    template_system <- file.path('prompts', glue::glue('system-{role}.md'))
    template_user  <- file.path('prompts', glue::glue('user-{role}.md'))
    
    list(
        system = interpolate_file(template_system, ...),
        task = interpolate_file(template_user, ...)
    )
}

execute_prompts <- function(chat, prompts) {
    # Проверка чата
    if (!ellmer:::is_chat(chat) | length(chat$get_turns()) != 0) {
        stop('Invalid `chat` argument: must be an empty Chat object.')
    }
    
    # Проверка инструкций
    if (!is.list(prompts) | !all.equal(c('system', 'task'), names(prompts))) {
        stop('Wrong `prompts` argument: must be a list with two slots.')
    }
    
    # Присваемваем модели роль
    chat$set_system_prompt(prompts$system)
    
    return(chat$chat(prompts$task, echo = "none"))
}

# Этап 1: Многомерный анализ текста (Эксперты) ----
## Функция-конструктор ----
analyse <- function(
        text,
        chat_base,
        role = c('linguist', 'domain', 'veteran'),
        ...
    ) {
    role <- match.arg(
        role,
        c('linguist', 'domain', 'veteran'),
        several.ok = FALSE
    )
    
    # Клонируем чат для нового, чистого взаимодействия
    chat <- chat_base$clone()
    
    # Загружаем инструкции
    prompts <- get_prompts(role, text = text, ...)
    
    # Исполняем инструкции
    execute_prompts(chat, prompts)
}

# Этап 2: Дебаты с улучшенным логическим выводом (Дебатеры) ----
# Функция для проведения дебатов по конкретной позиции
debate_stance <- function(
        chat_base,
        text,
        target,
        stance,
        analysis_results,
        type = c('object', 'statement')
    ) {
    target_type <- type_to_term(type)
    
    chat <- chat_base$clone()
    
    # Объединяем результаты анализа в одну строку для промпта
    LingResponse <- analysis_results$linguistic
    ExpertResponse <- analysis_results$domain
    UserResponse <- analysis_results$social_media
    
    prompts <- get_prompts(
        'debater',
        text = text,
        target_type = target_type,
        target = target,
        stance = stance,
        LingResponse = LingResponse,
        ExpertResponse = ExpertResponse,
        UserResponse = UserResponse
    )
    
    # Исполняем инструкции
    execute_prompts(chat, prompts)
}

# Этап 3: Заключение о позиции (Судья) ----
# 3. Судья
determine_stance <- function(
        chat_base,
        text,
        target,
        debate_results,
        type = c('object', 'statement')
    ) {
    target_type <- type_to_term(type)
    
    # Клонируем чат для нового, чистого взаимодействия
    chat <- chat_base$clone()
    
    # Аргументы дебатов
    FavourResponse <- debate_results$favour
    AgainstResponse <- debate_results$against
    NeutralResponse <- debate_results$neutral
    
   
    # Подготовка промптов
    prompts <- get_prompts(
        'judger',
        text = text,
        target_type = target_type,
        target = target,
        FavourResponse = FavourResponse,
        AgainstResponse = AgainstResponse,
        NeutralResponse = NeutralResponse
    )
    
    # Назначаем системную инструкцию
    chat$set_system_prompt(prompts$system)
    
    # Использование chat_structured для получения гарантированно структурированного результата
    final_stance <- chat$chat_structured(
        prompts$task,
        type = type_analysis
    )
    
    print(final_stance)
    
    return(final_stance)
}

# Общая функция-обертка COLA ----
cola_single_detection <- function(
        chat_base,
        text,
        target,
        type = c('object', 'statement'),
        domain_role = 'социолог'
) {
    
    target_type_gen <- switch(
        type,
        object = 'объекта',
        statement = 'утверждения'
    )
    target_type <- type_to_term(type)
    
    cat(glue::glue("--- Анализ текста для {target_type_gen} '{target}'---\n"))
    cat(paste0("Текст: ", text, "\n\n"))
    
    # ==========================================================
    # ЭТАП 1: Многомерный анализ текста
    # ==========================================================
    cat("1. Проведение многомерного анализа...\n")
    
    analysis_results <- list(
        linguistic = analyse(text, chat_base, role = 'linguist'),
        domain = analyse(
            text,
            chat_base,
            role = 'domain',
            target = target,
            target_type = target_type,
            domain = domain_role
        ),
        social_media = analyse(
            text, chat_base, role = 'veteran',
            target = target,
            target_type = target_type
        )
    )
    
    # cat("   Лингвистический анализ: ", analysis_results$linguistic, "\n")
    # cat("   Доменный анализ: ", analysis_results$domain, "\n")
    # cat("   Анализ соцсетей: ", analysis_results$social_media, "\n")
    
    # ==========================================================
    # ЭТАП 2: Дебаты с улучшенным логическим выводом
    # ==========================================================
    cat("2. Проведение дебатов...\n")
    
    debate_results <- list(
        favour = debate_stance(
            chat_base, text, target, "позитивная", analysis_results, type
        ),
        against = debate_stance(
            chat_base, text, target, "негативная", analysis_results, type
        ),
        neutral = debate_stance(
            chat_base, text, target, "нейтральная", analysis_results, type
        )
    )
    
    # cat("   Аргумент 'За': ", debate_results$favour, "\n")
    # cat("   Аргумент 'Против': ", debate_results$against, "\n")
    # cat("   Аргумент 'Нейтрально': ", debate_results$neutral, "\n")
    
    # ==========================================================
    # ЭТАП 3: Заключение о позиции
    # ==========================================================
    cat("3. Вынесение окончательного решения...\n")
    
    final_stance <- determine_stance(
        chat_base, text, target, debate_results, type
    )
    
    cat(paste0("--- ОКОНЧАТЕЛЬНАЯ ПОЗИЦИЯ: ", final_stance$stance, " ---\n\n"))
    
    return(
        list(
            text = text,
            target = target,
            target_type = type,
            stance = final_stance,
            analysis = analysis_results,
            debates = debate_results
        )
    )
}

# Функция для обработки нескольких текстов ----
cola_batch_detection <- function(
        chat_base,
        data_list,
        domain_role = 'социолог'
) {
    
    results <- lapply(data_list, function(item) {
        cola_single_detection(
            chat_base = chat_base,
            text = item$text,
            target = item$target,
            type = item$target_type,
            domain_role = domain_role
        )
    })
    
    # Преобразование результатов в удобный data frame
    final_df <- do.call(rbind, lapply(results, function(r) {
        data.frame(
            text = r$text,
            target = r$target,
            stance = r$stance$stance,
            stringsAsFactors = FALSE
        )
    }))
    
    return(list(
        summary_table = final_df,
        full_results = results
    ))
}

# Пример использования ----
# 1. Определяем тестовые данные ----
test_data <- list(
    list(
        text = "Роскомнадзор опять блокирует сайты под предлогом защиты данных, но на самом деле это просто цензура. Они контролируют интернет и ограничивают нашу свободу слова!",
        target = "Роскомнадзор защищает персональные данные граждан",
        target_type = 'statement'
    ),
    list(
        text = "Отличная новость! Центральный банк России повысил ключевую ставку, чтобы стабилизировать рубль и защитить сбережения граждан. Вот что мы называем разумной денежно-кредитной политикой!",
        target = "Центральный банк России",
        target_type = 'object'
    ),
    list(
        text = "Роскомнадзор стоит на страже информационного суверенитета России. Пример для всех ведомств.",
        target = "Роскомнадзор",
        target_type = 'object'
    ),
    list(
        text = "Полезное\n\nЭксперты Роскомнадзора расказали, как россиянам защитить персональные данные",
        target = "Роскомнадзор защищает персональные данные граждан",
        target_type = 'statement'
    )
)

res <- cola_single_detection(
    chat_base,
    text = test_data[[1]]$text,
    target = test_data[[1]]$target,
    type = test_data[[1]]$target_type
)

# 2. Запускаем пакетный анализ COLA ----
# (Убедитесь, что chat_base инициализирован выше)
# ВНИМАНИЕ: Этот процесс будет выполнять много запросов к API (3 эксперта + 3 дебатера + 1 судья = 7 запросов на текст).

# cola_results <- cola_batch_detection(chat_base, test_data)
# print(cola_results$summary_table)

cola_results$full_results[[4]]
