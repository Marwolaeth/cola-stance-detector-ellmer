# Служебные функции ----
type_to_term <- function(
        type = c('object', 'statement'),
        lang = rcola_available_languages()
) {
    type <- match.arg(type, c('object', 'statement'), several.ok = FALSE)
    lang <- match.arg(lang)
    
    switch(
        type,
        object = l(lang, 'object', 'dat'),
        statement = l(lang, 'statement', 'dat')
    )
}

get_prompts <- function(
        role,
        lang = rcola_available_languages(),
        ...
) {
    lang <- match.arg(lang)
    
    template_system <- file.path('prompts', lang, glue::glue('system-{role}.md'))
    template_user  <- file.path('prompts', lang, glue::glue('user-{role}.md'))
    
    list(
        system = interpolate_file(template_system, ...),
        task = interpolate_file(template_user, ...)
    )
}

catch <- function(expr, expr_name = deparse(substitute(expr))) {
    if (!is.character(expr_name)) {
        stop("`expr_name` must be a character string")
    }
    
    tryCatch(
        expr,
        error = function(e) {
            stop(glue::glue("Error in {expr_name}: {e$message}"))
        }
    )
}

validate_inputs <- function(inputs, n, input_name = 'Inputs') {
    if (!is.character(input_name)) {
        stop("`input_name` must be a character string")
    }
    
    if (is.null(inputs) || length(inputs) != n) {
        stop(glue::glue("{input_name} returned unexpected results"))
    }
}
# =====================================================================
# ЭТАП 1: ПОДГОТОВКА ЧАТОВ И ПРОМПТОВ ДЛЯ ЭКСПЕРТОВ
# =====================================================================

prepare_expert_chats <- function(
        texts,
        targets,
        target_types,
        lang,
        domain_role,
        chat_base,
        expert_role = c('linguist', 'domain', 'interpreter')
) {
    expert_role <- match.arg(expert_role)
    
    # Загружаем инструкции для эксперта
    prompts <- get_prompts(
        expert_role,
        lang = lang,
        text = texts,
        target = targets,
        target_type = target_types,
        domain = domain_role
    )
    
    # Клонируем чат и устанавливаем system prompt
    chat <- chat_base$clone()
    chat$set_system_prompt(prompts$system)
    
    list(
        chat = chat,
        tasks = prompts$task
    )
}

# =====================================================================
# ЭТАП 1: ПАРАЛЛЕЛЬНЫЙ АНАЛИЗ ТЕКСТОВ (ЭКСПЕРТЫ)
# =====================================================================

stage_1_parallel_analysis <- function(
        texts,
        targets,
        target_types,
        lang,
        domain_role,
        chat_base,
        verbose = TRUE,
        rpm = 500
) {
    n <- length(texts)
    
    if (verbose) {
        cat(glue::glue("⏳ Stage 1: Parallel expert analysis ({n} items)..."), "\n")
    }
    
    # =========================================================
    # ЛИНГВИСТИЧЕСКИЙ АНАЛИЗ
    # =========================================================
    if (verbose) cat("   📊 Linguistic analysis...\n")
    
    linguist_tasks <- prepare_expert_chats(
        texts = texts,
        targets = targets,
        target_types = target_types,
        lang = lang,
        domain_role = domain_role,
        chat_base = chat_base,
        expert_role = 'linguist'
    )
    
    linguistic_results <- ellmer::parallel_chat_text(
        chat = linguist_tasks$chat,
        prompts = linguist_tasks$tasks,
        rpm = rpm
    ) |> catch('linguistic analysis')
    
    # =========================================================
    # ДОМЕННЫЙ АНАЛИЗ
    # =========================================================
    
    if (verbose) cat("   📊 Domain expert analysis...\n")
    
    domain_tasks <- prepare_expert_chats(
        texts = texts,
        targets = targets,
        target_types = target_types,
        lang = lang,
        domain_role = domain_role,
        chat_base = chat_base,
        expert_role = 'domain'
    )
    
    domain_results <- ellmer::parallel_chat_text(
        chat = domain_tasks$chat,
        prompts = domain_tasks$tasks,
        rpm = rpm
    ) |> catch('domain expert analysis')
    
    # =========================================================
    # АНАЛИЗ СОЦИАЛЬНЫХ МЕДИА
    # =========================================================
    
    if (verbose) cat("   📊 Social media interpretation...\n")
    
    interpreter_tasks <- prepare_expert_chats(
        texts = texts,
        targets = targets,
        target_types = target_types,
        lang = lang,
        domain_role = domain_role,
        chat_base = chat_base,
        expert_role = 'interpreter'
    )
    
    social_results <- ellmer::parallel_chat_text(
        chat = interpreter_tasks$chat,
        prompts = interpreter_tasks$tasks,
        rpm = rpm
    ) |> catch('social media interpretation')
    
    # Возвращаем результаты в формате: список списков
    # Каждый элемент соответствует одному текста
    analysis_results <- list(
        linguistic = linguistic_results,
        domain = domain_results,
        social_media = social_results
    )
    
    if (verbose) cat("✅ Stage 1 complete\n\n")
    
    analysis_results
}

# =====================================================================
# ЭТАП 2: ПОДГОТОВКА ЧАТОВ ДЛЯ ДЕБАТЕРОВ
# =====================================================================

prepare_debater_chats <- function(
        texts,
        targets,
        target_types,
        lang,
        stance,
        analysis_results,
        chat_base
) {
    prompts <- get_prompts(
        'debater',
        lang = lang,
        text = texts,
        target_type = target_types,
        target = targets,
        stance = stance,
        LingResponse = analysis_results$linguistic,
        ExpertResponse = analysis_results$domain,
        UserResponse = analysis_results$social_media
    )
    
    chat <- chat_base$clone()
    chat$set_system_prompt(prompts$system)
    
    list(
        chat = chat,
        tasks = prompts$task
    )
}

# =====================================================================
# ЭТАП 2: ПАРАЛЛЕЛЬНЫЕ ДЕБАТЫ
# =====================================================================

stage_2_parallel_debates <- function(
        texts,
        targets,
        target_types,
        lang,
        analysis_results,
        chat_base,
        verbose = TRUE,
        rpm = 500
) {
    n <- length(texts)
    
    validate_inputs(analysis_results$linguistic, n, 'Linguistic analysis')
    validate_inputs(analysis_results$domain, n, 'Domain expert analysis')
    validate_inputs(analysis_results$social_media, n, 'Social media analysis')
    
    if (verbose) {
        cat(glue::glue("⏳ Stage 2: Parallel debates ({n} items × 3 stances)...\n"))
    }
    
    stance_labels <- c('positive', 'negative', 'neutral')
    debater_tasks <- lapply(
        stance_labels,
        function(stance_label) {
            prepare_debater_chats(
                texts = texts,
                targets = targets,
                target_types = target_types,
                lang,
                stance = l(lang, glue::glue('stance_{stance_label}')),
                analysis_results = analysis_results,
                chat_base = chat_base
            )
        }
    )
    names(debater_tasks) <- stance_labels
    
    # Параллельный запуск всех дебатов
    debate_results <- lapply(
        debater_tasks,
        function(debater_task) {
            ellmer::parallel_chat_text(
                chat = debater_task$chat,
                prompts = debater_task$tasks,
                rpm = rpm
            ) |> catch('stance debates')
        }
    )
    
    if (verbose) cat("✅ Stage 2 complete\n\n")
    
    debate_results
}

# =====================================================================
# ЭТАП 3: ПОДГОТОВКА ЧАТОВ ДЛЯ СУДЕЙ
# =====================================================================

prepare_judger_chats <- function(
        texts,
        targets,
        target_types,
        lang,
        debate_results,
        chat_base
) {
    prompts <- get_prompts(
        'judger',
        lang = lang,
        text = texts,
        target_type = target_types,
        target = targets,
        FavourResponse = debate_results$positive,
        AgainstResponse = debate_results$negative,
        NeutralResponse = debate_results$neutral
    )
    
    chat <- chat_base$clone()
    chat$set_system_prompt(prompts$system)
    
    list(
        chat = chat,
        tasks = prompts$task
    )
}

# =====================================================================
# ЭТАП 3: ПАРАЛЛЕЛЬНЫЙ ВЫНОС РЕШЕНИЙ (СУДЬИ)
# =====================================================================

stage_3_parallel_judgment <- function(
        texts,
        targets,
        target_types,
        lang,
        debate_results,
        chat_base,
        verbose = TRUE,
        rpm = 500
) {
    n <- length(texts)
    
    validate_inputs(debate_results$positive, n, 'Positive stance debates')
    validate_inputs(debate_results$negative, n, 'Negative stance debates')
    validate_inputs(debate_results$neutral, n, 'Neutral stance debates')
    
    if (verbose) {
        cat(glue::glue("⏳ Stage 3: Parallel judgment ({n} items)...\n"))
    }
    
    # Подготавливаем чаты для судей
    judger_tasks <- prepare_judger_chats(
        texts = texts,
        targets = targets,
        target_types = target_types,
        lang = lang,
        debate_results = debate_results,
        chat_base = chat_base
    )
    
    if (verbose) cat("   ⚖️ Running parallel judgments...\n")
    
    type_stance <- type_enum(
        values = c('Positive', 'Negative', 'Neutral'),
        description = l(lang, 'type_description')
    )
    
    type_analysis <- type_object(
        stance = type_stance,
        explanation = type_string(
            description = l(lang, 'explanation_description')
        )
    )
    
    # Определяем схему для структурированного вывода
    judgment_results <- ellmer::parallel_chat_structured(
        chat = judger_tasks$chat,
        prompts = judger_tasks$tasks,
        type = type_analysis,
        rpm = rpm
    ) |> catch('making final judgement')
    
    if (verbose) cat("✅ Stage 3 complete\n\n")
    
    judgment_results
}

# =====================================================================
# ГЛАВНАЯ ФУНКЦИЯ: stance()
# =====================================================================

llm_stance <- function(
        text,
        target,
        chat_base,
        type = c('object', 'statement'),
        lang = rcola_available_languages(),
        domain_role = NULL,
        verbose = TRUE,
        rpm = 20
) {
    # =====================================================================
    # ВАЛИДАЦИЯ И ПОДГОТОВКА
    # =====================================================================
    
    # Валидация text
    if (!is.character(text)) {
        stop("`text` must be a character vector")
    }
    if (length(text) == 0) {
        stop("`text` cannot be empty")
    }
    
    # Валидация target
    if (!is.character(target)) {
        stop("`target` must be a character vector")
    }
    if (length(target) == 0) {
        stop("`target` cannot be empty")
    }
    
    # Валидация type
    if (is.character(type)) {
        type <- match.arg(type, c('object', 'statement'), several.ok = TRUE)
    } else {
        stop("`type` must be a character vector")
    }
    
    # Валидация lang
    if (is.character(lang) & length(lang) == 1) {
        lang <- match.arg(lang, rcola_available_languages(), several.ok = FALSE)
    } else {
        stop("`lang` must be a single character string")
    }
    
    # Валидация domain_role
    if (is.null(domain_role)) {
        domain_role <- switch(
            lang,
            uk = 'соціолог',
            ru = 'социолог',
            'social commentator'
        )
    } else {
        if (!is.character(domain_role) || length(domain_role) != 1) {
            stop("`domain_role` must be a sinle character string")
        }
    }
    
    # Валидация chat_base
    if (!ellmer:::is_chat(chat_base)) {
        stop("`chat_base` must be a Chat object")
    }
    
    # =====================================================================
    # ОПРЕДЕЛЕНИЕ ДЛИНЫ И ПЕРЕРАБОТКА АРГУМЕНТОВ
    # =====================================================================
    
    n <- length(text)
    
    recycle_arg <- function(arg, n, arg_name) {
        if (length(arg) == 1) {
            return(rep(arg, n))
        } else if (length(arg) == n) {
            return(arg)
        } else {
            stop(
                glue::glue(
                    "`{arg_name}` must have length 1 or {n} (same as `text`). Got {length(arg)}."
                )
            )
        }
    }
    
    target <- recycle_arg(target, n, "target")
    type <- recycle_arg(type, n, "type")
    
    # =====================================================================
    # ВЫВОД ИНФОРМАЦИИ
    # =====================================================================
    
    if (verbose) {
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat(glue::glue("🔍 STANCE ANALYSIS - Processing {n} item(s)"), "\n")
        cat(strrep("=", 70), "\n\n")
        cat(glue::glue("Types: {paste(unique(type), collapse = ', ')}"), "\n")
        cat(glue::glue("Language: {lang}"), "\n")
        cat(glue::glue("Domain role: {domain_role}"), "\n")
        cat("\n")
    }
    
    # =====================================================================
    # ЭТАП 1: ПАРАЛЛЕЛЬНЫЙ АНАЛИЗ
    # =====================================================================
    
    target_types <- sapply(type, type_to_term, lang = lang)
    
    analysis_results <- stage_1_parallel_analysis(
        texts = text,
        targets = target,
        target_types = target_types,
        lang = lang,
        domain_role = domain_role,
        chat_base = chat_base,
        verbose = verbose,
        rpm = rpm
    )
    
    # =====================================================================
    # ЭТАП 2: ПАРАЛЛЕЛЬНЫЕ ДЕБАТЫ
    # =====================================================================
    
    debate_results <- stage_2_parallel_debates(
        texts = text,
        targets = target,
        target_types = type,
        lang = lang,
        analysis_results = analysis_results,
        chat_base = chat_base,
        verbose = verbose,
        rpm = rpm
    )
    
    # =====================================================================
    # ЭТАП 3: ПАРАЛЛЕЛЬНЫЙ ВЫНОС РЕШЕНИЙ
    # =====================================================================
    
    judgment_results <- stage_3_parallel_judgment(
        texts = text,
        targets = target,
        target_types = type,
        lang = lang,
        debate_results = debate_results,
        chat_base = chat_base,
        verbose = verbose,
        rpm = rpm
    )
    
    if (is.null(judgment_results) || nrow(judgment_results) != n) {
        stop("Final stance judgement returned unexpected results")
    }
    
    # =====================================================================
    # СОЗДАНИЕ ИТОГОВОЙ ТАБЛИЦЫ
    # =====================================================================
    
    summary_df <- data.frame(
        text = text,
        target = target,
        target_type = type,
        lang = lang
    ) |>
        cbind(judgment_results)
    
    if (verbose) {
        cat("📊 Summary Table:\n")
        print(summary_df)
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat("✅ Analysis complete!\n")
        cat(strrep("=", 70), "\n\n")
    }
    
    # =====================================================================
    # ВОЗВРАТ РЕЗУЛЬТАТОВ
    # =====================================================================
    
    structure(
        list(
            summary = summary_df,
            analysis = analysis_results,
            debates = debate_results,
            judgments = judgment_results,
            metadata = list(
                n_processed = n,
                language = lang,
                types = unique(type),
                domain_role = domain_role,
                timestamp = Sys.time()
            )
        ),
        class = c("stance_result", "list")
    )
}

# =====================================================================
# МЕТОДЫ ДЛЯ РАБОТЫ С РЕЗУЛЬТАТАМИ
# =====================================================================

#' @export
print.stance_result <- function(x, ...) {
    cat("Stance Analysis Result\n")
    cat(strrep("=", 60), "\n")
    cat(
        glue::glue("Processed: {x$metadata$n_processed}/{x$metadata$n_total} items"),
        "\n"
    )
    cat(glue::glue("Failed: {x$metadata$n_failed} items"), "\n")
    cat(glue::glue("Language: {x$metadata$language}"), "\n")
    cat(glue::glue("Types: {paste(x$metadata$types, collapse = ', ')}"), "\n")
    cat(glue::glue("Domain role: {x$metadata$domain_role}"), "\n")
    cat(glue::glue("Timestamp: {x$metadata$timestamp}"), "\n")
    cat(strrep("=", 60), "\n\n")
    
    cat("Summary Table:\n")
    print(x$summary)
    
    invisible(x)
}

#' @export
summary.stance_result <- function(object, ...) {
    cat("Stance Distribution:\n")
    print(table(object$summary$stance))
    cat("\n")
    
    cat("By Target:\n")
    print(table(object$summary$target, object$summary$stance))
    
    invisible(object)
}

#' @export
as.data.frame.stance_result <- function(x, row.names = NULL, optional = FALSE, ...) {
    x$summary
}

