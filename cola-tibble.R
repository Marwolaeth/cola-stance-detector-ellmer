llm_stance <- function(text, ...) {
    UseMethod("llm_stance")
}

#' @export
llm_stance.data.frame <- function(
        data,
        text,
        target,
        type = NULL,
        domain_role = NULL,
        language = NULL,
        chat_base,
        scale = 'categorical',
        prompts_dir = NULL,
        verbose = TRUE,
        rpm = 20,
        .output_col = '.stance',
        ...
) {
    # Квотируем аргументы для tidy evaluation
    text_quo <- rlang::enquo(text)
    target_quo <- rlang::enquo(target)
    type_quo <- rlang::enquo(type)
    language_quo <- rlang::enquo(language)
    domain_role_quo <- rlang::enquo(domain_role)
    
    # Проверяем, что data это data.frame
    if (!is.data.frame(data)) {
        cli::cli_abort("{.arg data} must be a data.frame, got {.cls {class(data)}}")
    }
    
    # Извлекаем значения из data или окружения
    text_vec <- rlang::eval_tidy(text_quo, data)
    target_vec <- rlang::eval_tidy(target_quo, data)
    type_vec <- rlang::eval_tidy(type_quo, data)
    language_vec <- rlang::eval_tidy(language_quo, data)
    domain_role_vec <- rlang::eval_tidy(domain_role_quo, data)
    
    # Валидация извлеченных значений
    if (is.null(text_vec)) {
        cli::cli_abort(
            c(
                "Cannot find {.arg text} in data or environment",
                "i" = "Use a column name or provide a vector"
            )
        )
    }
    
    if (is.null(target_vec)) {
        cli::cli_abort(
            c(
                "Cannot find {.arg target} in data or environment",
                "i" = "Use a column name or provide a vector"
            )
        )
    }
    
    # Если type/lang/domain_role не указаны или NULL, используем defaults
    if (rlang::is_null(type_vec)) {
        type_vec <- c('object', 'statement')
    }
    
    if (rlang::is_null(language_vec)) {
        language_vec <- rcola_available_languages()
    }
    
    if (rlang::is_null(domain_role_vec)) {
        domain_role_vec <- NULL
    }
    
    # Получаем имена колонок для информативных сообщений
    text_name <- rlang::as_name(text_quo)
    target_name <- rlang::as_name(target_quo)
    
    if (verbose) {
        cli::cli_inform(
            c(
                "i" = "Processing {.val {text_name}} and {.val {target_name}} columns"
            )
        )
    }
    
    # Вызываем основную функцию
    result <- llm_stance(
        text = text_vec,
        target = target_vec,
        type = type_vec,
        language = language_vec,
        domain_role = domain_role_vec,
        chat_base = chat_base,
        scale = scale,
        prompts_dir = prompts_dir,
        verbose = verbose,
        rpm = rpm,
        ...
    )
    
    # Добавляем результаты в data
    n_rows <- nrow(data)
    n_results <- nrow(result$summary)
    
    if (n_results != n_rows) {
        cli::cli_warn(
            c(
                "Result has {n_results} rows, but data has {n_rows} rows",
                "i" = "Returning only matching rows"
            )
        )
        result$summary <- result$summary[seq_len(min(n_rows, n_results)), ]
    }
    
    # Добавляем основной результат
    data[[.output_col]] <- result$summary$stance
    
    # Сохраняем метаданные как атрибут
    attr(data, 'llm_stance_metadata') <- result$metadata
    
    if (verbose) {
        cli::cli_inform(
            c(
                "v" = "Analysis complete!",
                "i" = "Results added to {.val {(.output_col)}} column"
            )
        )
    }
    
    return(data)
}

methods(llm_stance)
