# TBD ----
# 1. Up to three base chats as argument — Done!
# 2. Different scales: + numeric, likert
# 3. Nice Unicode handling in verbose() — Done!
# 4. cli_abort() instead of stop()

# Utils ----
truncate <- function(x, a, b) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(a))
    stopifnot(is.numeric(b))
    
    pmin(pmax(x, a), b)
}

catch <- function(expr, expr_name = deparse(substitute(expr))) {
    if (!is.character(expr_name)) {
        stop("`expr_name` must be a character string")
    }
    
    tryCatch(
        expr,
        error = function(e) {
            stop(glue::glue("Error in {expr_name}: {e$message}", call. = FALSE))
        }
    )
}

validate_inputs <- function(inputs, expected_length, input_name = 'Inputs') {
    if (!is.character(input_name)) {
        stop("`input_name` must be a character string")
    }
    
    if (is.null(inputs) || length(inputs) != expected_length) {
        stop(
            glue::glue("{input_name} returned unexpected results"),
            call. = FALSE
        )
    }
}

recycle_arg <- function(arg, n, arg_name) {
    if (length(arg) == 1) {
        return(rep(arg, n))
    } else if (length(arg) == n) {
        return(arg)
    } else {
        stop(
            glue::glue(
                "`{arg_name}` must have length 1 or {n} (same as `text`). "
            ),
            glue::glue("Got {length(arg)}."),
            call. = FALSE
        )
    }
}

stage_toc <- function(tic, toc, msg) {
    tocmsg <- paste0(round(toc - tic, 3), " seconds elapsed")
    
    if (is.null(msg) || is.na(msg) || length(msg) == 0) {
        outmsg <- tocmsg
    } else {
        outmsg <- glue::glue('\u2705 {msg} complete – {tocmsg}\n\n')
    }
    outmsg
}

validate_stage <- function(x, stage_name) {
    if (is.null(x)) {
        stop(glue::glue("{stage_name} returned NULL"), call. = FALSE)
    }
    x
}

# ellmer Utils ----
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
        templates,
        lang = rcola_available_languages(),
        add_reference_resolution = FALSE,
        add_statement_resolution = FALSE,
        ...
) {
    role <- match.arg(
        role,
        choices = c('linguist', 'domain', 'interpreter', 'debater', 'judger'),
        several.ok = FALSE
    )
    lang <- match.arg(lang)
    
    # A lot of work for a judger
    if (role == 'judger') {
        if (add_reference_resolution) {
            reference_instruction <- templates$reference_resolution
        } else {
            reference_instruction <- ''
        }
        
        if (add_statement_resolution) {
            statement_instruction <- templates$statement_resolution
        } else {
            # Default settings
            statement_instruction <- ''
        }
        
        scale_guide <- templates$scale_guide
    }
    
    template_system <- templates[[glue::glue('system-{role}')]]
    template_user <- templates[[glue::glue('user-{role}')]]
    
    # Check for file existence
    if (!file.exists(template_system)) {
        stop(
            glue::glue(
                'System template not found: {template_system}. ',
                'Available roles: linguist, domain, interpreter, debater, judger.'
            )
        )
    }
    if (!file.exists(template_user)) {
        stop(
            glue::glue(
                'User template not found: {template_user}. ',
                'Available roles: linguist, domain, interpreter, debater, judger.'
            )
        )
    }
    
    prompts <- list(
        system = ellmer::interpolate_file(template_system, ...),
        task = ellmer::interpolate_file(template_user, ...)
    )
    
    # Check for user prompt existence
    if (is.null(prompts$task)) {
        cli::cli_abort(
            "User prompt is NULL for role {.val {role}}"
        )
    }
    
    # Check for empty strings
    if (any(nchar(prompts$system) == 0)) {
        cli::cli_abort(
            "System prompt is empty after interpolation for role {.val {role}}"
        )
    }
    if (any(nchar(prompts$task) == 0)) {
        cli::cli_abort(
            "User prompt is empty after interpolation for role {.val {role}}"
        )
    }
    
    prompts
}

check_tasks <- function(tasks, expected_length) {
    if (length(tasks) != expected_length || !is.character(tasks)) {
        stop(
            'User prompt interpolation returned unexpected results. ',
            'Consider checking variable placeholders in user templates.',
            call. = FALSE
        )
    }
    
    if (any(nchar(tasks) == 0)) {
        stop(
            'User prompt is empty after interpolation. ',
            'Check that all required variables are provided.',
            call. = FALSE
        )
    }
}

prepare_tasks <- function(chat_base, prompts, n_texts) {
    # General case when there may be > 1 system prompts
    ## (e.g. multiple domain roles)
    chats <- lapply(
        prompts$system,
        function(system_prompt) {
            # Each time clone the base chat
            chat <- chat_base$clone(deep = TRUE)
            # Set a system prompt
            chat$set_system_prompt(system_prompt)
            return(chat)
        }
    )
    
    if (length(chats) != 1 & length(chats) != n_texts) {
        stop("Length of system and user prompts must match.")
    }

    check_tasks(prompts$task, n_texts)
    
    list(
        chats = chats,
        tasks = prompts$task
    )
}

# Schemas ----
type_stance_categorical <- function(lang) {
    type_enum(
        values = c('Positive', 'Negative', 'Neutral'),
        description = ellmer::interpolate_file(
            file.path('prompts', lang, 'description-categorical.md')
        )
    )
}

type_stance_numeric <- function(lang) {
    type_number(
        description = ellmer::interpolate_file(
            file.path('prompts', lang, 'description-numeric.md')
        )
    )
}

type_stance_likert <- function(lang) {
    type_enum(
        values = c(
            'Strongly Disagree',
            'Disagree',
            'Neutral',
            'Agree',
            'Strongly Agree'
        ),
        description = ellmer::interpolate_file(
            file.path('prompts', lang, 'description-likert.md')
        )
    )
}

type_stance_analysis <- function(lang, scale) {
    # A schema for the structured output
    type_stance <- switch(
        scale,
        numeric = type_stance_numeric(lang),
        likert = type_stance_likert(lang),
        type_stance_categorical(lang)
    )
    
    type_object(
        stance = type_stance,
        explanation = type_string(
            description = l(lang, 'explanation_description')
        )
    )
}

# Stage 1 - Expert Analysis ----
prepare_expert_chats <- function(
        inputs,
        chat_base,
        expert_role = c('linguist', 'domain', 'interpreter')
) {
    expert_role <- match.arg(
        expert_role,
        c('linguist', 'domain', 'interpreter'),
        several.ok = FALSE
    )
    
    # Загружаем инструкции для эксперта
    prompts <- with(
        inputs,
        get_prompts(
            expert_role,
            templates = prompt_templates,
            lang = lang,
            text = texts,
            target = targets,
            target_type = target_types,
            domain = domain_roles
        )
    )
    
    prepare_tasks(chat_base, prompts, length(inputs$texts))
}

execute_role <- function(
        expert_role,
        inputs,
        chat_base,
        info,
        rpm = 500,
        verbose = TRUE
    ) {
    tasks <- prepare_expert_chats(
        inputs,
        chat_base = chat_base,
        expert_role = expert_role
    )
    
    if (verbose) cat('    \U1F4CA', glue::glue('{info}...'), '\n')
    
    # If multiple system_prompts, e.g
    if (length(tasks$chats) > 1) {
        # Use sequential analysis
        results <- vapply(
            seq_along(tasks$tasks),
            function(task_i) {
                chat <- tasks$chats[[task_i]]
                result <- chat$chat(tasks$tasks[[task_i]], echo = 'none')
                if (!is.character(result)) {
                    stop(
                        glue::glue('Unexpected results in {info} {task_i}.'),
                        call. = FALSE
                    )
                }
                result
            },
            character(1)
        ) |> catch(tolower(info))
    } else {
        # Use parallel analysis
        results <- ellmer::parallel_chat_text(
            # Unpack the Chat object
            chat = tasks$chats[[1]],
            prompts = tasks$tasks,
            rpm = rpm
        ) |> catch(tolower(info))
    }
    
    if (!is.character(results) || length(results) != length(inputs$texts)) {
        stop(
            glue::glue(
                'Unexpected results in {info}: input and output lengths differ.'
            ),
            call. = FALSE
        )
    }
    
    results
}

stage_1_parallel_analysis <- function(
        inputs,
        chat_base,
        verbose = TRUE,
        rpm = 500
) {
    n <- length(inputs$texts)
    
    tictoc::tic('Stage 1')
    if (verbose) {
        cat(
            glue::glue("\U23F3 Stage 1: Expert analysis ({n} items)..."), 
            "\n"
        )
    }
    
    expert_roles <- c('linguist', 'domain', 'interpreter')
    expert_info <- c(
        linguist = 'Linguistic analysis',
        domain = 'Domain expert analysis',
        interpreter = 'Social media interpretation'
    )
    analysis_results <- lapply(
        expert_roles,
        function(role) {
            execute_role(
                role,
                inputs = inputs,
                chat_base = chat_base,
                info = expert_info[[role]],
                rpm = rpm,
                verbose = verbose
            )
        }
    )
    names(analysis_results) <- c('linguistic', 'domain', 'social_media')
    
    # The result is a list of character vectors of length length(texts)
    ## One vector for each role
    inputs[['analysis_results']] <- analysis_results
    
    tictoc::toc(func.toc = stage_toc, quiet = !verbose)
    
    inputs
}


# Stage 2 - Stance Debates ----
prepare_debater_chats <- function(
        stance,
        inputs,
        chat_base
) {
    prompts <- with(
        inputs,
        get_prompts(
            'debater',
            lang = lang,
            templates = prompt_templates,
            text = texts,
            target_type = target_types,
            target = targets,
            stance = stance,
            LingResponse = analysis_results$linguistic,
            ExpertResponse = analysis_results$domain,
            UserResponse = analysis_results$social_media
        )
    )
    
    if (length(prompts$system) > 1) {
        stop(
            'Multiple system prompts are not allowed at stage 2',
            'Consider checking variable placeholders in system templates.',
            call. = FALSE
        )
    }

    prepare_tasks(chat_base, prompts, length(inputs$texts))
}

stage_2_parallel_debates <- function(
        inputs,
        chat_base,
        verbose = TRUE,
        rpm = 500
) {
    n <- length(inputs$texts)
    
    validate_inputs(inputs$analysis_results$linguistic, n, 'Linguistic analysis')
    validate_inputs(inputs$analysis_results$domain, n, 'Domain expert analysis')
    validate_inputs(inputs$analysis_results$social_media, n, 'Social media analysis')
    
    tictoc::tic('Stage 2')
    if (verbose) {
        cat(
            glue::glue(
                "\U23F3 Stage 2: Parallel debates ({n} items × 3 stances)..."), 
            "\n"
        )
    }
    
    stance_labels <- c('positive', 'negative', 'neutral')
    debater_tasks <- lapply(
        stance_labels,
        function(stance_label) {
            prepare_debater_chats(
                stance = l(
                    inputs$lang,
                    glue::glue('stance_{stance_label}'),
                    inputs$scale
                ),
                inputs = inputs,
                chat_base = chat_base
            )
        }
    )
    names(debater_tasks) <- stance_labels
    
    # Parallel debates
    inputs[['debate_results']] <- lapply(
        debater_tasks,
        function(debater_task) {
            ellmer::parallel_chat_text(
                chat = debater_task$chats[[1]],
                prompts = debater_task$tasks,
                rpm = rpm
            ) |> catch('stance debates')
        }
    )
    
    tictoc::toc(func.toc = stage_toc, quiet = !verbose)
    
    inputs
}

# Stage 3 - Judging ----
prepare_judger_chats <- function(
        inputs,
        chat_base
) {
    prompts <- with(
        inputs,
        get_prompts(
            'judger',
            lang = lang,
            templates = prompt_templates,
            text = texts,
            target_type = target_types,
            target = targets,
            FavourResponse = debate_results$positive,
            AgainstResponse = debate_results$negative,
            NeutralResponse = debate_results$neutral,
            # Дополнительные инструкции
            add_reference_resolution = 'object' %in% types,
            add_statement_resolution = 'statement' %in% types,
        )
    )
    
    if (length(prompts$system) > 1) {
        stop(
            'Multiple system prompts are not allowed at stage 3',
            'Consider checking variable placeholders in system templates.',
            call. = FALSE
        )
    }
    
    prepare_tasks(chat_base, prompts, length(inputs$texts))
}

stage_3_parallel_judgment <- function(
        inputs,
        chat_base,
        verbose = TRUE,
        rpm = 500,
        ...
) {
    n <- length(inputs$texts)
    
    validate_inputs(inputs$debate_results$positive, n, 'Positive stance debates')
    validate_inputs(inputs$debate_results$negative, n, 'Negative stance debates')
    validate_inputs(inputs$debate_results$neutral, n, 'Neutral stance debates')
    
    tictoc::tic('Stage 3')
    if (verbose) {
        cat(glue::glue("\U23F3 Stage 3: Parallel judgment ({n} items)..."), "\n")
    }
    
    judger_tasks <- prepare_judger_chats(
        inputs,
        chat_base = chat_base
    )
    
    if (verbose) cat("    \u2696\UFE0F Running parallel judgments...\n")
    
    # Parallel decisions
    inputs[['judgment_results']] <- ellmer::parallel_chat_structured(
        chat = judger_tasks$chats[[1]],
        prompts = judger_tasks$tasks,
        type = type_stance_analysis(inputs$lang, inputs$scale),
        rpm = rpm,
        convert = TRUE,
        ...
    ) |> catch('making final judgement')
    
    if (any(is.na(inputs$judgment_results$stance))) {
        n_na <- sum(is.na(inputs$judgment_results$stance))
        warning(glue::glue("{n_na} items returned NA stance"))
    }
    
    tictoc::toc(func.toc = stage_toc, quiet = !verbose)
    
    inputs
}

# The Wrapper Function ----
#' Parallel Stance Analysis
#'
#' @param text Character vector of texts to analyze
#' @param target Character vector of targets (recycled if length 1)
#' @param chat_base an [ellmer::Chat] object from
#' @param type either "object" or "statement" (recycled if length 1)
#' @param lang Language code ("en", "ru", "uk")
#' @param domain_role Domain expert role (default: "sociologist", recycled if length 1)
#' @param verbose Show progress messages
#' @param rpm Rate limit (requests per minute)
#' @param ... Other arguments passed to [ellmer::parallel_chat_structured()]
#'
#' @return S3 object of class "stance_result" with:
#'   - summary: data.frame with results
#'   - analysis: expert analysis results
#'   - debates: debate results
#'   - judgments: final judgments
#'   - metadata: processing metadata
#'
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_openai()
#' result <- llm_stance(
#'     text = "Climate change is not real",
#'     target = "Climate Change is real",
#'     chat_base = chat,
#'     type = "statement",
#'     lang = "en"
#' )
#' print(result)
#' summary(result)
#' }
#'
#' @export
llm_stance <- function(
        text,
        target,
        chat_base,
        type = c('object', 'statement'),
        lang = rcola_available_languages(),
        scale = c('categorical', 'numeric', 'likert'),
        domain_role = NULL,
        prompts_dir = NULL,
        verbose = TRUE,
        rpm = 20,
        ...
) {
    ## Validation ----
    tictoc::tic('Analysis')
    
    if (!is.character(text)) {
        stop("`text` must be a character vector")
    }
    if (length(text) == 0) {
        stop("`text` cannot be empty")
    }
    
    n <- length(text)
    
    if (!is.character(target)) {
        stop("`target` must be a character vector")
    }
    if (length(target) == 0) {
        stop("`target` cannot be empty")
    }
    
    if (is.character(type)) {
        type <- match.arg(type, c('object', 'statement'), several.ok = TRUE)
    } else {
        stop("`type` must be a character vector")
    }
    
    if (is.null(lang) | length(lang) > 1) {
        lang <- cld2::detect_language(text[[1]], lang_code = TRUE)
        warning('The analysis language was automatically detected.')
    }
    if (is.character(lang) & length(lang) == 1) {
        # Проверка языка
        if (!lang %in% rcola_available_languages()) {
            stop("Language '", lang, "' not available. Use: ", 
                 paste(rcola_available_languages(), collapse = ", "))
        }
    } else {
        stop("`lang` must be a single character string")
    }
    
    scale <- match.arg(scale, c('categorical', 'numeric', 'likert'))
    
    if (is.null(domain_role)) {
        domain_role <- switch(
            lang,
            uk = 'соціолог',
            ru = 'социолог',
            'social commentator'
        )
    } else {
        if (!is.character(domain_role)|| length(domain_role) < 1) {
            stop("`domain_role` must be a character vector")
        }
        if (!(length(domain_role) == 1 | length(domain_role) == n)) {
            stop(
                glue::glue(
                    "`domain_role` must have length 1 or {n} (same as `text`). "
                ),
                glue::glue("Got {length(arg)}.")
            )
        }
    }
    
    if (length(domain_role) > 1) {
        warning(
            'Multiple domain roles detected. Parallel execution is unsupported.'
        )
    }
    
    ### Chat Validation ----
    # Validate and arrange the `chat_base` argument across stages
    # 
    # Recommendations (see vignette and docstrings):
    # 1. Expert analysis (Stage 1):
    #     - Excellent real-world awareness
    #     - Decent reasoning skills
    #     - Sufficient writing skills
    #     - Context width depends on task
    #     - Structured output: NOT required
    # 2. Stance debates (Stage 2):
    #     - Sufficient context width
    #     - Creative and writing skills
    #     - Structured output: NOT required
    # 3. Stance decision (Stage 3):
    #     - Large context width
    #     - Excellent reasoning skills
    #     - Excellent real-world awareness
    #     - Structured output: REQUIRED
    chats <- if (is.list(chat_base)) {
        # Validate each element
        for (i in seq_along(chat_base)) {
            if (!ellmer:::is_chat(chat_base[[i]])) {
                stop(
                    glue::glue("Element {i} of `chat_base` "),
                    "is not an `ellmer::Chat` object."
                )
            }
        }
        
        # Arrange based on list length
        indices <- switch(
            length(chat_base),
            `1` = c(1, 1, 1),
            `2` = c(1, 1, 2),
            `3` = c(1, 2, 3),
            stop(
                "`chat_base` must have 1, 2, or 3 elements. ",
                "You provided ", length(chat_base), "."
            )
        )
        lapply(indices, \(i) chat_base[[i]]$clone(deep = TRUE))
        
    } else if (ellmer:::is_chat(chat_base)) {
        rep(list(chat_base$clone(deep = TRUE)), times = 3)
        
    } else {
        stop(
            "`chat_base` must be an `ellmer::Chat` object ",
            "or a list of 1-3 `ellmer::Chat` objects."
        )
    }
    
    ## Prompts ----
    # DEFAULT_PROMPTS_DIR <- system.file(
    #     file.path('prompts', lang),
    #     package = 'rcola'
    # )
    DEFAULT_PROMPTS_DIR <- file.path('prompts', lang)
    
    search_dirs <- c(prompts_dir, DEFAULT_PROMPTS_DIR) |> 
        Filter(f = \(x) !is.null(x) && dir.exists(x))
    
    if (length(search_dirs) == 0) {
        stop('No prompt directories found')
    }
    
    role_templates <- expand.grid(
        call = c('system', 'user'),
        role = c('linguist', 'domain', 'interpreter', 'debater', 'judger'),
        stringsAsFactors = FALSE
    ) |>
        glue::glue_data('{call}-{role}.md') |>
        as.character() |>
        as.list()
    names(role_templates) <- gsub('\\.md', '', role_templates)
    
    prompt_files <- list(
        scale_description = glue::glue('description-{scale}.md'),
        scale_guide = glue::glue('guide-{scale}.md'),
        reference_resolution = 'reference-resolution.md',
        statement_resolution = 'statement-resolution.md'
    ) |>
        c(role_templates)
    
    prompt_templates <- list()
    
    for (name in names(prompt_files)) {
        filename <- prompt_files[[name]]
        
        # Ищем файл в порядке приоритета
        found <- FALSE
        for (dir in search_dirs) {
            filepath <- file.path(dir, filename)
            if (file.exists(filepath)) {
                prompt_templates[[name]] <- filepath
                found <- TRUE
                break
            }
        }
        
        # Если не найден — ошибка
        if (!found) {
            cli::cli_abort(
                "Prompt {.file {filename}} not found for language {.val {lang}}.",
            )
        }
    }
    
    ## Preparation ----
    target <- recycle_arg(target, n, "target")
    type <- recycle_arg(type, n, "type")
    
    target_types <- sapply(type, type_to_term, lang = lang)
    
    inputs <- list(
        texts = text,
        targets = target,
        types = type,
        target_types = target_types,
        lang = lang,
        scale = scale,
        domain_roles = domain_role,
        prompt_templates = prompt_templates
    )
    
    if (verbose) {
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat(glue::glue("\U1F50D STANCE ANALYSIS - Processing {n} item(s)"), "\n")
        cat(strrep("=", 70), "\n\n")
        cat(glue::glue("Types: {paste(unique(type), collapse = ', ')}"), "\n")
        cat(glue::glue("Language: {lang}"), "\n")
        cat(
            glue::glue("Domain roles: {paste(unique(domain_role), collapse = ', ')}"),
            "\n"
        )
        cat("\n")
    }
    
    ## Analysis ----
    output <- inputs |>
        stage_1_parallel_analysis(chats[[1]], verbose, rpm) |>
        validate_stage('Stage 1 (Expert analysis)') |>
        stage_2_parallel_debates(chats[[2]], verbose, rpm) |>
        validate_stage('Stage 2 (Stance debates)') |>
        stage_3_parallel_judgment(chats[[3]], verbose, rpm, ...)
    
    if (is.null(output$judgment_results) || nrow(output$judgment_results) != n) {
        stop("Final stance judgement returned unexpected results")
    }
    
    # Additional postprocessing of quantitative stance labels
    if (scale == 'numeric') {
        output$judgment_results$stance <- truncate(
            output$judgment_results$stance,
            -1,
            1
        )
    }
    
    ## Postprocessing ----
    summary_df <- data.frame(
        text = text,
        target = target,
        target_type = type,
        lang = lang
    ) |>
        cbind(output$judgment_results)
    
    if (verbose) {
        cat("\U1F4CA Summary Table:\n")
        print(summary_df)
        cat("\n")
        cat(strrep("=", 70), "\n")
        # cli::cli_alert_success("Analysis complete! – {.time {toc$callback_msg}}")
        toc <- tictoc::toc(func.toc = stage_toc, quiet = FALSE)
        cat(strrep("=", 70), "\n\n")
    } else {
        toc <- tictoc::toc(func.toc = stage_toc, quiet = TRUE)
    }
    
    ## Return ----
    structure(
        list(
            summary = summary_df,
            analysis = output$analysis_results,
            debates = output$debate_results,
            judgments = output$judgment_results,
            metadata = list(
                n_total = n,
                n_processed = nrow(output$judgment_results),
                n_failed = sum(is.na(output$judgment_results$stance)),
                language = lang,
                types = unique(type),
                domain_role = domain_role,
                elapsed = toc$toc - toc$tic,
                timestamp = Sys.time()
            )
        ),
        class = c("stance_result", "list")
    )
}

# Methods ----
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
    cat(
        glue::glue(
            "Domain role(s): {paste(x$metadata$domain_role, collapse = ', ')}"
        ),
        "\n"
    )
    cat(glue::glue("Time elapsed: {round(x$metadata$elapsed, 2)} sec"), "\n")
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

