# TBD: vector of domain roles

# Utils ----
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

validate_inputs <- function(inputs, expected_length, input_name = 'Inputs') {
    if (!is.character(input_name)) {
        stop("`input_name` must be a character string")
    }
    
    if (is.null(inputs) || length(inputs) != expected_length) {
        stop(glue::glue("{input_name} returned unexpected results"))
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
            glue::glue("Got {length(arg)}.")
        )
    }
}

stage_toc <- function(tic, toc, msg) {
    TICK <- '✅ '
    
    tocmsg <- paste0(round(toc - tic, 3), " seconds elapsed")
    
    if (is.null(msg) || is.na(msg) || length(msg) == 0) {
        outmsg <- tocmsg
    } else {
        outmsg <- glue::glue('{TICK}{msg} complete – {tocmsg}\n\n')
    }
    outmsg
}

validate_stage <- function(x, stage_name) {
    if (is.null(x)) {
        stop(glue::glue("{stage_name} returned NULL"))
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

type_stance_analysis <- function(lang) {
    # A schema for the structured output
    type_stance <- type_enum(
        values = c('Positive', 'Negative', 'Neutral'),
        description = l(lang, 'type_description')
    )
    
    type_object(
        stance = type_stance,
        explanation = type_string(
            description = l(lang, 'explanation_description')
        )
    )
}

get_prompts <- function(
        role,
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
    
    prompt_dir <- file.path('prompts', lang)
    if (!dir.exists(prompt_dir)) {
        # If a language is present in `translations.yaml`, but prompts are missing
        warning(
            glue::glue('Prompts not found for `{lang}`. Defaulting to `en`.')
        )
        lang <- 'en'
        prompt_dir <- file.path('prompts', lang)
    }
    
    if (add_reference_resolution) {
        # Add a reference resolution prompt if exists
        reference_file <- file.path(prompt_dir, 'reference-resolution.md')
        if (file.exists(reference_file)) {
            reference_instruction <- ellmer::interpolate_file(reference_file)
        } else {
            # Default settings
            reference_instruction <- l(lang, 'reference_instruction')
        }
    } else {
        reference_instruction <- ''
    }
    
    if (add_statement_resolution) {
        # Add a statement analysis instruction if exists
        statement_file <- file.path(prompt_dir, 'statement-resolution.md')
        if (file.exists(statement_file)) {
            statement_instruction <- ellmer::interpolate_file(statement_file)
        } else {
            # Default settings
            statement_instruction <- ''
        }
    } else {
        # Default settings
        statement_instruction <- ''
    }
    
    template_system <- file.path(prompt_dir, glue::glue('system-{role}.md'))
    template_user  <- file.path(prompt_dir, glue::glue('user-{role}.md'))
    
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
    
    # Check that system prompt is unique
    # if (length(prompts$system) != 1 || !is.character(prompts$system)) {
    #     stop(
    #         'System prompt interpolation returned unexpected results. ',
    #         'Consider checking variable placeholders in system templates.'
    #     )
    # }
    
    # Check for empty strings
    if (any(nchar(prompts$system) == 0)) {
        stop(
            'System prompt is empty after interpolation. ',
            'Check that all required variables are provided.'
        )
    }
    
    # Check for user prompt existence
    ## 
    if (is.null(prompts$task)) {
        stop(glue::glue('User prompt for the `{role}` is NULL'))
    }
    
    prompts
}

check_tasks <- function(tasks, expected_length) {
    if (length(tasks) != expected_length || !is.character(tasks)) {
        stop(
            'User prompt interpolation returned unexpected results. ',
            'Consider checking variable placeholders in user templates.'
        )
    }
    
    if (any(nchar(tasks) == 0)) {
        stop(
            'User prompt is empty after interpolation. ',
            'Check that all required variables are provided.'
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
            chat <- chat_base$clone()
            # Set a system prompt
            chat$set_system_prompt(system_prompt)
            return(chat)
        }
    )
    
    check_tasks(prompts$task, n_texts)
    
    list(
        chats = chats,
        tasks = prompts$task
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
    
    if (verbose) cat('    📊', glue::glue('{info}...'), '\n')
    
    # If multiple system_prompts, e.g
    if (length(tasks$chats) > 1) {
        # Use sequential analysis
        results <- vapply(
            seq_along(tasks$chats),
            function(task_i) {
                chat <- tasks$chat[[task_i]]
                chat$chat(tasks$tasks[[task_i]], echo = 'none')
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
            )
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
            glue::glue("⏳ Stage 1: Expert analysis ({n} items)..."), 
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
            text = texts,
            target_type = target_types,
            target = targets,
            stance = stance,
            LingResponse = analysis_results$linguistic,
            ExpertResponse = analysis_results$domain,
            UserResponse = analysis_results$social_media
        )
    )
    
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
                "⏳ Stage 2: Parallel debates ({n} items × 3 stances)..."), 
            "\n"
        )
    }
    
    stance_labels <- c('positive', 'negative', 'neutral')
    debater_tasks <- lapply(
        stance_labels,
        function(stance_label) {
            prepare_debater_chats(
                stance = l(inputs$lang, glue::glue('stance_{stance_label}')),
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
                chat = debater_task$chat[[1]],
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
    
    print(prompts$system)
    
    prepare_tasks(chat_base, prompts, length(inputs$texts))
}

stage_3_parallel_judgment <- function(
        inputs,
        chat_base,
        verbose = TRUE,
        rpm = 500,
        objects = FALSE,
        statements = FALSE,
        ...
) {
    n <- length(inputs$texts)
    
    validate_inputs(inputs$debate_results$positive, n, 'Positive stance debates')
    validate_inputs(inputs$debate_results$negative, n, 'Negative stance debates')
    validate_inputs(inputs$debate_results$neutral, n, 'Neutral stance debates')
    
    tictoc::tic('Stage 3')
    if (verbose) {
        cat(glue::glue("⏳ Stage 3: Parallel judgment ({n} items)..."), "\n")
    }
    
    judger_tasks <- prepare_judger_chats(
        inputs,
        chat_base = chat_base,
        objects = objects,
        statements = FALSE
    )
    
    if (verbose) cat("   ⚖️ Running parallel judgments...\n")
    
    # Parallel decisions
    inputs[['judgment_results']] <- ellmer::parallel_chat_structured(
        chat = judger_tasks$chat[[1]],
        prompts = judger_tasks$tasks,
        type = type_stance_analysis(inputs$lang),
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
        domain_role = NULL,
        verbose = TRUE,
        rpm = 20,
        ...
) {
    ## Validation ----
    tictoc::tic()
    
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
    
    if (is.character(lang) & length(lang) == 1) {
        lang <- match.arg(lang, rcola_available_languages(), several.ok = FALSE)
    } else {
        stop("`lang` must be a single character string")
    }
    
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
    
    if (!ellmer:::is_chat(chat_base)) {
        stop("`chat_base` must be a Chat object")
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
        domain_roles = domain_role,
        chat_base = chat_base
    )
    
    if (verbose) {
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat(glue::glue("🔍 STANCE ANALYSIS - Processing {n} item(s)"), "\n")
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
        stage_1_parallel_analysis(chat_base, verbose, rpm) |>
        validate_stage('Stage 1 (Expert analysis)') |>
        stage_2_parallel_debates(chat_base, verbose, rpm) |>
        validate_stage('Stage 2 (Stance debates)') |>
        stage_3_parallel_judgment(chat_base, verbose, rpm, ...)
    
    if (is.null(output$judgment_results) || nrow(output$judgment_results) != n) {
        stop("Final stance judgement returned unexpected results")
    }
    
    ## Postprocessing ----
    summary_df <- data.frame(
        text = text,
        target = target,
        target_type = type,
        lang = lang
    ) |>
        cbind(output$judgment_results)
    
    toc <- tictoc::toc(quiet = TRUE)
    
    TICK <- '✅ '
    if (verbose) {
        cat("📊 Summary Table:\n")
        print(summary_df)
        cat("\n")
        cat(strrep("=", 70), "\n")
        cat(TICK, "Analysis complete! – ", toc$callback_msg, "\n")
        cat(strrep("=", 70), "\n\n")
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
    cat(glue::glue("Domain role: {x$metadata$domain_role}"), "\n")
    cat(glue::glue("Time elapsed: {x$metadata$elapsed} sec"), "\n")
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

