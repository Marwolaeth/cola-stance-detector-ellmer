load_translations <- function() {
    # yaml_file <- system.file("i18n/translations.yaml", package = "rcola")
    yaml_file <- "i18n/translations.yaml"
    rlang::try_fetch(
        yaml::read_yaml(yaml_file),
        error = function(e) cli::cli_abort("Tranlsations not found.")
    )
}

rcola_available_languages <- function() {
    names(load_translations())
}

l <- function(language, caption_id, case = NULL) {
    # Проверка языка
    if (!language %in% rcola_available_languages()) {
        abort(
            "Language '{language}' not available. \\
            Use: {.val {rcola_available_languages()}}"
        )
    }
    
    dict <- load_translations()
    
    # Проверка ключа
    if (!caption_id %in% names(dict[[language]])) {
        cli::cli_abort(
            "Caption ID '{caption_id}' not found ",
            "in language '{language}'"
        )
    }
    
    translation <- dict[[language]][[caption_id]]
    
    if (!is.null(case)) {
        case <- rlang::arg_match(
            case,
            c('gen', 'dat', 'categorical', 'numeric', 'likert'),
            multiple = FALSE
        )
        
        if (!is.list(translation)) {
            cli::cli_abort(
                "Case '{case}' not supported for '{caption_id}' ",
                "(expected list with cases)"
            )
        }
        
        if (!case %in% names(translation)) {
            cli::cli_abort(
                "Case '{case}' not found for '{caption_id}'"
            )
        }
        
        translation[[case]]
    } else {
        if (is.list(translation)) {
            cli::cli_abort(
                "Case required for '{caption_id}'. ",
                "Available: {.val {names(translation)}}"
            )
        }
        
        translation
    }
}

rcola_check_translations <- function() {
    dict <- load_translations()
    
    cat("Available languages:", paste(names(dict), collapse = ", "), "\n\n")
    
    for (language in names(dict)) {
        cat("Language:", language, "\n")
        cat("  Keys:", length(dict[[language]]), "\n")
        
        # Проверка структуры
        cases_keys <- sapply(dict[[language]], function(x) is.list(x))
        if (any(cases_keys)) {
            cat("  Keys with cases:", sum(cases_keys), "\n")
        }
        cat("\n")
    }
    
    invisible(TRUE)
}

# rcola_check_translations()

# l('en', 'analysis')
# l('en', 'object', 'dat')
# l('en', 'object', 'gen')
# l('en', 'statement', 'gen')
# l('en', 'statement', 'dat')
# l('en', 'statement')
# l('uk', 'statement', 'dat')
# l('uk', 'statement')
# l('pl', 'statement', 'gen')
