load_translations <- function() {
    # yaml_file <- system.file("i18n/translations.yaml", package = "rcola")
    yaml_file <- "i18n/translations.yaml"
    yaml::read_yaml(yaml_file)
}

rcola_available_languages <- function() {
    names(load_translations())
}

l <- function(language, caption_id, case = NULL) {
    # Проверка языка
    if (!language %in% rcola_available_languages()) {
        stop("Language '", language, "' not available. Use: ", 
             paste(rcola_available_languages(), collapse = ", "))
    }
    
    dict <- load_translations()
    
    # Проверка ключа
    if (!caption_id %in% names(dict[[language]])) {
        stop("Caption ID '", caption_id, "' not found in language '", language, "'")
    }
    
    translation <- dict[[language]][[caption_id]]
    
    if (!is.null(case)) {
        case <- match.arg(
            case,
            c('gen', 'dat', 'categorical', 'numeric', 'likert'),
            several.ok = FALSE
        )
        
        if (!is.list(translation)) {
            stop("Case '", case, "' not supported for '", caption_id, 
                 "' (expected list with cases)")
        }
        
        if (!case %in% names(translation)) {
            stop("Case '", case, "' not found for '", caption_id, "'")
        }
        
        translation[[case]]
    } else {
        if (is.list(translation)) {
            stop("Case required for '", caption_id, "'. Available: ", 
                 paste(names(translation), collapse = ", "))
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
