.rcola_env <- new.env()

load_translations <- function() {
    if (!exists("dict", envir = .rcola_env)) {
        # yaml_file <- system.file("i18n/translations.yaml", package = "rcola")
        yaml_file <- "i18n/translations.yaml"
        .rcola_env$dict <- yaml::read_yaml(yaml_file)
    }
    .rcola_env$dict
}

rcola_available_languages <- function() {
    names(load_translations())
}

l <- function(lang, caption_id, case = NULL) {
    # Проверка языка
    if (!lang %in% rcola_available_languages()) {
        stop("Language '", lang, "' not available. Use: ", 
             paste(rcola_available_languages(), collapse = ", "))
    }
    
    dict <- load_translations()
    
    # Проверка ключа
    if (!caption_id %in% names(dict[[lang]])) {
        stop("Caption ID '", caption_id, "' not found in language '", lang, "'")
    }
    
    translation <- dict[[lang]][[caption_id]]
    
    if (!is.null(case)) {
        case <- match.arg(case, c('gen', 'dat'), several.ok = FALSE)
        
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
    
    for (lang in names(dict)) {
        cat("Language:", lang, "\n")
        cat("  Keys:", length(dict[[lang]]), "\n")
        
        # Проверка структуры
        cases_keys <- sapply(dict[[lang]], function(x) is.list(x))
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
