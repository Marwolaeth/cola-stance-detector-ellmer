load_translations <- function() {
    # yaml_file <- system.file("i18n/translations.yaml", package = "rcola")
    yaml_file <- "i18n/translations.yaml"
    yaml::read_yaml(yaml_file)
}

dict <- load_translations()


l <- function(lang, caption_id, case = NULL) {
    translation <- dict[[lang]][[caption_id]]
    
    if (!is.null(case)) {
        case <- match.arg(case, c('gen', 'dat'), several.ok = FALSE)
        
        if (length(translation) <= 1) {
            stop('Declination is not supported for `', caption_id, '`')
        }
        
        translation[[case]]
    } else {
        if (length(translation) >= 2) {
            stop('Case id required for `', caption_id, '`')
        }
        
        translation
    }
}

l('en', 'analysis')
l('en', 'object', 'dat')
l('en', 'object', 'gen')
l('en', 'statement', 'gen')
l('en', 'statement', 'dat')
l('en', 'statement')
