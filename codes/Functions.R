

# Graphiques --------------------------------------------------------------

graphiques <- function(var){
  ggplot(df) +
    geom_bar() +
    aes(x = {{var}}, fill = Gender) +
    theme_minimal() +
    scale_fill_brewer(palette = 'Set1') +
    labs(title = str_to_title(paste("Bar chart of",as_string(ensym(var)))),
         subtitle = 'According to gender',
         x = as_string(ensym(var)))
}



graph <- function(var){
  
  # IMPORTANT : Les variables appellées doivent être sous ""
  # La fonction prend une variable et sort une graphique suivant le type de la
  # variable. 
  # - Si la variable est un factor, ressort un bar graph.
  # - Si la variable est un double, ressort graphique en boîtes
  # - Ressort un message d'erreur si la variable être d'une autre nature
  # Remarques :
  # - Prise en compte du genre dans les graphiques
  # Options graphiques : thème minimal, titre des axes, couleur de l'option fill
  
  
  if(class(df[[var]]) == 'factor'){
    ggplot(df) +
      geom_bar() +
      aes(x = .data[[var]], fill = Gender) +
      theme_minimal() +
      scale_fill_brewer(palette = 'Set1') +
      labs(y = 'Count',
           caption = '0 équivaut à Non, 1 équivant à Oui',
           title = str_to_title(paste(as_string(ensym(var)),"' repartition among the population")),
           subtitle = 'Gender taken into account')
  }
  else if(class(df[[var]]) == 'numeric'){
    
    # Garde les statistiques de moyenne et de quantile de la variable passée en 
    # argument. Arrondis ces statistiques au centième et les transforme en string.
    # Permettra de faire varier la description du graphique en boîtes en fonction
    # des statistiques du genre féminin
    
    
    results <- df %>%
      select(Gender, .data[[var]]) %>%
      group_by(Gender) %>%
      summarize(Mean = mean(.data[[var]]),
                Quantile_25 = quantile(.data[[var]], probs = 0.25),
                Quantile_75 = quantile(.data[[var]], probs = 0.75)) %>%
      filter(Gender == 'Female') %>%
      mutate(across(where(is.numeric),~ round(., digits = 2))) %>%
      mutate(across(where(is.numeric), ~ toString(.)))
    Mean <- results$Mean 
    Quantile25 <- results$Quantile_25 
    Quantile75 <- results$Quantile_75
    
    
    
    ggplot(df) +
      geom_boxplot() +
      aes(y = .data[[var]], x = Gender) +
      theme_minimal() +
      labs(title = str_to_title(paste(as_string(ensym(var)),"' repartition among the population")),
           subtitle = 'Gender taken into account',
           caption = paste("Female's population has a mean",as_string(ensym(var)), 'of', Mean,
                           ' . The 25% and 75% quantile are respectively equal to', Quantile25, 
                           ' and', Quantile75)
      )
    
  }
  else{
    message("Erreur, ce format de variable n'est pas supporté par la fonction.
            La variable doit être un factor ou numeric.")
  }
}