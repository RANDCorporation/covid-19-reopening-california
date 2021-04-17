
#------------------------------------------------------------------------------#
# Code for the paper: Reopening California
#                     Seeking Robust, Non-Dominated COVID-19 Exit Strategies.
# 
# Author: Pedro Nascimento de Lima
# Copyright (C) 2021 by The RAND Corporation
# See LICENSE.txt and README.txt for information on usage and licensing
#------------------------------------------------------------------------------#

#### ADDITIONAL REGRET FUNCTIOS --------------------------------------------####

#------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This set of functions were not used in this paper but are kept 
# here for my future reference.
# 
#------------------------------------------------------------------------------#


##### CHOOSE CANDIDATE STRATEGY #####

#' choose_candidate_strategy
#'
#' Escolhe a estratégia candidata "mais robusta" (dentre as disponíveis) a partir de data simulados e de um resumo das estratégias contendo índices de perda de oportunidade.
#' @param data Dataframe com data simulados
#' @param strategy_summary dataframe de resumo das estratégias (gerado pela função compute_and_summarise_regret).
#' @param outcome_variable VAriávei de Resposta a considerar (string).
#' @param criterion_variable Variável a usar como critério (default é o Percentil 75 do Regret Relativo).
#' @param regret_type regret_type a utilizar na análise (default é min para minimizar o regret).
#'
#' @return uma (ou mais) estratégias candidatas. (ID da estratégia).
choose_candidate_strategy = function(data, strategy_summary, outcome_variable, criterion_variable = "RegretPercPercentil75", regret_type = "min") {
  
  variable_to_use_as_criteria = paste(outcome_variable, criterion_variable, sep = "")
  
  
  # Esta lista de criterios deve ser mantida igual à lista que a funcao summarise_outcome_variable()
  possible_criterion_variables = c("Percentil25", "Percentil75", "Medio", "Desvio", "MeanRegret", "RegretDesvio", "RegretPercentil25", "RegretPercentil75", "RegretPercMedio", "RegretPercDesvio", "RegretPercPercentil25", "RegretPercPercentil75")
  
  # Conferindo alguns pressupostos basicos:
  possible_vars_to_use = paste(outcome_variable, possible_criterion_variables, sep = "")
  
  # Conferindo se a variável de resposta e variável de critério combinam corretamente:
  if (!all(possible_vars_to_use %in% names(strategy_summary))){
    stop("Something is wrong with your response variable.")
  }
  
  # Conferindo se a Variable de criterio está correta.
  if(!criterion_variable %in% possible_criterion_variables){
    stop(paste("I cannot use this variable. Choose one among:",possible_criterion_variables))
  }
  
  # Agora sim, posso escolhenr a estratégia que tem o menor percentil percentual 75 (assim como Lempert):
  candidate_strategies = switch(regret_type,
                                "min" = choose_strategy_min(strategy_summary, variable_to_use_as_criteria),
                                "max" = choose_strategy_max(strategy_summary, variable_to_use_as_criteria))
  
  candidate_strategies
}




##### ANALYSE ENSEMBLE AND CHOOSE BEST STRATEGY #####
# Could be useful to analyse each strategy.
analyze_candidate_strategy_ensemble = function(ensemble, data_regret, sow_variable, strategy_variable, outcome_variable, candidate_strategy) {
  
  ensemble = as.data.frame(ensemble)
  data_regret = as.data.frame(data_regret)
  
  browser()
  
  # This seems to be hard coding that the maimum is the best.
  data_regret["BestStrategy"] = data_regret[outcome_variable] == data_regret$max_Scenario
  
  best_strategy_lines = which(data_regret[outcome_variable] == data_regret$max_Scenario)
  
  variable = c(sow_variable, strategy_variable, outcome_variable)
  
  best_strategies = as.data.frame(data_regret[best_strategy_lines, variable])
  
  ensemble_with_best_strategy = dplyr::inner_join(ensemble, best_strategies)
  
  ensemble_with_best_strategy["CandidateStrategy"] = ensemble_with_best_strategy[strategy_variable] == candidate_strategy
  
  #ensemble_com_melhor_estrategia = as.factor(ensemble_com_melhor_estrategia[strategy_variable])
  
  ensemble_with_best_strategy
  
}


##### VULNERABILITY ANALYSIS FUNCTIONS #####

#' obter_df_analise_vulnerabilidade
#'
#' @param results results da função de simulação do RDM (contendo análise de regret, necessáriamente).
#' @param candidate_strategy número da estratégia ser analisada.
#' @param outcome_variable nome da variávei de respota analisada nesta estratégia  (que deverá estar acima ou abaixo do threshold).
#' @param threshold número acima ou abaixo do qual a variável de resposta estará para ser considerada "interessante" para a análise.
#' @param inputs_spreadsheet planilha de inputs com as variáveis incertas, para filtrarmos somente as variáveis incertas.
#' @param regret_vulnerability_type pode ser ">=" ou "<=". Se ">=", os casos de interesse serão aqueles onde a variáveil de interesse seja >= ao threshold.
#'
#' @return dataframe com o ensemble, variável de resposta e indicação se cada caso é um caso de interesse ou não.
#' @export
get_vulnerability_dataframe = function(results, candidate_strategy, outcome_variable = "sNPVProfit1RegretPerc" , threshold = 0.1, inputs_spreadsheet, regret_vulnerability_type = ">=", regret_analysis_obj) {
  
  # Vulnerability Analysis
  browser()
  
  # Se não foi informada uma analise regret específica, utilizar a do Results.
  if(!exists(x = "regret_analysis_obj")){
    regret_analysis_obj = results$regret_analysis_obj
  }
  
  if(regret_vulnerability_type == ">=") {
    regret_analysis_obj$data$CaseOfInterest = as.numeric(regret_analysis_obj$data[,outcome_variable] >= threshold)  
  } else {
    regret_analysis_obj$data$CaseOfInterest = as.numeric(regret_analysis_obj$data[,outcome_variable] <= threshold)  
  }
  
  # Obter Ensemble com data Simulados:
  ensemble_and_results = dplyr::inner_join(as.data.frame(results$Ensemble), regret_analysis_obj$data, by = "SOW")
  
  ensemble_and_results = ensemble_and_results[which(ensemble_and_results$Strategy == candidate_strategy),]
  
  # Retirar NAs do Ensemble
  ensemble_and_results = na.omit(ensemble_and_results)
  
  
  # Review this:
  parametros_completos = readxl::read_xlsx(inputs_spreadsheet, sheet = "params")
  
  variaveis_incertas = parametros_completos$Variable[which(parametros_completos$Tipo=="Incerto")]
  
  x = ensemble_and_results[,c(outcome_variable,"SOW", "Strategy",variaveis_incertas)]
  y = as.numeric(ensemble_and_results$CaseOfInterest)
  
  data.frame(CaseOfInterest = y, x)
}


#' obter_df_diff_media
#' Esta função serve para listar as variáveis que potencialmente mais distinguem os Casos de Interesse dos demais casos.
#' Esta função compara a média de cada variável dos casos de interesse com a média de todo o ensemble.
#' @param vulnerability_df data.frame com a análise de vulnerabilidade retornado pela função get_vulnerability_dataframe.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
get_avg_difference_df = function(vulnerability_df, outcome_var) {
  
  browser()
  
  vars_to_remove = c("CaseOfInterest", "SOW", "Strategy", outcome_var)
  
  avgs_of_interest = vulnerability_df %>% dplyr::filter(CaseOfInterest == 1) %>% dplyr::select(-any_of(vars_to_remove)) %>% summarise_all(mean)
  
  global_averages = vulnerability_df %>% dplyr::filter(CaseOfInterest == 0) %>% dplyr::select(-any_of(vars_to_remove))  %>% dplyr::summarise_all(mean)
  
  global_max = vulnerability_df %>% dplyr::select(-any_of(vars_to_remove)) %>% dplyr::summarise_all(max)
  
  global_min = vulnerability_df %>% dplyr::select(-any_of(vars_to_remove)) %>% dplyr::summarise_all(min)
  
  range_global = global_max - global_min
  
  avg_diff = (avgs_of_interest - global_averages) / range_global
  
  v_averages_analyzed = colnames(avg_diff)
  
  v_avg_diff = unname(t(avg_diff)[,1])
  
  v_global_averages = unname(t(global_averages)[,1])
  
  v_range_global = unname(t(range_global)[,1])
  
  v_avgs_of_interest = unname(t(avgs_of_interest)[,1])
  
  order = order(abs(v_avg_diff), decreasing = TRUE)
  
  df_analise_medias = data.frame(
    Ranking = 1:length(v_averages_analyzed),
    Variable = v_averages_analyzed[order],
    DifMediaRelativa = v_avg_diff[order],
    MediaCasosInteresse = v_avgs_of_interest[order],
    MediaGlobal = v_global_averages[order],
    Range = v_range_global[order]
  )  
}


#' get_test_t_dataframe
#' Realiza um Teste T para cada variável de incerteza. 
#' @param vulnerability_df data.frame com a análise de vulnerabilidade retornado pela função get_vulnerability_dataframe.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
get_test_t_dataframe = function(vulnerability_df, outcome_variable) {
  
  vars_to_remove = c("SOW", "Strategy", outcome_var)
  
  cases_for_test = vulnerability_df %>% dplyr::select(-any_of(vars_to_remove))
  
  # Teste T para Diferença de Médias
  cases_for_test$CaseOfInterest = as.factor(cases_for_test$CaseOfInterest)
  t_test_results = t(sapply(cases_for_test[-1], function(x) 
    unlist(t.test(x~cases_for_test$CaseOfInterest)[c("estimate","p.value","statistic")])))
  
  t_test_results = as.data.frame(t_test_results)
  
  t_test_results$RejectH0_95Conf = t_test_results$p.value < 0.05
  
  t_test_results$RejectH0_99Conf = t_test_results$p.value < 0.01
  
  t_test_results$Variable = rownames(t_test_results)
  
  rownames(t_test_results) = NULL
  
  strategy = unique(vulnerability_df$Strategy)
  
  names(t_test_results) = c(paste("Média Est.",strategy,"não Falha"), paste("Média Est.",strategy,"Falha"), "Valor_P", "Est. T", "Rej. H0 95%", "Rej. H0 99%", "Variável")
  
  t_test_results = dplyr::arrange(.data = t_test_results, Valor_P)
  
  t_test_results$Rank = 1:nrow(t_test_results)
  
  t_test_results = t_test_results[,c(8,7,3,4,5,6,1,2)]
  
  t_test_results
}




grafico_whisker_por_lever = function(regret_data, variable, variable_name) {
  dados_por_estrategia = dplyr::group_by(regret_data, Lever)
  
  dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)
  
  # Gerando Grafico da Variável de Perda de Oportunidade
  call_grafico = substitute(
    expr = ggplot(dados_por_estrategia, aes(y = variable,x = Strategy, group = Strateg)),
    env = list(variable = as.name(variable))
  )
  
  p <- eval(call_grafico)
  p + geom_boxplot() + 
    scale_y_continuous(labels = format_for_humans) + 
    ylab(variable_name) + 
    theme(axis.text.x = element_text(size=7))
}



completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}



loess_countour_plot = function(results, x_variable = "bubble_size",
                               y_variable = "prevalence",
                               dependent_variable = "expected.number.of.inf",
                               facet_variable = "duration",
                               facet_vector = c(7, 14, 21),
                               binwidth = NULL,
                               nudge_y = 0,
                               nudge_x = 0,
                               skip = 0) {
  
  
  
  if(is.null(facet_variable)) {
    selected_results = results[,c(x_variable, y_variable, dependent_variable)]
    names(selected_results) = c("x","y", "z")
    loess_model = loess(z ~ x + y, data = selected_results)
  } else {
    selected_results = results[,c(x_variable, y_variable, facet_variable, dependent_variable)] 
    names(selected_results) = c("x","y","facet", "z")
    loess_model = loess(z ~ x + y + facet, data = selected_results)
  }
  
  x_vector= seq.default(from = min(selected_results$x), to = max(selected_results$x), length.out = 100)
  y_vector= seq.default(from = min(selected_results$y), to = max(selected_results$y), length.out = 100)
  
  
  
  if(is.null(facet_variable)) {
    meta_model_results <-  expand.grid(x_vector, y_vector)
    names(meta_model_results) = c("x","y")
    
    mtrx3d =  predict(loess_model, meta_model_results)
    
    # Transform data to long form
    mtrx.melt <- melt(mtrx3d, id.vars = c("x", "y"), measure.vars = "z")
    names(mtrx.melt) <- c("x", "y", "z")
    # Return data to numeric form
    mtrx.melt$x <- as.numeric(str_sub(mtrx.melt$x, str_locate(mtrx.melt$x, "=")[1,1] + 1))
    mtrx.melt$y <- as.numeric(str_sub(mtrx.melt$y, str_locate(mtrx.melt$y, "=")[1,1] + 1))
    
  } else {
    meta_model_results <-  expand.grid(x_vector, y_vector, facet_vector)
    names(meta_model_results) = c("x","y","facet")
    mtrx3d =  predict(loess_model, meta_model_results)
    
    # Transform data to long form
    mtrx.melt <- melt(mtrx3d, id.vars = c("x", "y", "facet"), measure.vars = "z")
    names(mtrx.melt) <- c("x", "y", "facet", "z")
    # Return data to numeric form
    mtrx.melt$x <- as.numeric(str_sub(mtrx.melt$x, str_locate(mtrx.melt$x, "=")[1,1] + 1))
    mtrx.melt$y <- as.numeric(str_sub(mtrx.melt$y, str_locate(mtrx.melt$y, "=")[1,1] + 1))
    mtrx.melt$facet <- as.numeric(str_sub(mtrx.melt$facet, str_locate(mtrx.melt$facet, "=")[1,1] + 1))
  }
  
  
  ### Results from the metamodel:
  
  plot_function = function(data, facet_variable) {
    
    if(is.null(facet_variable)) {
      names(data) = c("x","y","z")
    } else {
      names(data) = c("x","y",facet_variable, "z")
    }
    
    plot <- ggplot(data, aes(x, y)) +
      geom_tile(aes(fill=z)) + 
      ggplot2::geom_contour(aes(z = z), color = "black",  binwidth = binwidth, size = 1)
    
    
    if(!is.null(facet_variable)) {
      plot = plot + facet_wrap(facets = facet_variable, labeller = label_both)
    } 
    
    
    # computing breaks:
    bins = (max(data$z) - min(data$z))/binwidth
    breaks = pretty(x = data$z,n = bins)
    
    plot = plot +
      scale_fill_gradientn(colours=tim.colors(128), breaks = breaks) + 
      # Adjusts Contour text positions
      metR::geom_text_contour(aes(z = z),show.legend = T,stroke.color = "white", binwidth = binwidth, skip = skip, nudge_y = nudge_y, nudge_x = nudge_x) + 
      #hrbrthemes::theme_ipsum_ps(axis_title_just = "c") + 
      theme(legend.position="bottom") + 
      theme(axis.title = element_text(face=1))
    
    plot
    
  }
  
  mtrx.melt %>%
    plot_function(data = ., facet_variable = facet_variable)
  
} 



