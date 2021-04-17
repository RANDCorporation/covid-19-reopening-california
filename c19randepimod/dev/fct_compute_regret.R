

#### Compute Regret Functions --------------------------------------------------------####

#----------------------------------------------------------------------------------------#
# Author: Pedro Nascimento de Lima
# Purpose: This File contains functions to Compute Regret for any given strategy
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#



#----------------------------------------------------------------------------------------#
# Function: Safe Division
# Purpose: Performs division even when denominator is zero.
# Creation Date: May 2020
#----------------------------------------------------------------------------------------#




##### CALCULO DO REGRET (PERDA DE OPORTUNIDADE) #####

#' compute_regret
#'
#' @param sim_results dataframe de sim_results simulados para o calculo do Regret.
#' @param outcome variÃ¡vel de resposta a utilizar no calculo de regret (quanto mais, melhor)
#' @param group_var variÃ¡vel a agrupar (ex.: CenÃ¡rios)
#'
#' @return mesmo dataframe de entrada com variÃ¡veis a mais.
compute_regret = function(sim_results, outcome, group_var) {
  max_variable = paste("MaxBy", group_var, sep = "")
  min_variable = paste("MinBy", group_var, sep = "")
  regret_variable = paste(outcome, "Regret", sep = "")
  perc_regret_variable = paste(regret_variable, "Perc", sep = "")
  
  sim_results[max_variable] = calcular_maximo_por_variavel(outcome = outcome, group_var = group_var, sim_results = sim_results)
  
  sim_results[min_variable] = calcular_minimo_por_variavel(outcome = outcome, group_var = group_var, sim_results = sim_results)
  
  sim_results[regret_variable] = sim_results[max_variable] - sim_results[outcome]
  
  sim_results[perc_regret_variable] = sim_results[regret_variable] / (sim_results[max_variable] - sim_results[min_variable])
  
  sim_results
}


##### RESUMIR VARIÃVEL DE RESPOSTA PARA A ANÃLISE DO REGRET #####
#' resumir_variavel_resposta
#'
#' @param sim_results dataframe com sim_results para analise do regret.
#' @param outcome variÃ¡vel de resposta para anÃ¡lise do RDM.
#' @param group_var
#'
#' @return dataframe com resumo das variaveis por grupo definido.
resumir_variavel_resposta = function(sim_results = sim_results_ano_final, outcome = "Cash", group_var = "Lever") {
  regret_variable = paste(outcome, "Regret", sep = "")
  perc_regret_variable = paste(regret_variable, "Perc", sep = "")
  
  call = substitute(
    expr =
      dplyr::group_by(sim_results, VarGroup)
    %>% dplyr::select(VarGroup, VarResposta, VarRegret, VarRegretPerc)
    %>% dplyr::summarise(VarMedio = mean(VarResposta, na.rm = TRUE),
                         VarDev = sd(VarResposta, na.rm = TRUE),
                         Percentil25Var = quantile(VarResposta, probs = c(0.25), na.rm = TRUE),
                         Percentil75Var = quantile(VarResposta, probs = c(0.75), na.rm = TRUE),
                         RegretMedio = mean(VarRegret, na.rm = TRUE),
                         DesvioRegret = sd(VarRegret, na.rm = TRUE),
                         Percentil25Regret = quantile(VarRegret, probs = c(0.25), na.rm = TRUE),
                         Percentil75Regret = quantile(VarRegret, probs = c(0.75), na.rm = TRUE),
                         RegretMedioPerc = mean(VarRegretPerc, na.rm = TRUE),
                         DesvioRegretPerc = sd(VarRegretPerc, na.rm = TRUE),
                         Percentil25RegretPerc = quantile(VarRegretPerc, probs = c(0.25), na.rm = TRUE),
                         Percentil75RegretPerc = quantile(VarRegretPerc, probs = c(0.75), na.rm = TRUE)
    )
    ,
    env = list(VarGroup = as.name(group_var),
               VarResposta = as.name(outcome),
               VarRegret = as.name(regret_variable),
               VarRegretPerc = as.name(perc_regret_variable)
    )
  )
  
  resumo = eval(call)
  
  colnames(resumo) = c(
    group_var,
    paste(outcome, "Medio", sep = ""),
    paste(outcome, "Desvio", sep = ""),
    paste(outcome, "Percentil25", sep = ""),
    paste(outcome, "Percentil75", sep = ""),
    paste(regret_variable, "Medio", sep = ""),
    paste(regret_variable, "Desvio", sep = ""),
    paste(regret_variable, "Percentil25", sep = ""),
    paste(regret_variable, "Percentil75", sep = ""),
    paste(perc_regret_variable, "Medio", sep = ""),
    paste(perc_regret_variable, "Desvio", sep = ""),
    paste(perc_regret_variable, "Percentil25", sep = ""),
    paste(perc_regret_variable, "Percentil75", sep = "")
  )
  
  resumo
}


##### ESCOLHER ESTRATÃGIA CANDIDATA #####

#' escolher_estrategia_candidata
#'
#' Escolhe a estratÃ©gia candidata "mais robusta" (dentre as disponÃ?veis) a partir de sim_results simulados e de um resumo das estratÃ©gias contendo Ã?ndices de perda de oportunidade.
#' @param sim_results Dataframe com sim_results simulados
#' @param resumo_estrategias dataframe de resumo das estratÃ©gias (gerado pela funÃ§Ã£o calcular_e_resumir_regret).
#' @param outcome VAriÃ¡vei de Resposta a considerar (string).
#' @param var_criterio VariÃ¡vel a usar como critÃ©rio (default Ã© o Percentil 75 do Regret Relativo).
#' @param sentido Sentido a utilizar na anÃ¡lise (default Ã© min para minimizar o regret).
#'
#' @return uma (ou mais) estratÃ©gias candidatas. (ID da estratÃ©gia).
escolher_estrategia_candidata = function(sim_results, resumo_estrategias, outcome, var_criterio = "RegretPercPercentil75", sentido = "min") {
  
  var_respota_criterio = paste(outcome, var_criterio, sep = "")
  
  
  # Esta lista de criterios deve ser mantida igual Ã  lista que a funcao resumir_variavel_resposta()
  possiveis_var_criterios = c("Percentil25", "Percentil75", "Medio", "Desvio", "RegretMedio", "RegretDesvio", "RegretPercentil25", "RegretPercentil75", "RegretPercMedio", "RegretPercDesvio", "RegretPercPercentil25", "RegretPercPercentil75")
  
  # Conferindo alguns pressupostos basicos:
  possiveis_var_respota_e_criterios = paste(outcome, possiveis_var_criterios, sep = "")
  
  # Conferindo se a variÃ¡vel de resposta e variÃ¡vel de critÃ©rio combinam corretamente:
  if (!all(possiveis_var_respota_e_criterios %in% names(resumo_estrategias))){
    stop("Existe algo errado com a sua variavel de resposta ou variavel de criterio (a combinacao das duas no existe no resumo de estrategias).")
  }
  
  # Conferindo se a Variavel de criterio estÃ¡ correta.
  if(!var_criterio %in% possiveis_var_criterios){
    stop(paste("Esta variavel de criterio esta incorreta. escolha entre:",possiveis_var_criterios))
  }
  
  
  # Agora sim, posso escolhenr a estratÃ©gia que tem o menor percentil percentual 75 (assim como Lempert):
  estrategias_candidatas = switch(sentido,
                                  "min" = escolher_estrategia_min(resumo_estrategias, var_respota_criterio),
                                  "max" = escolher_estrategia_max(resumo_estrategias, var_respota_criterio))
  
  estrategias_candidatas
}


##### CALCULAR E RESUMIR REGRET #####

calcular_e_resumir_regret = function(sim_results, outcome, var_cenarios, var_estrategias) {
  sim_results = compute_regret(sim_results = sim_results, outcome = outcome, group_var = var_cenarios)
  
  # Resumindo VariÃ¡vel de Resposta Cash:
  resumo_estrategias = resumir_variavel_resposta(sim_results = sim_results, outcome = outcome, group_var = var_estrategias)
  
  # Formar lista de outputs dessta anÃ¡lise
  output = list(
    sim_results = sim_results,
    ResumoEstrategias = resumo_estrategias
  )
  
  output
}

##### ESCOLHER ESTRATÃGIA #####

escolher_estrategia_min = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == min(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]
  estrategia
}


escolher_estrategia_max = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == max(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]
  estrategia
}

##### ANALISAR ENSEMBLE DETERMINANDO A MELHOR ESTRATÃGIA #####
analisar_ensemble_com_melhor_estrategia = function(ensemble, sim_results_regret, var_cenarios, var_estrategias, outcome, estrategia_candidata) {
  
  
  ensemble = as.data.frame(ensemble)
  sim_results_regret = as.data.frame(sim_results_regret)
  
  
  sim_results_regret["MelhorEstrategia"] = sim_results_regret[outcome] == sim_results_regret$MaxByScenario
  
  linhas_melhores_estrategias = which(sim_results_regret[outcome] == sim_results_regret$MaxByScenario)
  
  variaveis = c(var_cenarios, var_estrategias, outcome)
  
  melhores_estrategias = as.data.frame(sim_results_regret[linhas_melhores_estrategias, variaveis])
  
  ensemble_com_melhor_estrategia = dplyr::inner_join(ensemble, melhores_estrategias)
  
  ensemble_com_melhor_estrategia["EstrategiaCandidata"] = ensemble_com_melhor_estrategia[var_estrategias] == estrategia_candidata
  
  #ensemble_com_melhor_estrategia = as.factor(ensemble_com_melhor_estrategia[var_estrategias])
  
  ensemble_com_melhor_estrategia
  
}


##### ANÃLISE DE VULNERABILIDADE #####

#' obter_df_analise_vulnerabilidade
#'
#' @param results results da funÃ§Ã£o de simulaÃ§Ã£o do RDM (contendo anÃ¡lise de regret, necessÃ¡riamente).
#' @param estrategia_candidata nÃºmero da estratÃ©gia ser analisada.
#' @param variavel_resposta nome da variÃ¡vei de respota analisada nesta estratÃ©gia  (que deverÃ¡ estar acima ou abaixo do threshold).
#' @param threshold nÃºmero acima ou abaixo do qual a variÃ¡vel de resposta estarÃ¡ para ser considerada "interessante" para a anÃ¡lise.
#' @param planilha_inputs planilha de inputs com as variÃ¡veis incertas, para filtrarmos somente as variÃ¡veis incertas.
#' @param sentido_vulnerabilidade pode ser ">=" ou "<=". Se ">=", os casos de interesse serÃ£o aqueles onde a variÃ¡veil de interesse seja >= ao threshold.
#'
#' @return dataframe com o ensemble, variÃ¡vel de resposta e indicaÃ§Ã£o se cada caso Ã© um caso de interesse ou nÃ£o.
#' @export
obter_df_vulnerabilidade = function(results, estrategia_candidata, variavel_resposta = "sNPVProfit1RegretPerc" , threshold = 0.1, planilha_inputs, sentido_vulnerabilidade = ">=") {
  
  if(sentido_vulnerabilidade == ">=") {
    results$AnaliseRegret$sim_results$CasoInteresse = as.numeric(results$AnaliseRegret$sim_results[,variavel_resposta] >= threshold)
  } else {
    results$AnaliseRegret$sim_results$CasoInteresse = as.numeric(results$AnaliseRegret$sim_results[,variavel_resposta] <= threshold)
  }
  
  # Obter Ensemble com sim_results Simulados:
  ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$sim_results, by = "Scenario")
  
  ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia_candidata),]
  
  # Retirar NAs do Ensemble
  ensemble_e_resultados = na.omit(ensemble_e_resultados)
  
  parametros_completos = readxl::read_xlsx(planilha_inputs, sheet = "params")
  
  variaveis_incertas = parametros_completos$Variavel[which(parametros_completos$Tipo=="Incerto")]
  
  x = ensemble_e_resultados[,c(variavel_resposta,"Scenario", "Lever",variaveis_incertas)]
  y = as.numeric(ensemble_e_resultados$CasoInteresse)
  
  data.frame(CasoInteresse = y, x)
}



selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)
}


calcular_maximo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Maximo = max(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Maximo
}

calcular_minimo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Minimo = min(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Minimo
}

