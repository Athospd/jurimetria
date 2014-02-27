#' Retorna um data.frame com todos os processos retornados pela query.
#' 
#' Cuidado! Os parâmetros que a função recebe (livre, classes, etc) precisam ser exatamente iguais às
#' informações que o TJSP precisa para retornar as informações.
#' 
#' @export
crawler_cjpg <- function(livre='', classes='', assuntos='', magistrados='', datas='', varas='') {
  query <- list(
    dadosConsulta.pesquisaLivre = livre,
    tipoNumero = 'UNIFICADO',
    numeroDigitoAnoUnificado = '',
    foroNumeroUnificado = '',
    dadosConsulta.nuProcesso = '',
    dadosConsulta.nuProcessoAntigo = '',
    classeTreeSelection.values = classes,
    classeTreeSelection.text = '',
    assuntoTreeSelection.values = assuntos,
    assuntoTreeSelection.text = '',
    agenteSelectedEntitiesList = '',
    contadoragente = '0',
    contadorMaioragente = '0',
    cdAgente = magistrados,
    nmAgente = '',
    dadosConsulta.dtInicio = '',
    dadosConsulta.dtFim = '',
    varasTreeSelection.values = datas,
    varasTreeSelection.text = '',
    dadosConsulta.ordenacao = 'DESC'
  )
  
  urlb <- list(scheme='https', hostname='esaj.tjsp.jus.br', path='cjpg/pesquisar.do', query=query)
  class(urlb) <- 'url'
  url <- build_url(urlb)
  url_pag <- 'https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=0'
  # REQUESTS
  r <- GET(url, config=list(ssl.verifypeer=F))
  r_pag <- GET(url_pag, config=c(ssl.verifypeer=F, set_cookies(unlist(r$cookies))))
  html <- htmlParse(content(r_pag, 'text'), encoding='UTF-8')
  nodes <- getNodeSet(html, "//tr[@class='fundocinza1']//table")  
  df <- ldply(nodes, parse_node)
  df
}

# other functions
parse_node_meta <- function(node) {
  val <- str_trim(str_split_fixed(gsub('[\n\r\t]','', xmlValue(node)), ':', 2))
  df <- data.frame(val[1,2], stringsAsFactors=F)
  names(df) <- val[1,1]
  df
}

parse_node <- function(node) {
  children <- xmlChildren(node)
  df <- do.call(cbind, lapply(children[2:(length(children)-1)], parse_node_meta))
  df$n_processo <- gsub('[\n\r\t ]', '', xmlValue(xmlChildren(xmlChildren(children[[1]])$td)$a))
  df$cod_sentenca <- xmlGetAttr(xmlChildren(xmlChildren(a)$td)$a,'name')
  df$txt <-  gsub('[\r\t]', '',xmlValue(xmlChildren(xmlChildren(children[[length(children)]])$td)[[4]]))
  df
}





