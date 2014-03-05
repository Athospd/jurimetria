#' Retorna um data.frame com todos os processos retornados pela query.
#' 
#' Cuidado! Os parâmetros que a função recebe (livre, classes, etc) precisam ser exatamente iguais às
#' informações que o TJSP precisa para retornar as informações.
#' 
#' @export
bug <- c()
crawler_cjpg <- function(livre='', classes='', assuntos='', magistrados='', datas=c('',''), varas='', pag=0, r=NULL) {
  if(is.null(r)) {
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
      dadosConsulta.dtInicio = datas[1],
      dadosConsulta.dtFim = datas[2],
      varasTreeSelection.values = varas,
      varasTreeSelection.text = '',
      dadosConsulta.ordenacao = 'DESC'
    )
    urlb <- list(scheme='https', hostname='esaj.tjsp.jus.br', path='cjpg/pesquisar.do', query=query)
    class(urlb) <- 'url'
    url <- build_url(urlb)
    r <- GET(url, config=list(ssl.verifypeer=F))
  }
  
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d',pag)
  # REQUESTS
  cat(sprintf('pagina: %d...', pag))

  cat('fazendo download...')
  r_pag <- GET(url_pag, config=c(ssl.verifypeer=F, set_cookies(unlist(r$cookies))))
  cat('download realizado! ')
  cat('inicializando parser...')
  try ({
    html <- htmlParse(content(r_pag, 'text'), encoding='UTF-8')
    nodes <- getNodeSet(html, "//tr[@class='fundocinza1']//table")  
    cat('rodando parser...')
    df <- ldply(nodes, parse_node)
    df$pag <- pag
    cat('OK!\n')
    return(df)  
  },TRUE)
  cat('BUGOU!!!!!\n')
  bug <<- append(bug, pag)
  return(data.frame())
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
  df$cod_sentenca <- xmlGetAttr(xmlChildren(xmlChildren(children[[1]])$td)$a,'name')
  df$txt <-  gsub('[\r\t]', '',xmlValue(xmlChildren(xmlChildren(children[[length(children)]])$td)[[4]]))
  df
}


View(cjpg)

# bug <- c(-1)
# inicio do download realizado em 2014-02-28 8:17
# aux <- crawler_cjpg(pag=1, datas=c('','27/02/2014'))
# load('../testes/r.RData')
# pags <- as.numeric(xmlGetAttr(getNodeSet(htmlParse(content(r,'text'), encoding='UTF-8'), "//a[@title='Última página']")[[1]], 'name'))
# lista_df <- list()
# 
# for(i in 22108:pags) {
#   if((i-1) %in% bug) {
#     lista_df[[i]] <- crawler_cjpg(pag=i, datas=c('','27/02/2014'))
#     load('../testes/r.RData')
#   } else {
#     lista_df[[i]] <- crawler_cjpg(pag=i, r=r)
#   }
#   if(i%%1000==0) {
#     cat('\n\nsalvando lista :)\n\n')
#     save(lista_df, file='../testes/lista_df.RData')
#   }
# }
# cjpg <- rbind.fill(lista_df)
# save(cjpg, file='../testes/cjpg.RData')
# cjpg_meta <- select(cjpg, -(txt))
# save(cjpg_meta, file='../testes/cjpg_meta.RData')
