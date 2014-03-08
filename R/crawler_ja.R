#' Retorna data.frames com metadados do Justiça Aberta.
#' 
#' Essa função retorna metadados úteis para a pesquisa jurimétrica através do sistema 
#' Justiça Aberta. Dependendo do tipo ("muni", "vara", "prod"), retorna um nível diferente de 
#' metadados. Os parâmetros e cod_muni, cod_vara ajudam a determinar quais municípios, 
#' produtividades ou varas serão retornados. Se forem nulos (padrão), a função baixará da internet
#' todos os metadados. Tome cuidado pois essa função pode demorar um tempo considerável para
#' rodar, pois acessa muitas páginas da internet (27 requisições para baixar os municípios;
#' ~5591 requisições para baixar as varas estaduais, e ~10000 requisições para baixar as 
#' produtividades). Para obter as produtividades a partir de um dado código de produtividade, 
#' considere a função crawler_prod_ja().
#' 
#' @export
crawler_metadata_ja <- function(tipo, ufs=NULL, cod_muni=NULL, cod_vara=NULL, instancia=1, justica=1) {
  if(instancia == 1) {
    if(tipo == 'muni') {
      if(is.null(ufs)) {
        cat('preciso de uma lista de ufs...\n')
        return()
      }
      return(meta_muni(ufs))
    } else if(tipo == 'vara') {
      if(is.null(cod_muni)) {
        if(is.null(ufs)) {
          cat('preciso de uma lista de ufs pois sua lista de municipios está vazia...\n')
          return()
        }
        cod_muni <- meta_muni(ufs)$cod_muni
      }
      return(meta_vara(cod_muni, justica))
    } else if(tipo == 'prod') {
      if(is.null(cod_vara)) {
        if(is.null(cod_muni)) {
          if(is.null(ufs)) {
            cat('preciso de uma lista de ufs pois suas listas de varas e municipios está vazia...\n')
            return()
          } else {
            cod_muni <- meta_muni(ufs)$cod_muni
            cod_vara <- meta_vara(cod_muni, justica)$cod_vara
          }
        } else {
          cod_vara <- meta_vara(cod_muni, justica)$cod_vara
        }
      }
      return(meta_prod(cod_vara))
    } else {
      cat(paste0('tipo de informação não suportado: ', tipo, '.\n'))
      return()
    }
  } else if(instancia == 2) {
    cat('not supported yet :(\n')
    return()
  } else {
    cat(paste0('não existe esse tipo de instância: ', instancia, '.\n'))
    return()
  }
}
crawler_ja <- function(post_data=NULL, xpath='', raw=FALSE, html=NULL, url = 'http://www.cnj.jus.br/corregedoria/justica_aberta/') {
  body <- append(post_data, list(d = 'consulta', a = 'consulta', token = ''))
  if(is.null(html)) {
    if(is.null(post_data)) {
      return()
    }
    r <- POST(url, body = body)
    html <- htmlParse(content(r, as = 'text'), encoding = 'UTF-8')  
  }
  if(raw) return(html)
  nodes <- getNodeSet(html, xpath)
  return(nodes)
}
crawler_muni <- function(uf) {
  crawler_ja(list(f='_buscarCidade', uf=uf), "//select[@id='cidade_serventia']//option[@value!='']")
}
scraper_muni <- function(node) {
  result <- c(xmlGetAttr(node, 'value'), xmlValue(node))
  return(result)
}
crawler_vara <- function(muni, tipo_justica) {
  crawler_ja(list(f='formPesquisaProdutividade', tipo_justica=tipo_justica, cidade_serventia=muni, anos=''), 
             "//table[@class='display']//tr[not(th)]")
}
scraper_vara <- function(node) {
  lista <- xmlChildren(node)
  parse <- function(s) {
    ifelse(is.null(xmlChildren(s)$a), xmlValue(s), str_extract(xmlGetAttr(xmlChildren(s)$a,'onclick'),'\\b[0-9]{1,5}\\b'))
  }
  dados <- sapply(lista, parse)
  dados <- as.vector(dados[names(dados) %in% 'td'])
  return(dados)
}
crawler_prod <- function(vara) {
  post_data <- list(f='formDadosServentia', SEQ_SERVENTIA_JUDICIAL=vara, ano='')
  html <- crawler_ja(post_data, raw = TRUE)
  n1 <- crawler_ja(xpath="//fieldset[@id='marca']//table[@class='tablesorter']//tr[not(th)]", html=html)
  n2 <- crawler_ja(xpath="//*[text()[contains(.,'Produtividades dos')]]/following::table[@id='display']//tbody//tr", html=html)
  n3 <- crawler_ja(xpath="//div[@id='accordion']//fieldset[@id=not('marca')]//table[@class='tablesorter']", html=html)
  return(list(n1, n2, n3))
}
scraper_prod_vara <- function(node) {
  lista <- xmlChildren(node)
  parse <- function(s) {
    ifelse(is.null(xmlChildren(s)$a), xmlValue(s), str_split_fixed(str_extract(xmlGetAttr(xmlChildren(s)$a,'onclick'),'SEQ_PRODUTIVIDADE_SERVENTIA=([0-9]{1,7})'),'=',2)[2])
  }
  dados <- sapply(lista, parse)
  dados <- as.vector(dados[names(dados) %in% 'td'])
  return(dados)
}
scraper_prod_mag <- function(node) {
  lista <- xmlChildren(node)
  parse <- function(s) {
    ifelse(is.null(xmlChildren(s)$a), xmlValue(s), str_split_fixed(str_extract(xmlGetAttr(xmlChildren(s)$a,'onclick'),'SEQ_PRODUTIVIDADE_MAGISTRADO=([0-9]{1,7})'),'=',2)[2])
  }
  dados <- sapply(lista, parse) 
  dados <- as.vector(dados[names(dados) %in% 'td'])
  return(dados)
}
scraper_prod_info <- function(node) {
  return('not implemented yet')
}
meta_muni <- function(uf) {
  monta <- function(nodes, uf) return(mutate(ldply(nodes, scraper_muni), uf=uf))
  lista <- lapply(uf, crawler_muni)
  validos <- sapply(lista, length) > 0
  lista <- lista[validos]
  uf <- uf[validos]
  df <- ldply(1:length(lista), function(x) monta(lista[[x]], uf[x]))
  if(nrow(df)>0) {
    names(df) <- c('cod_muni', 'muni', 'nome_uf')
    return(df)
  }
}
meta_vara <- function(muni, justica) {
  monta <- function(nodes, cod_muni) return(mutate(ldply(nodes, scraper_vara), cod_muni=cod_muni))
  lista <- lapply(muni, crawler_vara, tipo_justica=justica)
  validos <- sapply(lista, length) > 0
  lista <- lista[validos]
  muni <- muni[validos]
  df <- ldply(1:length(lista), function(x) monta(lista[[x]], muni[x]))
  if(nrow(df) > 0) {
    names(df) <- c('nome_vara', 'nome_muni', 'nome_uf','tipo_justica', 'cod_vara', 'cod_muni')  
    return(df)
  }
  return(NULL)
}
meta_prod <- function(varas) {
  monta_vara <- function(nodes, cod_vara) return(mutate(ldply(nodes, scraper_prod_vara), cod_vara=cod_vara))
  monta_mag <- function(nodes, cod_vara) return(mutate(ldply(nodes, scraper_prod_mag), cod_vara=cod_vara))
  monta_info <- function(nodes, cod_vara) return(mutate(ldply(nodes, scraper_prod_info), cod_vara=cod_vara))
  lista <- lapply(varas, crawler_prod)
  validos <- sapply(lista, length) > 0
  lista <- lista[validos]
  varas <- varas[validos]
  df_vara <- ldply(1:length(lista), function(x) monta_vara(lista[[x]][[1]], varas[x]))  
  df_mag <- ldply(1:length(lista), function(x) monta_mag(lista[[x]][[2]], varas[x]))  
  df_info <- ldply(1:length(lista), function(x) monta_info(lista[[x]][[3]], varas[x]))  
  
  if(nrow(df_vara)>0) {
    names(df_vara) <- c('nome_vara', 'nome_uf', 'periodo', 'data_inicio', 'data_fim', 'cod_prod', 'cod_vara')
  } else {
    df_vara <- NULL
  }
  if(nrow(df_mag>0)) {
    names(df_mag) <- c('nome_mag', 'posicao', 'periodo', 'data_inicio', 'data_fim', 'cod_prod', 'cod_vara')
  } else {
    df_mag <- NULL
  }
  list(df_vara, df_mag, df_info)
}
