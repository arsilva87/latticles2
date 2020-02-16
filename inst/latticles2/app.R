# --------------------------------------------------
# checar pacote
check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}
packages <- c("xml2", "shiny")
check.packages(packages)

# --------------------------------------------------
# latticles em lote

latticles2 <- function(direc, anos)
{
   # arquivos zip no diretorio
   files <- list.files(direc)
   n <- length(files)

   # loop
   xmls <- quanti <- list()
   for(i in 1:n) {
      xmls[[i]] <- read_xml(paste(direc, "\\", files[i], sep = ""))
      quanti[[i]] <- artigosLattes(XML = xmls[[i]], anos = anos)
   }

   # artigos pub
   spam <- do.call("rbind", lapply(quanti, "[[", "metricas"))

   # n revistas em que se publicou
   #nrev <- sapply(sapply(lapply(lapply(quanti, "[[", "planilha"), "[[", "ISSN"), unique), length)

   # trabalhos e resumos em eventos 
   spamT <- do.call("rbind", lapply(quanti, "[[", "trabalhos"))

   # livros e capitulos
   spamL <- do.call("rbind", lapply(quanti, "[[", "livros"))
   spamC <- do.call("rbind", lapply(quanti, "[[", "capitulos"))

   # orientacoes
   spamO <- do.call("rbind", lapply(quanti, "[[", "orientacoes"))

   # softwares e patentes
   spamS <- do.call("rbind", lapply(quanti, "[[", "software"))
   spamP <- do.call("rbind", lapply(quanti, "[[", "patentes"))

   # output
   nomes <- sapply(quanti, "[[", "nome")
   out <- cbind(artigos = spam, trabalhos = spamT,
      livros = spamL, capitulos = spamC, orien = spamO, 
      soft = spamS, pat = spamP)
   out2 <- data.frame("Nome                                                             " = c(nomes, "TOTAL"), 
      Data = c(sapply(quanti, "[[", "atual"), " "), 
      rbind(out, TOTAL = colSums(out)))
   rownames(out2) <- NULL
   invisible(out2)
}

# ----------------------------------------------------------
# function artigosLattes
qualis <- read.csv("Qualis_Novos.csv", colClasses = "character")

artigosLattes <- function(XML, anos)
{
   if(is.null(XML)) return()
   if(is.null(anos)) return()

   nome <- attr(as_list(xml_children(XML)[[1]]), "NOME-COMPLETO")
   atuali <- attr(as_list(XML)$"CURRICULO-VITAE", "DATA-ATUALIZACAO")
   datat <- strsplit(atuali, "")[[1]]
   datat. <- paste0(datat[1], datat[2], "-", datat[3], datat[4], "-", 
      datat[5], datat[6], datat[7], datat[8])

   # qualis -----------------------------------------------------------------
   qualis$ISSN. <- sapply(strsplit(qualis$ISSN, "-"), paste, collapse = "")

   # artigos publicados ------------------------------------------------------
   prod <- try(xml_children(XML)[[grep("ARTIGOS-PUBLICADOS", xml_children(XML))]], silent = TRUE)
   artpub <- try(as_list(xml_children(prod))[[grep("ARTIGOS-PUBLICADOS", xml_children(prod))]], silent = TRUE)

   if (class(artpub) != "try-error" & !is.null(artpub)) {
      # loop
      n <- length(artpub)
      artigodat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Periodico = NA, ISSN = NA,
         Qualis = NA, Vol = NA, pInicial = NA, pFinal = NA)
      i = 1
      repeat {
         art <- artpub[[i]]
         artigodat[i, ] <- 
            c(strsplit(attr(art[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
            attr(art[["DADOS-BASICOS-DO-ARTIGO"]], "ANO-DO-ARTIGO"),
            attr(art[["DADOS-BASICOS-DO-ARTIGO"]], "TITULO-DO-ARTIGO"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "TITULO-DO-PERIODICO-OU-REVISTA"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"),
            qualis[grep(attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"), qualis$ISSN.)[1], "Estrato"],
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "VOLUME"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "PAGINA-INICIAL"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "PAGINA-FINAL"))
         i = i + 1
         if(i > n) break()
      }
      artigodat$Qualis[is.na(artigodat$Qualis)] = "C"
   } else { 
      artigodat = data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Periodico = NA, ISSN = NA,
         Qualis = NA, Vol = NA, pInicial = NA, pFinal = NA)
   }
   artigodat$Qualis <- factor(artigodat$Qualis, 
      levels = c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C", "NP"))

   # trabalhos em eventos ----------------------------------------------------
   trab <- try(as_list(xml_children(prod))[[grep("TRABALHOS-EM-EVENTOS", xml_children(prod))]], silent = TRUE)
   if (class(trab) != "try-error" & !is.null(trab)) {
      # loop
      n3 <- length(trab)
      trabdat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Natureza = NA, Evento = NA, 
         Local = NA, ISBN = NA, Vol = NA, pInicial = NA, pFinal = NA)
      i = 1
      repeat {
         a <- trab[[i]]
         trabdat[i, ] <- 
            c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "ANO-DE-REALIZACAO"),
            attr(a[["DADOS-BASICOS-DO-TRABALHO"]], "TITULO-DO-TRABALHO"),
            attr(a[["DADOS-BASICOS-DO-TRABALHO"]], "NATUREZA"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "NOME-DO-EVENTO"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "CIDADE-DO-EVENTO"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "ISBN"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "VOLUME"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "PAGINA-INICIAL"),
            attr(a[["DETALHAMENTO-DO-TRABALHO"]], "PAGINA-FINAL"))
         i = i + 1
         if(i > n3) break()
      }
   } else { 
      trabdat = data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Natureza = NA, Evento = NA, 
         Local = NA, ISBN = NA, Vol = NA, pInicial = NA, pFinal = NA)   
   }
   trabdat$Natureza <- factor(trabdat$Natureza, 
	levels = c("COMPLETO", "RESUMO", "RESUMO_EXPANDIDO"))
   
   # livros ------------------------------------------------------------------
   licap <- try(as_list(xml_children(prod))[[grep("LIVROS-E-CAPITULOS", xml_children(prod))]], silent = TRUE)
   livro <- try(licap[["LIVROS-PUBLICADOS-OU-ORGANIZADOS"]], silent = TRUE)
   if (class(livro) != "try-error" & !is.null(livro)) {
      # loop
      n4 <- length(livro)
      livrodat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Ed = NA,
         Local = NA, Editora = NA, ISBN = NA, Pags = NA)
      i = 1
      repeat {
         a <- livro[[i]]
         livrodat[i, ] <- 
            c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
            attr(a[["DADOS-BASICOS-DO-LIVRO"]], "ANO"),
            attr(a[["DADOS-BASICOS-DO-LIVRO"]], "TITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "NUMERO-DA-EDICAO-REVISAO"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "CIDADE-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "NOME-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "ISBN"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "NUMERO-DE-PAGINAS"))
         i = i + 1
         if(i > n4) break()
      }  
   } else { 
      livrodat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Ed = NA,
         Local = NA, Editora = NA, ISBN = NA, Pags = NA)
   }

   # capitulos ---------------------------------------------------------------
   cap <- try(licap[["CAPITULOS-DE-LIVROS-PUBLICADOS"]], silent = TRUE)
   if (class(cap) != "try-error" & !is.null(cap)) {
      # loop
      n4. <- length(cap)
      capdat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Livro = NA, Ed = NA,
         Local = NA, Editora = NA, ISBN = NA, pInicial = NA, pFinal = NA)
      i = 1
      repeat {
         a <- cap[[i]]
         capdat[i, ] <- 
            c(strsplit(attr(a[["AUTORES"]], "NOME-PARA-CITACAO"), ";")[[1]][1],
            attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "ANO"),
            attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "TITULO-DO-CAPITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "TITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "NUMERO-DA-EDICAO-REVISAO"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "CIDADE-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "NOME-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "ISBN"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "PAGINA-INICIAL"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "PAGINA-FINAL"))
         i = i + 1
         if(i > n4.) break()
      }
   } else { 
      capdat <- data.frame(Autor1 = NA, Ano = NA, Titulo = NA, Livro = NA, Ed = NA,
         Local = NA, Editora = NA, ISBN = NA, pInicial = NA, pFinal = NA)
   }

   # orientacoes -------------------------------------------------------------
   orien <- try(as_list(xml_children(XML))[[grep("ORIENTACOES-CONCLUIDAS", xml_children(XML))]][["ORIENTACOES-CONCLUIDAS"]], silent = TRUE)
   if (class(orien) != "try-error" & !is.null(orien)) {
      # loop
      MS = DS = Outra = factor(levels = seq(anos[1], anos[2]))
      for(i in grep("MESTRADO", orien)) {
      if(is.null(attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"]], "ANO")))
         MS[i] <- NA else
         MS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"]], "ANO")
      }
      for(i in grep("DOUTORADO", orien)) {
         if(is.null(attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"]], "ANO")))
            DS[i] <- NA else
            DS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"]], "ANO")
      }
      for(i in grep("OUTRAS", orien)) {
         if(is.null(attr(orien[[i]][["DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"]], "ANO")))
            Outra[i] <- NA else
            Outra[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"]], "ANO")
      }
      outorien <- c(Doutorado = sum(table(DS)), Mestrado = sum(table(MS)), 
         Outras_orient = sum(table(Outra)))
   } else { 
      outorien = c(Mestrado = 0, Doutorado = 0, Outras_orient = 0)
   }

   # softwares ---------------------------------------------------------------
   tec1 <- try(xml_children(XML)[[grep("SOFTWARE", xml_children(XML))]], silent = TRUE)
   soft <- try(as_list(xml_children(tec1)[grep("SOFTWARE", xml_children(tec1))]), silent = TRUE)
   if (class(soft) != "try-error" & !is.null(soft)) {
      # loop
      n5 <- length(soft)
      softdat <- data.frame(Ano = NA, Titulo = NA, Registro = NA)
      i = 1
      repeat {
         s1 <- soft[[i]]
         softdat[i, ] <- 
            c(attr(s1[["DADOS-BASICOS-DO-SOFTWARE"]], "ANO"),
              attr(s1[["DADOS-BASICOS-DO-SOFTWARE"]], "TITULO-DO-SOFTWARE"),
              !is.null(attr(s1[["DETALHAMENTO-DO-SOFTWARE"]][["REGISTRO-OU-PATENTE"]], 
                 "CODIGO-DO-REGISTRO-OU-PATENTE")))
         i = i + 1
         if(i > n5) break()
      }
   } else { 
      softdat = data.frame(Ano = NA, Titulo = NA, Registro = NA)  
   }
   softdat$Registro <- factor(softdat$Registro, levels = c(FALSE, TRUE))

   # patentes -----------------------------------------------------------------
   tec2 <- try(xml_children(XML)[[grep("PATENTE", xml_children(XML))]], silent = TRUE)
   pat <- try(as_list(xml_children(tec2)[grep("PATENTE", xml_children(tec2))]), silent = TRUE)
   if (class(pat) != "try-error" & !is.null(pat)) {
      # loop
      n6 <- length(pat)
      patdat <- data.frame(Ano = NA, Titulo = NA, Registro = NA)
      i = 1
      repeat {
         p1 <- pat[[i]]
         patdat[i, ] <- 
            c(attr(p1[["DADOS-BASICOS-DA-PATENTE"]], "ANO-DESENVOLVIMENTO"),
              attr(p1[["DADOS-BASICOS-DA-PATENTE"]], "TITULO"),
              !is.null(attr(p1[["DETALHAMENTO-DA-PATENTE"]][["REGISTRO-OU-PATENTE"]], 
                 "CODIGO-DO-REGISTRO-OU-PATENTE")))
         i = i + 1
         if(i > n6) break()
      }
   } else { 
      patdat = data.frame(Ano = NA, Titulo = NA, Registro = NA)  
   }
   patdat$Registro <- factor(patdat$Registro, levels = c(FALSE, TRUE))

   # output ------------------------------------------------------------------
   artigodat <- subset(artigodat, Ano >= anos[1] & Ano <= anos[2])
   metrics <- table(artigodat$Qualis)
   resumo <- c(metrics, Total_art = sum(metrics, na.rm=TRUE), 
      Qualis_A = sum(metrics[1:4], na.rm=TRUE))

   trabdat <- subset(trabdat, Ano >= anos[1] & Ano <= anos[2])
   tabT <- table(trabdat$Natureza)
   resumoT <- c(tabT, Total_res = sum(tabT, na.rm=TRUE))

   outlivro <- c(Livros = sum(livrodat$Ano >= anos[1] & livrodat$Ano <= anos[2], na.rm=TRUE))
   outcap <- c(Capitulos = sum(capdat$Ano >= anos[1] & capdat$Ano <= anos[2], na.rm=TRUE))

   softdat <- subset(softdat, Ano >= anos[1] & Ano <= anos[2])
   outsoft <- table(softdat$Registro)
   names(outsoft) <- c("Software_nReg", "Software_Reg")

   patdat <- subset(patdat, Ano >= anos[1] & Ano <= anos[2])
   outpat <- c(Patentes = nrow(patdat))

   out <- list(nome = nome, atual = datat., metricas = resumo, 
      planilha = artigodat, trabalhos = resumoT,
      livros = outlivro, capitulos = outcap, orientacoes = outorien,
      software = outsoft, patentes = outpat )
   return(out)
}


# server ---------------------------------------------------
server <- shinyServer(function(input, output, session){
   direc <-reactive({
      f1 <- input$direc
      if(is.null(f1)) return() else f1
   })
   output$direc <- renderPrint(direc())
   anos <- reactive({
      ano <- input$range
      if(is.null(ano)) return() else as.integer(ano)
   })
   output$producao <- renderTable({ 
      latticles2(direc(), anos())
   }, digits = 0, spacing = "xs")
   output$downloadData <- downloadHandler(
         filename = function() {
            paste("producao", ".csv", sep = "")
         },
         content = function(file) {
            write.csv(latticles2(direc(), anos()), file)
   })
   onSessionEnded = function(callback) {
      "Registers the given callback to be invoked when the session is closed
      (i.e. the connection to the client has been severed). The return value
      is a function which unregisters the callback. If multiple callbacks are
      registered, the order in which they are invoked is not guaranteed."
      return(.closedCallbacks$register(callback))
   }
   session$onSessionEnded(function() {
        stopApp()
   })
})


# ui -------------------------------------------------------------
ui <- fluidPage(
   # App title
   titlePanel("Latticles App 2.0"),
   helpText("Importe artigos de um grupo de pesquisadores, classifique pelo 
             Qualis-CAPES (novo), contabilize producoes tecnicas e mais, tudo de forma 
             automatizada a partir de arquivos XML do curriculo Lattes."),
   sidebarLayout(
      # Menu here
      sidebarPanel(width = 3,
         textInput("direc", label = 'Informe o caminho do diretorio contendo os arquivos XML do Lattes'),
         br(),
         sliderInput("range", "Intervalo de tempo:",
              min = 1999, max = 2021, value = c(2017, 2019), step = 1, sep = ""),
         submitButton("Executar"),
         tags$hr(),
         h6("Powered by:",
         tags$img(src = "agrometrics.jpg", heigth = 110, width = 110),
         tags$img(src = "logoIF.png", heigth = 90, width = 90),
         helpText("Desenvolvido por: da Silva, A. R. (C) Copyright 2020\n
                  <anderson.silva[at]ifgoiano.edu.br>"),
         helpText("da Silva, A.R. (2019). Latticles: avaliacao eficiente da 
                  producao cientifica. Programa de computador. 
                  Registro INPI: BR512019001166-0. <https://arsilva87.github.io/latticles_app/>,
                  <https://arsilva87.github.io/latticles2/>"),
	 helpText("GNU General Public Licence, version 3.0")
      )
   ),
      mainPanel(
         tabPanel("Producoes",
            downloadButton("downloadData", "Exportar planilha (.csv)"),
            br(), tableOutput("producao"),
            helpText("Legenda. A1-C: Artigos qualificados no periodo, 
              NP: artigos publicados em nao periodicos, 
              Total_art: total de artigos no periodo,
              Qualis_A: total de artigos A1-A4, 
              COMPLETO: trabalhos completos em anais de eventos,
              RESUMO/RESUMO_EXPANDIDO: resumos/resumos expandidos em eventos, 
              Livros/Capitulos: livros/capitulos publicados no periodo,
              Doutorado/Mestrado/Outras_orient: orientacoes concluidas no periodo,
              Software_nReg/Reg: programas de computador nao/registrados,
              Patentes: patentes concedidas.")
         )
      )
   )
)

# Run the app
shinyApp(ui = ui, server = server)

