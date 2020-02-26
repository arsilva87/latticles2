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


# ----------------------------------------------------------
# function artigosLattes
qualis <- read.csv("Qualis_Novos.csv", colClasses = "character")

artigosLattes <- function(XML, anos)
{
   if(is.null(XML)) return()
   if(is.null(anos)) return()

   nome <- attr(as_list(xml_children(XML)[[1]]), "NOME-COMPLETO")
   atuali <- attr(as_list(xml_parent(XML))$"CURRICULO-VITAE", "DATA-ATUALIZACAO")
   datat <- strsplit(atuali, "")[[1]]
   datat. <- paste0(datat[1], datat[2], "-", datat[3], datat[4], "-", 
      datat[5], datat[6], datat[7], datat[8])

   # qualis -----------------------------------------------------------------
   qualis$ISSN. <- sapply(strsplit(qualis$ISSN, "-"), paste, collapse = "")

   # artigos publicados ------------------------------------------------------
   prod <- try(xml_children(XML)[[grep("PRODUCAO-BIBLIOGRAFICA", xml_children(XML))]], silent = TRUE)
   artpub <- try(as_list(xml_children(prod))[[grep("ARTIGOS-PUBLICADOS", xml_children(prod))]], silent = TRUE)

   if (class(artpub) != "try-error" & !is.null(artpub)) {
      # loop
      n <- length(artpub)
      artigodat <- data.frame(Ano = NA, Periodico = NA, ISSN = NA, Qualis = NA)
      i = 1
      repeat {
         art <- artpub[[i]]
         artigodat[i, ] <- 
            c(attr(art[["DADOS-BASICOS-DO-ARTIGO"]], "ANO-DO-ARTIGO"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "TITULO-DO-PERIODICO-OU-REVISTA"),
            attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"),
            qualis[grep(attr(art[["DETALHAMENTO-DO-ARTIGO"]], "ISSN"), qualis$ISSN.)[1], "Estrato"])
         i = i + 1
         if(i > n) break()
      }
      artigodat$Qualis[is.na(artigodat$Qualis)] = "C"
   } else { 
      artigodat = data.frame(Ano = NA, Periodico = NA, ISSN = NA, Qualis = NA)
   }
   artigodat$Qualis <- factor(artigodat$Qualis, 
      levels = c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C", "NP"))

   # trabalhos em eventos ----------------------------------------------------
   trab <- try(as_list(xml_children(prod))[[grep("TRABALHOS-EM-EVENTOS", xml_children(prod))]], silent = TRUE)
   if (class(trab) != "try-error" & !is.null(trab)) {
      trabdat <- data.frame(Ano = as.numeric(sapply(lapply(trab, "[[", "DETALHAMENTO-DO-TRABALHO"), attr, "ANO-DE-REALIZACAO")), 
         Natureza = sapply(lapply(trab, "[[", "DADOS-BASICOS-DO-TRABALHO"), attr, "NATUREZA"))
   } else { 
      trabdat = data.frame(Ano = NA, Natureza = NA)   
   }
   trabdat$Natureza <- factor(trabdat$Natureza, 
      levels = c("COMPLETO", "RESUMO", "RESUMO_EXPANDIDO"),
	labels = c("Completo", "Res_simples", "Res_expandido"))
   
   # livros ------------------------------------------------------------------
   licap <- try(as_list(xml_children(prod))[[grep("LIVROS-E-CAPITULOS", xml_children(prod))]], silent = TRUE)
   livro <- try(licap[["LIVROS-PUBLICADOS-OU-ORGANIZADOS"]], silent = TRUE)
   if (class(livro) != "try-error" & !is.null(livro)) {
      # loop
      n4 <- length(livro)
      livrodat <- data.frame(Ano = NA, Titulo = NA, Editora = NA, ISBN = NA)
      i = 1
      repeat {
         a <- livro[[i]]
         livrodat[i, ] <- 
            c(attr(a[["DADOS-BASICOS-DO-LIVRO"]], "ANO"),
            attr(a[["DADOS-BASICOS-DO-LIVRO"]], "TITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "NOME-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-LIVRO"]], "ISBN"))
         i = i + 1
         if(i > n4) break()
      }  
   } else { 
      livrodat <- data.frame(Ano = NA, Titulo = NA, Editora = NA, ISBN = NA)
   }

   # capitulos ---------------------------------------------------------------
   cap <- try(licap[["CAPITULOS-DE-LIVROS-PUBLICADOS"]], silent = TRUE)
   if (class(cap) != "try-error" & !is.null(cap)) {
      # loop
      n4. <- length(cap)
      capdat <- data.frame(Ano = NA, Titulo = NA, Livro = NA, Editora = NA, ISBN = NA)
      i = 1
      repeat {
         a <- cap[[i]]
         capdat[i, ] <- 
            c(attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "ANO"),
            attr(a[["DADOS-BASICOS-DO-CAPITULO"]], "TITULO-DO-CAPITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "TITULO-DO-LIVRO"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "NOME-DA-EDITORA"),
            attr(a[["DETALHAMENTO-DO-CAPITULO"]], "ISBN"))
         i = i + 1
         if(i > n4.) break()
      }
   } else { 
      capdat <- data.frame(Ano = NA, Titulo = NA, Livro = NA, Editora = NA, ISBN = NA)
   }

   # orientacoes -------------------------------------------------------------
   orien <- try(as_list(xml_children(XML))[[grep("ORIENTACOES-CONCLUIDAS", xml_children(XML))]][["ORIENTACOES-CONCLUIDAS"]], silent = TRUE)
   if (class(orien) != "try-error" & !is.null(orien)) {
      # loop
      MS = DS = Outra = c()
      for(i in grep("MESTRADO", orien)) {
         MS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-MESTRADO"]], "ANO")
      }
      for(i in grep("DOUTORADO", orien)) {
         DS[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-ORIENTACOES-CONCLUIDAS-PARA-DOUTORADO"]], "ANO")
      }
      for(i in grep("OUTRAS", orien)) {
         Outra[i] <- attr(orien[[i]][["DADOS-BASICOS-DE-OUTRAS-ORIENTACOES-CONCLUIDAS"]], "ANO")
      }
      outorien <- c(Mestrado = sum(as.numeric(MS) >= anos[1] & as.numeric(MS) <= anos[2], na.rm=TRUE), 
         Doutorado = sum(as.numeric(DS) >= anos[1] & as.numeric(DS) <= anos[2], na.rm=TRUE),
         Outras_orient = sum(as.numeric(Outra) >= anos[1] & as.numeric(Outra) <= anos[2], na.rm=TRUE))
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

   # artigos em revistas ou jornais -------------------------------------------
   artt <- try(as_list(xml_children(prod)[[grep("TEXTOS-EM-JORNAIS-OU-REVISTAS", xml_children(prod))]]), silent = TRUE)
   if (class(artt) != "try-error" & !is.null(artt)) {
      arttdat <- data.frame(Ano = as.numeric(sapply(lapply(artt, "[[", "DADOS-BASICOS-DO-TEXTO"), attr, "ANO-DO-TEXTO")), 
         Titulo = NA, Revista = NA)
   } else { 
      arttdat = data.frame(Ano = NA, Titulo = NA, Revista = NA)  
   }

   # org evento --------------------------------------------------------------
   t1 <- try(xml_children(XML)[[grep("PRODUCAO-TECNICA", xml_children(XML))]], silent = TRUE)
   t2 <- try(xml_children(t1)[[grep("DEMAIS-TIPOS-DE-PRODUCAO-TECNICA>", xml_children(t1))]], silent = TRUE)
   org <- try(as_list(xml_children(t2)[grep("ORGANIZACAO-DE-EVENTO", xml_children(t2))]), silent = TRUE)
   if (class(org) != "try-error" & !is.null(org)) {
      orgdat <- data.frame(Ano = as.numeric(sapply(lapply(org, "[[", "DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"), attr, "ANO")), 
         Titulo = sapply(lapply(org, "[[", "DADOS-BASICOS-DA-ORGANIZACAO-DE-EVENTO"), attr, "TITULO"))
   } else { 
      orgdat = data.frame(Ano = NA, Titulo = NA)  
   }

   # part em bancas --------------------------------------------------------------
   conc <- try(xml_children(XML)[[grep("PARTICIPACAO-EM-BANCA-TRABALHOS-CONCLUSAO", xml_children(XML))]], silent = TRUE)
   ban <- try(xml_children(conc)[[grep("PARTICIPACAO-EM-BANCA-TRABALHOS-CONCLUSAO", xml_children(conc))]], silent = TRUE)
   banGD <- try(as_list(xml_children(ban)[grep("PARTICIPACAO-EM-BANCA-DE-GRADUACAO", xml_children(ban))]), silent = TRUE)
   banQL <- try(as_list(xml_children(ban)[grep("PARTICIPACAO-EM-BANCA-DE-EXAME-QUALIFICACAO", xml_children(ban))]), silent = TRUE)
   banMS <- try(as_list(xml_children(ban)[grep("PARTICIPACAO-EM-BANCA-DE-MESTRADO", xml_children(ban))]), silent = TRUE)
   banDS <- try(as_list(xml_children(ban)[grep("PARTICIPACAO-EM-BANCA-DE-DOUTORADO", xml_children(ban))]), silent = TRUE)
   if (class(banGD) != "try-error" & !is.null(banGD)) {
      contGD <- sapply(lapply(banGD, "[[", "DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-GRADUACAO"), attr, "ANO")
   } else { contGD <- NA }
   if (class(banQL) != "try-error" & !is.null(banQL)) {
      contQL <- sapply(lapply(banQL, "[[", "DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-EXAME-QUALIFICACAO"), attr, "ANO")
   } else { contQL <- NA }
   if (class(banMS) != "try-error" & !is.null(banMS)) {
      contMS <- sapply(lapply(banMS, "[[", "DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-MESTRADO"), attr, "ANO")
   } else { contMS <- NA }
   if (class(banDS) != "try-error" & !is.null(banDS)) {
      contDS <- sapply(lapply(banDS, "[[", "DADOS-BASICOS-DA-PARTICIPACAO-EM-BANCA-DE-DOUTORADO"), attr, "ANO")
   } else { contDS <- NA }

   # output ------------------------------------------------------------------
   artigodat <- subset(artigodat, Ano >= anos[1] & Ano <= anos[2])
   metrics <- table(artigodat$Qualis)
   resumo <- c(metrics, Total_art = sum(metrics, na.rm=TRUE), 
      Qualis_A = sum(metrics[1:4], na.rm=TRUE))

   arttdat <- subset(arttdat, Ano >= anos[1] & Ano <= anos[2])
   outartt <- c(Artigos_tec = nrow(arttdat))

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

   orgdat <- subset(orgdat, Ano >= anos[1] & Ano <= anos[2])
   outorg <- c(Org_eventos = nrow(orgdat))

   bancas <- c(
      bancas_Grad = sum(contGD >= anos[1] & contGD <= anos[2], na.rm = TRUE),
      bancas_Qualif = sum(contQL >= anos[1] & contQL <= anos[2], na.rm = TRUE),
      bancas_MS = sum(contMS >= anos[1] & contMS <= anos[2], na.rm = TRUE),
      bancas_DS = sum(contDS >= anos[1] & contDS <= anos[2], na.rm = TRUE) )

   out <- list(nome = nome, atual = datat., metricas = resumo, 
      artigos_tec = outartt, trabalhos = resumoT,
      livros = outlivro, capitulos = outcap, orientacoes = outorien,
      software = outsoft, patentes = outpat, org = outorg, bancas = bancas)
   return(out)
}


# --------------------------------------------------
# latticles para multiplos curriculos

latticles_mult <- function(direc, anos)
{
   if(is.null(direc)) return()
   if(is.null(anos)) return()

   # arquivos zip no diretorio
   files <- list.files(direc, pattern = "zip|xml")
   n <- length(files)

   # loop
   progress <- shiny::Progress$new()
   on.exit(progress$close())
   xmls <- quanti <- list()
   for(i in 1:n) {
      xmls[[i]] <- read_xml(paste(direc, "\\", files[i], sep = ""))
      quanti[[i]] <- artigosLattes(XML = xmls[[i]], anos = anos)
      progress$set(message = "Computando as producoes", value = i/n,
         detail = paste("Curriculo", i, "de", n))
   }

   # artigos pub
   spam <- do.call("rbind", lapply(quanti, "[[", "metricas"))

   # artigos em revistas ou jornais
   spamAt <- do.call("rbind", lapply(quanti, "[[", "artigos_tec"))

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

   # org eventos
   spamOr <- do.call("rbind", lapply(quanti, "[[", "org"))

   # bancas
   spamB <- do.call("rbind", lapply(quanti, "[[", "bancas"))

   # output
   nomes <- sapply(quanti, "[[", "nome")
   out <- cbind(artigos = spam, artigos_tec = spamAt, trabalhos = spamT,
      livros = spamL, capitulos = spamC, orien = spamO, 
      soft = spamS, pat = spamP, org = spamOr, bancas = spamB)
   out2 <- data.frame("Nome-----------------------------------------------------------" = c(nomes, "TOTAL"), 
      Data = c(sapply(quanti, "[[", "atual"), " "), 
      rbind(out, TOTAL = colSums(out)))
   rownames(out2) <- NULL
   invisible(out2)
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
      if(is.null(ano)) return(NULL) else as.integer(ano)
   })
   output$producao <- renderTable({ 
      latticles_mult(direc(), anos())
   }, digits = 0, spacing = "xs")
   output$downloadData <- downloadHandler(
         filename = function() {
            paste("producao", ".csv", sep = "")
         },
         content = function(file) {
            write.csv(latticles_mult(direc(), anos()), file)
   })
   onSessionEnded = function(callback) {
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
         helpText("Desenvolvido por: Da Silva, A. R. (C) Copyright 2020\n
                  <anderson.silva[at]ifgoiano.edu.br>"),
         helpText("Da Silva, A.R. (2019). Latticles: avaliacao eficiente da 
                  producao cientifica. Programa de computador. 
                  Registro INPI: BR512019001166-0. <https://arsilva87.github.io/latticles_app/>"),
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
              Artigos_tec: artigos tecnicos em revistas ou jornais,
              Completo: trabalhos completos em anais de eventos,
              Res_simples/Res_expandido: resumos em anais de eventos, 
              Livros/Capitulos: livros/capitulos publicados no periodo,
              Doutorado/Mestrado/Outras_orient: orientacoes concluidas no periodo,
              Software_nReg/Reg: programas de computador nao/registrados,
              Patentes: patentes concedidas,
              Org_eventos: eventos tecnico-cientificos organizados,
              bancas_Grad/_Qualif/_MS/_DS: participacao em bancas de trabalhos de 
                conclusao de graduacao, exames de qualificacao, defesas de mestrado 
                e doutorado.")
         )
      )
   )
)

# Run the app
shinyApp(ui = ui, server = server)
