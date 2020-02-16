# Descrição
Versão 2.0 do app latticles, um pacote shiny do software R criado para facilitar a importação de artigos, classificação pelo Qualis-Capes (novo), contabilizar produções técnicas e mais, tudo de forma automatizada a partir de arquivos XML do currículo Lattes. Com esta nova versão, é possível analisar simultaneamente os currículos de um grupo de pesquisadores. Para isso, basta inserir todos os arquivos XML num diretório e informar no web-app o caminho para o diretório, bem como o intervalo de tempo desejado.

Latticles2 é uma iniciativa sem fins lucrativos desenvolvido especialmente para autoavaliação de Programas de Pós-Graduação nacionais, com o objetivo primário de promover o crescimento e fortalecimento destes.

# Instalação

Para instalação do app, será necessária uma versão recente (>3.4) do software livre R (www.r-project.org). Em seguida, o pacote devtools deverá ser instalado.

```r
install.packages(devtools)
```
Depois, basta executar o comando de instalação do latticles2 a partir da plataforma GitHub.
```r
devtools::install_github("arsilva87/latticles2")
```

# Execute o app
```r
latticles2::latticles2()
```

# Contato
Desenvolvedor/mantenedor do pacote: da Silva, A. R. <anderson.silva@ifgoiano.edu.br>.
Instituto Federal Goiano - Campus Urutaí

# Citação
Para citar o programa em publicações, use:

da Silva, A.R. (2019). Latticles: avaliacao eficiente da producao cientifica. Programa de computador. Registro INPI: BR512019001166-0. <https://arsilva87.github.io/latticles_app>, <https://arsilva87.github.io/latticles2>

# Licenca
GNU General Public Licence, version 3.0

Latticles2 (C) Copyright 2020 da Silva, A.R.
