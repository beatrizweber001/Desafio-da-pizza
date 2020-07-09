      *Divisão de identificação do programa
       identification division.
       program-id. "desafiopizza".
       author. "Anderson Weber Junior".
       installation. "PC".
       date-written. 09/07/2020.
       date-compiled. 09/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 areapizza                            pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 diferenca                            pic S9(04)v99.
           05 filler                               pic x(03)
              value " % ".


       77  ind                                     pic 9(02).
       77  aux_ind                                 pic 9(02).
       77  controle                                pic 9(05).
       77  aux                                     pic 9(05)v99.
       77  menu                                    pic x(01).



      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

      *-----------------------------------------------------------------
      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move 1 to controle
           move   "S"       to     menu
           .
       inicializa-exit.
           exit.

      *-----------------------------------------------------------------
       processamento section.
           move 0 to ind

      *perform para inserir ate 20 informacoes sobre pizzas
           perform until menu <> "S"
               perform inserirdados
           end-perform

      *zerando o valor ind
           move 0 to ind

      *reorganizando por melhor custo beneficio
           perform until controle <> 1
               perform ordenando
           end-perform

      *zerando o valor ind
           move 0 to ind

      *definindo diferenca em porcento
           perform until ind = 20 or ind = aux_ind
               perform calculodiferenca
           end-perform

      *perform para mostrar os dados calculados e inseridos
           perform varying ind from 1 by 1 until ind > 20
       or nome(ind) = space
               display relatorio(ind)
           end-perform
           .
       processamento-exit.
           exit.

      *-----------------------------------------------------------------
      *sessao para declarar cada pizza
       inserirdados section.

           display erase
           add 1 to ind

           if ind > 20 then
               display "Vc atingiu o limite de 20 pizzas"
           else
               display "Informe o nome da pizza "
               accept nome(ind)

               display "Informe o diametro "
               accept diametro(ind)

               display "Informe o preco "
               accept preco(ind)
           end-if

      *levando para sessao de calculo
           perform calculoarea


      *conferir se quer continuar
           display "deseja cadastrar mais uma pizza? ('S'/'N')"
           accept menu
           move function upper-case (menu) to menu

      *variavel para auxiliar para ordernar
           if menu <>"S" then
               move ind to aux_ind
           end-if
           .
       inserirdados-exit.
           exit.

      *-----------------------------------------------------------------
       calculoarea section.
      *calculo da area da pizza
           compute areapizza(ind) = 3,14 * ((diametro(ind)/2) *
           (diametro(ind)/2))

      *calculo para preco por cm 2
           compute preco_cm2(ind) =  preco(ind) / areapizza(ind)
           .
       calculoarea-exit.
           exit.


      *-----------------------------------------------------------------
       ordenando section.
           display erase
           move 1 to ind
           move 0 to controle

      *perform para comparar cada custo beneficio e ordena-los
           perform until ind = 20 or ind = aux_ind
               if preco_cm2(ind) > preco_cm2(ind + 1) then
                   move preco_cm2(ind + 1) to aux
                   move preco_cm2(ind)     to preco_cm2(ind + 1)
                   move aux                to preco_cm2(ind)

                   move 1 to controle
               end-if
               add 1 to ind
           end-perform
           .
       ordenando-exit.
           exit.


      *-----------------------------------------------------------------
       calculodiferenca section.
           add 1 to ind
      *calculo para diferenca, preco(ind) - menorpreco / menorpreco *100
           compute diferenca(ind) = ((preco_cm2(ind) - preco_cm2(1)) /
           preco_cm2(1)) * 100
           .
       calculodiferenca-exit.
           exit.


      *-----------------------------------------------------------------
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.

