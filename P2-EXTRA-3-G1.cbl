       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRA3.
       AUTHOR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ESTADOS-BR.
           05 FILLER       PIC X(21) VALUE "AMAMAZONAS".
           05 FILLER       PIC X(21) VALUE "PAPARA".
           05 FILLER       PIC X(21) VALUE "RRRORAIMA".
           05 FILLER       PIC X(21) VALUE "ACACRE".
           05 FILLER       PIC X(21) VALUE "RORONDONIA".
           05 FILLER       PIC X(21) VALUE "APAMAPA".
           05 FILLER       PIC X(21) VALUE "MTMATOGROSSO".
           05 FILLER       PIC X(21) VALUE "MSMATO GROSSO DO SUL".
           05 FILLER       PIC X(21) VALUE "TOTOCANTINS".
           05 FILLER       PIC X(21) VALUE "GOGOIAS".
           05 FILLER       PIC X(21) VALUE "MGMINAS GERAIS".
           05 FILLER       PIC X(21) VALUE "MAMARANHAO".
           05 FILLER       PIC X(21) VALUE "PIPIAUI".
           05 FILLER       PIC X(21) VALUE "CECEARA".
           05 FILLER       PIC X(21) VALUE "RNRIO GRANDE DO NORTE".
           05 FILLER       PIC X(21) VALUE "PBPARAIBA".
           05 FILLER       PIC X(21) VALUE "PEPERNAMBUCO".
           05 FILLER       PIC X(21) VALUE "ALALAGOAS".
           05 FILLER       PIC X(21) VALUE "SESERGIPE".
           05 FILLER       PIC X(21) VALUE "BABAHIA".
           05 FILLER       PIC X(21) VALUE "ESESPIRITO SANTO".
           05 FILLER       PIC X(21) VALUE "RJRIO DE JANEIRO".
           05 FILLER       PIC X(21) VALUE "SPSAO PAULO".
           05 FILLER       PIC X(21) VALUE "PRPARANA".
           05 FILLER       PIC X(21) VALUE "SCSANTA CATARINA".
           05 FILLER       PIC X(21) VALUE "RSRIO GRANDE DO SUL".
       01 TABELA-ESTADOS REDEFINES ESTADOS-BR.
           05 TAB-ESTADOS OCCURS 26 TIMES
                   INDEXED BY TABELA-ESTADOS-INDEX.
               10 SIGLA-T        PIC X(2).
               10 ESTADO-T       PIC X(19).
       01 DATA-DO-SISTEMA.
           02 ANO          PIC 9(02) VALUE ZEROS.
           02 MES          PIC 9(02) VALUE ZEROS.
           02 DIA          PIC 9(02) VALUE ZEROS.
       01 DADOS.
           05 OPCAO        PIC A(1) VALUE SPACES.
           05 OPCAO-M      PIC 9(1) VALUE ZEROS.
           05 I            PIC 9(2) VALUE ZEROS.
           05 K            PIC 9(4) VALUE ZEROS.
           05 J            PIC 9(4) VALUE ZEROS.
           05 SIGLA        PIC X(2) VALUE SPACES.
           05 CR           PIC X(9) VALUE SPACES.
           05 LINHA        PIC 9(2) VALUE ZEROS.

       SCREEN SECTION.
       01 TELA.
           02 LINE 01 COLUMN 05 PIC 9(02)/ USING DIA.
           02 LINE 01 COLUMN 08 PIC 9(02)/ USING MES.
           02 LINE 01 COLUMN 11 PIC 9(02) USING ANO.
           02 LINE 02 COLUMN 15 VALUE
               "1)PROGRAMA DAS SIGLAS DOS ESTADOS".
       01 MENU.
           02 LINE 05 COLUMN 10 VALUE
                   "************** MENU PRINCIPAL **************".
           02 LINE 07 COLUMN 10 VALUE
               "[1] LISTAR ESTADOS BRASILEIROS".
           02 LINE 08 COLUMN 10 VALUE
               "[2] EXIBIR NOME DO ESTADO PELA SIGLA".
           02 LINE 09 COLUMN 10 VALUE
               "[9] SAIR DO SISTEMA".
           02 LINE 11 COLUMN 10 VALUE
               "DIGITE A OPCAO DESEJADA".
           02 OPT LINE 11 COLUMN 38 PIC 9 USING OPCAO-M  AUTO.

       PROCEDURE DIVISION.
       PROCESSO.
           PERFORM ABERTURA.
           PERFORM MENU-PRINCIPAL.

       ABERTURA.
           DISPLAY SPACES ERASE EOS.
           DISPLAY TELA.
           ACCEPT  DATA-DO-SISTEMA FROM DATE.
           MOVE SPACES TO DADOS.
           MOVE ZEROS TO I.

       MENU-PRINCIPAL.
           DISPLAY MENU.
           ACCEPT MENU.
           EVALUATE OPCAO-M
               WHEN 1
                   PERFORM MOSTRA
                   PERFORM PROCESSO
               WHEN 2
                   PERFORM ESCOLHE
                   PERFORM PROCESSO
               WHEN 9
                   PERFORM CONTINUA
               WHEN OTHER
                   PERFORM PROCESSO
           END-EVALUATE.

       MOSTRA.
           PERFORM ABERTURA.
           MOVE 01 TO I.
           MOVE 0510 TO K.
           MOVE 0530 TO J.
           DISPLAY "********** ESTADOS BRASILEIROS **********" AT 0310.
           DISPLAY SPACE.
           PERFORM UNTIL I GREATER 26
               DISPLAY SIGLA-T(I)" - "ESTADO-T(I) AT K
               ADD 100 TO K
               ADD 01 TO I
               DISPLAY SIGLA-T(I)" - "ESTADO-T(I) AT J
               ADD 100 TO J
               ADD 01 TO I
           END-PERFORM.
           ACCEPT CR.

       ESCOLHE.
           DISPLAY "ESCOLHA A SIGLA DO ESTADO BRASILEIRO: " AT 1310.
           PERFORM UNTIL SIGLA <> SPACES
               ACCEPT SIGLA AT 1349 WITH PROMPT AUTO
           END-PERFORM.
           MOVE 1 TO I.
           PERFORM UNTIL I GREATER 26
               IF SIGLA = SIGLA-T(I)
                   DISPLAY "ESTADO ESCOLHIDO: " AT 1430
                   DISPLAY ESTADO-T(I) AT 1449
                   ACCEPT CR
                   PERFORM PROCESSO
               END-IF
               ADD 1 TO I
           END-PERFORM.
           IF I > 26
               DISPLAY "ESTADO INEXISTENTE..." AT 1449
           END-IF.
           ACCEPT CR.

       CONTINUA.
           DISPLAY "TEM CERTEZA QUE QUER SAIR (S/N)?" AT 1310.
           PERFORM UNTIL OPCAO = "S" OR "s" OR "N" OR "n"
           ACCEPT OPCAO AT 1343 WITH PROMPT AUTO
           END-PERFORM.
           IF OPCAO = "s" OR "S"
               DISPLAY "FIM DO PROGRAMA..." AT 1510
               DISPLAY SPACE
               STOP RUN
           ELSE
               PERFORM PROCESSO
           END-IF.
