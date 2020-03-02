       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTRA1.
       AUTHOR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           Special-names.
               Decimal-point is comma.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MESES.
           05 FILLER    PIC X(13) VALUE '01JANEIRO'.
           05 FILLER    PIC X(13) VALUE '02FEVEREIRO'.
           05 FILLER    PIC X(13) VALUE '03MARCO'.
           05 FILLER    PIC X(13) VALUE '04ABRIL'.
           05 FILLER    PIC X(13) VALUE '05MAIO'.
           05 FILLER    PIC X(13) VALUE '06JUNHO'.
           05 FILLER    PIC X(13) VALUE '07JULHO'.
           05 FILLER    PIC X(13) VALUE '08AGOSTO'.
           05 FILLER    PIC X(13) VALUE '09SETEMBRO'.
           05 FILLER    PIC X(13) VALUE '10OUTUBRO'.
           05 FILLER    PIC X(13) VALUE '11NOVEMBRO'.
           05 FILLER    PIC X(13) VALUE '12DEZEMBRO'.
       01 AREA-MESES    REDEFINES MESES.
           05 TABELA-MESES OCCURS 12 TIMES.
               10 TAB-MES               PIC 9(02).
               10 TAB-DESCRICAO-MES    PIC X(11).
       01 DATA-DO-SISTEMA.
           02 ANO      PIC 9(02) VALUE ZEROS.
           02 MES      PIC 9(02) VALUE ZEROS.
           02 DIA      PIC 9(02) VALUE ZEROS.
       01 GERAL.
           05 WS-CONTINUA      PIC A(01).
           05 WK-MES           PIC 9(02).
           05 I                   PIC S9(02) VALUE ZEROS.
       SCREEN SECTION.
       01 TELA01.
           02 LINE 02 COLUMN 05 PIC 9(02)/ USING DIA.
           02 LINE 02 COLUMN 08 PIC 9(02)/ USING MES.
           02 LINE 02 COLUMN 11 PIC 9(02) USING ANO.
           02 LINE 04 COLUMN 18 VALUE
               "1)PROGRAMA DE TABELA DOS MESES DO ANO".

       PROCEDURE DIVISION.
       INICIO.
       PERFORM TELA.
       ACCEPT  DATA-DO-SISTEMA FROM DATE.
       PERFORM PROCESSO.

       PROCESSO.
       PERFORM TELA.
       PERFORM MOSTRA.
       PERFORM ESCOLHE.
       PERFORM CONTINUA.

       TELA.
       DISPLAY SPACES ERASE EOS.
       DISPLAY  TELA01      AT 0101.
       MOVE SPACES TO WS-CONTINUA.
       MOVE ZEROS TO WK-MES.

       MOSTRA.
       MOVE 01 TO I.
       DISPLAY " ".
       PERFORM UNTIL I GREATER 12
           DISPLAY TAB-MES(I)" - " TAB-DESCRICAO-MES(I)
           ADD 1 TO I
       END-PERFORM.

       ESCOLHE.
           DISPLAY "ESCOLHA UM MES - DE 01 A 12:  " AT 0725.
           PERFORM UNTIL WK-MES <> ZEROS
               ACCEPT WK-MES AT 0756 WITH PROMPT AUTO
           END-PERFORM.
           IF WK-MES GREATER THAN 12 OR LESS THAN 1
               DISPLAY "NAO ENCONTRADO...." AT 0925
           ELSE
               DISPLAY "MES ESCOLHIDO: " AT 0939
               DISPLAY TAB-DESCRICAO-MES(WK-MES) AT 0956
           END-IF.

       CONTINUA.
           DISPLAY "DESEJA CONTINUAR (S/N)?" AT 1325.
           PERFORM UNTIL WS-CONTINUA = "S" OR "s" OR "N" OR "n"
               ACCEPT WS-CONTINUA AT 1349 WITH PROMPT AUTO
           END-PERFORM.
           IF WS-CONTINUA = "N" OR "n"
               DISPLAY "FIM DO PROGRAMA..." AT 1525
               DISPLAY SPACE
               STOP RUN
           ELSE
               PERFORM INICIO
           END-IF.
