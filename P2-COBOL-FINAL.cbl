              IDENTIFICATION DIVISION.
       PROGRAM-ID. INDICE_EXTRA2.
       AUTHOR. CINTIA SEDENHO DIAS.
       ENVIRONMENT DIVISION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN TO DISK
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLI-CPF
           FILE STATUS IS CLI-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES LABEL RECORD STANDARD
           DATA RECORD IS REG-CLI
           VALUE OF FILE-ID IS "CLIENTES.DAT".
       01 REG-CLI.
           05 CLI-AGENCIA      PIC 9(4).
           05 CLI-CONTA        PIC 9(6)
           05 CLI-NOME         PIC X(30).
           05 CLI-SALDO        PIC 9(6)V99.
           05 CLI-CPF          PIC 9(11).
       WORKING-STORAGE SECTION.
       01 DATA-SIS.
           05 ANO          PIC 99.
           05 MES          PIC 99.
           05 DIA          PIC 99.
       01 CLI-STATUS       PIC X(2).
       01 OPCAO            PIC X(1) VALUE SPACES.
       01 SALVA            PIC X(1) VALUE SPACES.
       01 IGUAL            PIC 9 VALUE ZEROS.
       01 ESPACO           PIC X(40) VALUE SPACES.
       01 MENS1            PIC X(20) VALUE "FIM DE PROGRAMA".
       01 OPCAO-M          PIC 9(1) VALUE ZEROS.
       01 CR               PIC X(9) VALUE SPACES.
       01 DADOS-EDITADOS.
           05 AGENCIA       PIC 9.999.
           05 CONTA         PIC 99.999/9.
           05 NOME          PIC X(30) VALUE SPACES.
           05 SALDO         PIC $ZZZ.ZZ9,99.
           05 CPF           PIC 999.999.999/99.
           05 WS-AGENCIA    PIC 9999.
           05 WS-CONTA      PIC 999999.
           05 WS-NOME       PIC X(30) VALUE SPACES.
           05 WS-SALDO      PIC 9999999,99.
           05 WS-CPF        PIC 9(11).

       SCREEN SECTION.
       01 MENU-PRINCIPAL.
           05 LINE 03 COLUMN 10 VALUE
           "************* BANCO GRUPO-DOIS *************".
           05 LINE 05 COLUMN 10 VALUE
           "************** MENU PRINCIPAL **************".
           05 LINE 07 COLUMN 10 VALUE "[1] INCLUSAO DE CONTA".
           05 LINE 08 COLUMN 10 VALUE "[2] ALTERACAO DE DADOS".
           05 LINE 09 COLUMN 10 VALUE "[3] EXCLUSAO DE CONTA".
           05 LINE 10 COLUMN 10 VALUE "[4] CONSULTA".
           05 LINE 12 COLUMN 10 VALUE "[9] SAIR DO SISTEMA".
           05 LINE 15 COLUMN 10 VALUE "DIGITE A OPCAO DESEJADA".
           05 OPT LINE 15 COLUMN 38 PIC 9 USING OPCAO-M  AUTO.

           05 LINE 07  COL 61  VALUE "XXXX".
           05 LINE 08  COL 60  VALUE "XXXXXX".
           05 LINE 09  COL 60  VALUE "XX".
           05 LINE 09  COL 64  VALUE "XX".
           05 LINE 10  COL 60  VALUE "XX".
           05 LINE 11  COL 60  VALUE "XX".
           05 LINE 12  COL 60  VALUE "XX".
           05 LINE 12  COL 63  VALUE "XXX".
           05 LINE 13  COL 60  VALUE "XX".
           05 LINE 13  COL 63  VALUE "XXX".
           05 LINE 14  COL 60  VALUE "XX".
           05 LINE 14  COL 64  VALUE "XX".
           05 LINE 15  COL 60  VALUE "XXXXXX".
           05 LINE 16  COL 61  VALUE "XXXX".

           05 LINE 07  COL 70  VALUE "XXXX".
           05 LINE 08  COL 69  VALUE "XXXXXX".
           05 LINE 09  COL 69  VALUE "XX".
           05 LINE 09  COL 73  VALUE "XX".
           05 LINE 10  COL 73  VALUE "XX".
           05 LINE 11  COL 73  VALUE "XX".
           05 LINE 12  COL 72  VALUE "XX".
           05 LINE 13  COL 71  VALUE "XX".
           05 LINE 14  COL 69  VALUE "XXX".
           05 LINE 15  COL 68  VALUE "XXXXXXX".
           05 LINE 16  COL 68  VALUE "XXXXXXX".

       01 TELA.
           05 LINE 1   COL 10   VALUE "  ".
           05 LINE 2   COL 22  VALUE
           "************** CADASTRO DE CONTAS **************".
           05 LINE 4   COL 19  VALUE "AGENCIA:".
           05 LINE 6   COL 19  VALUE "CONTA CORRENTE:".
           05 LINE 8   COL 19  VALUE "NOME:".
           05 LINE 10  COL 19  VALUE "CPF:".
           05 LINE 12  COL 19  VALUE "SALDO:".
           05 LINE 15  COL 25  VALUE "MENSAGEM:".

       01 TELA-CPF.
           05 LINE 1   COL 10   VALUE "  ".
           05 LINE 2   COL 22  VALUE
           "************** INSERIR O CPF **************".
           05 LINE 10  COL 19  VALUE "CPF:".

       PROCEDURE DIVISION.
       MENU-PRI.
           PERFORM LIMPA-TELA.
           MOVE ZERO TO OPCAO-M.
           DISPLAY MENU-PRINCIPAL.
           ACCEPT MENU-PRINCIPAL.

           EVALUATE OPCAO-M
               WHEN 1
                   PERFORM MENU-INCLUSAO
               WHEN 2
                   PERFORM MENU-ALTERA
               WHEN 3
                   PERFORM MENU-EXCLUI
               WHEN 4
                   PERFORM MENU-CONSULTA
               WHEN 9
                   DISPLAY MENS1 AT 1730
                   PERFORM STOP RUN
               WHEN OTHER
                   PERFORM MENU-PRI
           END-EVALUATE.

       MENU-INCLUSAO.
           PERFORM LIMPA-TELA
           DISPLAY TELA
           DISPLAY "************ INCLUSAO ************" AT 0330
           PERFORM ABRE-ARQ
           PERFORM RECEBE
           PERFORM GRAVA UNTIL SALVA = "S" OR "N"
           CLOSE CLIENTES
      *    PERFORM CONTINUA
           PERFORM MENU-PRI.

       MENU-ALTERA.
           PERFORM LIMPA-TELA
           DISPLAY TELA
           DISPLAY "************ ALTERACAO ************" AT 0330
           PERFORM ABRE-ARQ
           PERFORM RECEBE-CPF
           PERFORM ALTERA UNTIL SALVA = "S" OR "N"
      *    PERFORM CONTINUA UNTIL OPCAO = "S" OR "N"
           CLOSE CLIENTES
           PERFORM MENU-PRI.

       MENU-EXCLUI.
           PERFORM LIMPA-TELA
           DISPLAY TELA
           DISPLAY "************ EXCLUSAO ************" AT 0330
           PERFORM ABRE-ARQ
           PERFORM RECEBE-CPF
           PERFORM DELETA UNTIL SALVA = "S" OR "N"
      *    PERFORM CONTINUA UNTIL OPCAO = "S" OR "N"
           CLOSE CLIENTES
           PERFORM MENU-PRI.

       MENU-CONSULTA.
           PERFORM LIMPA-TELA
           DISPLAY TELA
           DISPLAY "************ CONSULTA ************" AT 0330
           PERFORM ABRE-ARQ
           PERFORM TESTA-CPF
           PERFORM CONSULTA
      *    UNTIL SALVA = "S" OR "N"
      *    PERFORM CONTINUA UNTIL OPCAO = "S" OR "N"
           CLOSE CLIENTES
           PERFORM MENU-PRI.

       LIMPA-TELA.
           DISPLAY ERASE AT 0101.
           ACCEPT DATA-SIS FROM DATE.
           DISPLAY DIA AT 0205.
           DISPLAY MES AT 0208.
           DISPLAY ANO AT 0211.
      *-------------INICIALIZAÇÃO DAS VARIÁVEIS
           MOVE SPACE TO OPCAO SALVA.
           MOVE SPACES TO NOME.
           MOVE ZEROS TO AGENCIA CONTA SALDO CPF.
           MOVE SPACES TO CLI-NOME.
           MOVE ZEROS TO CLI-AGENCIA CLI-CONTA CLI-SALDO CLI-CPF.
           DISPLAY ESPACO AT 1535.

       LIMPA-DADOS.
           MOVE SPACE TO OPCAO SALVA.
           MOVE SPACES TO NOME.
           MOVE ZEROS TO AGENCIA CONTA SALDO CPF.
           MOVE SPACES TO CLI-NOME.
           MOVE ZEROS TO CLI-AGENCIA CLI-CONTA CLI-SALDO CLI-CPF.
           DISPLAY ESPACO AT 1535.


       ABRE-ARQ.
           OPEN I-O CLIENTES.
           IF CLI-STATUS NOT = "00"
              CLOSE CLIENTES
              OPEN OUTPUT CLIENTES.

       RECEBE.
           PERFORM AGENCIA1 UNTIL CLI-AGENCIA NOT = ZEROS.
           PERFORM CONTA1 UNTIL CLI-CONTA NOT = ZEROS.
           PERFORM NOME1 UNTIL CLI-NOME NOT = SPACES.
           PERFORM CPF1 UNTIL CLI-CPF NOT = ZEROS.
           PERFORM SALDO1.

        RECEBE-ALTERA.
           PERFORM TESTA-AGENCIA UNTIL CLI-AGENCIA NOT = ZEROS.
           PERFORM TESTA-CONTA UNTIL CLI-CONTA NOT = ZEROS.
           PERFORM TESTA-NOME UNTIL CLI-NOME NOT = SPACES.
      *     PERFORM TESTA-CPF UNTIL CLI-CPF NOT = ZEROS.
           PERFORM TESTA-SALDO.

       AGENCIA1.
           ACCEPT AGENCIA AT 0438 WITH PROMPT AUTO.
           MOVE AGENCIA TO WS-AGENCIA.
           MOVE WS-AGENCIA TO CLI-AGENCIA.
           IF CLI-AGENCIA = ZEROS
           THEN
               DISPLAY "AGENCIA IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.

       CONTA1.
           ACCEPT CONTA AT 0638 WITH PROMPT AUTO.
           MOVE CONTA TO WS-CONTA.
           MOVE WS-CONTA TO CLI-CONTA.
           IF CLI-CONTA = ZEROS
           THEN
               DISPLAY "CONTA IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.

       NOME1.
           ACCEPT NOME AT 0838 WITH PROMPT AUTO.
           MOVE NOME TO WS-NOME.
           MOVE WS-NOME TO CLI-NOME.
           IF CLI-NOME = SPACES
               DISPLAY "NOME EM BRANCO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.

       CPF1.
           SET IGUAL TO 0.
           ACCEPT CPF AT 1038 WITH PROMPT AUTO.
           MOVE CPF TO WS-CPF.
           MOVE WS-CPF TO CLI-CPF.
           IF CLI-CPF = ZEROS
           THEN
               DISPLAY "CPF IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
      *    ELSE
      *        READ CLIENTES NOT INVALID KEY
      *            PERFORM JA-CADASTRADO
      *        END-READ
           END-IF.

       SALDO1.
           ACCEPT SALDO AT 1238 WITH PROMPT AUTO.
           MOVE SALDO TO WS-SALDO.
           MOVE WS-SALDO TO CLI-SALDO.
           DISPLAY ESPACO AT 1535.


       TESTA-AGENCIA.
           ACCEPT AGENCIA AT 0438 WITH PROMPT AUTO.
           MOVE AGENCIA TO CLI-AGENCIA.
           IF CLI-AGENCIA = ZEROS
           THEN
           DISPLAY "AGENCIA IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.

       TESTA-CONTA.
           ACCEPT CONTA AT 0638 WITH PROMPT AUTO.
           MOVE CONTA TO CLI-CONTA.
           IF CLI-CONTA = ZEROS
           THEN
               DISPLAY "CONTA IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.
       TESTA-NOME.
           ACCEPT NOME AT 0838 WITH PROMPT AUTO.
           MOVE NOME TO CLI-NOME.
           IF CLI-NOME = SPACES
               DISPLAY "NOME EM BRANCO. DIGITE NOVAMENTE..." AT 1535
           ELSE
               DISPLAY ESPACO AT 1535
           END-IF.

       TESTA-CPF.
          SET IGUAL TO 0.
          ACCEPT CPF AT 1038 WITH PROMPT AUTO.
           MOVE CPF TO CLI-CPF.
           IF CLI-CPF = ZEROS
           THEN
               DISPLAY "CPF IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
      *    ELSE
      *        READ CLIENTES NOT INVALID KEY
      *            PERFORM JA-CADASTRADO
      *        END-READ
           END-IF.

       TESTA-SALDO.
           ACCEPT SALDO AT 1238 WITH PROMPT AUTO.
           MOVE SALDO TO CLI-SALDO.
           DISPLAY ESPACO AT 1535.

       RECEBE-CPF.
      *    DISPLAY TELA-CPF.
           SET IGUAL TO 0.
           ACCEPT CPF AT 1038 WITH PROMPT AUTO.
           MOVE CPF TO CLI-CPF.
           IF CLI-CPF = ZEROS
           THEN
               DISPLAY "CPF IGUAL ZERO. DIGITE NOVAMENTE..." AT 1535
      *    ELSE
      *        READ CLIENTES NOT INVALID KEY
      *            PERFORM JA-CADASTRADO
      *        END-READ
      *     ELSE
           END-IF.

       JA-CADASTRADO.
           DISPLAY "JA EXISTE ESTE CADASTRO" AT 1535.
           SET IGUAL TO 1.
      *   --É PRECISO ZERAR NOVAMENTE OS CAMPOS-------------
           MOVE SPACES TO CLI-NOME.
           MOVE ZEROS TO CLI-AGENCIA CLI-CONTA CLI-SALDO CLI-CPF.

       GRAVA.
           READ CLIENTES NOT INVALID KEY
               PERFORM JA-CADASTRADO
               ACCEPT CR
               PERFORM MENU-PRI
           END-READ.
           DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
           ACCEPT SALVA AT 1445 WITH PROMPT AUTO.
           IF SALVA = "S" OR "s"
               WRITE REG-CLI
                   INVALID KEY
                   DISPLAY "ERRO AO GRAVAR!!" AT 1535
               END-WRITE.

       ALTERA.
           READ CLIENTES NOT INVALID KEY
      *     READ CLIENTES INTO REG-CLI
      *         AT END
              IF CLI-CPF EQUAL CPF.
                   MOVE CLI-AGENCIA TO AGENCIA.
                   MOVE CLI-CONTA TO CONTA.
                   MOVE CLI-NOME TO NOME.
                   MOVE CLI-CPF TO CPF.
                   MOVE CLI-SALDO TO SALDO.
                   DISPLAY AGENCIA AT 0438.
                   DISPLAY CONTA AT 0638.
                   DISPLAY NOME AT 0838.
                   DISPLAY CPF AT 1038.
                   DISPLAY SALDO AT 1238.

      *             DISPLAY "REGISTRO CORRETO (S/N)? [ ]" AT 1430.
      *             ACCEPT SALVA AT 1455 WITH PROMPT AUTO.
      *             IF SALVA = "S" OR "s"
                       DISPLAY ESPACO AT 1430
                       PERFORM REGRAVAR
                       PERFORM MENU-PRI
      *             ELSE IF SALVA = "N" OR "n"
      *                 PERFORM MENU-PRI
      *             ELSE
      *                 DISPLAY ESPACO AT 1535
      *                 DISPLAY "DIGITE S OU N" AT 1535
      *             END-IF
      *
               READ CLIENTES INVALID KEY
                   DISPLAY "REGISTRO NAO EXISTE"
                   PERFORM MENU-ALTERA.


       REGRAVAR.
           OPEN I-O CLIENTES
           READ CLIENTES NOT INVALID KEY
           PERFORM LIMPA-DADOS.
           PERFORM RECEBE.
                   MOVE WS-AGENCIA TO CLI-AGENCIA.
                   MOVE WS-CONTA TO CLI-CONTA.
                   MOVE WS-NOME TO CLI-NOME.
                   MOVE WS-CPF TO CLI-CPF.
                   MOVE WS-SALDO TO CLI-SALDO.
           DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
           ACCEPT SALVA AT 1445 WITH PROMPT AUTO.
           IF SALVA = "S" OR "s"
           REWRITE REG-CLI
               INVALID KEY
                   DISPLAY "ERRO AO REGRAVAR!!" AT 1535
                   ACCEPT CR
               NOT INVALID KEY
                   DISPLAY "REGISTRO ALTERADO" AT 1535
                   ACCEPT CR
               CLOSE CLIENTES
           END-REWRITE.


      *     DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
      *     ACCEPT SALVA AT 1445 WITH PROMPT AUTO.
      *     IF SALVA = "S"
      *         REWRITE REG-CLI
      *             INVALID KEY
      *             DISPLAY "ERRO AO REGRAVAR!!" AT 1535
      *         END-REWRITE.

       DELETA.
           READ CLIENTES INTO REG-CLI
      *         IF CLI-CPF EQUAL CPF.
                   MOVE CLI-AGENCIA TO AGENCIA.
                   MOVE CLI-CONTA TO CONTA.
                   MOVE CLI-NOME TO NOME.
                   MOVE CLI-CPF TO CPF.
                   MOVE CLI-SALDO TO SALDO.
                   DISPLAY AGENCIA AT 0438.
                   DISPLAY CONTA AT 0638.
                   DISPLAY NOME AT 0838.
                   DISPLAY CPF AT 1038.
                   DISPLAY SALDO AT 1238.

                   DISPLAY "REGISTRO CORRETO (S/N)? [ ]" AT 1430
                   ACCEPT SALVA AT 1455 WITH PROMPT AUTO
                   IF SALVA = "N" OR "n"
                       PERFORM MENU-PRI
                   ELSE IF SALVA = "S" OR "s"
                       DISPLAY ESPACO AT 1430
                       MOVE SPACE TO SALVA
                       DISPLAY "EXCLUIR (S/N)? [ ]" AT 1430.
                       ACCEPT SALVA AT 1446 WITH PROMPT AUTO.
                           IF SALVA = "S" OR "s"
                           DELETE CLIENTES NOT INVALID KEY
                           DISPLAY "ok" AT 1535
      *                         INVALID KEY
      *                         DISPLAY "ERRO AO EXCLUIR!!" AT 1535
      *                         NOT INVALID KEY
      *                         DISPLAY "REGISTRO EXCLUIDO!!" AT 1535
                           END-DELETE
                           ACCEPT CR
                           PERFORM MENU-PRI.
      *            ELSE
      *           IF SALVA = "N" OR "n"
      *                    PERFORM MENU-PRI
      *                     DISPLAY ESPACO AT 1535
      *                     DISPLAY "DIGITE S OU N" AT 1535
      *             END-IF.

       CONSULTA.
          READ CLIENTES NOT INVALID KEY
      *     READ CLIENTES INTO REG-CLI
      *         AT END
               IF CLI-CPF EQUAL CPF.
                   MOVE CLI-AGENCIA TO AGENCIA.
                   MOVE CLI-CONTA TO CONTA.
                   MOVE CLI-NOME TO NOME.
                   MOVE CLI-CPF TO CPF.
                   MOVE CLI-SALDO TO SALDO.
                   DISPLAY AGENCIA AT 0438.
                   DISPLAY CONTA AT 0638.
                   DISPLAY NOME AT 0838.
                   DISPLAY CPF AT 1038.
                   DISPLAY SALDO AT 1238.
           READ CLIENTES INVALID KEY
                DISPLAY "REGISTRO NAO EXISTE!!" AT 1535
           END-READ.
           ACCEPT CR.

      *   DISPLAY "CONSULTAR OUTRO (S/N)? [ ]" AT 1430.
      *   ACCEPT SALVA AT 1454 WITH PROMPT AUTO.
      *   IF SALVA = "N" OR "n"
      *       PERFORM MENU-PRI
      *   ELSE IF SALVA = "S" OR "s"
      *       PERFORM MENU-CONSULTA
      *   ELSE
      *       DISPLAY ESPACO AT 1535
      *       DISPLAY "DIGITE S OU N" AT 1535
      *   END-IF.

       CONTINUA.
           DISPLAY "CONTINUA (S/N)? [ ]" AT 1430
           ACCEPT OPCAO AT 1447 WITH PROMPT AUTO.
           IF OPCAO = "S" OR "s" OR = "N" OR "n"
           THEN
               DISPLAY ESPACO AT 1430
               DISPLAY ESPACO AT 1535

           ELSE IF OPCAO = "N" or "n"
               STOP RUN
           ELSE
               DISPLAY ESPACO AT 1535
               DISPLAY "DIGITE S OU N" AT 1535
           END-IF.
