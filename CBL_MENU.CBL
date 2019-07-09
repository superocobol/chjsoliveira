      ******************************************************************
      * Author: CARLOS OLIVEIRA
      * Date: 07/07/2019
      * Purpose: Menu principal
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_MENU.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MODULO.
           05 FILLER PIC X(17) VALUE "FRANQUIA AMBEV - ".
           05 WS-OP PIC X(53) VALUE SPACES.

       77 WS-DS-OPCAO PIC X(30) VALUE "ESCOLHA A OPCAO.".
       77 WS-DS-OPCAO-ERR PIC X(30) VALUE "OPCAO INVALIDA.".

       01 LK-PARAMETERS-CLI.
           05 LK-CLI-RETURN-CODE PIC 99 VALUE 0.

       01 LK-PARAMETERS-VEN.
           05 LK-VEN-RETURN-CODE PIC 99 VALUE 0.

       01 LK-PARAMETERS-EXE.
           05 LK-EXE-RETURN-CODE PIC 99 VALUE 0.

       01 LK-PARAM-REL-CLI.
           05 LK-REL-VEN-RETURN-CODE PIC 99 VALUE 0.

       01 LK-PARAM-REL-VEN.
           05 LK-REL-VEN-RETURN-CODE PIC 99 VALUE 0.

       77 WS-OPCAO PIC X.
           88 E-CADASTRO  VALUE IS "1".
           88 E-RELATORIO VALUE IS "2".
           88 E-EXECUTAR  VALUE IS "3".
           88 E-ENCERRAR  VALUE IS "X" "x".
           88 E-OPCAO-OK  VALUE ARE "1" "2" "3" "X" "x".

       77 WS-OPCAO-CAD PIC X.
           88 E-CADASTRO-CLI  VALUE IS "1".
           88 E-CADASTRO-VEN  VALUE IS "2".
           88 E-CADASTRO-ENC  VALUE IS "X" "x".
           88 E-OPCAO-CAD-OK  VALUE ARE "1" "2" "X" "x".

       77 WS-OPCAO-REL PIC X.
           88 E-RELATORIO-CLI  VALUE IS "1".
           88 E-RELATORIO-VEN  VALUE IS "2".
           88 E-RELATORIO-ENC  VALUE IS "X" "x".
           88 E-OPCAO-REL-OK  VALUE ARE "1" "2" "X" "x".

       77 WS-OPCAO-EXE PIC X.
           88 E-EXECUCAO-EXE  VALUE IS "1".
           88 E-EXECUCAO-ENC  VALUE IS "X" "x".
           88 E-OPCAO-EXE-OK  VALUE ARE "1" "X" "x".

       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 WS-NUML PIC 999.
       77 WS-NUML-ANT PIC 999.
       77 WS-NUMC PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).
       77 WS-MSGERRO PIC X(80).

       COPY screenio.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(70) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML-ANT COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.

       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "01 - CADASTROS".
           05 LINE 08 COLUMN 15 VALUE "02 - RELATORIOS".
           05 LINE 09 COLUMN 15 VALUE "03 - EXECUTAR".
           05 LINE 11 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COL PLUS 1 USING WS-OPCAO AUTO.

       01 SS-MENU-CADASTRO FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "01 - Cadastro de Cliente".
           05 LINE 08 COLUMN 15 VALUE "02 - Cadastro de Vendedor".
           05 LINE 11 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COL PLUS 1 USING WS-OPCAO-CAD AUTO.

       01 SS-MENU-RELATORIO FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "01 - Relatorio de Cliente".
           05 LINE 08 COLUMN 15 VALUE "02 - Relatorio de Vendedor".
           05 LINE 11 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COL PLUS 1 USING WS-OPCAO-REL AUTO.

       01 SS-MENU-EXECUCAO FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "01 - Executar Distribuicao de Cli
      -"entes".
           05 LINE 11 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COL PLUS 1 USING WS-OPCAO-EXE AUTO.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION.

       0000-PRINCIPAL SECTION.

           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT WS-NUML FROM LINES
           COMPUTE WS-NUML-ANT = WS-NUML - 1
           ACCEPT WS-NUMC FROM COLUMNS

           PERFORM UNTIL E-ENCERRAR
               MOVE "MENU" TO WS-OP
               MOVE WS-DS-OPCAO TO WS-STATUS
               MOVE SPACES TO WS-OPCAO
               DISPLAY SS-CLS
               ACCEPT SS-MENU
               EVALUATE TRUE
                   WHEN E-CADASTRO
                       MOVE SPACES TO WS-OPCAO-CAD
                       PERFORM 1000-ACS-MENU-CAD UNTIL E-CADASTRO-ENC
                   WHEN E-RELATORIO
                       MOVE SPACES TO WS-OPCAO-REL
                       PERFORM 1100-ACS-MENU-REL UNTIL E-RELATORIO-ENC
                   WHEN E-EXECUTAR
                       MOVE SPACES TO WS-OPCAO-EXE
                       PERFORM 1200-ACS-MENU-EXE UNTIL E-EXECUCAO-ENC
                       MOVE SPACES TO WS-OPCAO
                   WHEN NOT E-OPCAO-OK
                       MOVE WS-DS-OPCAO-ERR TO WS-MSGERRO
                       PERFORM 9000-MOSTRA-ERRO
                   END-EVALUATE
           END-PERFORM.

           PERFORM 9999-FINALIZA.

       0000-PRINCIPALX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE CADASTRO
       1000-ACS-MENU-CAD SECTION.

            MOVE "01 - CADASTRO" TO WS-OP
            MOVE WS-DS-OPCAO TO WS-STATUS
            MOVE SPACES TO WS-OPCAO-CAD
            DISPLAY SS-CLS
            ACCEPT SS-MENU-CADASTRO
            EVALUATE TRUE
                WHEN E-CADASTRO-CLI
                    CALL 'CBL_MENU_CLIENTE' USING LK-PARAMETERS-CLI
                    MOVE SPACES TO WS-OPCAO-CAD
                WHEN E-CADASTRO-VEN
                    CALL 'CBL_MENU_VENDEDOR' USING LK-PARAMETERS-VEN
                    MOVE SPACES TO WS-OPCAO-CAD
                WHEN NOT E-OPCAO-CAD-OK
                    MOVE WS-DS-OPCAO-ERR TO WS-MSGERRO
                    PERFORM 9000-MOSTRA-ERRO
            END-EVALUATE.

       1000-ACS-MENU-CAD-FIMX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE RELATORIOS
       1100-ACS-MENU-REL SECTION.

            MOVE "02 - RELATORIO" TO WS-OP
            MOVE WS-DS-OPCAO TO WS-STATUS
            MOVE SPACES TO WS-OPCAO-REL
            DISPLAY SS-CLS
            ACCEPT SS-MENU-RELATORIO
            EVALUATE TRUE
                WHEN E-RELATORIO-CLI
                    CALL 'CBL_MENU_REL_CLI' USING LK-PARAM-REL-CLI
                    MOVE SPACES TO WS-OPCAO-REL
                WHEN E-RELATORIO-VEN
                    CALL 'CBL_MENU_REL_VEN' USING LK-PARAM-REL-VEN
                    MOVE SPACES TO WS-OPCAO-REL
                WHEN NOT E-OPCAO-REL-OK
                    MOVE WS-DS-OPCAO-ERR TO WS-MSGERRO
                    PERFORM 9000-MOSTRA-ERRO
            END-EVALUATE.

       1100-ACS-MENU-REL-FIMX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE EXECUCAO
       1200-ACS-MENU-EXE SECTION.

            MOVE "03 - EXECUCAO" TO WS-OP
            MOVE WS-DS-OPCAO TO WS-STATUS
            MOVE SPACES TO WS-OPCAO-EXE
            DISPLAY SS-CLS
            ACCEPT SS-MENU-EXECUCAO

            EVALUATE TRUE
                WHEN E-EXECUCAO-EXE
                    CALL 'CBL_MENU_DISTRIBUICAO' USING LK-PARAMETERS-EXE
                    MOVE SPACES TO WS-OPCAO-EXE
                WHEN NOT E-OPCAO-EXE-OK
                    MOVE WS-DS-OPCAO-ERR TO WS-MSGERRO
                    PERFORM 9000-MOSTRA-ERRO
            END-EVALUATE.

       1200-ACS-MENU-EXE-FIMX. EXIT.

      * -----------------------------------
      * MOSTRA MENSAGEM, ESPERA ENTER, ATUALIZA BARRA STATUS
       9000-MOSTRA-ERRO SECTION.

           DISPLAY SS-ERRO
           ACCEPT SS-ERRO
           DISPLAY SS-STATUS.

       9000-MOSTRA-ERROX. EXIT.

      * -----------------------------------
      * FINALIZAR PROCESSO
       9999-FINALIZA SECTION.

            STOP RUN.

       9999-FINALIZAX. EXIT.
       END PROGRAM CBL_MENU.
