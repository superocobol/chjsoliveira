      ******************************************************************
      * Author: CARLOS OLIVEIRA
      * Date: 07/07/2019
      * Purpose: Menu Relatório de Vendas
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_MENU_REL_VEN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
               ORGANIZATION       IS INDEXED
               ACCESS MODE        IS DYNAMIC
               RECORD KEY         IS FS-VEN-KEY
               LOCK MODE          IS MANUAL
               FILE STATUS        IS FS-STAT-VEN.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-VENDEDOR.

       COPY FS-ARQ-VENDEDOR.

       WORKING-STORAGE SECTION.
       01 WS-MODULO.
           05 FILLER PIC X(17) VALUE "FRANQUIA AMBEV - ".
           05 WS-OP PIC X(53) VALUE SPACES.

       77 WS-DS-OPCAO PIC X(30) VALUE "ESCOLHA A OPCAO.".
       77 WS-DS-SAIR PIC  X(30) VALUE "EXC PARA RETORNAR.".
       77 WS-DS-OPCAO-ERR PIC X(30) VALUE "OPCAO INVALIDA.".
x".

       77 W-VAL-ENTRADA PIC X.
           88 W-VAL-ENTRADA-OK VALUE 'S'.

       77 FS-STAT-VEN PIC 9(03).
           88 FS-STAT-VEN-OK         VALUE IS 00.
           88 FS-STAT-VEN-CANCELA    VALUE 99.
           88 FS-STAT-VEN-NAO-EXISTE VALUE 35.

       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 WS-NUML PIC 999.
       77 WS-NUML-ANT PIC 999.
       77 WS-NUMC PIC 999.
       77 WS-NUML-CURR PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).

       77 WS-MSGERRO PIC X(80).

       01 WS-FILTER.
          05 WS-FIL-CD-VENDEDOR       PIC 9(07).
          05 WS-FIL-NM-VENDEDOR      PIC X(40).

       01 WS-LINHA.
           05 WS-CD-VENDEDOR              PIC 9(7).
           05 FILLER                      PIC X(7).
           05 FILLER                      PIC X(1) VALUE "|".
           05 WS-CPF                      PIC 9(11).
           05 FILLER                      PIC X(1) VALUE "|".
           05 WS-NM-VENDEDOR              PIC X(15).
           05 FILLER                      PIC X(1) VALUE "|".
           05 WS-VL-LATITUDE              PIC -9(3).9(8).
           05 FILLER                      PIC X(1) VALUE "|".
           05 WS-VL-LONGITUDE             PIC -9(3).9(8).

       COPY CPY_ID_ARQ_VENDEDOR.

       COPY screenio.

       LINKAGE SECTION.
       01 LK-PARAM.
           05 LK-RETURN-CODE PIC 99 VALUE 0.

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
               10 LINE WS-NUML-ANT COLUMN 1 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.

       01 SS-TELA-VENDEDOR.
           05 SS-REPORT-FILTER.
               10 LINE 02 COLUMN 2 VALUE "Codigo Vendedor:".
               10 COLUMN PLUS 2 PIC 9(07) USING WS-FIL-CD-VENDEDOR.
               10 LINE 03 COLUMN 2 VALUE "Nome:".
               10 COLUMN PLUS 2 PIC X(40) USING WS-FIL-NM-VENDEDOR.
           05 SS-REPORT-HEADER.
               10 LINE 04 COLUMN 1 VALUE "==============================
      -"==================================================".
               10 LINE 05 COLUMN 1 VALUE "Codigo Vendedor".
               10 COLUMN PLUS 1 VALUE "|".
               10 COLUMN PLUS 1 VALUE "CPF           ".
               10 COLUMN PLUS 1 VALUE "|".
               10 COLUMN PLUS 1 VALUE "Nome           ".
               10 COLUMN PLUS 1 VALUE "|".
               10 COLUMN PLUS 1 VALUE "Latitude     ".
               10 COLUMN PLUS 1 VALUE "|".
               10 COLUMN PLUS 1 VALUE "Longitude    ".
               10 LINE 06 COLUMN 1 VALUE "==============================
      -"==================================================".

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION  USING LK-PARAM.

       0000-PRINCIPAL SECTION.

           SET ENVIRONMENT 'DB_HOME' TO WNM-PATH-VENDEDOR.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.

           ACCEPT WS-NUML FROM LINES
           COMPUTE WS-NUML-ANT = WS-NUML - 1
           ACCEPT WS-NUMC FROM COLUMNS
           MOVE  7          TO WS-NUML-CURR
           MOVE "RELATORIO" TO WS-OP

           DISPLAY SS-CLS

           DISPLAY SS-REPORT-FILTER
           DISPLAY SS-REPORT-HEADER

           PERFORM 5000-ABRIR-ARQ-VENDEDOR
           MOVE LOW-VALUES TO FS-VEN-KEY
           PERFORM 6000-LER-ARQ-VENDEDOR

           PERFORM
             UNTIL NOT FS-STAT-VEN-OK

               MOVE FS-VEN-CD-VENDEDOR     TO WS-CD-VENDEDOR
               MOVE FS-VEN-CPF             TO WS-CPF
               MOVE FS-VEN-NM-VENDEDOR     TO WS-NM-VENDEDOR
               MOVE FS-VEN-VL-LATITUDE    TO WS-VL-LATITUDE
               MOVE FS-VEN-VL-LONGITUDE   TO WS-VL-LONGITUDE

               DISPLAY WS-LINHA LINE WS-NUML-CURR COLUMN 1

               ADD 1 TO WS-NUML-CURR

               PERFORM 6000-LER-ARQ-VENDEDOR
           END-PERFORM

           PERFORM 7000-FECHA-ARQ-VENDEDOR

           ACCEPT SS-REPORT-FILTER

           PERFORM 9999-FINALIZA.

       0000-PRINCIPALX. EXIT.

      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
       5000-ABRIR-ARQ-VENDEDOR  SECTION.

           OPEN I-O ARQ-VENDEDOR.

           IF FS-STAT-VEN-NAO-EXISTE THEN
               OPEN OUTPUT ARQ-VENDEDOR
               PERFORM 7000-FECHA-ARQ-VENDEDOR
               OPEN I-O ARQ-VENDEDOR
           END-IF.

       5000-ABRIR-ARQ-VENDEDORX. EXIT.
      * -----------------------------------
      * LE VENDEDOR E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       6000-LER-ARQ-VENDEDOR SECTION.

002260     START ARQ-VENDEDOR KEY > FS-VEN-KEY
002300       NOT INVALID KEY
002310         READ ARQ-VENDEDOR NEXT
               END-READ
002390     END-START.

       6000-LER-ARQ-VENDEDOR-FIMX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO DE VENDEDORS
       7000-FECHA-ARQ-VENDEDOR SECTION.

           CLOSE ARQ-VENDEDOR.

       7000-FECHA-ARQ-VENDEDORX. EXIT.

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

            GOBACK.

       9999-FINALIZAX. EXIT.
       END PROGRAM CBL_MENU_REL_VEN.
