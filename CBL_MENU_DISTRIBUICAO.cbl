      ******************************************************************
      * Author: CARLOS OLIVEIRA
      * Date: 07/07/2019
      * Purpose: Menu Distribuição
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_MENU_DISTRIBUICAO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
               ORGANIZATION       IS INDEXED
               ACCESS MODE        IS DYNAMIC
               RECORD KEY         IS FS-CLI-KEY
               LOCK MODE          IS MANUAL
               FILE STATUS        IS FS-STAT-CLI.

           SELECT ARQ-VENDEDOR ASSIGN TO DISK WID-ARQ-VENDEDOR
               ORGANIZATION       IS INDEXED
               ACCESS MODE        IS DYNAMIC
               RECORD KEY         IS FS-VEN-KEY
               LOCK MODE          IS MANUAL
               FILE STATUS        IS FS-STAT-VEN.

           SELECT ARQ-CSV ASSIGN TO WID-ARQ-CSV
               FILE STATUS        IS FS-STAT-CSV
               ORGANIZATION       IS LINE SEQUENTIAL.

           SELECT SORT-FILE ASSIGN TO DISK "SORTWORK".

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTE.
       COPY FS-ARQ-CLIENTE.

       FD ARQ-VENDEDOR.
       COPY FS-ARQ-VENDEDOR.

       FD  ARQ-CSV.
       01  ARQ-CSV-REC.
           05  FS-CSV-CD-CLIENTE       PIC 9(007).
           05  FILLER                  PIC X.
           05  FS-CSV-RAZAO-SOCIAL     PIC X(040).
           05  FILLER                  PIC X.
           05  FS-CSV-CD-VENDEDOR      PIC 9(007).
           05  FILLER                  PIC X.
           05  FS-CSV-NM-VENDEDOR      PIC X(040).
           05  FILLER                  PIC X.
           05  FS-CSV-DIST-KM          PIC -9(008).9(011)
                                       SIGN IS LEADING SEPARATE.
       SD  SORT-FILE.

       01  SORT-RECORD.
           05 SD-VEN-CD-VENDEDOR       PIC  9(007).
           05 SD-NM-VENDEDOR           PIC  X(040).
           05 SD-DIST-KM               PIC S9(008)V9(11).

       WORKING-STORAGE SECTION.
       01 WS-MODULO.
           05 FILLER PIC X(17) VALUE "FRANQUIA AMBEV - ".
           05 WS-OP PIC X(53) VALUE SPACES.

       77 WS-DS-OPCAO PIC X(30) VALUE "ESCOLHA A OPCAO.".
       77 WS-DS-SAIR PIC  X(30) VALUE "EXC PARA RETORNAR.".
       77 WS-DS-OPCAO-ERR PIC X(30) VALUE "OPCAO INVALIDA.".

       77 W-VAL-ENTRADA PIC X.
           88 W-VAL-ENTRADA-OK VALUE 'S'.

       77 FS-STAT-CSV PIC 9(02).
           88 FS-STAT-CSV-OK         VALUE 00.
           88 FS-STAT-CSV-NAO-EXISTE VALUE 35.

       77 FS-STAT-CLI PIC 9(02).
           88 FS-STAT-CLI-OK         VALUE 00.
           88 FS-STAT-CLI-CANCELA    VALUE 99.
           88 FS-STAT-CLI-NAO-EXISTE VALUE 35.

       77 FS-STAT-VEN PIC 9(02).
           88 FS-STAT-VEN-OK         VALUE 00.
           88 FS-STAT-VEN-CANCELA    VALUE 99.
           88 FS-STAT-VEN-NAO-EXISTE VALUE 35.

       77 WS-FIM-SORT-FILE           PIC X(01) VALUE 'N'.

       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".
           88 E-NAO VALUES ARE "N" "n".

       77 WS-NUML PIC 999.
       77 WS-NUML-ANT PIC 999.
       77 WS-NUMC PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).

       77 WS-MSGERRO PIC X(80).

       01 WQT-TOTAIS.
           05 WQT-ARQ-IMPORTACAO     PIC 9(9) VALUE ZEROS.
           05 WQT-ARQ-OK             PIC 9(9) VALUE ZEROS.
           05 WQT-ARQ-DUP            PIC 9(9) VALUE ZEROS.

       COPY CPY_ID_ARQ_CSV.

       COPY CPY_ID_ARQ_CLIENTE.

       COPY CPY_ID_ARQ_VENDEDOR.

       COPY CPY_CALC_DISTANCIA.

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
               10 LINE WS-NUML-ANT COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.

       01 SS-TELA-RESULTADO.
           05 LINE 13 COLUMN 13 VALUE "Quantidade Clientes:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-IMPORTACAO.
           05 LINE 14 COLUMN 10 VALUE "Quantidade Atualizados:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-OK.
           05 LINE 15 COLUMN 13 VALUE "Quantidade Mantidos:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-DUP.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.

       PROCEDURE DIVISION USING LK-PARAM.

       0000-PRINCIPAL SECTION.

           SET ENVIRONMENT 'DB_HOME' TO WNM-PATH-CLIENTE.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           MOVE WNM-PATH-CLIENTE TO WNM-PATH-VENDEDOR.
           MOVE "N"              TO W-VAL-ENTRADA
           MOVE ALL "0"          TO WQT-TOTAIS

           ACCEPT WS-NUML FROM LINES
           COMPUTE WS-NUML-ANT = WS-NUML - 1
           ACCEPT WS-NUMC FROM COLUMNS

           MOVE "DISTRIBUIR" TO WS-OP
           MOVE WS-DS-SAIR   TO WS-STATUS

           MOVE "DESEJA REALIZAR DISTRIBUICAO(S/N)?" TO WS-MSGERRO

           PERFORM
             UNTIL W-VAL-ENTRADA-OK

               MOVE SPACES       TO WS-ERRO

               DISPLAY SS-CLS
               DISPLAY SS-ERRO
               ACCEPT SS-ERRO

               EVALUATE TRUE
                   WHEN E-SIM

                       PERFORM 5000-ABRIR-ARQ-CLIENTE
                       PERFORM 5200-ABRIR-ARQ-CSV

                       MOVE LOW-VALUE  TO FS-CLI-KEY
                       PERFORM 6000-LER-ARQ-CLIENTE
                       DISPLAY FS-STAT-CLI

                       PERFORM
                         UNTIL NOT FS-STAT-CLI-OK

                           MOVE "N"     TO WS-FIM-SORT-FILE

                           SORT SORT-FILE ASCENDING KEY SD-DIST-KM
                                INPUT PROCEDURE  1000-SORT-INPUT
                                OUTPUT PROCEDURE 2000-SORT-OUTPUT

                           PERFORM 6000-LER-ARQ-CLIENTE

                       END-PERFORM

                   PERFORM 7200-FECHA-ARQ-CSV
                   PERFORM 7000-FECHA-ARQ-CLIENTE

                   MOVE "S"      TO W-VAL-ENTRADA
               WHEN COB-CRT-STATUS EQUAL COB-SCR-ESC
                   MOVE 'S' TO W-VAL-ENTRADA
               WHEN E-NAO
                   MOVE 'S' TO W-VAL-ENTRADA
               END-EVALUATE

           END-PERFORM.

           IF E-SIM
               MOVE SPACES       TO WS-ERRO
               DISPLAY SS-TELA-RESULTADO
               MOVE "DISTRIBUICAO CONCLUIDA COM SUCESSO"
                 TO WS-MSGERRO
               PERFORM 9000-MOSTRA-ERRO
           END-IF.

           PERFORM 9999-FINALIZA.

       0000-PRINCIPALX. EXIT.

       1000-SORT-INPUT SECTION.

           PERFORM 5100-ABRIR-ARQ-VENDEDOR.
           MOVE LOW-VALUES TO FS-VEN-KEY.
           PERFORM 6100-LER-ARQ-VENDEDOR.

           PERFORM
             UNTIL NOT FS-STAT-VEN-OK

               MOVE FS-CLI-VL-LATITUDE TO LK-LATITUDE-ORI
               MOVE FS-CLI-VL-LONGITUDE TO LK-LATITUDE-ORI
               MOVE FS-VEN-VL-LATITUDE TO LK-LATITUDE-DES
               MOVE FS-VEN-VL-LONGITUDE TO LK-LATITUDE-DES

               CALL 'CBL_CALC_DISTANCIA' USING LK-DIST-PARAMETERS

               MOVE FS-VEN-CD-VENDEDOR TO SD-VEN-CD-VENDEDOR
               MOVE FS-VEN-NM-VENDEDOR TO SD-NM-VENDEDOR
               MOVE LK-DIST-KM-S TO SD-DIST-KM

               RELEASE SORT-RECORD

               PERFORM 6100-LER-ARQ-VENDEDOR

           END-PERFORM.

           PERFORM 7100-FECHA-ARQ-VENDEDOR.

       1000-SORT-INPUTX. EXIT.

       2000-SORT-OUTPUT SECTION.

           RETURN SORT-FILE AT END
               MOVE "S" TO WS-FIM-SORT-FILE
           END-RETURN.

           IF WS-FIM-SORT-FILE = "N"

               IF FS-CLI-CD-VENDEDOR   EQUAL SD-VEN-CD-VENDEDOR
                   ADD 1 TO WQT-ARQ-DUP
               ELSE
                   MOVE SD-VEN-CD-VENDEDOR TO FS-CLI-CD-VENDEDOR

                   REWRITE ARQ-CLIENTE-REC
                   ADD 1 TO WQT-ARQ-OK
               END-IF

               MOVE ALL ";"           TO ARQ-CSV-REC

               MOVE FS-CLI-CD-CLIENTE TO FS-CSV-CD-CLIENTE
               MOVE FS-CLI-NM-RAZAO-SOCIAL TO FS-CSV-RAZAO-SOCIAL
               MOVE SD-VEN-CD-VENDEDOR TO FS-CSV-CD-VENDEDOR
               MOVE SD-NM-VENDEDOR TO FS-CSV-NM-VENDEDOR

               COMPUTE FS-CSV-DIST-KM =  SD-DIST-KM / 1000

               PERFORM 6200-GRAVA-ARQ-CSV
           END-IF.

       2000-SORT-OUTPUTX. EXIT.

      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
       5000-ABRIR-ARQ-CLIENTE  SECTION.

           OPEN I-O ARQ-CLIENTE.

           IF FS-STAT-CLI-NAO-EXISTE THEN
               OPEN OUTPUT ARQ-CLIENTE
               PERFORM 7000-FECHA-ARQ-CLIENTE
               OPEN I-O ARQ-CLIENTE
           END-IF.

       5000-ABRIR-ARQ-CLIENTEX. EXIT.

      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
       5100-ABRIR-ARQ-VENDEDOR  SECTION.

           OPEN INPUT ARQ-VENDEDOR.

           IF FS-STAT-VEN-NAO-EXISTE
               OPEN OUTPUT ARQ-VENDEDOR
               PERFORM 7100-FECHA-ARQ-VENDEDOR
               OPEN INPUT ARQ-VENDEDOR
           END-IF.

       5100-ABRIR-ARQ-VENDEDORX. EXIT.

      * -----------------------------------
      * ABRE ARQUIVOS CSV
       5200-ABRIR-ARQ-CSV SECTION.

           OPEN OUTPUT ARQ-CSV.

       5200-ABRIR-ARQ-CSVX. EXIT.

      * -----------------------------------
      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       6000-LER-ARQ-CLIENTE SECTION.

002260     START ARQ-CLIENTE KEY > FS-CLI-KEY
002300       NOT INVALID KEY
002310         READ ARQ-CLIENTE NEXT
               END-READ
002390     END-START.

           IF FS-STAT-CLI-OK
               ADD 1 TO WQT-ARQ-IMPORTACAO
           END-IF.

       6000-LER-ARQ-CLIENTE-FIMX. EXIT.

      * -----------------------------------
      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       6100-LER-ARQ-VENDEDOR SECTION.

002260      START ARQ-VENDEDOR KEY > FS-VEN-KEY
002300        NOT INVALID KEY
002310          READ ARQ-VENDEDOR NEXT
002370          END-READ
002390      END-START.

       6100-LER-ARQ-VENDEDOR-FIMX. EXIT.

      * -----------------------------------
      * GRABAR ARQUIVO CSV
       6200-GRAVA-ARQ-CSV SECTION.

           WRITE ARQ-CSV-REC.

       6200-GRAVA-ARQ-CSVX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO DE CLIENTES
       7000-FECHA-ARQ-CLIENTE SECTION.

           CLOSE ARQ-CLIENTE.

       7000-FECHA-ARQ-CLIENTEX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO DE VENDEDOR
       7100-FECHA-ARQ-VENDEDOR SECTION.

           CLOSE ARQ-VENDEDOR.

       7100-FECHA-ARQ-VENDEDORX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO CSV
       7200-FECHA-ARQ-CSV SECTION.

           CLOSE ARQ-CSV.

       7200-FECHA-ARQ-CSVX. EXIT.

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
       END PROGRAM CBL_MENU_DISTRIBUICAO.
