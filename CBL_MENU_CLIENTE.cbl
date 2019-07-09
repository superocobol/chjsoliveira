      ******************************************************************
      * Author: CARLOS OLIVEIRA
      * Date: 07/07/2019
      * Purpose: Mmenu Cliente - CRUD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_MENU_CLIENTE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-CLIENTE ASSIGN TO DISK WID-ARQ-CLIENTE
               ORGANIZATION       IS INDEXED
               ACCESS MODE        IS DYNAMIC
               RECORD KEY         IS FS-CLI-KEY
               ALTERNATE RECORD KEY IS FS-CLI-CNPJ
               LOCK MODE          IS MANUAL
               FILE STATUS        IS FS-STAT-CLI.

           SELECT ARQ-IMPORTACAO ASSIGN TO WID-ARQ-IMPORTACAO
               ORGANIZATION       IS INDEXED
               ACCESS MODE        IS DYNAMIC
               RECORD KEY         IS FS-IMP-KEY
               LOCK MODE          IS MANUAL
               FILE STATUS        IS FS-STAT-IMP.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTE.

       COPY FS-ARQ-CLIENTE.

       FD ARQ-IMPORTACAO.
       01 ARQ-IMPORTACAO-REC.
           05 FS-IMP-KEY.
               10 FS-CD-CLIENTE-IMP      PIC  9(007).
           05 FS-IMP-DADOS.
               10 FS-CNPJ-IMP            PIC  9(014) BLANK WHEN ZEROS.
               10 FS-NM-RAZAO-SOCIAL-IMP PIC  X(040).
               10 FS-VL-LATITUDE-IMP     PIC S9(003)V9(008).
               10 FS-VL-LONGITUDE-IMP    PIC S9(003)V9(008).

       WORKING-STORAGE SECTION.
       01 WS-MODULO.
           05 FILLER PIC X(17) VALUE "FRANQUIA AMBEV - ".
           05 WS-OP PIC X(53) VALUE SPACES.

       77 WS-DS-OPCAO PIC X(30) VALUE "ESCOLHA A OPCAO.".
       77 WS-DS-SAIR PIC  X(30) VALUE "EXC PARA RETORNAR.".
       77 WS-DS-OPCAO-ERR PIC X(30) VALUE "OPCAO INVALIDA.".

       77 WS-OPCAO PIC X.
           88 E-INCLUIR   VALUE IS "1".
           88 E-ALTERAR   VALUE IS "2".
           88 E-EXCLUIR   VALUE IS "3".
           88 E-IMPORTAR  VALUE IS "4".
           88 E-ENCERRAR  VALUE IS "X" "x".
           88 E-OPCAO-OK  VALUE ARE "1" "2" "3" "4" "X" "x".


       77 WS-OPCAO-EXE PIC X.
           88 E-EXECUCAO-EXE  VALUE IS "1".
           88 E-EXECUCAO-ENC  VALUE IS "X" "x".
           88 E-OPCAO-EXE-OK  VALUE ARE "1" "X" "x".

       77 W-VAL-ENTRADA PIC X.
           88 W-VAL-ENTRADA-OK VALUE 'S'.

       77 FS-STAT-CLI PIC 9(02).
           88 FS-STAT-CLI-OK         VALUE 00.
           88 FS-STAT-CLI-EOF        VALUE 10 23.
           88 FS-STAT-CLI-CANCELA    VALUE 99.
           88 FS-STAT-CLI-NAO-EXISTE VALUE 35.
           88 FS-STAT-CLI-DUP        VALUE 21.

       77 FS-STAT-IMP PIC 9(02).
           88 FS-STAT-IMP-OK         VALUE IS 00.
           88 FS-STAT-IMP-EOF VALUE 10.
           88 FS-STAT-IMP-CANCELA    VALUE 99.
           88 FS-STAT-IMP-NAO-EXISTE VALUE 35.

       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 WS-NUML PIC 999.
       77 WS-NUML-ANT PIC 999.
       77 WS-NUMC PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).

       77 WS-MSGERRO PIC X(80).

       01 WS-CLIENTE-REC.
           05 WS-KEY.
               10 WS-CD-CLIENTE      PIC  9(007) BLANK WHEN ZEROS.
           05 WS-DADOS.
               10 WS-CNPJ            PIC  9(014).
               10 WS-NM-RAZAO-SOCIAL PIC  X(040).
               10 WS-VL-LATITUDE     PIC S9(003)V9(008).
               10 WS-VL-LONGITUDE    PIC S9(003)V9(008).
               10 FILLER             PIC  X(020).

       01 WQT-TOTAIS.
           05 WQT-ARQ-IMPORTACAO     PIC 9(9) VALUE ZEROS.
           05 WQT-ARQ-OK             PIC 9(9) VALUE ZEROS.
           05 WQT-ARQ-DUP            PIC 9(9) VALUE ZEROS.
           05 WQT-ARQ-ERR            PIC 9(9) VALUE ZEROS.

       COPY CPY_ID_ARQ_CLIENTE.

       01 WID-ARQ-IMPORTACAO.
000380     05 WNM-PATH-IMPORTACAO PIC X(17).
           05 WNM-ARQ-IMPORTACAO PIC X(50) VALUE "CARGA.IDX".

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

       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "01 - INCLUIR".
           05 LINE 08 COLUMN 15 VALUE "02 - ALTERAR".
           05 LINE 09 COLUMN 15 VALUE "03 - EXCLUIR".
           05 LINE 10 COLUMN 15 VALUE "04 - IMPORTAR".
           05 LINE 11 COLUMN 15 VALUE "X  - ENCERRAR".
           05 LINE 12 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 12 COL PLUS 1 USING WS-OPCAO AUTO.
+1
       01 SS-TELA-CLIENTE.
           05 SS-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE "Codigo Cliente:".
               10 COLUMN PLUS 2 PIC 9(07) USING WS-CD-CLIENTE
                  BLANK WHEN ZEROS.
           05 SS-DADOS.
               10 LINE 11 COLUMN 20 VALUE "CNPJ:".
               10 COLUMN PLUS 2 PIC 9(14) USING WS-CNPJ.
               10 LINE 12 COLUMN 12 VALUE "Razao Social:".
               10 COLUMN PLUS 2 PIC X(40) USING WS-NM-RAZAO-SOCIAL.
               10 LINE 13 COLUMN 16 VALUE "Latitude:".
               10 COLUMN PLUS 2 PIC +999.99999999
                                          USING WS-VL-LATITUDE.
               10 LINE 14 COLUMN 15 VALUE "Longitude:".
               10 COLUMN PLUS 2 PIC +999.99999999
                                          USING WS-VL-LONGITUDE.

       01 SS-TELA-IMPORTACAO.
           05 LINE 10 COLUMN 10 VALUE "Nome do Arquivo com extensao:".
           05 COLUMN PLUS 2 PIC X(20) USING WNM-ARQ-IMPORTACAO.
           05 LINE 11 COLUMN 10 VALUE "Caminho:".
           05 LINE 12 COLUMN 10 PIC X(50) FROM WNM-PATH-IMPORTACAO.

       01 SS-TELA-RESULTADO.
           05 LINE 13 COLUMN 10 VALUE "Quantidade:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-IMPORTACAO.
           05 LINE 14 COLUMN 13 VALUE "Sucesso:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-OK.
           05 LINE 15 COLUMN 10 VALUE "Duplicados:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-DUP.
           05 LINE 16 COLUMN 16 VALUE "Erro:".
           05 COLUMN PLUS 2 PIC 9(9) USING WQT-ARQ-ERR.

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
           MOVE WNM-PATH-CLIENTE TO WNM-PATH-IMPORTACAO.
           MOVE SPACES TO WS-OPCAO

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
                   WHEN E-INCLUIR
                       PERFORM 1000-ACS-INCLUIR-CLIENTE
                       MOVE SPACES TO WS-OPCAO
                   WHEN E-ALTERAR
                       PERFORM 1100-ACS-ALTERAR-CLIENTE
                       MOVE SPACES TO WS-OPCAO
                   WHEN E-EXCLUIR
                       PERFORM 1200-ACS-EXCLUIR-CLIENTE
                       MOVE SPACES TO WS-OPCAO
                   WHEN E-IMPORTAR
                       PERFORM 1300-ACS-IMPORTAR-CLIENTE
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
       1000-ACS-INCLUIR-CLIENTE SECTION.

           MOVE "01 - INCLUSAO" TO WS-OP
           MOVE WS-DS-SAIR TO WS-STATUS
           MOVE 'N' TO W-VAL-ENTRADA
           MOVE SPACES TO WS-ERRO

           MOVE SPACES TO ARQ-CLIENTE-REC WS-CLIENTE-REC

           PERFORM 5000-ABRIR-ARQ-CLIENTE

           PERFORM
             UNTIL W-VAL-ENTRADA-OK

               DISPLAY SS-CLS
               ACCEPT SS-TELA-CLIENTE

               MOVE WS-CNPJ TO FS-CLI-CNPJ

               START ARQ-CLIENTE
                  KEY = FS-CLI-CNPJ
               END-START

               EVALUATE TRUE
                   WHEN FS-STAT-CLI-OK
                        MOVE "CNPJ DUPLICADO, NAO INSERIDO"
                          TO WS-MSGERRO
                        PERFORM 9000-MOSTRA-ERRO
                        MOVE 'N' TO W-VAL-ENTRADA
                   WHEN WS-CD-CLIENTE      NOT EQUAL SPACES AND
                        WS-CNPJ            GREATER ZEROS AND
                        WS-NM-RAZAO-SOCIAL NOT EQUAL SPACES
                        MOVE 'S' TO W-VAL-ENTRADA
                   WHEN COB-CRT-STATUS EQUAL COB-SCR-ESC
                        MOVE 'S' TO W-VAL-ENTRADA
                   WHEN WS-CD-CLIENTE      EQUAL SPACES OR
                        WS-CNPJ            NOT GREATER ZEROS OR
                        WS-NM-RAZAO-SOCIAL EQUAL SPACES
                        STRING
                          "FAVOR INFORMAR Codigo, CNPJ e "
                          "Razao social do Cliente" DELIMITED BY SIZE
                          INTO WS-MSGERRO
                        PERFORM 9000-MOSTRA-ERRO
               END-EVALUATE

           END-PERFORM.

           IF COB-CRT-STATUS EQUAL COB-SCR-OK

               MOVE WS-CLIENTE-REC TO ARQ-CLIENTE-REC

               WRITE ARQ-CLIENTE-REC
               INVALID KEY
                   MOVE "CLIENTE JA EXISTE" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
                   MOVE ZEROS TO FS-CLI-KEY
               END-WRITE

               IF FS-STAT-CLI-OK
                   MOVE "CLIENTE ADICIONADO COM SUCESSO" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
               END-IF
           END-IF.

           PERFORM 7000-FECHA-ARQ-CLIENTE.

       1000-ACS-INCLUIR-CLIENTE-FIMX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE ALTERAÇÃO
       1100-ACS-ALTERAR-CLIENTE SECTION.

           MOVE "02 - ALTERAR" TO WS-OP
           MOVE WS-DS-SAIR TO WS-STATUS
           MOVE 'N' TO W-VAL-ENTRADA
           MOVE SPACES TO WS-ERRO

           MOVE SPACES TO ARQ-CLIENTE-REC WS-CLIENTE-REC

           PERFORM 5000-ABRIR-ARQ-CLIENTE

           PERFORM
             UNTIL W-VAL-ENTRADA-OK

               DISPLAY SS-CLS
               MOVE SPACES TO WS-DADOS FS-CLI-DADOS

               ACCEPT SS-CHAVE

               MOVE WS-CD-CLIENTE TO FS-CLI-CD-CLIENTE

               PERFORM 6000-LER-ARQ-CLIENTE

               IF FS-STAT-CLI-EOF
                   MOVE "CLIENTE NAO ENCONTRADO" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
               END-IF

               IF FS-STAT-CLI-OK
                   MOVE ARQ-CLIENTE-REC TO WS-CLIENTE-REC

                   ACCEPT SS-DADOS

                   MOVE WS-CNPJ TO FS-CLI-CNPJ

                   START ARQ-CLIENTE
                      KEY = FS-CLI-CNPJ
                   END-START

                   EVALUATE TRUE
                       WHEN FS-STAT-CLI-OK AND
                            WS-CD-CLIENTE  NOT EQUAL FS-CLI-CD-CLIENTE
                            MOVE "CNPJ DUPLICADO NAO ATUALIZADO"
                              TO WS-MSGERRO
                            PERFORM 9000-MOSTRA-ERRO
                            MOVE 'N' TO W-VAL-ENTRADA
                       WHEN WS-CD-CLIENTE NOT EQUAL SPACES AND
                            WS-CNPJ GREATER ZEROS AND
                            WS-NM-RAZAO-SOCIAL NOT EQUAL SPACES
                            MOVE 'S' TO W-VAL-ENTRADA
                       WHEN COB-CRT-STATUS EQUAL COB-SCR-ESC
                            MOVE 'S' TO W-VAL-ENTRADA
                       WHEN WS-CD-CLIENTE EQUAL SPACES OR
                            WS-CNPJ NOT GREATER ZEROS OR
                            WS-NM-RAZAO-SOCIAL EQUAL SPACES
                            STRING
                              "FAVOR INFORMAR Codigo do Cliente,"
                              "CNPJ E Razao social" DELIMITED BY SIZE
                              INTO WS-MSGERRO
                            PERFORM 9000-MOSTRA-ERRO
                   END-EVALUATE
               END-IF

           END-PERFORM.

           IF COB-CRT-STATUS NOT EQUAL COB-SCR-ESC

               REWRITE ARQ-CLIENTE-REC
               INVALID KEY
                   MOVE "ERRO AO GRAVAR" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
                   MOVE ZEROS TO FS-CLI-KEY
               END-REWRITE

               IF FS-STAT-CLI-OK
                   MOVE "CLIENTE ALTERADO COM SUCESSO" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
               END-IF
           END-IF.

           PERFORM 7000-FECHA-ARQ-CLIENTE.

       1100-ACS-ALTERAR-CLIENTE-FIMX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE EXCLUSAO
       1200-ACS-EXCLUIR-CLIENTE SECTION.

           MOVE "03 - EXCLUIR" TO WS-OP
           MOVE WS-DS-SAIR TO WS-STATUS
           MOVE 'N' TO W-VAL-ENTRADA
           MOVE SPACES TO WS-ERRO

           MOVE SPACES TO ARQ-CLIENTE-REC WS-CLIENTE-REC

           PERFORM 5000-ABRIR-ARQ-CLIENTE

           PERFORM
             UNTIL W-VAL-ENTRADA-OK

               DISPLAY SS-CLS
               MOVE SPACES TO ARQ-CLIENTE-REC WS-CLIENTE-REC

               ACCEPT SS-CHAVE

               MOVE WS-CD-CLIENTE TO FS-CLI-CD-CLIENTE

               PERFORM 6000-LER-ARQ-CLIENTE

               IF FS-STAT-CLI-EOF
                   MOVE "CLIENTE NAO ENCONTRADO" TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
                   MOVE "N" TO W-VAL-ENTRADA
               END-IF

               IF FS-STAT-CLI-OK
                   MOVE SPACES TO WS-ERRO
                   MOVE "CONFIRMA A EXCLUSAO DO CLIENTE (S/N)?"
                     TO WS-MSGERRO

                   ACCEPT SS-ERRO

                   IF E-SIM
                       DELETE ARQ-CLIENTE
                           INVALID KEY
                               MOVE "ERRO AO EXCLUIR" TO WS-MSGERRO
                               PERFORM 9000-MOSTRA-ERRO
                       END-DELETE
                       MOVE SPACES TO WS-ERRO
                       IF FS-STAT-CLI-OK
                           MOVE "CLIENTE EXCLUIDO COM SUCESSO"
                             TO WS-MSGERRO
                           PERFORM 9000-MOSTRA-ERRO
                           MOVE "S" TO W-VAL-ENTRADA
                       END-IF
                   END-IF
                   MOVE SPACES TO WS-ERRO
               END-IF

           END-PERFORM.

           PERFORM 7000-FECHA-ARQ-CLIENTE.

       1200-ACS-EXCLUIR-CLIENTEX. EXIT.

      * -----------------------------------
      * ACESSA MENU DE IMPORTACAO
       1300-ACS-IMPORTAR-CLIENTE SECTION.

           MOVE "03 - EXCLUIR" TO WS-OP
           MOVE WS-DS-SAIR TO WS-STATUS
           MOVE 'N' TO W-VAL-ENTRADA

           PERFORM
             UNTIL W-VAL-ENTRADA-OK

               DISPLAY SS-CLS
               MOVE SPACES TO ARQ-CLIENTE-REC

               ACCEPT SS-TELA-IMPORTACAO

               MOVE ZEROS TO WQT-TOTAIS

               IF COB-CRT-STATUS EQUAL COB-SCR-ESC
                   MOVE 'S' TO W-VAL-ENTRADA
               END-IF

               IF COB-CRT-STATUS EQUAL COB-SCR-OK
                   PERFORM 5100-ABRIR-ARQ-IMPORTACAO
                   PERFORM 5000-ABRIR-ARQ-CLIENTE

                   MOVE LOW-VALUES TO FS-IMP-KEY

                   PERFORM 6100-LER-ARQ-IMPORTACAO

                   PERFORM
                     UNTIL NOT FS-STAT-IMP-OK

                       MOVE FS-CNPJ-IMP TO FS-CLI-CNPJ
                       MOVE FS-CD-CLIENTE-IMP TO FS-CLI-CD-CLIENTE
                       MOVE FS-NM-RAZAO-SOCIAL-IMP
                         TO FS-CLI-NM-RAZAO-SOCIAL
                       MOVE FS-VL-LATITUDE-IMP TO FS-CLI-VL-LATITUDE
                       MOVE FS-VL-LONGITUDE-IMP TO FS-CLI-VL-LONGITUDE

                       IF FS-CLI-CD-CLIENTE EQUAL SPACES OR
                          FS-CLI-CNPJ NOT GREATER ZEROS OR
                          FS-CLI-NM-RAZAO-SOCIAL EQUAL SPACES
                           ADD 1 TO  WQT-ARQ-ERR
                       ELSE
                           WRITE ARQ-CLIENTE-REC
002300                         INVALID KEY
                                   ADD 1 TO  WQT-ARQ-DUP
                               NOT INVALID KEY
                                   ADD 1 TO  WQT-ARQ-OK
                           END-WRITE
                       END-IF

                       PERFORM 6100-LER-ARQ-IMPORTACAO

                   END-PERFORM

                   DISPLAY SS-TELA-RESULTADO

                   MOVE "IMPORTACAO CONCLUIDA COM SUCESSO"
                     TO WS-MSGERRO
                   PERFORM 9000-MOSTRA-ERRO
                   PERFORM 7000-FECHA-ARQ-CLIENTE
                   PERFORM 7100-FECHA-ARQ-IMPORTACAO

               END-IF

           END-PERFORM.

       1300-ACS-IMPORTAR-CLIENTEX. EXIT.

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
       5100-ABRIR-ARQ-IMPORTACAO  SECTION.

           OPEN INPUT ARQ-IMPORTACAO.

           IF FS-STAT-IMP-NAO-EXISTE
               OPEN OUTPUT ARQ-IMPORTACAO
               PERFORM 7100-FECHA-ARQ-IMPORTACAO
               OPEN INPUT ARQ-IMPORTACAO
           END-IF.

       5100-ABRIR-ARQ-IMPORTACAOX. EXIT.

      * -----------------------------------
      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       6000-LER-ARQ-CLIENTE SECTION.

           IF COB-CRT-STATUS EQUAL COB-SCR-OK
               READ ARQ-CLIENTE
           ELSE
               MOVE 99  TO FS-STAT-CLI
               MOVE 'S' TO W-VAL-ENTRADA
           END-IF.

       6000-LER-ARQ-CLIENTE-FIMX. EXIT.

      * -----------------------------------
      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NÃO EXISTE
       6100-LER-ARQ-IMPORTACAO SECTION.

002260      START ARQ-IMPORTACAO KEY > FS-IMP-KEY
002300        NOT INVALID KEY
002310          READ ARQ-IMPORTACAO NEXT
002350            NOT AT END
002360               ADD 1 TO  WQT-ARQ-IMPORTACAO
002370          END-READ
002390      END-START.

       6100-LER-ARQ-IMPORTACAO-FIMX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO DE CLIENTES
       7000-FECHA-ARQ-CLIENTE SECTION.

           CLOSE ARQ-CLIENTE.

       7000-FECHA-ARQ-CLIENTEX. EXIT.

      * -----------------------------------
      * FECHA ARQUIVO DE CLIENTES
       7100-FECHA-ARQ-IMPORTACAO SECTION.

           CLOSE ARQ-IMPORTACAO.

       7100-FECHA-ARQ-IMPORTACAOX. EXIT.

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
       END PROGRAM CBL_MENU_CLIENTE.
