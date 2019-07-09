      ******************************************************************
      * Author: CARLOS OLIVEIRA
      * Date: 07/07/2019
      * Purpose: Calcular a distância em KM entre dois pontos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL_CALC_DISTANCIA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CONSTANTS.
           05 WS-PI              PIC S9(1)V9(5) VALUE 3.14159.
           05 WS-RAIO-TERRA      PIC S9(4)V9(5) VALUE 6371.0.
           05 WS-RAIO            PIC S9(3)V9    VALUE 180.

       01  WS-PI-LATITUDE-ORI    PIC S9(4)V9(8) VALUE ZEROS.
       01  WS-PI-LONGITUDE-ORI   PIC S9(4)V9(8) VALUE ZEROS.
       01  WS-PI-LATITUDE-DES    PIC S9(4)V9(8) VALUE ZEROS.
       01  WS-PI-LONGITUDE-DES   PIC S9(4)V9(8) VALUE ZEROS.
       01  WS-VAL                PIC S9(5)V9(8) VALUE ZEROS.
       01  WS-RAIZ-A             PIC S9(5)V9(8) VALUE ZEROS.
       01  WS-RAIZ-B             PIC S9(5)V9(8) VALUE ZEROS.
       01  WS-DIST               PIC S9(5)V9(8) VALUE ZEROS.

       01  WS-LATITUDE           PIC S9(4)V9(8) VALUE ZEROS.
       01  WS-LONGITUDE          PIC S9(4)V9(8) VALUE ZEROS.

       LINKAGE SECTION.

       COPY CPY_CALC_DISTANCIA.

       PROCEDURE DIVISION USING LK-DIST-PARAMETERS.

       0000-PRINCIPAL SECTION.

           PERFORM 1000-INICIALIZA.

           COMPUTE WS-PI-LATITUDE-ORI =
                   LK-LATITUDE-ORI * WS-PI / WS-RAIO

           COMPUTE WS-PI-LATITUDE-DES =
                   LK-LATITUDE-DES * WS-PI / WS-RAIO

           COMPUTE WS-PI-LONGITUDE-ORI =
                   LK-LONGITUDE-ORI * WS-PI / WS-RAIO

           COMPUTE WS-PI-LONGITUDE-DES =
                   LK-LONGITUDE-DES * WS-PI / WS-RAIO

           COMPUTE WS-LATITUDE =
                   LK-LATITUDE-DES - LK-LATITUDE-ORI

           COMPUTE WS-LONGITUDE =
                   LK-LONGITUDE-DES - LK-LONGITUDE-ORI

           COMPUTE WS-VAL =
                   FUNCTION SIN(WS-LATITUDE / 2) *
                   FUNCTION SIN(WS-LATITUDE / 2) +
                   FUNCTION COS(WS-PI-LATITUDE-ORI)*
                   FUNCTION COS(WS-PI-LATITUDE-DES)  *
                   FUNCTION SIN(WS-LONGITUDE / 2) *
                   FUNCTION SIN(WS-LONGITUDE / 2)

           COMPUTE WS-RAIZ-A =
                   FUNCTION SQRT(WS-VAL)

           COMPUTE WS-RAIZ-B =
                   FUNCTION SQRT(1 - WS-VAL)

           EVALUATE TRUE
               WHEN WS-RAIZ-B GREATER ZEROS
                    COMPUTE WS-DIST =
                        FUNCTION ATAN(WS-RAIZ-A/WS-RAIZ-B)
               WHEN WS-RAIZ-B LESS ZEROS
                    COMPUTE WS-DIST =
                        FUNCTION ATAN(WS-RAIZ-A/WS-RAIZ-B)+WS-PI
               WHEN WS-RAIZ-B EQUAL ZEROS
                    COMPUTE WS-DIST = WS-PI / 2 *
                        (WS-RAIZ-A/ FUNCTION ABS(WS-RAIZ-A))
           END-EVALUATE

           COMPUTE WS-DIST = WS-DIST * WS-RAIO-TERRA

           PERFORM 9999-FINALIZA.

       0000-PRINCIPALX. EXIT.

      * -----------------------------------
      * INICIALIZA VARIAVEIS
       1000-INICIALIZA SECTION.

           MOVE ZEROS         TO WS-DIST
                                 WS-RAIZ-A
                                 WS-RAIZ-B
                                 WS-VAL
                                 WS-LATITUDE
                                 WS-LONGITUDE
                                 WS-PI-LATITUDE-ORI
                                 WS-PI-LATITUDE-DES
                                 WS-PI-LONGITUDE-ORI
                                 WS-PI-LONGITUDE-DES
                                 LK-DIST-KM-S.

       1000-INICIALIZAX. EXIT.

      * -----------------------------------
      * FINALIZAR PROCESSO
       9999-FINALIZA SECTION.

           MOVE WS-DIST TO LK-DIST-KM-S
           MOVE 0 TO LK-DIST-RC-S

           GOBACK.

       9999-FINALIZAX. EXIT.
       END PROGRAM CBL_CALC_DISTANCIA.
