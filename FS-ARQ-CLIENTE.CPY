       01 ARQ-CLIENTE-REC.
           05 FS-CLI-KEY.
               10 FS-CLI-CD-CLIENTE      PIC  9(007) BLANK WHEN ZEROS.
           05 FS-CLI-DADOS.
               10 FS-CLI-CNPJ            PIC  9(014).
               10 FS-CLI-NM-RAZAO-SOCIAL PIC  X(040).
               10 FS-CLI-VL-LATITUDE     PIC S9(003)V9(008).
               10 FS-CLI-VL-LONGITUDE    PIC S9(003)V9(008).
               10 FS-CLI-CD-VENDEDOR     PIC  9(007).
               10 FILLER                 PIC  X(020).
