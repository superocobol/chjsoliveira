       01 ARQ-VENDEDOR-REC.
           05 FS-VEN-KEY.
               10 FS-VEN-CD-VENDEDOR PIC  9(007) BLANK WHEN ZEROS.
           05 FS-VEN-DADOS.
               10 FS-VEN-CPF         PIC  9(011).
               10 FS-VEN-NM-VENDEDOR PIC  X(040).
               10 FS-VEN-VL-LATITUDE PIC S9(003)V9(008).
               10 FS-VEN-VL-LONGITUDE PIC S9(003)V9(008).
               10 FILLER             PIC  X(020).
