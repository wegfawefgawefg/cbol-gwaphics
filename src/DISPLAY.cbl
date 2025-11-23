       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISP.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  OUT-LINE      PIC X(40).

       LINKAGE SECTION.
           COPY DISPARGS.

       PROCEDURE DIVISION USING DISP-ARGS.
           MOVE SPACES TO OUT-LINE
                 STRING
                      FUNCTION TRIM(DISP-X) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(DISP-Y) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(DISP-R) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(DISP-G) DELIMITED BY SIZE
                      " " DELIMITED BY SIZE
                      FUNCTION TRIM(DISP-B) DELIMITED BY SIZE
                      INTO OUT-LINE
                 END-STRING

                 DISPLAY OUT-LINE
           GOBACK.
