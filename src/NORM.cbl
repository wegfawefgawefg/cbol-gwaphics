       IDENTIFICATION DIVISION.
       PROGRAM-ID. NORM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  LOCAL-RANGE         PIC S9(9).
       01  LOCAL-NUMERATOR     PIC S9(9).

       LINKAGE SECTION.
           COPY NORMARGS.

       PROCEDURE DIVISION USING NORM-ARGS.

           COMPUTE LOCAL-RANGE = NORM-HIGH - NORM-LOW

           IF LOCAL-RANGE <= 0
              MOVE 0 TO NORM-RESULT
              GOBACK
           END-IF

           IF NORM-X <= NORM-LOW
              MOVE 0 TO NORM-RESULT
              GOBACK
           END-IF

           IF NORM-X >= NORM-HIGH
              MOVE 255 TO NORM-RESULT
              GOBACK
           END-IF

           COMPUTE LOCAL-NUMERATOR = (NORM-X - NORM-LOW) * 255
           DIVIDE LOCAL-RANGE INTO LOCAL-NUMERATOR
                 GIVING NORM-RESULT ROUNDED.

           GOBACK.
