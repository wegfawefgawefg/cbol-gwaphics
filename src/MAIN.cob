       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X          PIC S9(4).
       01  Y          PIC S9(4).
       01  R          PIC S9(3).
       01  G          PIC S9(3).
       01  B          PIC S9(3).

           COPY SETTINGS.
           COPY NORMARGS.
           COPY DISPARGS.
    
       PROCEDURE DIVISION.
           PERFORM VARYING Y FROM 0 BY 1 UNTIL Y >= HEIGHT
              PERFORM VARYING X FROM 0 BY 1 UNTIL X >= WIDTH
      
      *    Normalize X to R (0-255)
                 MOVE X TO NORM-X
                 MOVE 0 TO NORM-LOW
                 COMPUTE NORM-HIGH = WIDTH - 1
                 CALL 'NORM' USING NORM-ARGS
                 MOVE NORM-RESULT TO R

      *    Normalize Y to G (0-255)
                 MOVE Y TO NORM-X
                 MOVE 0 TO NORM-LOW
                 COMPUTE NORM-HIGH = HEIGHT - 1
                 CALL 'NORM' USING NORM-ARGS
                 MOVE NORM-RESULT TO G

      *    Compute B as average of R and G
                 COMPUTE B = (R + G) / 2
               
      *    blit pixel at (X,Y) with color (R,G,B)
                 MOVE X TO DISP-X
                 MOVE Y TO DISP-Y
                 MOVE R TO DISP-R
                 MOVE G TO DISP-G
                 MOVE B TO DISP-B
                 CALL 'DISP' USING DISP-ARGS
              END-PERFORM
           END-PERFORM
           STOP RUN.
