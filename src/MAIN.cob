      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01  BOUNCE-VARS.
               05  BOUNCE-X      PIC S9(4) VALUE 100.
               05  BOUNCE-Y      PIC S9(4) VALUE 100.
               05  BOUNCE-DX     PIC S9(4) VALUE 1.
               05  BOUNCE-DY     PIC S9(4) VALUE 1.
               05  COLOR-R       PIC 9(3) VALUE 0.
               05  COLOR-G       PIC 9(3) VALUE 0.
               05  COLOR-B       PIC 9(3) VALUE 0.
               05  TEMP-COLOR    PIC 9(4).
               05  DELAY-COUNTER PIC 9(8).
               05  I             PIC 9(8).

           COPY GFXARGS.

       PROCEDURE DIVISION.
      *    clear screen once at the start
           MOVE 'CLR' TO GFX-OPCODE
           CALL 'GRAPHICS' USING GFX-ARGS
           .

      * WHILE LOOP where we bounce a small square around the screen
           PERFORM FOREVER
      *        Draw the bouncing rectangle with current color
               MOVE 'RECT' TO GFX-OPCODE
               MOVE COLOR-R TO GFX-COLOR-R
               MOVE COLOR-G TO GFX-COLOR-G
               MOVE COLOR-B TO GFX-COLOR-B
               MOVE BOUNCE-X TO GFX-RECT-X
               MOVE BOUNCE-Y TO GFX-RECT-Y
               MOVE 2     TO GFX-RECT-W
               MOVE 2     TO GFX-RECT-H
               CALL 'GRAPHICS' USING GFX-ARGS

      *        Delay loop to slow things down
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100000
                   CONTINUE
               END-PERFORM

      *        Update position
               ADD BOUNCE-DX TO BOUNCE-X
               ADD BOUNCE-DY TO BOUNCE-Y

      *        Bounce off edges
               IF BOUNCE-X <= 0 OR BOUNCE-X >= 619
               MULTIPLY BOUNCE-DX BY -1 GIVING BOUNCE-DX
               END-IF

               IF BOUNCE-Y <= 0 OR BOUNCE-Y >= 459
               MULTIPLY BOUNCE-DY BY -1 GIVING BOUNCE-DY
               END-IF

      *        Update colors at different rates
      *        R advances by 3 each frame
               ADD 3 TO COLOR-R
               DIVIDE COLOR-R BY 256 GIVING TEMP-COLOR
                   REMAINDER COLOR-R

      *        G advances by 5 each frame
               ADD 5 TO COLOR-G
               DIVIDE COLOR-G BY 256 GIVING TEMP-COLOR
                   REMAINDER COLOR-G

      *        B advances by 7 each frame
               ADD 7 TO COLOR-B
               DIVIDE COLOR-B BY 256 GIVING TEMP-COLOR
                   REMAINDER COLOR-B
           END-PERFORM.

           STOP RUN.
