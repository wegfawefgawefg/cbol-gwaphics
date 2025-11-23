      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Grid is 64x64 = 4096 cells
      * 0 = white, 1 = black
           01  GRID-DATA.
               05  GRID-ROW OCCURS 64 TIMES.
                   10  GRID-CELL PIC 9 OCCURS 64 TIMES VALUE 0.

      * Ant state
           01  ANT-VARS.
               05  ANT-X         PIC 99 VALUE 32.
               05  ANT-Y         PIC 99 VALUE 32.
               05  ANT-DIR       PIC 9 VALUE 0.
      *        0=North, 1=East, 2=South, 3=West
               05  CURRENT-CELL  PIC 9.
               05  NEW-DIR       PIC S9(2).

      * Loop counters
           01  COUNTERS.
               05  I             PIC 99.
               05  J             PIC 99.
               05  STEPS         PIC 9(8) VALUE 0.
               05  DELAY-I       PIC 9(8).

           COPY GFXARGS.

       PROCEDURE DIVISION.
      *    Clear screen once
           MOVE 'CLR' TO GFX-OPCODE
           CALL 'GRAPHICS' USING GFX-ARGS
           .

      * Main loop - run Langton's Ant
           PERFORM FOREVER
      *        Get current cell state (1-indexed in COBOL)
               COMPUTE I = ANT-Y + 1
               COMPUTE J = ANT-X + 1
               MOVE GRID-CELL(I, J) TO CURRENT-CELL

      *        Turn based on current cell
      *        White (0): turn right, Black (1): turn left
               IF CURRENT-CELL = 0
                   COMPUTE NEW-DIR = ANT-DIR + 1
                   IF NEW-DIR > 3
                       MOVE 0 TO NEW-DIR
                   END-IF
               ELSE
                   COMPUTE NEW-DIR = ANT-DIR - 1
                   IF NEW-DIR < 0
                       MOVE 3 TO NEW-DIR
                   END-IF
               END-IF
               MOVE NEW-DIR TO ANT-DIR

      *        Flip the current cell
               IF CURRENT-CELL = 0
                   MOVE 1 TO GRID-CELL(I, J)
      *            Draw black pixel
                   MOVE 'RECT' TO GFX-OPCODE
                   MOVE 0 TO GFX-COLOR-R
                   MOVE 0 TO GFX-COLOR-G
                   MOVE 0 TO GFX-COLOR-B
                   MOVE ANT-X TO GFX-RECT-X
                   MOVE ANT-Y TO GFX-RECT-Y
                   MOVE 1 TO GFX-RECT-W
                   MOVE 1 TO GFX-RECT-H
                   CALL 'GRAPHICS' USING GFX-ARGS
               ELSE
                   MOVE 0 TO GRID-CELL(I, J)
      *            Draw white pixel
                   MOVE 'RECT' TO GFX-OPCODE
                   MOVE 255 TO GFX-COLOR-R
                   MOVE 255 TO GFX-COLOR-G
                   MOVE 255 TO GFX-COLOR-B
                   MOVE ANT-X TO GFX-RECT-X
                   MOVE ANT-Y TO GFX-RECT-Y
                   MOVE 1 TO GFX-RECT-W
                   MOVE 1 TO GFX-RECT-H
                   CALL 'GRAPHICS' USING GFX-ARGS
               END-IF

      *        Move forward based on direction
               EVALUATE ANT-DIR
                   WHEN 0
      *                North
                       SUBTRACT 1 FROM ANT-Y
                   WHEN 1
      *                East
                       ADD 1 TO ANT-X
                   WHEN 2
      *                South
                       ADD 1 TO ANT-Y
                   WHEN 3
      *                West
                       SUBTRACT 1 FROM ANT-X
               END-EVALUATE

      *        Wrap around edges (toroidal)
               IF ANT-X < 0
                   MOVE 63 TO ANT-X
               END-IF
               IF ANT-X > 63
                   MOVE 0 TO ANT-X
               END-IF
               IF ANT-Y < 0
                   MOVE 63 TO ANT-Y
               END-IF
               IF ANT-Y > 63
                   MOVE 0 TO ANT-Y
               END-IF

      *        Optional: Add delay to watch it run
      *        Comment out for full speed
               PERFORM VARYING DELAY-I FROM 1 BY 1
                   UNTIL DELAY-I > 100000
                   CONTINUE
               END-PERFORM

               ADD 1 TO STEPS
           END-PERFORM.

           STOP RUN.
