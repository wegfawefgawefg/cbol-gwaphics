      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAPHICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * --- Output buffer for the pipe ---
       01  OUT-LINE      PIC X(40).

      * --- Variables for Bresenham Line Algorithm ---
       01  LINE-VARS.
           05  L-DX      PIC S9(4).
           05  L-DY      PIC S9(4).
           05  L-SX      PIC S9(4).
           05  L-SY      PIC S9(4).
           05  L-ERR     PIC S9(4).
           05  L-E2      PIC S9(4).
           05  L-X       PIC S9(4).
           05  L-Y       PIC S9(4).

      * --- Variables for Rect Loop ---
       01  RECT-VARS.
           05  R-CURR-X  PIC S9(4).
           05  R-CURR-Y  PIC S9(4).
           05  R-END-X   PIC S9(4).
           05  R-END-Y   PIC S9(4).

      * --- Variables for Circle Algorithm ---
       01  CIRC-VARS.
           05  C-X       PIC S9(4).
           05  C-Y       PIC S9(4).
           05  C-ERR     PIC S9(4).
           05  C-PLOT-X  PIC S9(4).
           05  C-PLOT-Y  PIC S9(4).

           COPY SETTINGS.

       01  MAX-X         PIC 9(4).
       01  MAX-Y         PIC 9(4).

       LINKAGE SECTION.
           COPY GFXARGS.

       PROCEDURE DIVISION USING GFX-ARGS.
           COMPUTE MAX-X = WIDTH - 1
           COMPUTE MAX-Y = HEIGHT - 1
           .

      * THE DISPATCHER SWITCH
           EVALUATE TRUE
              WHEN OP-DRAW-LINE
                   PERFORM DRAW-LINE-LOGIC
              WHEN OP-DRAW-RECT
                   PERFORM DRAW-RECT-LOGIC
              WHEN OP-DRAW-CIRC
                   PERFORM DRAW-CIRC-LOGIC
              WHEN OP-FILL
                   PERFORM VARYING L-Y FROM 0 BY 1 UNTIL L-Y > MAX-Y
                      PERFORM VARYING L-X FROM 0 BY 1 UNTIL L-X > MAX-X
                         PERFORM EMIT-PIXEL
                      END-PERFORM
                   END-PERFORM
              WHEN OP-CLEAR
                   MOVE 0 TO GFX-COLOR-R
                   MOVE 0 TO GFX-COLOR-G
                   MOVE 0 TO GFX-COLOR-B

                   PERFORM VARYING L-Y FROM 0 BY 1 UNTIL L-Y > MAX-Y
                      PERFORM VARYING L-X FROM 0 BY 1 UNTIL L-X > MAX-X
                         PERFORM EMIT-PIXEL
                      END-PERFORM
                   END-PERFORM
              WHEN OTHER
                   DISPLAY "UNKNOWN OPCODE: " GFX-OPCODE
           END-EVALUATE.

           GOBACK.

      * -----------------------------------------------------------
      * HELPER: Emit Pixel to stdout
      * Formats: "X Y R G B"
      * -----------------------------------------------------------
       EMIT-PIXEL.
           MOVE SPACES TO OUT-LINE
           STRING
               FUNCTION TRIM(L-X) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(L-Y) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(GFX-COLOR-R) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(GFX-COLOR-G) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(GFX-COLOR-B) DELIMITED BY SIZE
               INTO OUT-LINE
           END-STRING
           DISPLAY OUT-LINE.

      * -----------------------------------------------------------
      * LOGIC: Bresenham's Line Algorithm
      * -----------------------------------------------------------
       DRAW-LINE-LOGIC.
           COMPUTE L-DX = FUNCTION ABS(GFX-LINE-X2 - GFX-LINE-X1)
           COMPUTE L-DY = FUNCTION ABS(GFX-LINE-Y2 - GFX-LINE-Y1) * -1

           IF GFX-LINE-X1 < GFX-LINE-X2
              MOVE 1 TO L-SX
           ELSE
              MOVE -1 TO L-SX
           END-IF
           IF GFX-LINE-Y1 < GFX-LINE-Y2
              MOVE 1 TO L-SY
           ELSE
              MOVE -1 TO L-SY
           END-IF

           COMPUTE L-ERR = L-DX + L-DY
           MOVE GFX-LINE-X1 TO L-X
           MOVE GFX-LINE-Y1 TO L-Y

           PERFORM UNTIL 1 = 0
              PERFORM EMIT-PIXEL

              IF L-X = GFX-LINE-X2 AND L-Y = GFX-LINE-Y2
                 EXIT PERFORM
              END-IF

              COMPUTE L-E2 = 2 * L-ERR

              IF L-E2 >= L-DY
                 ADD L-DY TO L-ERR
                 ADD L-SX TO L-X
              END-IF

              IF L-E2 <= L-DX
                 ADD L-DX TO L-ERR
                 ADD L-SY TO L-Y
              END-IF
           END-PERFORM.

      * -----------------------------------------------------------
      * LOGIC: Simple Rectangle Loop
      * -----------------------------------------------------------
       DRAW-RECT-LOGIC.
           COMPUTE R-END-X = GFX-RECT-X + GFX-RECT-W - 1
           COMPUTE R-END-Y = GFX-RECT-Y + GFX-RECT-H - 1

           PERFORM VARYING R-CURR-Y FROM GFX-RECT-Y BY 1
                   UNTIL R-CURR-Y > R-END-Y
              PERFORM VARYING R-CURR-X FROM GFX-RECT-X BY 1
                      UNTIL R-CURR-X > R-END-X

                 MOVE R-CURR-X TO L-X
                 MOVE R-CURR-Y TO L-Y
                 PERFORM EMIT-PIXEL
              END-PERFORM
           END-PERFORM.

      * -----------------------------------------------------------
      * LOGIC: Midpoint Circle Algorithm
      * -----------------------------------------------------------
       DRAW-CIRC-LOGIC.
           MOVE GFX-CIRC-R TO C-X
           MOVE 0 TO C-Y
           COMPUTE C-ERR = 1 - C-X

           PERFORM UNTIL C-X < C-Y
              PERFORM PLOT-CIRCLE-POINTS
              ADD 1 TO C-Y
              IF C-ERR <= 0
                 COMPUTE C-ERR = C-ERR + (2 * C-Y) + 1
              ELSE
                 SUBTRACT 1 FROM C-X
                 COMPUTE C-ERR = C-ERR + (2 * C-Y) - (2 * C-X) + 1
              END-IF
           END-PERFORM.

       PLOT-CIRCLE-POINTS.
      * Symmetry: Plot all 8 octants
           COMPUTE L-X = GFX-CIRC-CX + C-X
           COMPUTE L-Y = GFX-CIRC-CY + C-Y
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX + C-X
           COMPUTE L-Y = GFX-CIRC-CY - C-Y
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX - C-X
           COMPUTE L-Y = GFX-CIRC-CY + C-Y
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX - C-X
           COMPUTE L-Y = GFX-CIRC-CY - C-Y
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX + C-Y
           COMPUTE L-Y = GFX-CIRC-CY + C-X
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX + C-Y
           COMPUTE L-Y = GFX-CIRC-CY - C-X
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX - C-Y
           COMPUTE L-Y = GFX-CIRC-CY + C-X
           PERFORM EMIT-PIXEL

           COMPUTE L-X = GFX-CIRC-CX - C-Y
           COMPUTE L-Y = GFX-CIRC-CY - C-X
           PERFORM EMIT-PIXEL.
