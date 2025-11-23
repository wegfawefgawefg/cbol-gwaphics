       01  GFX-ARGS.
      * -----------------------------------------------------------
      * THE TAG (Opcode)
      * -----------------------------------------------------------
           05  GFX-OPCODE        PIC X(4).
               88 OP-DRAW-LINE   VALUE 'LINE'.
               88 OP-DRAW-RECT   VALUE 'RECT'.
               88 OP-DRAW-CIRC   VALUE 'CIRC'.
               88 OP-CLEAR       VALUE 'CLR '.
               88 OP-FILL        VALUE 'FILL'.

      * -----------------------------------------------------------
      * COMMON ATTRIBUTES (Color)
      * -----------------------------------------------------------
           05  GFX-COLOR-R       PIC 9(3).
           05  GFX-COLOR-G       PIC 9(3).
           05  GFX-COLOR-B       PIC 9(3).

      * -----------------------------------------------------------
      * THE UNION BUFFER (Allocated Size = 128 Bytes)
      * We reserve enough space here for the largest shape.
      * -----------------------------------------------------------
           05  GFX-PARAMS        PIC X(128).

      * -----------------------------------------------------------
      * OVERLAY 1: Line Arguments
      * -----------------------------------------------------------
           05  GFX-LINE-PARAMS   REDEFINES GFX-PARAMS.
               10  GFX-LINE-X1   PIC S9(4).
               10  GFX-LINE-Y1   PIC S9(4).
               10  GFX-LINE-X2   PIC S9(4).
               10  GFX-LINE-Y2   PIC S9(4).

      * -----------------------------------------------------------
      * OVERLAY 2: Rectangle Arguments
      * -----------------------------------------------------------
           05  GFX-RECT-PARAMS   REDEFINES GFX-PARAMS.
               10  GFX-RECT-X    PIC S9(4).
               10  GFX-RECT-Y    PIC S9(4).
               10  GFX-RECT-W    PIC 9(4).
               10  GFX-RECT-H    PIC 9(4).

      * -----------------------------------------------------------
      * OVERLAY 3: Circle Arguments
      * -----------------------------------------------------------
           05  GFX-CIRC-PARAMS   REDEFINES GFX-PARAMS.
               10  GFX-CIRC-CX   PIC S9(4).
               10  GFX-CIRC-CY   PIC S9(4).
               10  GFX-CIRC-R    PIC S9(4).
