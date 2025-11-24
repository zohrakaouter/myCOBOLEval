       IDENTIFICATION DIVISION.
       PROGRAM-ID. MONOTONIC.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-FLAGS.
           05 WS-INCREASING PIC X VALUE 'Y'.
               88 INCREASING VALUE 'Y'.
               88 NOT-INCREASING VALUE 'N'.
           05 WS-DECREASING PIC X VALUE 'Y'.
               88 DECREASING VALUE 'Y'.
               88 NOT-DECREASING VALUE 'N'.
           05 WS-MONOTONIC PIC X VALUE 'Y'.
               88 MONOTONIC VALUE 'Y'.
               88 NOT-MONOTONIC VALUE 'N'.
       01 I PIC 99 VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 4 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * Return True is list elements are monotonically increasing or decreasing.
      * >>> monotonic([1, 2, 4, 20])
      * True
      * >>> monotonic([1, 20, 4, 10])
      * False
      * >>> monotonic([4, 1, 0, -10])
      * True
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = 4 OR NOT MONOTONIC
               IF I < 4
                   IF L-L(I) < L-L(I + 1)
                       SET NOT-DECREASING TO TRUE
                   END-IF
                   IF L-L(I) > L-L(I + 1)
                       SET NOT-INCREASING TO TRUE
                   END-IF
                   IF NOT INCREASING AND NOT DECREASING
                       SET NOT-MONOTONIC TO TRUE
                   END-IF
               END-IF
           END-PERFORM

           IF MONOTONIC
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM MONOTONIC.
