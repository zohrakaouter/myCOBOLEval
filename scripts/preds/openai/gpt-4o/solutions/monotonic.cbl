       IDENTIFICATION DIVISION.
       PROGRAM-ID. MONOTONIC.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 PREVIOUS-NUMBER PIC S9(10).
       01 CURRENT-NUMBER PIC S9(10).
       01 INCREASING PIC X VALUE 'T'.
       01 DECREASING PIC X VALUE 'T'.
       01 I PIC 9 VALUE 1.

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
           SET NI TO 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= 4
                IF I = 1 THEN
                    MOVE L-L(I) TO PREVIOUS-NUMBER
                ELSE
                    MOVE L-L(I) TO CURRENT-NUMBER
                    IF PREVIOUS-NUMBER < CURRENT-NUMBER THEN
                        MOVE 'F' TO DECREASING
                    ELSE IF PREVIOUS-NUMBER > CURRENT-NUMBER THEN
                        MOVE 'F' TO INCREASING
                    END-IF
                    MOVE CURRENT-NUMBER TO PREVIOUS-NUMBER
                END-IF
           END-PERFORM

           IF INCREASING = 'T' OR DECREASING = 'T' THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM MONOTONIC.
