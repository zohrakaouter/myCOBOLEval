       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNIQUE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-WORK-AREAS.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-K PIC S9(10) VALUE 0.
           05 WS-TEMP PIC S9(10) VALUE 0.
           05 WS-UNIQUE-COUNT PIC S9(10) VALUE 0.
           05 WS-FOUND-FLAG PIC X VALUE 'N'.
               88 FOUND VALUE 'Y'.
               88 NOT-FOUND VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 9 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Return sorted unique elements in a list
      * >>> unique([5, 3, 5, 2, 3, 3, 9, 0, 123])
      * [0, 2, 3, 5, 9, 123]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       MAIN-PARA.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               SET NI TO WS-I
               MOVE L-L(NI) TO WS-TEMP
               SET WS-J TO 0
               SET NOT-FOUND TO TRUE
               PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-UNIQUE-COUNT
                   SET NJ TO WS-K
                   IF WS-TEMP = RESULT(NJ)
                       SET FOUND TO TRUE
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF NOT-FOUND
                   ADD 1 TO WS-UNIQUE-COUNT
                   SET NJ TO WS-UNIQUE-COUNT
                   MOVE WS-TEMP TO RESULT(NJ)
               END-IF
           END-PERFORM

           PERFORM SORT-RESULT VARYING WS-I FROM 1 BY 1
               UNTIL WS-I >= WS-UNIQUE-COUNT

           GOBACK.

       SORT-RESULT.
           PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > WS-UNIQUE-COUNT
               SET NJ TO WS-J
               SET NI TO WS-J - 1
               IF RESULT(NJ) < RESULT(NI)
                   MOVE RESULT(NI) TO WS-TEMP
                   MOVE RESULT(NJ) TO RESULT(NI)
                   MOVE WS-TEMP TO RESULT(NJ)
               END-IF
           END-PERFORM.
       END PROGRAM UNIQUE.
