       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-MAX-VALUE PIC S9(10) VALUE -1.
           05 WS-CURRENT-NUMBER PIC S9(10) VALUE 0.
           05 WS-FOUND PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * '''
      * You are given a non-empty list of positive integers. Return the greatest integer that is greater than
      * zero, and has a frequency greater than or equal to the value of the integer itself.
      * The frequency of an integer is the number of times it appears in the list.
      * If no such a value exist, return -1.
      * Examples:
      * search([4, 1, 2, 2, 1]) == 2
      * search([5, 5, 4, 4, 4]) == -1
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE L-LST(WS-I) TO WS-CURRENT-NUMBER
               MOVE 0 TO WS-COUNT

               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
                   IF L-LST(WS-J) = WS-CURRENT-NUMBER
                       ADD 1 TO WS-COUNT
                   END-IF
               END-PERFORM

               IF WS-CURRENT-NUMBER > 0 AND
                  WS-COUNT >= WS-CURRENT-NUMBER AND
                  WS-CURRENT-NUMBER > WS-MAX-VALUE
                   MOVE WS-CURRENT-NUMBER TO WS-MAX-VALUE
                   MOVE 1 TO WS-FOUND
               END-IF
           END-PERFORM

           IF WS-FOUND = 1
               MOVE WS-MAX-VALUE TO RESULT
           ELSE
               MOVE -1 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM SEARCH.
