       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ITEM-FREQUENCIES.
           05 WS-ITEM-VALUE         PIC S9(10) OCCURS 5 TIMES.
           05 WS-ITEM-COUNT         PIC S9(10) OCCURS 5 TIMES.

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
           
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               ADD +1 TO WS-ITEM-COUNT (L-LST(NI))
               MOVE L-LST(NI) TO WS-ITEM-VALUE (NI)
           END-PERFORM.

           MOVE ZERO TO RESULT.

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               IF WS-ITEM-COUNT (NI) >= 1
                   IF WS-ITEM-COUNT (NI) >= WS-ITEM-VALUE (NI)
                       IF WS-ITEM-VALUE (NI) > RESULT THEN
                           MOVE WS-ITEM-VALUE (NI) TO RESULT
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

           IF RESULT = 0 THEN
               MOVE -1 TO RESULT
           END-IF
            
           GOBACK.
           
       END PROGRAM SEARCH.
