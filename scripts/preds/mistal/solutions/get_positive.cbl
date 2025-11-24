       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-POSITIVE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC 9(3) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * Return only positive numbers in the list.
      * >>> get_positive([-1, 2, -4, 5, 6])
      * [2, 5, 6]
      * >>> get_positive([5, 3, -5, 2, -3])
      * [5, 3, 2]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > 5
               IF L-L(NI) > 0
                   ADD 1 TO WS-COUNTER
                   MOVE L-L(NI) TO RESULT(WS-COUNTER)
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM GET-POSITIVE.
