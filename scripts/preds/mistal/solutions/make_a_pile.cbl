       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAKE-A-PILE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC 9(3) VALUE 0.
       01 WS-CURRENT-NUMBER PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given a positive integer n, you have to make a pile of n levels of stones.
      * The first level has n stones.
      * The number of stones in the next level is:
      * - the next odd number if n is odd.
      * - the next even number if n is even.
      * Return the number of stones in each level in a list, where element at index
      * i represents the number of stones in the level (i+1).
      * 
      * Examples:
      * >>> make_a_pile(3)
      * [3, 5, 7]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > L-N
               IF NI = 1
                   MOVE L-N TO RESULT(NI)
                   MOVE L-N TO WS-CURRENT-NUMBER
               ELSE
                   IF FUNCTION MOD(L-N 2) = 0
                       ADD 2 TO WS-CURRENT-NUMBER
                   ELSE
                       ADD 2 TO WS-CURRENT-NUMBER
                   END-IF
                   MOVE WS-CURRENT-NUMBER TO RESULT(NI)
               END-IF
           END-PERFORM.

           GOBACK.
       END PROGRAM MAKE-A-PILE.
