       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTORIZE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10).
           05 WS-DIVISOR PIC S9(5) VALUE 2.
           05 WS-INDEX PIC 9(3) VALUE 0.
           05 WS-TEMP PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Return list of prime factors of given integer in the order from smallest to largest.
      * Each of the factors should be listed number of times corresponding to how many times it appeares in factorization.
      * Input number should be equal to the product of all factors
      * >>> factorize(8)
      * [2, 2, 2]
      * >>> factorize(25)
      * [5, 5]
      * >>> factorize(70)
      * [2, 5, 7]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-N TO WS-NUMBER
           MOVE 0 TO WS-INDEX

           PERFORM UNTIL WS-NUMBER = 1
               DIVIDE WS-NUMBER BY WS-DIVISOR GIVING WS-TEMP
               REMAINDER WS-NUMBER

               IF WS-NUMBER = 0
                   MOVE WS-DIVISOR TO RESULT(NI)
                   ADD 1 TO WS-INDEX
                   MOVE WS-TEMP TO WS-NUMBER
               ELSE
                   ADD 1 TO WS-DIVISOR
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM FACTORIZE.
