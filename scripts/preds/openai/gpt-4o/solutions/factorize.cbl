       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTORIZE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-VARIABLES.
           05 WS-N PIC S9(10) COMP-5.
           05 WS-I PIC S9(10) COMP-5 VALUE 2.
           05 WS-INDEX PIC 9(3) VALUE 1.

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

       MAIN-LOGIC SECTION.
           MOVE L-N TO WS-N
           PERFORM FACTORIZATION UNTIL WS-N = 1
           EXIT PROGRAM.

       FACTORIZATION SECTION.
           IF WS-N MOD WS-I = 0
               MOVE WS-I TO RESULT(WS-INDEX)
               ADD 1 TO WS-INDEX
               DIVIDE WS-I INTO WS-N
           ELSE
               ADD 1 TO WS-I
           END-IF.

       END PROGRAM FACTORIZE.
