       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEST-PRIME-FACTOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 VARS.
           05 I PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * Return the largest prime factor of n. Assume n > 1 and is not a prime.
      * >>> largest_prime_factor(13195)
      * 29
      * >>> largest_prime_factor(2048)
      * 2
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       MAIN-PROCEDURE SECTION.
           MOVE 2 TO I
       
           PERFORM UNTIL L-N = 1
               IF L-N MOD I = 0
                   MOVE L-N / I TO L-N
                   MOVE I TO RESULT
               ELSE
                   ADD 1 TO I
               END-IF
           END-PERFORM
           
           GOBACK.

       END PROGRAM LARGEST-PRIME-FACTOR.
