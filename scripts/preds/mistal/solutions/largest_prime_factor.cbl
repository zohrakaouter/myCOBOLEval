       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEST-PRIME-FACTOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC S9(10).
           05 WS-LARGEST-FACTOR PIC S9(10) VALUE 0.
           05 WS-REMAINDER PIC S9(10).

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
           MOVE L-N TO WS-I
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > L-N
               DIVIDE L-N BY WS-I GIVING WS-REMAINDER REMAINDER WS-REMAINDER
               IF WS-REMAINDER = 0
                   MOVE WS-I TO WS-LARGEST-FACTOR
                   DIVIDE L-N BY WS-I GIVING L-N
                   MOVE WS-I TO WS-I
               END-IF
           END-PERFORM

           MOVE WS-LARGEST-FACTOR TO RESULT
           GOBACK.
       END PROGRAM LARGEST-PRIME-FACTOR.
