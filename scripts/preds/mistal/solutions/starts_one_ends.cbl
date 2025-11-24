       IDENTIFICATION DIVISION.
       PROGRAM-ID. STARTS-ONE-ENDS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER-OF-NUMBERS PIC 9(10).
           05 WS-FIRST-DIGIT-NUMBERS PIC 9(10).
           05 WS-LAST-DIGIT-NUMBERS PIC 9(10).
           05 WS-BOTH-DIGIT-NUMBERS PIC 9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given a positive integer n, return the count of the numbers of n-digit
      * positive integers that start or end with 1.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           COMPUTE WS-NUMBER-OF-NUMBERS = 9 * 10 ** (L-N - 1)
           COMPUTE WS-FIRST-DIGIT-NUMBERS = 1 * 10 ** (L-N - 1)
           COMPUTE WS-LAST-DIGIT-NUMBERS = 9 ** (L-N - 1)
           COMPUTE WS-BOTH-DIGIT-NUMBERS = 1 * 10 ** (L-N - 2)

           COMPUTE RESULT = WS-FIRST-DIGIT-NUMBERS + WS-LAST-DIGIT-NUMBERS - WS-BOTH-DIGIT-NUMBERS

           GOBACK.
       END PROGRAM STARTS-ONE-ENDS.
