       IDENTIFICATION DIVISION.
       PROGRAM-ID. LARGEST-DIVISOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I PIC S9(10) VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * For a given number n, find the largest number that divides n evenly, smaller than n
      * >>> largest_divisor(15)
      * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING I FROM L-N BY -1
               UNTIL I = 1
               IF L-N MOD I = 0
                   THEN 
                       MOVE I TO RESULT
                       EXIT PERFORM
               END-IF
           END-PERFORM
           .
           
           GOBACK.
       END PROGRAM LARGEST-DIVISOR.
