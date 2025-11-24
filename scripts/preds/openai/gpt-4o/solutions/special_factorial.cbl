       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIAL-FACTORIAL.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I PIC S9(10).
       01 J PIC S9(10).
       01 TEMP-RESULT PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * The Brazilian factorial is defined as:
      * brazilian_factorial(n) = n! * (n-1)! * (n-2)! * ... * 1!
      * where n > 0
      * 
      * For example:
      * >>> special_factorial(4)
      * 288
      * 
      * The function will receive an integer as input and should return the special
      * factorial of this integer.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 1 TO RESULT.

           PERFORM VARYING I FROM L-N BY -1 UNTIL I < 1
               MOVE 1 TO TEMP-RESULT
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > I
                   MULTIPLY J BY TEMP-RESULT GIVING TEMP-RESULT
               END-PERFORM
               MULTIPLY TEMP-RESULT BY RESULT GIVING RESULT
           END-PERFORM

           GOBACK.
       END PROGRAM SPECIAL-FACTORIAL.
