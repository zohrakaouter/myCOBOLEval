       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-EQUAL-TO-SUM-EVEN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 IS-SUM-OF-EVENS PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC 9.

      * Evaluate whether the given number n can be written as the sum of exactly 4 positive even numbers
      * Example
      * is_equal_to_sum_even(4) == False
      * is_equal_to_sum_even(6) == False
      * is_equal_to_sum_even(8) == True
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM IS-SUM-EVEN-CHECK
           MOVE IS-SUM-OF-EVENS TO RESULT
           GOBACK.

       IS-SUM-EVEN-CHECK SECTION.
           IF L-N < 8
               MOVE 0 TO IS-SUM-OF-EVENS
           ELSE
               MOVE 1 TO IS-SUM-OF-EVENS
           END-IF.
       END PROGRAM IS-EQUAL-TO-SUM-EVEN.
