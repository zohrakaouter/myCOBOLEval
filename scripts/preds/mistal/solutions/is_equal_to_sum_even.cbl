       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-EQUAL-TO-SUM-EVEN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNTER PIC 9(3) VALUE 0.
       01 WS-TEMP-NUM PIC 9(10).
       01 WS-REM PIC 9(10).

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
           MOVE 0 TO RESULT

           IF L-N <= 0 OR L-N < 8
               GOBACK
           END-IF

           COMPUTE WS-REM = FUNCTION MOD(L-N, 8)
           IF WS-REM = 0
               MOVE 1 TO RESULT
           END-IF

           GOBACK
           .
       END PROGRAM IS-EQUAL-TO-SUM-EVEN.
