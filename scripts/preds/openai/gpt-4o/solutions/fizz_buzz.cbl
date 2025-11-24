       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZ-BUZZ.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 I PIC S9(10) VALUE 0.
       77 J PIC S9(10) VALUE 0.
       77 CURRENT-NUMBER PIC S9(10).
       77 DIGIT-FOUND PIC 9 VALUE 7.
       77 REMAINDER PIC 9(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * Return the number of times the digit 7 appears in integers less than n which are divisible by 11 or 13.
      * >>> fizz_buzz(50)
      * 0
      * >>> fizz_buzz(78)
      * 2
      * >>> fizz_buzz(79)
      * 3
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
       PERFORM VARYING CURRENT-NUMBER FROM 1 BY 1 UNTIL CURRENT-NUMBER >= L-N
           IF CURRENT-NUMBER REM 11 = 0 OR CURRENT-NUMBER REM 13 = 0 THEN
               MOVE CURRENT-NUMBER TO REMAINDER
               PERFORM CHECK-DIGITS
           END-IF
       END-PERFORM
       
       MOVE J TO RESULT
       GOBACK.

       CHECK-DIGITS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL REMAINDER = 0
               IF REMAINDER REM 10 = DIGIT-FOUND THEN
                   ADD 1 TO J
               END-IF
               DIVIDE REMAINDER BY 10 GIVING REMAINDER
           END-PERFORM.

       END PROGRAM FIZZ-BUZZ.
