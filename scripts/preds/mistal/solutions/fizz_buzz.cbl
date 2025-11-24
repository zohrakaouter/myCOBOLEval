       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZ-BUZZ.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC S9(10).
       01 TEMP-NUM PIC S9(10).
       01 DIGIT-COUNT PIC S9(10) VALUE 0.
       01 DIGIT PIC 9.
       01 REMAINDER PIC S9(10).

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
           PERFORM VARYING I FROM 1 BY 1 UNTIL I >= L-N
               IF FUNCTION MOD(I, 11) = 0 OR FUNCTION MOD(I, 13) = 0
                   MOVE I TO TEMP-NUM
                   PERFORM COUNT-DIGITS
               END-IF
           END-PERFORM
           MOVE DIGIT-COUNT TO RESULT
           GOBACK.

       COUNT-DIGITS.
           PERFORM UNTIL TEMP-NUM = 0
               DIVIDE TEMP-NUM BY 10 GIVING TEMP-NUM REMAINDER DIGIT
               IF DIGIT = 7
                   ADD 1 TO DIGIT-COUNT
               END-IF
           END-PERFORM.
