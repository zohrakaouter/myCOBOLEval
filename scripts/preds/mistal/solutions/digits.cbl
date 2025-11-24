       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGITS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10).
           05 WS-REM PIC 9(1).
           05 WS-PRODUCT PIC S9(10) VALUE 1.
           05 WS-ALL-EVEN PIC 9 VALUE 1.
           05 WS-TEMP-NUM PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * Given a positive integer n, return the product of the odd digits.
      * Return 0 if all digits are even.
      * For example:
      * digits(1)  == 1
      * digits(4)  == 0
      * digits(235) == 15
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-N TO WS-NUMBER
           MOVE WS-NUMBER TO WS-TEMP-NUM

           PERFORM UNTIL WS-TEMP-NUM = 0
               DIVIDE WS-TEMP-NUM BY 10 GIVING WS-TEMP-NUM
                   REMAINDER WS-REM
               IF FUNCTION MOD(WS-REM, 2) NOT = 0
                   MULTIPLY WS-PRODUCT BY WS-REM GIVING WS-PRODUCT
                   MOVE 0 TO WS-ALL-EVEN
               END-IF
           END-PERFORM

           IF WS-ALL-EVEN = 1
               MOVE 0 TO RESULT
           ELSE
               MOVE WS-PRODUCT TO RESULT
           END-IF

           GOBACK.
       END PROGRAM DIGITS.
