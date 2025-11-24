       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGITS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ODD-PRODUCT PIC S9(10) VALUE 1.
       01 WS-CURR-DIGIT   PIC 9(1).
       01 WS-N-TEXT       PIC X(10).
       01 WS-N-LEN        PIC 9(2).
       01 WS-I            PIC 9(2).

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
           MOVE L-N TO WS-N-TEXT
           UNSTRING WS-N-TEXT DELIMITED BY SPACE INTO WS-N-TEXT
           COMPUTE WS-N-LEN = FUNCTION LENGTH(WS-N-TEXT)

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N-LEN
               MOVE FUNCTION NUMVAL(FUNCTION INSPECT WS-N-TEXT(WS-I:1) 
                   CONVERTING "0123456789" TO "0123456789") TO WS-CURR-DIGIT

               IF FUNCTION MOD(WS-CURR-DIGIT 2) NOT EQUAL TO 0
                   MULTIPLY WS-ODD-PRODUCT BY WS-CURR-DIGIT
               END-IF
           END-PERFORM

           IF WS-ODD-PRODUCT = 1 AND L-N /= 1
               MOVE 0 TO RESULT
           ELSE
               MOVE WS-ODD-PRODUCT TO RESULT
           END-IF

           GOBACK.
       END PROGRAM DIGITS.
