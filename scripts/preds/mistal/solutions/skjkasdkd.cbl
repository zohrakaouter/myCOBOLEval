       IDENTIFICATION DIVISION.
       PROGRAM-ID. SKJKASDKD.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-I PIC S9(10).
           05 WS-J PIC S9(10).
           05 WS-NUM PIC S9(10).
           05 WS-LARGEST-PRIME PIC S9(10) VALUE 0.
           05 WS-SUM-DIGITS PIC S9(10) VALUE 0.
           05 WS-REM PIC S9(10).
           05 WS-PRIME-FLAG PIC X VALUE 'Y'.
               88 IS-PRIME VALUE 'Y'.
               88 NOT-PRIME VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-LST OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * You are given a list of integers.
      * You need to find the largest prime value and return the sum of its digits.
      * 
      * Examples:
      * For lst = [0,81,12,3,1,21] the output should be 3
      * For lst = [0,8,1,2,1,7] the output should be 7
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE L-LST(WS-I) TO WS-NUM
               IF WS-NUM > 1
                   PERFORM CHECK-PRIME
                   IF IS-PRIME
                       IF WS-NUM > WS-LARGEST-PRIME
                           MOVE WS-NUM TO WS-LARGEST-PRIME
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF WS-LARGEST-PRIME > 0
               PERFORM SUM-DIGITS
               MOVE WS-SUM-DIGITS TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.

       CHECK-PRIME.
           MOVE 'Y' TO WS-PRIME-FLAG
           IF WS-NUM = 2
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION MOD(WS-NUM, 2) = 0
               MOVE 'N' TO WS-PRIME-FLAG
               EXIT PARAGRAPH
           END-IF
           PERFORM VARYING WS-J FROM 3 BY 2
                   UNTIL WS-J > FUNCTION INTEGER(FUNCTION SQRT(WS-NUM))
               IF FUNCTION MOD(WS-NUM, WS-J) = 0
                   MOVE 'N' TO WS-PRIME-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       SUM-DIGITS.
           MOVE 0 TO WS-SUM-DIGITS
           MOVE WS-LARGEST-PRIME TO WS-NUM
           PERFORM UNTIL WS-NUM = 0
               DIVIDE WS-NUM BY 10 GIVING WS-NUM REMAINDER WS-REM
               ADD WS-REM TO WS-SUM-DIGITS
           END-PERFORM.
