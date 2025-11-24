       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVEN-ODD-COUNT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-NUMBER PIC S9(10).
           05 WS-ABS-NUMBER PIC 9(10).
           05 WS-DIGIT PIC 9.
           05 WS-EVEN-COUNT PIC 9(3) VALUE 0.
           05 WS-ODD-COUNT PIC 9(3) VALUE 0.
           05 WS-REM PIC 9(10).
           05 WS-QUOTIENT PIC 9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUM PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * Given an integer. return a list that has the number of even and odd digits respectively.
      * 
      * Example:
      * even_odd_count(-12) ==> [1, 1]
      * even_odd_count(123) ==> [1, 2]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-NUM TO WS-NUMBER
           COMPUTE WS-ABS-NUMBER = FUNCTION ABS(WS-NUMBER)

           PERFORM UNTIL WS-ABS-NUMBER = 0
               DIVIDE WS-ABS-NUMBER BY 10
                   GIVING WS-QUOTIENT
                   REMAINDER WS-REM
               END-DIVIDE
               MOVE WS-REM TO WS-DIGIT
               IF FUNCTION MOD(WS-DIGIT, 2) = 0
                   ADD 1 TO WS-EVEN-COUNT
               ELSE
                   ADD 1 TO WS-ODD-COUNT
               END-IF
               MOVE WS-QUOTIENT TO WS-ABS-NUMBER
           END-PERFORM

           MOVE WS-EVEN-COUNT TO RESULT(1)
           MOVE WS-ODD-COUNT TO RESULT(2)

           GOBACK.
       END PROGRAM EVEN-ODD-COUNT.
