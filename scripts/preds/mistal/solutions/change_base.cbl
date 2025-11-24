       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHANGE-BASE.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-X PIC S9(10) VALUE 0.
           05 WS-BASE PIC S9(10) VALUE 0.
           05 WS-REM PIC S9(10) VALUE 0.
           05 WS-QUOTIENT PIC S9(10) VALUE 0.
           05 WS-RESULT-LEN PIC 9(3) VALUE 0.
           05 WS-RESULT-STR PIC X(100) VALUE SPACES.
           05 WS-IDX PIC 9(3) VALUE 0.
           05 WS-TEMP-STR PIC X(100) VALUE SPACES.
           05 WS-CHAR PIC X VALUE SPACE.
           05 WS-NEGATIVE-FLAG PIC X VALUE 'N'.
               88 IS-NEGATIVE VALUE 'Y'.
               88 IS-POSITIVE VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X PIC S9(10).
           05 L-BASE PIC S9(10).
           05 RESULT PIC X(100).

      * Change numerical base of input number x to base.
      * return string representation after the conversion.
      * base numbers are less than 10.
      * >>> change_base(8, 3)
      * '22'
      * >>> change_base(8, 2)
      * '1000'
      * >>> change_base(7, 2)
      * '111'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-X TO WS-X
           MOVE L-BASE TO WS-BASE

           IF WS-X < 0 THEN
               SET IS-NEGATIVE TO TRUE
               COMPUTE WS-X = WS-X * -1
           ELSE
               SET IS-POSITIVE TO TRUE
           END-IF

           PERFORM UNTIL WS-X = 0
               DIVIDE WS-X BY WS-BASE GIVING WS-QUOTIENT
                   REMAINDER WS-REM
               MOVE WS-QUOTIENT TO WS-X

               ADD 1 TO WS-IDX
               MOVE WS-REM TO WS-CHAR
               STRING WS-CHAR DELIMITED BY SIZE
                   INTO WS-TEMP-STR
                   WITH POINTER WS-IDX
           END-PERFORM

           MOVE FUNCTION REVERSE(WS-TEMP-STR(1:WS-IDX)) TO WS-RESULT-STR

           IF IS-NEGATIVE THEN
               STRING "-" DELIMITED BY SIZE
                   WS-RESULT-STR(1:WS-IDX) DELIMITED BY SPACE
                   INTO RESULT
           ELSE
               MOVE WS-RESULT-STR(1:WS-IDX) TO RESULT
           END-IF

           GOBACK.
       END PROGRAM CHANGE-BASE.
