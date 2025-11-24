       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANY-INT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-TEMP-VALUES.
           05 WS-X PIC S9(9)V9(9) COMP-3.
           05 WS-Y PIC S9(9)V9(9) COMP-3.
           05 WS-Z PIC S9(9)V9(9) COMP-3.
           05 WS-SUM PIC S9(9)V9(9) COMP-3.
           05 WS-REM PIC S9(9)V9(9) COMP-3.
       01 WS-FLAGS.
           05 WS-ALL-INTEGERS PIC X VALUE 'Y'.
               88 ALL-INTEGERS VALUE 'Y'.
               88 NOT-ALL-INTEGERS VALUE 'N'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-X COMP-2.
           05 L-Y COMP-2.
           05 L-Z COMP-2.
           05 RESULT PIC 9.

      * '''
      * Create a function that takes 3 numbers.
      * Returns true if one of the numbers is equal to the sum of the other two, and all numbers are integers.
      * Returns false in any other cases.
      * 
      * Examples
      * any_int(5, 2, 7) ➞ True
      * 
      * any_int(3, 2, 2) ➞ False
      * 
      * any_int(3, -2, 1) ➞ True
      * 
      * any_int(3.6, -2.2, 2) ➞ False
      * 
      * 
      * 
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-X TO WS-X
           MOVE L-Y TO WS-Y
           MOVE L-Z TO WS-Z

           PERFORM CHECK-INTEGERS

           IF NOT-ALL-INTEGERS
               MOVE 0 TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-SUM = WS-X + WS-Y
           IF WS-SUM = WS-Z
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-SUM = WS-X + WS-Z
           IF WS-SUM = WS-Y
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           COMPUTE WS-SUM = WS-Y + WS-Z
           IF WS-SUM = WS-X
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           MOVE 0 TO RESULT
           GOBACK.

       CHECK-INTEGERS.
           COMPUTE WS-REM = FUNCTION MOD(WS-X 1)
           IF WS-REM NOT = 0
               MOVE 'N' TO WS-ALL-INTEGERS
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-REM = FUNCTION MOD(WS-Y 1)
           IF WS-REM NOT = 0
               MOVE 'N' TO WS-ALL-INTEGERS
               EXIT PARAGRAPH
           END-IF

           COMPUTE WS-REM = FUNCTION MOD(WS-Z 1)
           IF WS-REM NOT = 0
               MOVE 'N' TO WS-ALL-INTEGERS
               EXIT PARAGRAPH
           END-IF.

       END PROGRAM ANY-INT.
