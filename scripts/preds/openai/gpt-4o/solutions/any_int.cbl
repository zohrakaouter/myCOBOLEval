       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANY-INT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-X PIC 9V9 VALUE 0.
       01 WS-Y PIC 9V9 VALUE 0.
       01 WS-Z PIC 9V9 VALUE 0.

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

           IF FUNCTION INTEGER(WS-X) = WS-X AND
              FUNCTION INTEGER(WS-Y) = WS-Y AND
              FUNCTION INTEGER(WS-Z) = WS-Z
              
              IF WS-X = WS-Y + WS-Z OR
                 WS-Y = WS-X + WS-Z OR
                 WS-Z = WS-X + WS-Y
                 
                 MOVE 1 TO RESULT
              ELSE
                 MOVE 0 TO RESULT
              END-IF
           ELSE
              MOVE 0 TO RESULT
           END-IF

           GOBACK.

       END PROGRAM ANY-INT.
