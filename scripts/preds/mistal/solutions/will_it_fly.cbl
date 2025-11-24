       IDENTIFICATION DIVISION.
       PROGRAM-ID. WILL-IT-FLY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-SUM PIC S9(10) VALUE 0.
           05 WS-IS-BALANCED PIC X(3) VALUE 'YES'.
           05 WS-I PIC 9(2).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-Q OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 L-W PIC S9(10).
           05 RESULT PIC 9.

      * '''
      * Write a function that returns True if the object q will fly, and False otherwise.
      * The object q will fly if it's balanced (it is a palindromic list) and the sum of its elements is less than or equal the maximum possible weight w.
      * 
      * Example:
      * will_it_fly([1, 2], 5) ➞ False
      * # 1+2 is less than the maximum possible weight, but it's unbalanced.
      * 
      * will_it_fly([3, 2, 3], 1) ➞ False
      * # it's balanced, but 3+2+3 is more than the maximum possible weight.
      * 
      * will_it_fly([3, 2, 3], 9) ➞ True
      * # 3+2+3 is less than the maximum possible weight, and it's balanced.
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               ADD L-Q(WS-I) TO WS-SUM
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               IF L-Q(WS-I) NOT EQUAL TO L-Q(4 - WS-I)
                   MOVE 'NO' TO WS-IS-BALANCED
               END-IF
           END-PERFORM

           IF WS-SUM <= L-W AND WS-IS-BALANCED = 'YES'
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM WILL-IT-FLY.
