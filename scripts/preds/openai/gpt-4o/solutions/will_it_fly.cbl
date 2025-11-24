       IDENTIFICATION DIVISION.
       PROGRAM-ID. WILL-IT-FLY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ELEMENT-SUM PIC S9(10) VALUE 0.
       01 WS-LENGTH-Q   PIC S9(10).

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
           SET RESULT TO 0
           SET NI TO 1
           INITIALIZE WS-ELEMENT-SUM
       
       * Calculate Length of Q
           PERFORM VARYING NI FROM 1 BY 1 UNTIL L-Q(NI) = LOW-VALUE
               SET WS-LENGTH-Q UP BY 1
           END-PERFORM
        
       * Check if Q is palindromic
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > WS-LENGTH-Q / 2
               IF L-Q(NI) NOT = L-Q(WS-LENGTH-Q - NI + 1)
                   GOBACK
               END-IF
           END-PERFORM
       
       * Compute sum of elements
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > WS-LENGTH-Q
               ADD L-Q(NI) TO WS-ELEMENT-SUM
           END-PERFORM
       
       * Check for weight condition
           IF WS-ELEMENT-SUM <= L-W
               SET RESULT TO 1
           END-IF

           GOBACK.

       END PROGRAM WILL-IT-FLY.
