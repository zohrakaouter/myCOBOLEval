       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-ODD-COLLATZ.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-NUM             PIC S9(10).
       01 SEQ-END-FLAG         PIC X VALUE 'N'.
       01 INDEX                PIC 9(10) COMP-3 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NI PIC S9(10).

      * 
      * Given a positive integer n, return a sorted list that has the odd numbers in collatz sequence.
      * 
      * The Collatz conjecture is a conjecture in mathematics that concerns a sequence defined
      * as follows: start with any positive integer n. Then each term is obtained from the
      * previous term as follows: if the previous term is even, the next term is one half of
      * the previous term. If the previous term is odd, the next term is 3 times the previous
      * term plus 1. The conjecture is that no matter what value of n, the sequence will always reach 1.
      * 
      * Note:
      * 1. Collatz(1) is [1].
      * 2. returned list sorted in increasing order.
      * 
      * For example:
      * get_odd_collatz(5) returns [1, 5] # The collatz sequence for 5 is [5, 16, 8, 4, 2, 1], so the odd numbers are only 1, and 5.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           MOVE L-N TO TEMP-NUM
           PERFORM UNTIL TEMP-NUM = 1 OR SEQ-END-FLAG = 'Y'
               IF TEMP-NUM MOD 2 NOT = 0 
                   THEN
                       SEARCH RESULT
                           WHEN RESULT(NI) = 0 
                               ADD 1 TO INDEX 
                               ADD 1 TO INDEX(RESULT) 
                               MOVE TEMP-NUM TO RESULT(INDEX)
                               DISPLAY 'HIT ODD: ' TEMP-NUM
                       END-SEARCH
                   END-IF
                   
               IF TEMP-NUM = 1 THEN 
                   SET SEQ-END-FLAG TO TRUE
               ELSE 
                   IF TEMP-NUM MOD 2 = 0 
                       THEN
                           DIVIDE TEMP-NUM BY 2 GIVING TEMP-NUM
                       ELSE
                           MULTIPLY TEMP-NUM BY 3
                           ADD 1 TO TEMP-NUM
                   END-IF
               END-IF
           END-PERFORM
           
           SORT RESULT ASCENDING KEY RESULT

           DISPLAY 'RESULT:'
           PERFORM VARYING NI FROM 1 BY 1 UNTIL RESULT(NI) = 0
               DISPLAY RESULT(NI)
           END-PERFORM

           GOBACK.

       END PROGRAM GET-ODD-COLLATZ.
