       IDENTIFICATION DIVISION.
       PROGRAM-ID. GET-ODD-COLLATZ.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-N PIC S9(10) VALUE 0.
           05 WS-COUNT PIC S9(4) VALUE 0.
           05 WS-INDEX PIC S9(4) VALUE 0.
           05 WS-TEMP PIC S9(10) VALUE 0.
           05 WS-I PIC S9(4) VALUE 0.
           05 WS-J PIC S9(4) VALUE 0.
           05 WS-SWAP PIC S9(10) VALUE 0.

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
           MOVE L-N TO WS-N.

           PERFORM UNTIL WS-N = 1
               IF FUNCTION MOD(WS-N, 2) = 1
                   ADD 1 TO WS-COUNT
                   MOVE WS-N TO RESULT(WS-COUNT)
               END-IF
               IF FUNCTION MOD(WS-N, 2) = 0
                   DIVIDE WS-N BY 2 GIVING WS-N
               ELSE
                   COMPUTE WS-N = WS-N * 3 + 1
               END-IF
           END-PERFORM.

           ADD 1 TO WS-COUNT.
           MOVE 1 TO RESULT(WS-COUNT).

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I >= WS-COUNT
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J >= WS-COUNT
                   IF RESULT(WS-J) < RESULT(WS-I)
                       MOVE RESULT(WS-I) TO WS-SWAP
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-SWAP TO RESULT(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

           GOBACK.
       END PROGRAM GET-ODD-COLLATZ.
