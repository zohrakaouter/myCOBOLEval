       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAR-RACE-COLLISION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I PIC S9(10) COMP-5 VALUE 0.
       01 L-COUNTER PIC S9(10) COMP-5 VALUE 0.
       01 R-COUNTER PIC S9(10) COMP-5 VALUE 0.
       01 TOTAL-COLLISIONS PIC S9(10) COMP-5 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-N PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Imagine a road that's a perfectly straight infinitely long line.
      * n cars are driving left to right;  simultaneously, a different set of n cars
      * are driving right to left.   The two sets of cars start out being very far from
      * each other.  All cars move in the same speed.  Two cars are said to collide
      * when a car that's moving left to right hits a car that's moving right to left.
      * However, the cars are infinitely sturdy and strong; as a result, they continue moving
      * in their trajectory as if they did not collide.
      * 
      * This function outputs the number of such collisions.
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I GREATER THAN L-N
               ADD 1 TO L-COUNTER
               MODIFY TOTAL-COLLISIONS USING L-COUNTER R-COUNTER
           END-PERFORM.

           MOVE TOTAL-COLLISIONS TO RESULT.

           GOBACK.

       MODIFY.
           ADD L-COUNTER TO R-COUNTER
           SUBTRACT 1 FROM L-COUNTER
           ADD L-COUNTER TO TOTAL-COLLISIONS.

       END PROGRAM CAR-RACE-COLLISION.
