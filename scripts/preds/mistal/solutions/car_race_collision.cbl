       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAR-RACE-COLLISION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-COUNT           PIC S9(10) VALUE 0.
       01 WS-I               PIC S9(10).
       01 WS-J               PIC S9(10).
       01 WS-TEMP            PIC S9(10).

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
           MOVE 0 TO RESULT.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > L-N
               PERFORM VARYING WS-J FROM WS-I + 1 BY 1 UNTIL WS-J > L-N
                   COMPUTE WS-TEMP = (WS-I - 1) * (L-N - WS-J + 1)
                   ADD WS-TEMP TO RESULT
               END-PERFORM
           END-PERFORM.

           GOBACK.
       END PROGRAM CAR-RACE-COLLISION.
