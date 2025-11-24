       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTERSECTION.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  INTERVAL1-START  PIC S9(10). 
       01  INTERVAL1-END    PIC S9(10).
       01  INTERVAL2-START  PIC S9(10).
       01  INTERVAL2-END    PIC S9(10).

       01  INTERSECTION-START  PIC S9(10).
       01  INTERSECTION-END    PIC S9(10).
       01  LENGTH-OF-INTERSECTION PIC S9(10).

       01  IS-PRIME      PIC X VALUE 'Y'.
       01  I             PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-INTERVAL1 OCCURS 100 TIMES INDEXED BY NI PIC S9(10).
           05 L-INTERVAL2 OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).
           05 RESULT PIC X(100).

      * You are given two intervals,
      * where each interval is a pair of integers. For example, interval = (start, end) = (1, 2).
      * The given intervals are closed which means that the interval (start, end)
      * includes both start and end.
      * For each given interval, it is assumed that its start is less or equal its end.
      * Your task is to determine whether the length of intersection of these two
      * intervals is a prime number.
      * Example, the intersection of the intervals (1, 3), (2, 4) is (2, 3)
      * which its length is 1, which not a prime number.
      * If the length of the intersection is a prime number, return "YES",
      * otherwise, return "NO".
      * If the two intervals don't intersect, return "NO".
      * 
      * 
      * [input/output] samples:
      * intersection((1, 2), (2, 3)) ==> "NO"
      * intersection((-1, 1), (0, 4)) ==> "NO"
      * intersection((-3, -1), (-5, 5)) ==> "YES"
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           SET NI TO 1
           SET NJ TO 1
           
           PERFORM UNTIL L-INTERVAL1(NI) EQUAL 0
               MOVE L-INTERVAL1(NI) TO INTERVAL1-START
               MOVE L-INTERVAL1(NI + 1) TO INTERVAL1-END
               MOVE L-INTERVAL2(NJ) TO INTERVAL2-START
               MOVE L-INTERVAL2(NJ + 1) TO INTERVAL2-END
               
               IF INTERVAL1-END < INTERVAL2-START OR INTERVAL2-END < INTERVAL1-START
                   MOVE "NO" TO RESULT(NI)
               ELSE
                   COMPUTE INTERSECTION-START = FUNCTION MAX(INTERVAL1-START, INTERVAL2-START)
                   COMPUTE INTERSECTION-END = FUNCTION MIN(INTERVAL1-END, INTERVAL2-END)
                   COMPUTE LENGTH-OF-INTERSECTION = INTERSECTION-END - INTERSECTION-START + 1
                   
                   IF LENGTH-OF-INTERSECTION < 2
                       MOVE "NO" TO RESULT(NI)
                   ELSE
                       INITIALIZE IS-PRIME
                       PERFORM VARYING I FROM 2 BY 1 UNTIL I * I > LENGTH-OF-INTERSECTION
                           IF LENGTH-OF-INTERSECTION REM I = 0
                               MOVE "N" TO IS-PRIME
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                       
                       IF IS-PRIME = "Y"
                           MOVE "YES" TO RESULT(NI)
                       ELSE
                           MOVE "NO" TO RESULT(NI)
                       END-IF
                   END-IF
               END-IF
               
               SET NI UP BY 2
               SET NJ UP BY 2
           END-PERFORM.

           GOBACK.
       END PROGRAM INTERSECTION.
