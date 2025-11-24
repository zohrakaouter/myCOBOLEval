       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIAN.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-I PIC S9(10).
           05 WS-J PIC S9(10).
           05 WS-TEMP PIC S9(10).
           05 WS-MEDIAN PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT COMP-2.

      * Return median of elements in the list l.
      * >>> median([3, 1, 2, 4, 5])
      * 3
      * >>> median([-10, 4, 6, 1000, 10])
      * 6
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > 5
                   IF L-L(WS-J) < L-L(WS-I)
                       MOVE L-L(WS-I) TO WS-TEMP
                       MOVE L-L(WS-J) TO L-L(WS-I)
                       MOVE WS-TEMP TO L-L(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM

           COMPUTE WS-MEDIAN = L-L(3)
           MOVE WS-MEDIAN TO RESULT

           GOBACK
           .
       END PROGRAM MEDIAN.
