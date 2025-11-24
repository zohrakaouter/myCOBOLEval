       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT-ARRAY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-WORK-AREAS.
           05 WS-I PIC S9(10) COMP-5.
           05 WS-J PIC S9(10) COMP-5.
           05 WS-TEMP PIC S9(10).
           05 WS-ONES-I PIC S9(10) COMP-5.
           05 WS-ONES-J PIC S9(10) COMP-5.
           05 WS-NUMBER PIC S9(10).
           05 WS-BINARY PIC X(32).
           05 WS-ONE-COUNT PIC S9(10) COMP-5.
           05 WS-K PIC S9(10) COMP-5.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * In this Kata, you have to sort an array of non-negative integers according to
      * number of ones in their binary representation in ascending order.
      * For similar number of ones, sort based on decimal value.
      * 
      * It must be implemented like this:
      * >>> sort_array([1, 5, 2, 3, 4]) == [1, 2, 3, 4, 5]
      * >>> sort_array([-2, -3, -4, -5, -6]) == [-6, -5, -4, -3, -2]
      * >>> sort_array([1, 0, 2, 3, 4]) [0, 1, 2, 3, 4]
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE L-ARR(WS-I) TO RESULT(WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > 5
                   COMPUTE WS-ONES-I = FUNCTION POPCOUNT(RESULT(WS-I))
                   COMPUTE WS-ONES-J = FUNCTION POPCOUNT(RESULT(WS-J))

                   IF WS-ONES-I > WS-ONES-J
                       MOVE RESULT(WS-I) TO WS-TEMP
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-TEMP TO RESULT(WS-J)
                   ELSE
                       IF WS-ONES-I = WS-ONES-J AND RESULT(WS-I) > RESULT(WS-J)
                           MOVE RESULT(WS-I) TO WS-TEMP
                           MOVE RESULT(WS-J) TO RESULT(WS-I)
                           MOVE WS-TEMP TO RESULT(WS-J)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM

           GOBACK.
       END PROGRAM SORT-ARRAY.
