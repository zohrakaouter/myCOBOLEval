       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-ONE-BALL.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-I PIC S9(10).
           05 WS-J PIC S9(10).
           05 WS-COUNT PIC S9(10) VALUE 0.
           05 WS-SORTED-FLAG PIC X VALUE 'Y'.
           05 WS-TEMP-ARR OCCURS 5 TIMES PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 5 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC 9.

      * We have an array 'arr' of N integers arr[1], arr[2], ..., arr[N].The
      * numbers in the array will be randomly ordered. Your task is to determine if
      * it is possible to get an array sorted in non-decreasing order by performing
      * the following operation on the given array:
      * You are allowed to perform right shift operation any number of times.
      * 
      * One right shift operation means shifting all elements of the array by one
      * position in the right direction. The last element of the array will be moved to
      * the starting position in the array i.e. 0th index.
      * 
      * If it is possible to obtain the sorted array by performing the above operation
      * then return True else return False.
      * If the given array is empty then return True.
      * 
      * Note: The given list is guaranteed to have unique elements.
      * 
      * For Example:
      * 
      * move_one_ball([3, 4, 5, 1, 2])==>True
      * Explanation: By performin 2 right shift operations, non-decreasing order can
      * be achieved for the given array.
      * move_one_ball([3, 5, 4, 1, 2])==>False
      * Explanation:It is not possible to get non-decreasing order for the given
      * array by performing any number of right shift operations.
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           IF FUNCTION NUMVAL(NI) = 0
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE L-ARR(WS-I) TO WS-TEMP-ARR(WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > 4
                   IF WS-TEMP-ARR(WS-J) > WS-TEMP-ARR(WS-J + 1)
                       MOVE 'N' TO WS-SORTED-FLAG
                   END-IF
               END-PERFORM

               IF WS-SORTED-FLAG = 'Y'
                   MOVE 1 TO RESULT
                   GOBACK
               END-IF

               MOVE 'Y' TO WS-SORTED-FLAG

               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
                   IF WS-J = 5
                       MOVE WS-TEMP-ARR(1) TO WS-TEMP-ARR(WS-J)
                   ELSE
                       MOVE WS-TEMP-ARR(WS-J + 1) TO WS-TEMP-ARR(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE 0 TO RESULT.

       GOBACK.
       END PROGRAM MOVE-ONE-BALL.
