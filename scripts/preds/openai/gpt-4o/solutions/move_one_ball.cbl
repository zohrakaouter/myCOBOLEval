       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE-ONE-BALL.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 N PIC 9 VALUE ZERO.
       01 I PIC 9 VALUE ZERO.
       01 INVERSION-COUNT PIC 9 VALUE ZERO.

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
           IF L-ARR(1) = 0 THEN
               MOVE 1 TO RESULT
               GOBACK
           END-IF

           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI = 5
               ADD 1 TO N
           END-PERFORM

           SET NI TO 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I = N
               IF L-ARR(I) > L-ARR(I + 1)
                   ADD 1 TO INVERSION-COUNT
               END-IF
           END-PERFORM

           IF INVERSION-COUNT = 0 THEN
               MOVE 1 TO RESULT
           ELSE IF INVERSION-COUNT = 1 
                AND L-ARR(1) >= L-ARR(N) THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF

           GOBACK.

       END PROGRAM MOVE-ONE-BALL.
