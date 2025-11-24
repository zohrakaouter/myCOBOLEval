       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAXIMUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       

       01 WS-VARIABLES.
           05 WS-I PIC S9(10) VALUE 0.
           05 WS-J PIC S9(10) VALUE 0.
           05 WS-TEMP PIC S9(10) VALUE 0.
           05 WS-COUNT PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 L-K PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * Given an array arr of integers and a positive integer k, return a sorted list
      * of length k with the maximum k numbers in arr.
      * 
      * Example 1:
      * 
      * Input: arr = [-3, -4, 5], k = 3
      * Output: [-4, -3, 5]
      * 
      * Example 2:
      * 
      * Input: arr = [4, -4, 4], k = 2
      * Output: [4, 4]
      * 
      * 
      * Note:
      * 1. The length of the array will be 3
      * 2. The elements in the array will be in the range of [-1000, 1000].
      * 3. 0 <= k <= len(arr)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE L-ARR(WS-I) TO RESULT(WS-I)
           END-PERFORM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > 3
                   IF RESULT(WS-J) > RESULT(WS-I)
                       MOVE RESULT(WS-I) TO WS-TEMP
                       MOVE RESULT(WS-J) TO RESULT(WS-I)
                       MOVE WS-TEMP TO RESULT(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM

           IF L-K < 3
               MOVE 0 TO WS-COUNT
               PERFORM VARYING WS-I FROM L-K BY 1 UNTIL WS-I > 3
                   MOVE 0 TO RESULT(WS-I)
                   ADD 1 TO WS-COUNT
               END-PERFORM
           END-IF

           GOBACK.
       END PROGRAM MAXIMUM.
