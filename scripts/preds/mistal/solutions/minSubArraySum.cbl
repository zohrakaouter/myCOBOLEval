       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINSUBARRAYSUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-MIN-SUM PIC S9(10) VALUE 0.
           05 WS-CURRENT-SUM PIC S9(10) VALUE 0.
           05 WS-I PIC 9(5) VALUE 0.
           05 WS-J PIC 9(5) VALUE 0.
           05 WS-ARRAY-SIZE PIC 9(5) VALUE 6.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-NUMS OCCURS 6 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given an array of integers nums, find the minimum sum of any non-empty sub-array
      * of nums.
      * Example
      * minSubArraySum([2, 3, 4, 1, 2, 4]) == 1
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE L-NUMS(1) TO WS-MIN-SUM

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-SIZE
               MOVE 0 TO WS-CURRENT-SUM
               PERFORM VARYING WS-J FROM WS-I BY 1 UNTIL WS-J > WS-ARRAY-SIZE
                   ADD L-NUMS(WS-J) TO WS-CURRENT-SUM
                   IF WS-CURRENT-SUM < WS-MIN-SUM
                       MOVE WS-CURRENT-SUM TO WS-MIN-SUM
                   END-IF
               END-PERFORM
           END-PERFORM

           MOVE WS-MIN-SUM TO RESULT
           GOBACK.
