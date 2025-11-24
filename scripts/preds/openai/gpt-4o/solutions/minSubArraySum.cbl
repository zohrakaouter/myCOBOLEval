       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINSUBARRAYSUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-NUMS-LENGTH         PIC S9(10) VALUE 6.
       01 INDEX                  PIC S9(10).
       01 WS-CURRENT-SUM         PIC S9(10).
       01 WS-MINIMUM-SUM         PIC S9(10) VALUE +9999999999.
       
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

           SET NI TO 1
           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > WS-NUMS-LENGTH
               ADD L-NUMS (INDEX) TO WS-CURRENT-SUM
               
               IF WS-CURRENT-SUM < WS-MINIMUM-SUM THEN
                   MOVE WS-CURRENT-SUM TO WS-MINIMUM-SUM
               END-IF
               
               IF WS-CURRENT-SUM > 0 THEN
                   MOVE 0 TO WS-CURRENT-SUM
               END-IF
           END-PERFORM
           
           MOVE WS-MINIMUM-SUM TO RESULT

           GOBACK.
       
       END PROGRAM MINSUBARRAYSUM.
