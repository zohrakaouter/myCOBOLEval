       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-ITEMS.
           05 WS-ARR OCCURS 9 TIMES PIC S9(10) VALUE 0.
           05 WS-K PIC S9(10) VALUE 0.
           05 WS-SUM PIC S9(10) VALUE 0.
           05 I PIC S9(10) VALUE 1.
           05 TEMP-VAR PIC S9(10) VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 9 TIMES INDEXED BY NI PIC S9(10).
           05 L-K PIC S9(10).
           05 RESULT PIC S9(10).

      * 
      * Given a non-empty array of integers arr and an integer k, return
      * the sum of the elements with at most two digits from the first k elements of arr.
      * 
      * Example:
      * 
      * Input: arr = [111,21,3,4000,5,6,7,8,9], k = 4
      * Output: 24 # sum of 21 + 3
      * 
      * Constraints:
      * 1. 1 <= len(arr) <= 100
      * 2. 1 <= k <= len(arr)
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       BEGIN-PROGRAM.

           MOVE L-K TO WS-K.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-K
               MOVE L-ARR(I) TO TEMP-VAR
               EVALUATE FUNCTION NUMVAL-C (TEMP-VAR)
                   WHEN 0 THRU 9 
                       ADD TEMP-VAR TO WS-SUM
                   WHEN -9 THRU -1
                       ADD TEMP-VAR TO WS-SUM
                   WHEN 10 THRU 99
                       ADD TEMP-VAR TO WS-SUM
                   WHEN -99 THRU -10
                       ADD TEMP-VAR TO WS-SUM
                   WHEN OTHER
                       CONTINUE
               END-EVALUATE
           END-PERFORM

           MOVE WS-SUM TO RESULT.

       END-PROGRAM.

       GOBACK.
