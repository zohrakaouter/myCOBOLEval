       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-ELEMENTS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 I PIC 9(3) VALUE 0.
       01 CURRENT-NUMBER PIC S9(10).

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
           MOVE 0 TO RESULT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > L-K
               MOVE L-ARR(I) TO CURRENT-NUMBER
               IF CURRENT-NUMBER >= -99 AND CURRENT-NUMBER <= 99
                   ADD CURRENT-NUMBER TO RESULT
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM ADD-ELEMENTS.
