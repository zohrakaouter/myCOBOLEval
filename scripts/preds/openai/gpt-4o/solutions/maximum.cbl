       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAXIMUM.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 W-ARR OCCURS 3 TIMES PIC S9(10) VALUE 0.
       01 W-K PIC S9(10) VALUE 0.
       01 W-TEMP PIC S9(10).

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

           MOVE L-ARR(1) TO W-ARR(1)
           MOVE L-ARR(2) TO W-ARR(2)
           MOVE L-ARR(3) TO W-ARR(3)
           MOVE L-K TO W-K
            
           PERFORM SORT-ARRAY
           
           PERFORM POPULATE-RESULT
           
           GOBACK.

       SORT-ARRAY.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI = 3
               PERFORM VARYING NJ FROM 1 BY 1 UNTIL NJ = 2
                   IF W-ARR(NJ) > W-ARR(NJ + 1)
                       MOVE W-ARR(NJ) TO W-TEMP
                       MOVE W-ARR(NJ + 1) TO W-ARR(NJ)
                       MOVE W-TEMP TO W-ARR(NJ + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.

       POPULATE-RESULT.
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI > W-K
               MOVE W-ARR(NI) TO RESULT(NI)
           END-PERFORM.

       END PROGRAM MAXIMUM.
