       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLUCK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 MIN-EVEN-VALUE    PIC S9(10) VALUE +9999999999.
       77 MIN-EVEN-INDEX    PIC S9(10) VALUE +9999999999.
       77 CURRENT-VALUE     PIC S9(10).
       77 CURRENT-INDEX     PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-ARR OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT OCCURS 100 TIMES INDEXED BY NJ PIC S9(10).

      * 
      * "Given an array representing a branch of a tree that has non-negative integer nodes
      * your task is to pluck one of the nodes and return it.
      * The plucked node should be the node with the smallest even value.
      * If multiple nodes with the same smallest even value are found return the node that has smallest index.
      * 
      * The plucked node should be returned in a list, [ smalest_value, its index ],
      * If there are no even values or the given array is empty, return [].
      * 
      * Example 1:
      * Input: [4,2,3]
      * Output: [2, 1]
      * Explanation: 2 has the smallest even value, and 2 has the smallest index.
      * 
      * Example 2:
      * Input: [1,2,3]
      * Output: [2, 1]
      * Explanation: 2 has the smallest even value, and 2 has the smallest index.
      * 
      * Example 3:
      * Input: []
      * Output: []
      * 
      * Example 4:
      * Input: [5, 0, 3, 0, 4, 2]
      * Output: [0, 1]
      * Explanation: 0 is the smallest value, but  there are two zeros,
      * so we will choose the first zero, which has the smallest index.
      * 
      * Constraints:
      * * 1 <= nodes.length <= 10000
      * * 0 <= node.value
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           PERFORM VARYING NI FROM 1 BY 1 UNTIL NI GREATER THAN 3
               IF L-ARR(NI) MOD 2 = 0 THEN
                   MOVE L-ARR(NI) TO CURRENT-VALUE
                   COMPUTE CURRENT-INDEX = NI - 1
                   
                   IF CURRENT-VALUE < MIN-EVEN-VALUE THEN
                       MOVE CURRENT-VALUE TO MIN-EVEN-VALUE
                       MOVE CURRENT-INDEX TO MIN-EVEN-INDEX
                   END-IF

               END-IF
           END-PERFORM

           IF MIN-EVEN-VALUE < +9999999999 THEN
               MOVE MIN-EVEN-VALUE TO RESULT(1)
               MOVE MIN-EVEN-INDEX TO RESULT(2)
           ELSE
               CONTINUE
           END-IF

           GOBACK.
