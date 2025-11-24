       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLUCK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-MIN-EVEN-VALUE PIC S9(10) VALUE 0.
           05 WS-MIN-EVEN-INDEX PIC S9(10) VALUE 0.
           05 WS-FOUND-EVEN     PIC X(3) VALUE 'NO'.
               88 FOUND-EVEN     VALUE 'YES'.
               88 NO-EVEN        VALUE 'NO'.
           05 WS-I              PIC S9(10) VALUE 0.

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
       MAIN-PARAGRAPH.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               IF FUNCTION MOD(L-ARR(WS-I), 2) = 0
                   IF NO-EVEN
                       MOVE L-ARR(WS-I) TO WS-MIN-EVEN-VALUE
                       MOVE WS-I TO WS-MIN-EVEN-INDEX
                       SET FOUND-EVEN TO TRUE
                   ELSE
                       IF L-ARR(WS-I) < WS-MIN-EVEN-VALUE
                           MOVE L-ARR(WS-I) TO WS-MIN-EVEN-VALUE
                           MOVE WS-I TO WS-MIN-EVEN-INDEX
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           IF FOUND-EVEN
               MOVE WS-MIN-EVEN-VALUE TO RESULT(1)
               MOVE WS-MIN-EVEN-INDEX TO RESULT(2)
           END-IF

           GOBACK.
       END PROGRAM PLUCK.
