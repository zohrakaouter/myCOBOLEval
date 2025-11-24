       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-NESTED.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 OPEN-BRACKETS PIC 9 VALUE 0.
       01 IS-NESTED-FLAG PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-STRING PIC X(100).
           05 RESULT PIC 9.

      * '''
      * Create a function that takes a string as input which contains only square brackets.
      * The function should return True if and only if there is a valid subsequence of brackets
      * where at least one bracket in the subsequence is nested.
      * 
      * is_nested('[[]]') ➞ True
      * is_nested('[]]]]]]][[[[[]') ➞ False
      * is_nested('[][]') ➞ False
      * is_nested('[]') ➞ False
      * is_nested('[[][]]') ➞ True
      * is_nested('[[]][[') ➞ True
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       BEGIN.
           PERFORM VARYING RESULT FROM 1 BY 1 UNTIL RESULT > FUNCTION LENGTH(L-STRING)
               EVALUATE FUNCTION INSPECT(L-STRING, TALLYING OCCURRENCES FOR LEADING PREV-RESULT)
               WHEN "[]" THRU "]" 
                   CONTINUE
               WHEN OTHER
                   EVALUATE L-STRING(RESULT:1)
                   WHEN "["
                       ADD 1 TO OPEN-BRACKETS
                       IF OPEN-BRACKETS > 1 THEN
                           SET IS-NESTED-FLAG TO TRUE
                       END-IF
                   WHEN "]"
                       IF OPEN-BRACKETS > 0 THEN
                           SUBTRACT 1 FROM OPEN-BRACKETS
                       END-IF
                   END-EVALUATE
               END-EVALUATE
               SET RESULT DOWN BY 1
           END-PERFORM
           
           IF IS-NESTED-FLAG = 1 THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

           GOBACK.

