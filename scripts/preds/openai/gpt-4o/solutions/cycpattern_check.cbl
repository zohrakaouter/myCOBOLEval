       IDENTIFICATION DIVISION.
       PROGRAM-ID. CYCPATTERN-CHECK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-A PIC X(200).
       01 L-B-LEN PIC 9(3) VALUE 0.
       01 L-A-LEN PIC 9(3) VALUE 0.
       01 COUNTER PIC 9(3) VALUE 0.
       01 FOUND-INDICATOR PIC 9 VALUE 0.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC X(100).
           05 L-B PIC X(100).
           05 RESULT PIC 9.

      * You are given 2 words. You need to return True if the second word or any of its rotations is a substring in the first word
      * cycpattern_check("abcd","abd") => False
      * cycpattern_check("hello","ell") => True
      * cycpattern_check("whassup","psus") => False
      * cycpattern_check("abab","baa") => True
      * cycpattern_check("efef","eeff") => False
      * cycpattern_check("himenss","simen") => True
      * 
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

       MAIN-PROCEDURE.
           PERFORM INIT
           PERFORM CHECK-ROTATIONS
           MOVE FOUND-INDICATOR TO RESULT
           GOBACK.

       INIT.
           INSPECT L-B TALLYING L-B-LEN FOR CHARACTERS
           INSPECT L-A TALLYING L-A-LEN FOR CHARACTERS
           MOVE FUNCTION CONCATENATE(L-B, L-B) TO TEMP-A.


       CHECK-ROTATIONS.
           MOVE 1 TO COUNTER
           PERFORM UNTIL COUNTER > L-B-LEN
               IF FUNCTION INSPECT(TEMP-A, 1, L-B-LEN, (COUNTER))
                  CONSTRUCTION L-A-LEN FOR CHARACTERS = L-A-LEN
                  AND FUNCTION INDEX(L-A, TEMP-A, 1) > 0
               THEN
                   MOVE 1 TO FOUND-INDICATOR
                   EXIT PERFORM
               END-IF
               ADD 1 TO COUNTER
           END-PERFORM.

       END PROGRAM CYCPATTERN-CHECK.
