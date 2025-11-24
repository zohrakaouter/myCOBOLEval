       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-HAPPY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 I PIC 9(3) VALUE 1.
       01 LEN PIC 9(3).
       01 IS-HAPPY-FLAG PIC X VALUE 'Y'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-S PIC X(100).
           05 RESULT PIC 9.

      * You are given a string s.
      * Your task is to check if the string is happy or not.
      * A string is happy if its length is at least 3 and every 3 consecutive letters are distinct
      * For example:
      * is_happy(a) => False
      * is_happy(aa) => False
      * is_happy(abcd) => True
      * is_happy(aabb) => False
      * is_happy(adb) => True
      * is_happy(xyy) => False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       BEGIN.
           UNSTRING L-S DELIMITED BY SPACE INTO L-S END-UNSTRING.
           INSPECT L-S TALLYING LEN FOR ALL CHARACTERS.
           
           IF LEN < 3 THEN
               MOVE 0 TO RESULT
           ELSE
               PERFORM CHECK-HAPPY
           END-IF.
           
       DISPLAY RESULT.

       CHECK-HAPPY.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN - 2
               IF L-S(I:1) = L-S(I+1:1) 
                  OR L-S(I+1:1) = L-S(I+2:1) 
                  OR L-S(I:1) = L-S(I+2:1) THEN
                   MOVE 'N' TO IS-HAPPY-FLAG 
                   EXIT PERFORM
               END-IF
           END-PERFORM
           
           IF IS-HAPPY-FLAG = 'Y' THEN
               MOVE 1 TO RESULT
           ELSE
               MOVE 0 TO RESULT
           END-IF.

      GOBACK.
      END PROGRAM IS-HAPPY.
