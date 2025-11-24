       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-HAPPY.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-LENGTH PIC 9(3).
           05 WS-I PIC 9(3).
           05 WS-CHAR1 PIC X.
           05 WS-CHAR2 PIC X.
           05 WS-CHAR3 PIC X.
           05 WS-HAPPY-FLAG PIC 9 VALUE 1.

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
           MOVE 0 TO RESULT
           COMPUTE WS-LENGTH = FUNCTION LENGTH(L-S)

           IF WS-LENGTH < 3
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LENGTH - 2
               MOVE L-S(WS-I:1) TO WS-CHAR1
               MOVE L-S(WS-I + 1:1) TO WS-CHAR2
               MOVE L-S(WS-I + 2:1) TO WS-CHAR3

               IF WS-CHAR1 = WS-CHAR2 OR WS-CHAR1 = WS-CHAR3
                   OR WS-CHAR2 = WS-CHAR3
                   MOVE 0 TO WS-HAPPY-FLAG
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-HAPPY-FLAG = 1
               MOVE 1 TO RESULT
           END-IF

           GOBACK.
       END PROGRAM IS-HAPPY.
