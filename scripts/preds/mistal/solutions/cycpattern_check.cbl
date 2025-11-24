       IDENTIFICATION DIVISION.
       PROGRAM-ID. CYCPATTERN-CHECK.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-LEN-A PIC 9(3) VALUE 0.
           05 WS-LEN-B PIC 9(3) VALUE 0.
           05 WS-I PIC 9(3) VALUE 0.
           05 WS-J PIC 9(3) VALUE 0.
           05 WS-K PIC 9(3) VALUE 0.
           05 WS-TEMP-B PIC X(100) VALUE SPACES.
           05 WS-ROTATED-B PIC X(100) VALUE SPACES.
           05 WS-FOUND PIC 9 VALUE 0.

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
           MOVE 0 TO RESULT

           COMPUTE WS-LEN-A = FUNCTION LENGTH(FUNCTION TRIM(L-A))
           COMPUTE WS-LEN-B = FUNCTION LENGTH(FUNCTION TRIM(L-B))

           IF WS-LEN-B = 0 OR WS-LEN-A = 0
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN-B
               MOVE SPACES TO WS-ROTATED-B
               MOVE L-B TO WS-TEMP-B

               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-LEN-B
                   COMPUTE WS-K = (WS-J + WS-I - 1)
                   IF WS-K > WS-LEN-B
                       COMPUTE WS-K = WS-K - WS-LEN-B
                   END-IF
                   MOVE WS-TEMP-B(WS-K:1) TO WS-ROTATED-B(WS-J:1)
               END-PERFORM

               INSPECT L-A TALLYING WS-FOUND FOR ALL WS-ROTATED-B
               IF WS-FOUND > 0
                   MOVE 1 TO RESULT
                   GOBACK
               END-IF
           END-PERFORM

           GOBACK.
       END PROGRAM CYCPATTERN-CHECK.
