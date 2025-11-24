       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-XOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-LOOP-COUNTER PIC 9(3) VALUE 0.
       01 WS-A-LEN PIC 9(3) VALUE 0.
       01 WS-B-LEN PIC 9(3) VALUE 0.
       01 WS-MAX-LEN PIC 9(3) VALUE 0.
       01 WS-A-CHAR PIC X.
       01 WS-B-CHAR PIC X.
       01 WS-RESULT-CHAR PIC X.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-A PIC X(100).
           05 L-B PIC X(100).
           05 RESULT PIC X(100).

      * Input are two strings a and b consisting only of 1s and 0s.
      * Perform binary XOR on these inputs and return result also as a string.
      * >>> string_xor('010', '110')
      * '100'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           INSPECT FUNCTION REVERSE(L-A) TALLYING WS-A-LEN FOR CHARACTERS
           INSPECT FUNCTION REVERSE(L-B) TALLYING WS-B-LEN FOR CHARACTERS

           IF WS-A-LEN > WS-B-LEN
               MOVE WS-A-LEN TO WS-MAX-LEN
           ELSE
               MOVE WS-B-LEN TO WS-MAX-LEN
           END-IF

           PERFORM VARYING WS-LOOP-COUNTER FROM 1 BY 1 UNTIL WS-LOOP-COUNTER > WS-MAX-LEN
               MOVE L-A(WS-LOOP-COUNTER:1) TO WS-A-CHAR
               MOVE L-B(WS-LOOP-COUNTER:1) TO WS-B-CHAR

               EVALUATE TRUE
                   WHEN WS-A-CHAR = '0' AND WS-B-CHAR = '0'
                       MOVE '0' TO WS-RESULT-CHAR
                   WHEN WS-A-CHAR = '1' AND WS-B-CHAR = '1'
                       MOVE '0' TO WS-RESULT-CHAR
                   WHEN WS-A-CHAR = '0' AND WS-B-CHAR = '1'
                       MOVE '1' TO WS-RESULT-CHAR
                   WHEN WS-A-CHAR = '1' AND WS-B-CHAR = '0'
                       MOVE '1' TO WS-RESULT-CHAR
                   WHEN OTHER
                       MOVE '0' TO WS-RESULT-CHAR
               END-EVALUATE

               MOVE WS-RESULT-CHAR TO RESULT(WS-LOOP-COUNTER:1)
           END-PERFORM

           GOBACK.
       END PROGRAM STRING-XOR.
