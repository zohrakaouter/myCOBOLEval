       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-XOR.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 INDEX          PIC 9(3) VALUE 1.
       01 A-BYTE         PIC X.
       01 B-BYTE         PIC X.
       01 RESULT-BYTE    PIC X.

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
           PERFORM VARYING INDEX FROM 1 BY 1 UNTIL INDEX > FUNCTION LENGTH(L-A)
               MOVE L-A(INDEX:1) TO A-BYTE
               MOVE L-B(INDEX:1) TO B-BYTE

               IF A-BYTE NOT = B-BYTE THEN
                   MOVE '1' TO RESULT(INDEX:1)
               ELSE
                   MOVE '0' TO RESULT(INDEX:1)
               END-IF
           END-PERFORM
           GOBACK.
       END PROGRAM STRING-XOR.
