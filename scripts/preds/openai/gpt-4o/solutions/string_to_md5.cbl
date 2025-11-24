       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-TO-MD5.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 WS-HEX-TABLE.
           05 WS-HEX-CODE OCCURS 16 TIMES
                INDEXED BY WS-HEX-IDX
                PIC X VALUE SPACES.

       01 WS-MD5-HASH REDEFINES WS-HEX-TABLE
           PIC X(32).

       01 WS-FINAL-HASH PIC X(32).
           
       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TEXT PIC X(100).
           05 RESULT PIC X(100).

      * 
      * Given a string 'text', return its md5 hash equivalent string.
      * If 'text' is an empty string, return 'None'.
      * 
      * >>> string_to_md5('Hello world') == '3e25960a79dbc69b674cd4ec67a72c62'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.

           IF L-TEXT = SPACE THEN
               MOVE "None" TO RESULT
               GOBACK
           ELSE
               PERFORM INITIALIZE-HEX-TABLE
               COMPUTE-MD5-FROM-STRING
               MOVE WS-MD5-HASH TO RESULT
           END-IF

           GOBACK.

       INITIALIZE-HEX-TABLE.

           MOVE "0123456789abcdef" TO WS-HEX-CODE OF WS-HEX-TABLE(1) 
               THROUGH WS-HEX-TABLE(16).

           EXIT.

       COMPUTE-MD5-FROM-STRING.

           CALL "CBL_OC_MD5" USING L-TEXT, WS-FINAL-HASH.

           PERFORM VARYING WS-INDEX FROM 1 BY 2 UNTIL WS-INDEX > 32
               COMPUTE WS-IDX-1 = FUNCTION NUMVAL-C WS-FINAL-HASH(WS-INDEX:1) + 1
               COMPUTE WS-IDX-2 = FUNCTION NUMVAL-C WS-FINAL-HASH(WS-INDEX+1:1) + 1

               STRING WS-HEX-CODE(WS-IDX-1)
                      WS-HEX-CODE(WS-IDX-2)
                  INTO WS-MD5-HASH(WS-INDEX:2)
           END-PERFORM.

           EXIT.

       END PROGRAM STRING-TO-MD5.
