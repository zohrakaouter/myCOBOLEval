       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-TO-MD5.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-MD5-HASH.
           05 WS-MD5-BUFFER PIC X(128).
       01 WS-EMPTY-FLAG PIC X VALUE 'N'.
           88 WS-EMPTY VALUE 'Y'.
       01 WS-MD5-LENGTH PIC 9(4) VALUE 32.
       01 WS-TEXT-LENGTH PIC 9(4).
       01 WS-HEX-TABLE.
           05 FILLER PIC X(16) VALUE "0123456789abcdef".

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

           IF L-TEXT = SPACES OR L-TEXT = LOW-VALUES
               MOVE 'None' TO RESULT
               GOBACK
           END-IF

           CALL 'CBL_MD5_HASH' USING
               BY CONTENT L-TEXT(1:FUNCTION LENGTH(FUNCTION TRIM(L-TEXT)))
               BY REFERENCE WS-MD5-BUFFER
           END-CALL

           STRING WS-MD5-BUFFER(1:WS-MD5-LENGTH) DELIMITED BY SIZE
               INTO RESULT
           END-STRING

           GOBACK.
       END PROGRAM STRING-TO-MD5.
