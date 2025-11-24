       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-PALINDROME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARS.
           05 WS-LEN PIC 9(3).
           05 WS-I PIC 9(3).
           05 WS-J PIC 9(3).
           05 WS-CHAR1 PIC X.
           05 WS-CHAR2 PIC X.
           05 WS-IS-PALINDROME PIC 9 VALUE 1.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TEXT PIC X(100).
           05 RESULT PIC 9.

      * 
      * Checks if given string is a palindrome
      * >>> is_palindrome('')
      * True
      * >>> is_palindrome('aba')
      * True
      * >>> is_palindrome('aaaaa')
      * True
      * >>> is_palindrome('zbcd')
      * False
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 1 TO RESULT

           INSPECT FUNCTION REVERSE(L-TEXT) TALLYING WS-LEN FOR CHARACTERS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN / 2
               COMPUTE WS-J = WS-LEN - WS-I + 1
               MOVE L-TEXT(WS-I:1) TO WS-CHAR1
               MOVE L-TEXT(WS-J:1) TO WS-CHAR2
               IF WS-CHAR1 NOT EQUAL TO WS-CHAR2
                   MOVE 0 TO RESULT
                   EXIT PERFORM
               END-IF
           END-PERFORM

           GOBACK.
