       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-LAST-CHAR-LETTER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       
       01 WS-VARIABLES.
           05 WS-LEN PIC 9(3).
           05 WS-LAST-CHAR PIC X.
           05 WS-PREV-CHAR PIC X VALUE SPACES.
           05 WS-I PIC 9(3).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TXT PIC X(100).
           05 RESULT PIC 9.

      * '''
      * Create a function that returns True if the last character
      * of a given string is an alphabetical character and is not
      * a part of a word, and False otherwise.
      * Note: "word" is a group of characters separated by space.
      * 
      * Examples:
      * is_last_char_letter("apple pie") ➞ False
      * is_last_char_letter("apple pi e") ➞ True
      * is_last_char_letter("apple pi e ") ➞ False
      * is_last_char_letter("") ➞ False
      * '''

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
           MOVE 0 TO RESULT
           MOVE FUNCTION LENGTH(FUNCTION TRIM(L-TXT)) TO WS-LEN

           IF WS-LEN = 0
               GOBACK
           END-IF

           MOVE L-TXT(WS-LEN:1) TO WS-LAST-CHAR

           IF WS-LAST-CHAR IS NOT ALPHABETIC
               GOBACK
           END-IF

           PERFORM VARYING WS-I FROM WS-LEN BY -1 UNTIL WS-I < 1
               IF L-TXT(WS-I:1) NOT = SPACE
                   MOVE L-TXT(WS-I:1) TO WS-PREV-CHAR
                   EXIT PERFORM
               END-IF
           END-PERFORM

           IF WS-PREV-CHAR = SPACE
               MOVE 1 TO RESULT
           END-IF

           GOBACK
           .
       END PROGRAM IS-LAST-CHAR-LETTER.
