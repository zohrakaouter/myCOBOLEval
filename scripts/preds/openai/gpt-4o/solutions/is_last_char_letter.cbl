       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-LAST-CHAR-LETTER.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 LAST-CHAR     PIC X.
       77 SPACE-BEFORE  PIC X.
       77 IS-LETTER     PIC 9 VALUE 0.
       77 POSITION      PIC 99.

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

       MAIN-LOGIC SECTION.

       BEGIN.
           MOVE FUNCTION TRIM(L-TXT TRAILING) TO L-TXT
           COMPUTE POSITION = FUNCTION LENGTH(L-TXT)
           
           IF POSITION = 0 THEN
               MOVE 0 TO IS-LETTER
           ELSE
               MOVE L-TXT(POSITION:1) TO LAST-CHAR
               IF POSITION > 1 THEN
                   MOVE L-TXT(POSITION-1:1) TO SPACE-BEFORE
               ELSE
                   MOVE ' ' TO SPACE-BEFORE
               END-IF

               IF LAST-CHAR IS ALPHABETIC
                  AND SPACE-BEFORE = ' ' THEN
                   MOVE 1 TO IS-LETTER
               ELSE
                   MOVE 0 TO IS-LETTER
               END-IF
           END-IF

           MOVE IS-LETTER TO RESULT
           GOBACK.

       END PROGRAM IS-LAST-CHAR-LETTER.
