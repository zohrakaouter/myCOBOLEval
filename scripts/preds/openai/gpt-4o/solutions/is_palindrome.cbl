       IDENTIFICATION DIVISION.
       PROGRAM-ID. IS-PALINDROME.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       77 REVERSED-TEXT PIC X(100).
       77 LENGTH        PIC 99.
       77 I             PIC 99.

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

           DISPLAY "Input received: " L-TEXT

           COMPUTE LENGTH = FUNCTION LENGTH(TRIM(L-TEXT))
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH
               MOVE FUNCTION REVERSE(L-TEXT) TO REVERSED-TEXT
               IF L-TEXT (I: LENGTH-I+1) NOT = REVERSED-TEXT (I: LENGTH-I+1)
                   MOVE 0 TO RESULT
                   DISPLAY "Not a palindrome."
                   GOBACK
               END-IF
           END-PERFORM

           MOVE 1 TO RESULT
           DISPLAY "Is a palindrome."

           GOBACK.

       END PROGRAM IS-PALINDROME.
