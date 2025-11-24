       IDENTIFICATION DIVISION.
       PROGRAM-ID. REMOVE-VOWELS.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01  WS-TEXT.
           05  WS-CHAR   PIC X OCCURS 100 TIMES.
           05  WS-NULL   PIC X VALUE LOW-VALUE.

       01  WS-VOWELS     PIC X(10) VALUE 'aeiouAEIOU'.

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-TEXT PIC X(100).
           05 RESULT PIC X(100).

      * 
      * remove_vowels is a function that takes string and returns string without vowels.
      * >>> remove_vowels('')
      * ''
      * >>> remove_vowels("abcdef\nghijklm")
      * 'bcdf\nghjklm'
      * >>> remove_vowels('abcdef')
      * 'bcdf'
      * >>> remove_vowels('aaaaa')
      * ''
      * >>> remove_vowels('aaBAA')
      * 'B'
      * >>> remove_vowels('zbcd')
      * 'zbcd'
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
       
           MOVE L-TEXT TO WS-TEXT
        
           PERFORM VARYING INDEX FROM 1 BY 1
               UNTIL WS-CHAR(INDEX) = WS-NULL

               IF WS-CHAR(INDEX) NOT EQUAL SPACES
                  AND WS-CHAR(INDEX) NOT EQUAL HIGH-VALUE
               THEN
                   IF WS-CHAR(INDEX) NOT IN WS-VOWELS
                   THEN
                       STRING
                           RESULT DELIMITED BY SIZE
                           WS-CHAR(INDEX) DELIMITED BY SIZE 
                       INTO RESULT
                   END-IF
               END-IF
           END-PERFORM.

           GOBACK.
