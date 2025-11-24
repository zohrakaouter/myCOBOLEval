       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAX-ELEMENT.

       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       
       01 TEMP-MAXIMUM PIC S9(10).

       LINKAGE SECTION.

       01 LINKED-ITEMS.
           05 L-L OCCURS 3 TIMES INDEXED BY NI PIC S9(10).
           05 RESULT PIC S9(10).

      * Return maximum element in the list.
      * >>> max_element([1, 2, 3])
      * 3
      * >>> max_element([5, 3, -5])
      * 5
      * 

      * Complete the WORKING-STORAGE SECTION and the PROCEDURE DIVISION
      * Store the result in the RESULT variable and mark the end of your program with END PROGRAM

       PROCEDURE DIVISION USING LINKED-ITEMS.
      
           SET NI TO 1
           MOVE L-L(NI) TO TEMP-MAXIMUM

           PERFORM VARYING NI FROM 2 BY 1 UNTIL NI > 3
               IF L-L(NI) > TEMP-MAXIMUM THEN
                   MOVE L-L(NI) TO TEMP-MAXIMUM
               END-IF
           END-PERFORM
           
           MOVE TEMP-MAXIMUM TO RESULT
          
           GOBACK.

       END PROGRAM MAX-ELEMENT.
