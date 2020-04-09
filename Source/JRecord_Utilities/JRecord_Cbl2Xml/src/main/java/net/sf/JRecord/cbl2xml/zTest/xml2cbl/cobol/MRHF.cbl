      *******************************************************************
      * Multi-Record hierarchial file. The records are arranged in the following
      * hierarchy
      *
      *  Rec-1
      *    +-- Rec11 
      *         +-- Rec111 
      *         +-- Rec112
      *         +-- Rec113
      * 
      *    +-- Rec12 
      *         +-- Rec111
      *          
      *  Rec-2
      *    +-- Rec21 
      *         +-- Rec211
      *
      *  Java Array Representation {{3,1}, {1,1}}
      *
      *******************************************************************
      
       01  Rec-1.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r1-field1                     pic 9(4).
       01  Rec-11.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r11-field1                    pic 9(4).
       01  Rec-111.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r111-field1                   pic 9(5).
           03 r111-field2                   pic 9(5).
       01  Rec-112.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r112-field1                   pic 9(5).
       01  Rec-113.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r113-field1                   pic 9(5).
           03 r113-field2                   pic 9(5).
           03 r113-field3                   pic 9(5).

       01  Rec-12.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r12-field1                    pic 9(4).
       01  Rec-121.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r121-field1                   pic 9(5).
           03 r121-field2                   pic 9(5).
 
       01  Rec-2.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r2-field1                     pic 9(4).
       01  Rec-21.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r11-field1                    pic 9(4).
       01  Rec-211.
           03 Record-Type                   pic 999.
           03 Record-Name                   pic X(8).
           03 r211-field1                   pic 9(5).
           03 r211-field2                   pic 9(5).
           