C   Array descriptor class: name = 'ARRAY_DESC'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Array Descriptor" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   An array descriptor contains information about the array.
C   
C   Class Public members:
C      NDIM     I       Number of dimensions in the array
C      NAXIS    I(*)    Dimension of each axis
C      TRC      I(*)    Top right corner of subimage, 0's=>all
C      BLC      I(*)    Bottom left corner of subimage, 0's=>all
C      DATATYPE C*8     Element type, 'REAL', 'COMPLEX'
C      ANAME    C*8     Name of array if memory resident
C      FNAME    C*48?   Physical name of array file if disk resident.
C      FDISK    I       Disk number for FNAME.
C      BLANK    R       If 0.0 or absent then the array has no blanking.
C
C   Public functions:
C      ARDGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return array descriptor member.
C      ARDPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store array descriptor member.
C      ARDCOP (namein, namout, ierr)
C         Copy array descriptor info from namein to namout.
C      ARDSCP (namein, namout, ierr)
C         Copy array descriptor info not related to object size.
C
LOCAL INCLUDE 'ARRAY_DESC.INC'
C                                       ARRAY_DESC class include
      INTEGER   NMEML
      PARAMETER (NMEML = 9)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
      DATA MEMS /'NAXIS', 'NDIM', 'TRC', 'BLC', 'DATATYPE', 'ANAME',
     *   'FNAME', 'FDISK', 'BLANK'/
      DATA THSCLS /'ARRAY_DESC'/
LOCAL END
      SUBROUTINE ARDGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return array description member value.
C   Inputs:
C      NAME    C*?   Object name
C      KEYWRD  C*?   Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find., 
C                    2= Input error. 
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ARRAY_DESC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members. 
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it? 
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBGET (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not found
         IF (IERR.EQ.1) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' NOT FOUND'
            CALL MSGWRT (6)
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE ARDPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store array description member value.
C   Inputs:
C      NAME    C*?    Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find., 
C                    2= Input error. 
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ARRAY_DESC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       No base classes exist for this
C                                       class.
      IF (POINT.GE.1) THEN
         IERR = 2
         MSGTXT = 'NO BASE CLASSES FOR CLASS ' // THSCLS
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) THEN
         MEMBER = KEYWRD
      ELSE
         MEMBER = KEYWRD(1:POINT-1)
         END IF
C                                       Search list of recognized
C                                       members. 
      IMEM = -1
      DO 10 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 10      CONTINUE
C                                       Find it? 
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Fetch value:
         CALL OBPUT (OBJNUM, KEYWRD(POINT+1:), TYPE, DIM, VALUE, VALUEC,
     *      IERR)
C                                       Message if not stored
         IF (IERR.NE.0) THEN
            MSGTXT = 'MEMBER ' // MEMBER // ' COULD NOT BE STORED'
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE ARDCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy all non derived keywords of ARRAY_DESC except file name
C   Inputs:
C      NAMEIN  C*?    Input object name
C      NAMOUT  C*?    Output object name
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find., 
C                    2= Input error. 
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAMEIN*(*), NAMOUT*(*)
C
      INTEGER NDER
C                                       NDER = no. derived keywords, and
C                                       others not to copy.
      PARAMETER (NDER=3)
      INTEGER   TYPE, DIM(7), LOOP, IVAL(50), ID
      CHARACTER CVAL*200, DERMEM(NDER)*8
      LOGICAL   ISDER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ARRAY_DESC.INC'
      DATA DERMEM /'ANAME',' FNAME', 'FDISK'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Loop over class members
      DO 100 LOOP = 1,NMEML
C                                       Can't do derived keywords
         ISDER = .FALSE.
         DO 50 ID = 1,NDER
            ISDER = ISDER .OR. (MEMS(LOOP).EQ.DERMEM(ID))
 50         CONTINUE
         IF (.NOT.ISDER) THEN
            CALL ARDGET (NAMEIN, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            CALL ARDPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING ARRAY_DESC ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE ARDSCP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy selected portion of ARRAY_DESC not related to size or file name
C   Inputs:
C      NAMEIN  C*?    Input object name
C      NAMOUT  C*?    Output object name
C   Outputs:
C      IERR    I     Error code, 0=OK.  1=> didn't find., 
C                    2= Input error. 
C-----------------------------------------------------------------------
      INTEGER   IERR
      CHARACTER NAMEIN*(*), NAMOUT*(*)
C
      INTEGER NDER
C                                       NDER = no. derived keywords, and
C                                       keywords not to copy.
      PARAMETER (NDER=5)
      INTEGER   TYPE, DIM(7), LOOP, IVAL(50), ID
      REAL      RVAL(50)
      DOUBLE PRECISION DVAL(25)
      CHARACTER CVAL*200, DERMEM(NDER)*8
      LOGICAL   ISDER
      EQUIVALENCE (IVAL, RVAL, DVAL)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'ARRAY_DESC.INC'
      DATA DERMEM /'NDIM', 'NAXIS', 'ANAME', 'FNAME', 'FDISK'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Loop over class members
      DO 100 LOOP = 1,NMEML
C                                       Can't do derived keywords
         ISDER = .FALSE.
         DO 50 ID = 1,NDER
            ISDER = ISDER .OR. (MEMS(LOOP).EQ.DERMEM(ID))
 50         CONTINUE
         IF (.NOT.ISDER) THEN
            CALL ARDGET (NAMEIN, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            CALL ARDPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, IVAL, CVAL,
     *         IERR)
            IF (IERR.NE.0) GO TO 995
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING ARRAY_DESC ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
