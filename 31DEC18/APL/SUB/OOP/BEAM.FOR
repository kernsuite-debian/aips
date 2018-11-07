C   Beam Class:  name = 'BEAM'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "BEAM" class library
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
C   Contains restoring beam and other deconvolution information.
C
C   Class Public members:
C      PRODUCT   I   Clean product code.
C      NITER     I   Number of Clean components or MEM iterations
C      BMAJ      R   Clean beam major axis
C      BMIN      R   Clean beam minor axis
C      BPA       R   Clean beam position angle
C
C   Public functions:
C      BEMGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return beam member.
C      BEMPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store beam member.
C      BEMCOP (namein, namout, ierr)
C         Copy beam from one object to another.
C
LOCAL INCLUDE 'BEAM.INC'
C                                       BEAM class include
      INTEGER   NMEML
      PARAMETER (NMEML = 5)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
      DATA MEMS /'PRODUCT','NITER','BMAJ','BMIN','BPA'/
      DATA THSCLS /'BEAM'/
LOCAL END
      SUBROUTINE BEMGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return beam member.
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
      INCLUDE 'BEAM.INC'
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
      SUBROUTINE BEMPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store beam member.
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
      INCLUDE 'BEAM.INC'
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
            MSGTXT = 'OBJECT =' // NAME
            CALL MSGWRT (6)
            END IF
C
 999  RETURN
      END
      SUBROUTINE BEMCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Convolving BEAM from one Image object to another.
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
      INTEGER   TYPE, DIM(7), LOOP, IVAL(5)
      REAL      RVAL(5)
      CHARACTER CVAL*20
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (IVAL, RVAL)
      INCLUDE 'BEAM.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Copy BEAM
      DO 50 LOOP = 1,NMEML
         CALL BEMGET (NAMEIN, MEMS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL BEMPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 50      CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING BEAM ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
