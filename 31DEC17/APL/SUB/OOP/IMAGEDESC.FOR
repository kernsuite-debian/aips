C   Image descriptor class:  name = 'IMAGE_DESC.'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Image Descriptor" class library
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
C   The image descriptor contains descriptive information about an
C   image. 
C
C   Class public members;
C      OBJECT   C*8  Source name
C      TELESCOP C*8  Telescope name
C      INSTRUME C*8  Receiver name
C      OBSERVER C*8  Observer name
C      DATE-OBS C*8  Observing date as dd/mm/yy
C      DATE-MAP C*8  Creation date as dd/mm/yy
C      DOCHECK L       True if array labeling should be compared before
C                      binary operations with other arrays.
C      BUNIT   C*8     Units of the array
C      EPOCH   R       Mean epoch of celestial position (1950, 2000)
C      USERNO  I       User ID number
C      CTYPE   C*8(*)  Label for each axis
C      CRVAL   D(*)    Coordinate value at reference pixel
C      CDELT   R(*)    Coordinate increment
C      CRPIX   R(*)    Reference pixel for axis
C      CROTA   R(*)    Coordinate rotation for each axis.
C
C   Public functions:
C      IMDGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return image descriptor member.
C      IMDPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store image descriptor member.
C      IMDCOP (namein, namout, ierr)
C         Copy image descriptor info from namein to namout.
C
LOCAL INCLUDE 'IMAGE_DESC.INC'
C                                       IMAGE_DESC class include
      INTEGER   NMEML
      PARAMETER (NMEML = 15)
      CHARACTER MEMS(NMEML)*8, THSCLS*16
      DATA MEMS /'OBJECT', 'TELESCOP', 'INSTRUME', 'OBSERVER',
     *   'DATE-OBS', 'DATE-MAP', 'DOCHECK', 'BUNIT', 'EPOCH', 'USERNO',
     *   'CTYPE', 'CRVAL', 'CDELT', 'CRPIX', 'CROTA'/
      DATA THSCLS /'IMAGE_DESC'/
LOCAL END
      SUBROUTINE IMDGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return Image descriptor member.
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
      INTEGER    IMEM, LOOP, OBJNUM, POINT
      CHARACTER MEMBER*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IMAGE_DESC.INC'
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
      SUBROUTINE IMDPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Store Image descriptor member.
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
      INCLUDE 'IMAGE_DESC.INC'
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
     *   IERR)
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
      SUBROUTINE IMDCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy IMAGE_DESC from NAMEIN to NAMOUT.  Also copies associated
C   POSITION, BEAM and VELOCITY members.
C   Note: the Image descriptor should be copied AFTER the output image
C   is fully instantiated as creations of the AIPS files will reset some
C   of this information.
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
      INTEGER   NPOS, NVEL, NBEM
C                                       NPOS = no. POSITION to copy
      PARAMETER (NPOS = 4)
C                                       NVEL = no. VELOCITY to copy
      PARAMETER (NVEL = 4)
C                                       NBEM = no. BEAM to copy
      PARAMETER (NBEM = 5)
      INTEGER   TYPE, DIM(7), LOOP, IVAL(50)
      CHARACTER CVAL*200, CPOS(NPOS)*8, CVEL(NVEL)*8, CBEM(NBEM)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'IMAGE_DESC.INC'
      DATA CPOS /'OBSRA', 'OBSDEC', 'XSHIFT', 'YSHIFT'/
      DATA CVEL /'VELREF' ,'ALTRVAL', 'ALTRPIX', 'RESTFREQ'/
      DATA CBEM /'PRODUCT', 'NITER', 'BMAJ', 'BMIN', 'BPA'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Copy position
      DO 30 LOOP = 1,NPOS
         CALL PSNGET (NAMEIN, CPOS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL PSNPUT (NAMOUT, CPOS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 30      CONTINUE
C                                       Copy VELOCITY
      DO 40 LOOP = 1,NVEL
         CALL VELGET (NAMEIN, CVEL(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL VELPUT (NAMOUT, CVEL(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 40      CONTINUE
C                                       Copy BEAM
      DO 50 LOOP = 1,NBEM
         CALL BEMGET (NAMEIN, CBEM(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL BEMPUT (NAMOUT, CBEM(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 50      CONTINUE
C                                       Loop over class members
      DO 100 LOOP = 1,NMEML
         CALL IMDGET (NAMEIN, MEMS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL IMDPUT (NAMOUT, MEMS(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 995
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ERROR COPYING IMAGE_DESC ' // NAMEIN
      CALL MSGWRT (8)
      MSGTXT = ' TO ' // NAMOUT
      CALL MSGWRT (8)
C
 999  RETURN
      END
