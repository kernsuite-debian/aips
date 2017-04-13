C   Array Class:  name ='ARRAY'
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "ARRAY" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2002, 2012, 2015
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
C   An array is a regular array of values with several descriptive base
C   classes. General access to members is through ARRGET and ARRPUT
C   although efficient access to the array may be had through ARREAD and
C   ARRWRI.  Access may be by element, row, plane or image.  Arrays may
C   contain either real of complex data type elements.  Blanking is also
C   allowed.
C
C   Class Public members:
C      array   R(*,*...)  Array of Pixel values.  May be memory or disk
C                         resident.
C      ARRAY_PNT  class   Array access pointer
C      ARRAY_DESC class   Array description
C      ARRAY_STAT class   Array statistical information
C
C   Private class data:
C      ARRFDV     I(*,MAXIO) Internal array for I/O routine
C      ARRTYP     I(MAXIO)   Access type: 1=pixel, 2=row, 3=plane,
C                            4=array.
C      ARRPT      I(MAXIO)   Row element pointer
C      ARRDIM     I(7,MAXIO) Dimension of window in array
C
C   Public functions:
C      ARRGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return array subarray.  Access may be by pixel, row, plane or
C         array as determined from ARRAY_DESC.
C      ARRPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store array subarray.  Access may be by pixel, row, plane or
C         array as determined from ARRAY_DESC.
C      ARREAD (name, dim, data, ierr)
C         Read a section of an array
C      ARRWRI (name, dim, data, ierr)
C         Write a section of an array
C      ARDRED (name, dim, data, ierr)
C         Read a section of an array to double memory
C      ARDWRI (name, dim, data, ierr)
C         Write a section of an array from double memory
C      ARRCLO (name, ierr)
C         Close I/O to an array
C      ARROPN (name, status, ierr)
C         Setup for I/O to an array
C      ARSSET (name, ierr)
C         Determines array statistics and updates ARRAY_STAT
C      ARHIST (name, nhis, hmin, hmax, hist, ierr)
C         Return histogram of array.
C      CHKBLK (n, data, valid)
C         Checks an array for blanking and returns a validity array
C      SETBLK (n, valid, data)
C         Blanks an array on the basis of a validity array.
C      ARRNEG (in, out, ierr)
C         Negate the values of an input array and write an out out
C         array.
C      ARRFFT (dir, in, scr, out, ierr)
C         FFT an array.
C      ARRADD (in1, in2, out, ierr)
C         Adds two arrays.
C      ARRSUB (in1, in2, out, ierr)
C         Subtracts array in2 from array in1.
C      ARRMUL (in1, in2, out, ierr)
C         Multiplies two arrays.
C      ARRDIV (in1, in2, out, ierr)
C         Divides array in1 by array in2.
C      ARRPAD (in, out, ierr)
C         Copy one array to another with zero padding.
C      ARRSCL (in, factor, out, ierr)
C         Scale an array with a real factor.
C      ARRCOP (in, out, ierr)
C         Copy the element of an array
C      ARRFIL (scalar, out, ierr)
C         Fill an array with a real scalar
C      ARRSAD (in, scalar, out, ierr)
C         Add a scalar to the elements of an array.
C      ARRSMU (in, scalar, out, ierr)
C         Multiply a scalar times the elements of an array.
C      ARRBSC (plus, minus, out, ierr)
C         Combine beam-switched images
C
C   Private Function:
C      ARRCHK (in1, in2, ierr)
C         Check that two arrays are compatible.
C      ARRWIN (name, blc, trc, dim, ierr)
C         Returns window information about an array
C      ARRCWI (name, ierr)
C         Clears the window (BLC, TRC) for first two dimensions of an
C         array object.
C      ARRIO (opcode, name, fdvec, ibuff, ipnt, ierr)
C         Handles I/O to disk resident array
C      ARRMEM (????)  Not implemented
C         Handles access to memory resident array.
LOCAL INCLUDE 'ARRAY.INC'
C                                       ARRAY class include
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   NMEML, NBASE
C                                       NMEML=no. member names
      PARAMETER (NMEML = 1)
C                                       NBASE = .no. base classes.
      PARAMETER (NBASE = 3)
C
      CHARACTER MEMS(NMEML)*8, BASE(NBASE)*16, THSCLS*16
      INTEGER   ARRFDV(80,MAXIO), ARRTYP(MAXIO), ARRPT(MAXIO),
     *   ARRDIM(7,MAXIO)
      LOGICAL   VALID1(MAXIMG), VALID2(MAXIMG), VALID3(MAXIMG),
     *   VALIDX(MAXIMG)
      COMMON /ARRIOC/ ARRFDV, ARRTYP, ARRPT, ARRDIM,
     *   VALIDX, VALID1, VALID2, VALID3
C                                       Recognized member names
      DATA MEMS /'ARRAY'/
C                                       Recognized base classes
      DATA BASE /'ARRAY_PNT', 'ARRAY_DESC', 'ARRAY_STAT'/
C                                       Class name
      DATA THSCLS /'ARRAY'/
LOCAL END
      SUBROUTINE ARRGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return a portion of an array.
C   value (array) for a specified member
C   Inputs:
C      NAME    C*(*)  Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      REAL      VALUE(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT, IBASE, JERR
      CHARACTER MEMBER*16, BASCLS*16
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       Call necessary base class
C                                       access function.
      IF (POINT.GE.1) THEN
         BASCLS = KEYWRD(1:POINT-1)
C                                       Search list of recognized
C                                       members.
         IBASE = -1
         DO 20 LOOP = 1,NBASE
            IF (BASCLS.EQ.BASE(LOOP)) IBASE = LOOP
 20         CONTINUE
C                                       Find it?
            IF (IBASE.LE.0) THEN
               IERR = 2
               MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' BASE CLASS ' //
     *            BASCLS
               CALL MSGWRT (7)
               GO TO 999
            ELSE
C                                       Call access function:
               GO TO (110, 120, 130), IBASE
C                                       ARRAY_PNT
 110              CALL ARPGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
C                                       ARRAY_DESC
 120              CALL ARDGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
C                                       ARRAY_STAT
 130              CALL ARSGET (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
               END IF
            GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) POINT = LEN (KEYWRD) + 1
      MEMBER = KEYWRD(1:POINT-1)
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 500 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 500     CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call I/O routine:
      CALL ARREAD (NAME, DIM, VALUE, IERR)
      TYPE = 2
      IF (IERR.NE.0) GO TO 999
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE ARRPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public
C   Stores a portion of an array.
C   value (array) for a specified member
C   Inputs:
C      NAME    C*(*)  Object name
C      KEYWRD  C*(*)  Keyword in form 'mem1.mem2...'
C   Outputs:
C      TYPE    I     data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM     I(*)  Dimensionality of the array.
C      VALUE   ?     associated value (non character)
C      VALUEC  C*?   associated value (character)
C      IERR    I     Error code, 0=OK.  1=> didn't find.,
C                    2= Input error.
C-----------------------------------------------------------------------
      INTEGER   TYPE, DIM(*), IERR
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      REAL      VALUE(*)
C
      INTEGER   IMEM, LOOP, OBJNUM, POINT, IBASE, JERR
      CHARACTER MEMBER*16, BASCLS*16
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Lookup NAME
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Look for base class.member name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       Call necessary base class
C                                       access function.
      IF (POINT.GE.1) THEN
         BASCLS = KEYWRD(1:POINT-1)
C                                       Search list of recognized
C                                       members.
         IBASE = -1
         DO 20 LOOP = 1,NBASE
            IF (BASCLS.EQ.BASE(LOOP)) IBASE = LOOP
 20         CONTINUE
C                                       Find it?
            IF (IBASE.LE.0) THEN
               IERR = 2
               MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' BASE CLASS ' //
     *            BASCLS
               CALL MSGWRT (7)
               GO TO 999
            ELSE
C                                       Call access function:
               GO TO (110, 120, 130), IBASE
C                                       ARRAY_PNT
 110              CALL ARPPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
C                                       ARRAY_DESC
 120              CALL ARDPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
C                                       ARRAY_STAT
 130              CALL ARSPUT (NAME, KEYWRD(POINT+1:), TYPE, DIM, VALUE,
     *               VALUEC, JERR)
                  GO TO 900
               END IF
            GO TO 999
         END IF
C                                       Save member name
      IF (POINT.LE.0) POINT = LEN (KEYWRD) + 1
      MEMBER = KEYWRD(1:POINT-1)
C                                       Search list of recognized
C                                       members.
      IMEM = -1
      DO 500 LOOP = 1,NMEML
         IF (MEMBER.EQ.MEMS(LOOP)) IMEM = LOOP
 500     CONTINUE
C                                       Find it?
      IF (IMEM.LE.0) THEN
         IERR = 2
         MSGTXT = 'UNRECOGNIZED ' // THSCLS // ' MEMBER ' // MEMBER
         CALL MSGWRT (7)
         GO TO 999
         END IF
C                                       Call I/O routine:
      CALL ARRWRI (NAME, DIM, VALUE, IERR)
      IF (IERR.NE.0) GO TO 999
 900  IERR = 0
      IF (JERR.NE.0) IERR = 2
C
 999  RETURN
      END
      SUBROUTINE ARREAD (NAME, DIM, DATA, IERR)
C-----------------------------------------------------------------------
C   Public
C   Reads a portion of an array.  Initiates I/O if necessary.
C   Access may be by pixel, row, plane or array as specified by
C   ARRAY_PNT.ACCESS. Default = 'row'
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      DIM   I(*)  The dimensionality of the value returned in DATA
C      DATA  R(*)  The next row of the image
C      IERR  I     Error return code, 0=OK, -1 => I/O complete when done
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      REAL      DATA(*)
      INTEGER   DIM(*), IERR
C
      INTEGER   BUFNO, NROW, LOOP, PNT(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Need to initialize?
C                                       Check I/O stream pointer
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL ARROPN (NAME, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       I/O by access type
      GO TO (100, 200, 300, 400), ARRTYP(BUFNO)
C                                       Pixel
 100     IF (ARRPT(BUFNO).GT.ARRDIM(1,BUFNO)) THEN
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = 0
            END IF
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 0
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         PNT(1) = ARRPT(BUFNO) + 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return values
         DIM(1) = 1
         DIM(2) = 0
         DATA(1) = OBUFFR(BUFPNT(BUFNO)+ARRPT(BUFNO),BUFNO)
         ARRPT(BUFNO) = ARRPT(BUFNO) + 1
         GO TO 999
C                                       Row
 200     CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *      BUFPNT(BUFNO), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return values
         DIM(1) = ARRDIM(1,BUFNO)
         DIM(2) = 0
         CALL RCOPY (ARRDIM(1,BUFNO), OBUFFR(BUFPNT(BUFNO),BUFNO), DATA)
         GO TO 999
C                                       Plane
 300     ARRPT(BUFNO) = 1
         DO 320 LOOP = 1,ARRDIM(2,BUFNO)
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            CALL RCOPY (ARRDIM(1,BUFNO), OBUFFR(BUFPNT(BUFNO),BUFNO),
     *         DATA(ARRPT(BUFNO)))
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 320        CONTINUE
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return actual dimension
         DIM(1) = ARRDIM(1,BUFNO)
         DIM(2) = ARRDIM(2,BUFNO)
         DIM(3) = 0
         GO TO 999
C                                       Array
 400     ARRPT(BUFNO) = 1
C                                       Find total number of rows
         NROW = 1
         DO 420 LOOP = 2,7
            IF (ARRDIM(LOOP,BUFNO).GT.1) NROW = NROW *
     *         ARRDIM(LOOP,BUFNO)
 420        CONTINUE
         ARRPT(BUFNO) = 1
         DO 440 LOOP = 1,NROW
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            CALL RCOPY (ARRDIM(1,BUFNO), OBUFFR(BUFPNT(BUFNO),BUFNO),
     *         DATA(ARRPT(BUFNO)))
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 440        CONTINUE
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return actual dimension
         CALL COPY (6, ARRDIM(1,BUFNO), DIM)
         DIM(7) = 0
         GO TO 999
C                                       I/O error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
 995  MSGTXT = 'ARREAD: OBJECT IS ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ARREAD: I/O ERROR ',I4)
      END
      SUBROUTINE ARRWRI (NAME, DIM, DATA, IERR)
C-----------------------------------------------------------------------
C   Public
C   Stores a portion of an array.  Initiates I/O if necessary.
C   Access may be by pixel, row, plane or array as specified by
C   the value in DIM.  Mixed modes of access type are not allowed.
C   Inputs:
C      NAME  C*32  The name of the object.
C      DIM   I(*)  The dimensionality of the value in DATA.
C      DATA  R(*)  The portion of the array.
C   Output:
C      IERR  I     Error return code, 0=OK, -1 => I/O complete when done
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      REAL      DATA(*)
      INTEGER   DIM(*), IERR
C
      INTEGER   BUFNO, NROW, LOOP, PNT(7), IDIM(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Need to initialize?
C                                       Check I/O stream pointer
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL ARROPN (NAME, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       I/O by access type
      GO TO (100, 200, 300, 400), ARRTYP(BUFNO)
C                                       Pixel
 100     IF (ARRPT(BUFNO).GE.ARRDIM(1,BUFNO)) THEN
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = 0
            END IF
         OBUFFR(BUFPNT(BUFNO)+ARRPT(BUFNO),BUFNO) = DATA(1)
         ARRPT(BUFNO) = ARRPT(BUFNO) + 1
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 0
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         PNT(1) = ARRPT(BUFNO) + 2
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Row
 200     CALL RCOPY (ARRDIM(1,BUFNO), DATA, OBUFFR(BUFPNT(BUFNO),BUFNO))
         CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *      BUFPNT(BUFNO), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Plane
 300     ARRPT(BUFNO) = 1
         DO 320 LOOP = 1,ARRDIM(2,BUFNO)
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            CALL RCOPY (ARRDIM(1,BUFNO), DATA(ARRPT(BUFNO)),
     *         OBUFFR(BUFPNT(BUFNO),BUFNO))
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 320        CONTINUE
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Array
 400     ARRPT(BUFNO) = 1
C                                       Find total number of rows
         NROW = 1
         DO 420 LOOP = 2,7
            IF (ARRDIM(LOOP,BUFNO).GT.1) NROW = NROW *
     *         ARRDIM(LOOP,BUFNO)
 420        CONTINUE
         ARRPT(BUFNO) = 1
         DO 440 LOOP = 1,NROW
            CALL RCOPY (ARRDIM(1,BUFNO), DATA(ARRPT(BUFNO)),
     *         OBUFFR(BUFPNT(BUFNO),BUFNO))
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 440        CONTINUE
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       I/O error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
 995  MSGTXT = 'ARRWRI: OBJECT IS ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ARRWRI: I/O ERROR ',I4)
      END
      SUBROUTINE ARDRED (NAME, DIM, DATA, IERR)
C-----------------------------------------------------------------------
C   Public
C   Reads a portion of an array.  Initiates I/O if necessary.
C   Access may be by pixel, row, plane or array as specified by
C   ARRAY_PNT.ACCESS. Default = 'row'
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      DIM   I(*)  The dimensionality of the value returned in DATA
C      DATA  D(*)  The next row of the image
C      IERR  I     Error return code, 0=OK, -1 => I/O complete when done
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      DOUBLE PRECISION DATA(*)
      INTEGER   DIM(*), IERR
C
      INTEGER   BUFNO, NROW, LOOP, PNT(7), IV
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Need to initialize?
C                                       Check I/O stream pointer
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL ARROPN (NAME, 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       I/O by access type
      GO TO (100, 200, 300, 400), ARRTYP(BUFNO)
C                                       Pixel
 100     IF (ARRPT(BUFNO).GT.ARRDIM(1,BUFNO)) THEN
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = 0
            END IF
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 0
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         PNT(1) = ARRPT(BUFNO) + 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return values
         DIM(1) = 1
         DIM(2) = 0
         DATA(1) = OBUFFR(BUFPNT(BUFNO)+ARRPT(BUFNO),BUFNO)
         ARRPT(BUFNO) = ARRPT(BUFNO) + 1
         GO TO 999
C                                       Row
 200     CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *      BUFPNT(BUFNO), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return values
         DIM(1) = ARRDIM(1,BUFNO)
         DIM(2) = 0
         DO 210 IV = 1,ARRDIM(1,BUFNO)
            DATA(IV) = OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO)
 210        CONTINUE
         GO TO 999
C                                       Plane
 300     ARRPT(BUFNO) = 1
         DO 320 LOOP = 1,ARRDIM(2,BUFNO)
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            DO 310 IV = 1,ARRDIM(1,BUFNO)
               DATA(ARRPT(BUFNO)+IV-1) =
     *            OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO)
 310           CONTINUE
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 320        CONTINUE
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return actual dimension
         DIM(1) = ARRDIM(1,BUFNO)
         DIM(2) = ARRDIM(2,BUFNO)
         DIM(3) = 0
         GO TO 999
C                                       Array
 400     ARRPT(BUFNO) = 1
C                                       Find total number of rows
         NROW = 1
         DO 420 LOOP = 2,7
            IF (ARRDIM(LOOP,BUFNO).GT.1) NROW = NROW *
     *         ARRDIM(LOOP,BUFNO)
 420        CONTINUE
         ARRPT(BUFNO) = 1
         DO 440 LOOP = 1,NROW
            CALL ARRIO ('READ', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            DO 410 IV = 1,ARRDIM(1,BUFNO)
               DATA(ARRPT(BUFNO)+IV-1) =
     *            OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO)
 410           CONTINUE
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 440        CONTINUE
C                                       Array pointer
         DIM(1) = 7
         DIM(2) = 1
         DIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, DIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Return actual dimension
         CALL COPY (6, ARRDIM(1,BUFNO), DIM)
         DIM(7) = 0
         GO TO 999
C                                       I/O error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
 995  MSGTXT = 'ARDRED: OBJECT IS ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ARDRED: I/O ERROR ',I4)
      END
      SUBROUTINE ARDWRI (NAME, DIM, DATA, IERR)
C-----------------------------------------------------------------------
C   Public
C   Stores a portion of an array.  Initiates I/O if necessary.
C   Access may be by pixel, row, plane or array as specified by
C   the value in DIM.  Mixed modes of access type are not allowed.
C   Inputs:
C      NAME  C*32  The name of the object.
C      DIM   I(*)  The dimensionality of the value in DATA.
C      DATA  D(*)  The portion of the array.
C   Output:
C      IERR  I     Error return code, 0=OK, -1 => I/O complete when done
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      DOUBLE PRECISION DATA(*)
      INTEGER   DIM(*), IERR
C
      INTEGER   BUFNO, NROW, LOOP, PNT(7), IDIM(7), IV
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Need to initialize?
C                                       Check I/O stream pointer
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
         CALL ARROPN (NAME, 'WRIT', IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBINFO (NAME, BUFNO, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       I/O by access type
      GO TO (100, 200, 300, 400), ARRTYP(BUFNO)
C                                       Pixel
 100     IF (ARRPT(BUFNO).GE.ARRDIM(1,BUFNO)) THEN
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = 0
            END IF
         OBUFFR(BUFPNT(BUFNO)+ARRPT(BUFNO),BUFNO) = DATA(1)
         ARRPT(BUFNO) = ARRPT(BUFNO) + 1
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 0
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         PNT(1) = ARRPT(BUFNO) + 2
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Row
 200     DO 210 IV = 1,ARRDIM(1,BUFNO)
            OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO) = DATA(IV)
 210        CONTINUE
         CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *      BUFPNT(BUFNO), IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Plane
 300     ARRPT(BUFNO) = 1
         DO 320 LOOP = 1,ARRDIM(2,BUFNO)
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            DO 310 IV = 1,ARRDIM(1,BUFNO)
               OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO) =
     *            DATA(ARRPT(BUFNO)+IV-1)
 310           CONTINUE
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 320        CONTINUE
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       Array
 400     ARRPT(BUFNO) = 1
C                                       Find total number of rows
         NROW = 1
         DO 420 LOOP = 2,7
            IF (ARRDIM(LOOP,BUFNO).GT.1) NROW = NROW *
     *         ARRDIM(LOOP,BUFNO)
 420        CONTINUE
         ARRPT(BUFNO) = 1
         DO 440 LOOP = 1,NROW
            DO 410 IV = 1,ARRDIM(1,BUFNO)
               OBUFFR(BUFPNT(BUFNO)+IV-1,BUFNO) =
     *            DATA(ARRPT(BUFNO)+IV-1)
 410           CONTINUE
            CALL ARRIO ('WRIT', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *         BUFPNT(BUFNO), IERR)
            IF (IERR.NE.0) GO TO 980
            IF (LOOP.EQ.1) CALL COPY (7, ARRFDV(FDIMPT,BUFNO), PNT)
            ARRPT(BUFNO) = ARRPT(BUFNO) + ARRDIM(1,BUFNO)
 440        CONTINUE
C                                       Array pointer
         IDIM(1) = 7
         IDIM(2) = 1
         IDIM(3) = 1
         CALL ARPPUT (NAME, 'POSN', OOAINT, IDIM, PNT, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         GO TO 999
C                                       I/O error
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
 995  MSGTXT = 'ARDWRI: OBJECT IS ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ARDWRI: I/O ERROR ',I4)
      END
      SUBROUTINE ARRCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes I/O associated with an object flushing the buffer on write.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   BUFNO, DISK, CNO, TYPE, DIM(7), DUMMY
      CHARACTER DATYPE*8
      LOGICAL   DOCMPL
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       I/O open
C                                       Check I/O stream pointer
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (BUFNO.LE.0) THEN
C                                       Don't bother if closed.
         IERR = 0
         GO TO 999
      END IF
C                                       Close
      CALL ARRIO ('CLOS', NAME, ARRFDV(1,BUFNO), OBUFFR(1,BUFNO),
     *   BUFPNT(BUFNO), IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Update disk header if write
      IF (ARRFDV(FDRW,BUFNO).EQ.1) THEN
C                                       Find disk and slot
         CALL OBDSKC (NAME, DISK, CNO, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL OBHGET (NAME, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Complex?
         CALL ARDGET (NAME, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
         DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
         IERR = 0
C                                       Reset if complex
         IF (DOCMPL) CATBLK(KINAX) = CATBLK(KINAX) * 2
         CALL OBHPUT (NAME, CATBLK, IERR)
         IF (IERR.NE.0) GO TO 995
C                                       Store  CATBLK
         CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', SBUFF, IERR)
         IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
            WRITE (MSGTXT,1000) IERR
            GO TO 990
            END IF
         IERR = 0
         END IF
      CALL OBCLOS (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Deassign LUN
      CALL OBLUFR (ARRFDV(FDLUNI,BUFNO))
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARRCLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ARRCLO: CATIO ERROR ',I3,' UPDATING CATALOG')
      END
      SUBROUTINE ARROPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Sets up to open an array file
C   Inputs:
C      NAME   C*32  The name of the object.
C      STATUS C*4   'READ' or 'WRIT'
C   Output:
C      IERR   I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER   BUFNO, TYPE, DIM(7), BLC(7), TRC(7), DISK, CNO, DUMMY
      LOGICAL   DOCMPL
      CHARACTER ATYPE*8, DATYPE*8
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open array
      CALL OBOPEN (NAME, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (NAME, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (NAME, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Fetch disk header
C                                       Find disk and slot
      CALL OBDSKC (NAME, DISK, CNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Store  CATBLK
      CALL CATIO ('READ', DISK, CNO, CATBLK, 'REST', SBUFF, IERR)
      IF ((IERR.GE.1) .AND. (IERR.LE.4)) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IERR = 0
C                                       Reset if complex
      IF (DOCMPL) CATBLK(KINAX) = CATBLK(KINAX) / 2
      CALL OBHPUT (NAME, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Access type
      CALL ARPGET (NAME, 'ACCESS', TYPE, DIM, DUMMY, ATYPE, IERR)
C                                       Default = 'ROW'
      IF (IERR.NE.0) THEN
         ATYPE = 'ROW'
         IERR = 0
         END IF
      IF (ATYPE.EQ.'PIXEL') ARRTYP(BUFNO) = 1
      IF (ATYPE.EQ.'ROW') ARRTYP(BUFNO) = 2
      IF (ATYPE.EQ.'PLANE') ARRTYP(BUFNO) = 3
      IF (ATYPE.EQ.'ARRAY') ARRTYP(BUFNO) = 4
C                                       Set buffer pointer for read or
C                                       write.
      IF (STATUS.EQ.'READ') THEN
         ARRPT(BUFNO) = ARRDIM(1,BUFNO) + 1
      ELSE IF (STATUS.EQ.'WRIT') THEN
         ARRPT(BUFNO) = 0
      ELSE
C                                       Unknown status
         MSGTXT = 'ARROPN: UNKNOWN STATUS= ' // STATUS
         IERR = 1
         GO TO 990
         END IF
C                                       Get size of the window
      CALL ARRWIN (NAME, BLC, TRC, ARRDIM(1,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      IF (DOCMPL) ARRDIM(1,BUFNO) = ARRDIM(1,BUFNO) * 2
C                                       Init FDV.
      CALL FILL (80, 0, ARRFDV(1,BUFNO))
C                                       Buffer size
      ARRFDV(FDSZIB,BUFNO) = BUFSIZ * 2
C                                       Read or write
      IF (STATUS.EQ.'READ') THEN
         ARRFDV(FDRW,BUFNO) = 0
      ELSE
         ARRFDV(FDRW,BUFNO) = 1
         END IF
C                                       Assign LUN
      CALL OBLUN (ARRFDV(FDLUNI,BUFNO), IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARROPN: ERROR OPENING ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ARROPN: CATIO ERROR ',I3,' READING CATBLK')
      END
      SUBROUTINE ARRCHK (IN1, IN2, IERR)
C-----------------------------------------------------------------------
C   Private
C   Check that two arrays have the same structure
C   Inputs:
C      IN1   C*32  The name of the first image object.
C      IN2   C*32  The name of the second image object.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*)
      INTEGER   IERR
C
      INTEGER   NAXIS1(7), NAXIS2(7), BLC1(7), BLC2(7), TRC1(7),
     *   TRC2(7), LOOP, NDIM1, NDIM2, TYPE, DIM(7)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Dimensionality
      CALL ARDGET (IN1, 'NDIM', TYPE, DIM, NDIM1, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARDGET (IN2, 'NDIM', TYPE, DIM, NDIM2, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Axis dimensions, allow for
C                                       subimaging.
      CALL ARRWIN (IN1, BLC1, TRC1, NAXIS1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRWIN (IN2, BLC2, TRC2, NAXIS2, IERR)
      IF (IERR.NE.0) GO TO 995
      DO 100 LOOP = 1,7
         IF (LOOP.GT.NDIM1) NAXIS1(LOOP) = 1
         IF (LOOP.GT.NDIM2) NAXIS2(LOOP) = 1
         IF (NAXIS1(LOOP).NE.NAXIS2(LOOP)) THEN
            IERR = 1
            WRITE (MSGTXT,1000) LOOP, NAXIS1(LOOP), NAXIS2(LOOP)
            GO TO 990
            END IF
 100     CONTINUE
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
 995  MSGTXT = 'ARRCHK: FIRST OBJECT IS ' // IN1
      CALL MSGWRT (6)
      MSGTXT = 'ARRCHK: SECOND OBJECT IS ' // IN2
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ARRCHK: ARRAY AXIS ',I3,' HAVE UNEQUAL SIZE ',2I5)
      END
      SUBROUTINE ARRWIN (NAME, BLC, TRC, NDIM, IERR)
C-----------------------------------------------------------------------
C   Private
C   Determine the specified window in an image
C   Inputs:
C      NAME  C*32  The name of the image object.
C   Output:
C      BLC   I(*)  Bottom left corner
C      TRC   I(*)  Top Right corner
C      NDIM  I(*)  Dimension of the window
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   BLC(*), TRC(*), NDIM(*), IERR
C
      INTEGER   DIM(7), NAXIS(7), IBLC(7), ITRC(7), LOOP, TYPE, ND
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Dimensionality
      CALL ARDGET (NAME, 'NDIM', TYPE, DIM, ND, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Axis dimensions
      CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Specified BLC
      CALL ARDGET (NAME, 'BLC', TYPE, DIM, IBLC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, IBLC)
C                                       Specified TRC
      CALL ARDGET (NAME, 'TRC', TYPE, DIM, ITRC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, ITRC)
C                                       Fill BLC, TRC
      DO 100 LOOP = 1,7
         IF (LOOP.GT.ND) NAXIS(LOOP) = 1
         BLC(LOOP) = MAX (1, IBLC(LOOP))
         BLC(LOOP) = MIN (BLC(LOOP), NAXIS(LOOP))
         TRC(LOOP) = ITRC(LOOP)
         IF (TRC(LOOP).LE.0) TRC(LOOP) = NAXIS(LOOP)
         TRC(LOOP) = MIN (TRC(LOOP), NAXIS(LOOP))
         NDIM(LOOP) = MAX (1, (TRC(LOOP) - BLC(LOOP) + 1))
 100     CONTINUE
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRWIN: ERROR WITH WINDOW FOR ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRCWI (NAME, IERR)
C-----------------------------------------------------------------------
C   Private
C   Clears the window (BLC, TRC) for first two dimensions of an array
C   object
C   Inputs:
C      NAME  C*32  The name of the image object.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   DIM(7), NAXIS(7), IBLC(7), ITRC(7), TYPE
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Axis dimensions
      CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARDGET (NAME, 'BLC', TYPE, DIM, IBLC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, IBLC)
      CALL ARDGET (NAME, 'TRC', TYPE, DIM, ITRC, CDUMMY, IERR)
      IF (IERR.NE.0) CALL FILL (7, 0, ITRC)
C                                       Set BLC
      IBLC(1) = 1
      IBLC(2) = 1
      CALL ARDPUT (NAME, 'BLC', OOAINT, DIM, IBLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Set TRC
      ITRC(1) = 0
      ITRC(2) = NAXIS(2)
      CALL ARDPUT (NAME, 'TRC', OOAINT, DIM, ITRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARRCWI: ERROR RESETTING WINDOW: ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRIO (OPCODE, NAME, FDVEC, IBUFF, IPNT, IERR)
C-----------------------------------------------------------------------
C   Private
C   Reads or writes rows of a disk resident array.
C      Most of the necessary information needed to define the file,
C   windows etc. is saved in the array FDVEC.  A standard include
C   file IMGERR.INC defines pointers into this array and it is advised
C   to use these pointers rather than hardcoded subscripts.
C      Mixed reads and writes are not supported using the same LUNs and
C   buffers but ARRIO may be used with separate LUNs, buffers and FDVEC
C   to read and write the same file.
C      ARRIO keeps track of the max. and min. values when OPCODE='WRIT'.
C   Magic value blanking is supported and if ARRAY.ARRAY_DESC.BLANK is
C   non zero then the associated array contains blanked elements.
C   Usage notes:
C     2) Opening the file.  If ARRIO determines that the file is not
C        open it will do so.   Once the file is open the file
C        descriptor vector FDVEC must be used in each call.  No explicit
C        open call is needed.
C     3) Initialization.  ARRIO initializes the I/O using the values in
C        FDVEC when it opens the file.
C     4) Closing the file.  The file may be closed with a call with
C        opcode 'CLOS'.  If OPCODE is 'WRIT' then the buffers are
C        flushed before the file is closed.
C     5) Selecting a subset of the first axis is not allowed for
C        OPCODE='WRIT'.
C     6) Buffers will be flushed and files closed after the last call on
C        a specified subimage.
C     7) For OPCODE='WRIT' the initial values of IPNT is 1.
C   Inputs:
C      OPCODE C*4     Operation code: 'READ','WRIT','CLOS'
C      NAME   C*?     Name of object
C   Input/Output:
C      FDVEC  I(80)  File descriptor vector; indices are the pointers
C                    from IMGERR.INC. Used internally by ARRIO.
C                    FDDISK = Disk number to use.
C                    FDLUNI = LUN to use for image, set before first
C                             call.
C                    FDBLC = BLC of window in the image. 0's => all
C                            non-error axes. (array of 7 values)
C                    FDTRC = TRC of window in the image. 0's => all
C                            non-error axes. (array of 7 values)
C                    FDSZIB = Size of IBUFF in AIPS bytes (1/2 words)
C                    FDFTI = FTAB pointer for image I/O. > 0 =>file open
C                    FDERR = Subroutine error return code.
C                    FDPOS = pointer in image to word in image buffer
C                            pointed to by IPNT. (array of 7 values)
C                    FDNLEF = Number of rows left in current plane.
C                    FDRW   = 0 if read, 1 if write.
C                    FDIMSI = Image size (array of 7 values)
C                    FDIMPT = Plane pointer (array of 7 values)
C                    FDMAX  = Image max. value as REAL
C                    FDMIN  = Image min. value as REAL
C                    FDBLK  = 0 if no blanking, 1 if blanked.
C      IBUFF  R(*)   Buffer for Image I/O must be large enough for the
C                    largest transfer rounded up to the next larger
C                    number of disk blocks.
C   Outputs:
C      IPNT   I      The location in IBUFF of the start of the next
C                    record. Before the first write call this should
C                    be set to 1 to determine where to start filling
C                    IBUFF.
C     IERR    I      Error return: 0 => ok,
C                                  1 => Input error
C                                  2 => ZOPEN error
C                                  3 => ARDGET error
C                                  5 => MINIT error
C                                  6 => MDISK error
C                                  7 => ZCLOSE error
C                                  9 => COMOFF error
C                                  Subroutine error codes are returned
C                                  in FDVEC(FDERR).
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, NAME*(*)
      INTEGER   FDVEC(*), IPNT, IERR
      REAL      IBUFF(*)
C
      CHARACTER FNAME*48, OPTMP*4, DATYPE*8, CDUMMY*1
      INTEGER   BUFNO, BOI, WIN(4), LOOP, IMAXV, IMINV, NCHK, ITEMP,
     *   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7), IBLANK, DUMMY
      LOGICAL   DOCMPL
      REAL      MAXV, MINV, BLANK
      LOGICAL   NEXT, ISBLNK
      INCLUDE 'INCS:IMGERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IMAXV, MAXV),  (IMINV, MINV), (BLANK, IBLANK)
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
C                                       Check OPCODE
      IF ((OPCODE.NE.'READ') .AND. (OPCODE.NE.'WRIT') .AND.
     *   (OPCODE.NE.'CLOS')) THEN
         IERR = 1
         MSGTXT = 'ARRIO: UNKNOWN OPCODE =' // OPCODE
         GO TO 990
         END IF
C                                       Close?
      IERR = 3
      CALL OBINFO (NAME, BUFNO, FDVEC(FDERR))
      IF (FDVEC(FDERR).NE.0) GO TO 995
C                                       Close?
      IF (OPCODE.EQ.'CLOS') GO TO 800
C                                       Need to Open?  Check if
C                                       file is already opened.
      IF (FDVEC(FDFTI).LE.0) THEN
C                                       Get array descriptive info
C                                       File name
         IERR = 3
         CALL ARDGET (NAME, 'FNAME', TYPE, DIM, DUMMY, FNAME,
     *      FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) GO TO 995
C                                       Disk number
         CALL ARDGET (NAME, 'FDISK', TYPE, DIM, FDVEC(FDDISK), CDUMMY,
     *      FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) GO TO 995
C                                       Complex?
         CALL ARDGET (NAME, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
         DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
         IERR = 0
C                                       Dimensions, window
         CALL ARRWIN (NAME, BLC, TRC, NAXIS, FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) GO TO 995
         CALL ARDGET (NAME, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY,
     *      FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) GO TO 995
         IF (DOCMPL) THEN
            NAXIS(1) = NAXIS(1) * 2
            TRC(1) = TRC(1) * 2
            END IF
         CALL COPY (7, BLC, FDVEC(FDBLC))
         CALL COPY (7, TRC, FDVEC(FDTRC))
         CALL COPY (7, NAXIS, FDVEC(FDIMSI))
C                                       Save info - image size
C                                       Set default window.
         DO 200 LOOP = 1,7
            FDVEC(FDIMSI+LOOP-1) = MAX (FDVEC(FDIMSI+LOOP-1), 1)
            FDVEC(FDBLC+LOOP-1) = MAX (FDVEC(FDBLC+LOOP-1), 1)
            IF (FDVEC(FDTRC+LOOP-1).LE.0) FDVEC(FDTRC+LOOP-1) =
     *         FDVEC(FDIMSI+LOOP-1)
C                                       Init plane pointers.
            FDVEC(FDIMPT+LOOP-1) = FDVEC(FDBLC+LOOP-1)
 200        CONTINUE
C                                       Max/min and blanking
C                                       Note use of EQUIVALENCE
         FDVEC(FDBLK) = 0
         IF ((FDVEC(FDBLC+2).GT.1) .OR. (FDVEC(FDBLC+3).GT.1) .OR.
     *      (FDVEC(FDBLC+4).GT.1). OR. (FDVEC(FDBLC+5).GT.1)) THEN
C                                       Use old values if starting in
C                                       middle of image.
            IERR = 3
            CALL ARSGET (NAME, 'DATAMAX', TYPE, DIM, MAXV, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
            CALL ARSGET (NAME, 'DATAMIN', TYPE, DIM, MINV, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
C                                       Blanking?
            CALL ARDGET (NAME, 'BLANK', TYPE, DIM, BLANK, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
            IF ((FDVEC(FDERR).EQ.0) .AND. (BLANK.NE.0.0))
     *         FDVEC(FDBLK) = 1
            FDVEC(FDERR) = 0
         ELSE
C                                       Init.
            MAXV = -1.0E20
            MINV = 1.0E20
            END IF
C                                       Save values (need only for
C                                       'WRIT')
         FDVEC(FDMAX) = IMAXV
         FDVEC(FDMIN) = IMINV
C                                       Open file as necessary
         IF (FDVEC(FDFTI).LE.0) THEN
            CALL ZOPEN (FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDDISK),
     *         FNAME, .TRUE., .FALSE., .TRUE., FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 4
               WRITE (MSGTXT,1001) FDVEC(FDERR), NAME
               GO TO 990
               END IF
            END IF
C                                       Set values in FDVEC
C                                       Number of rows left - force init
         FDVEC(FDNLEF) = 0
C                                       Subroutine error
         FDVEC(FDERR) = 0
C                                       Read/write flag
         FDVEC(FDRW) = 0
         IF (OPCODE.EQ.'WRIT') FDVEC(FDRW) = 1
         END IF
C----------------------------------------------------------------------
C                                       Time to init files?
      IF (FDVEC(FDNLEF).LE.0) THEN
C                                       Done if pointer on 3rd axis .le.
C                                       0.
         IF (FDVEC(FDIMPT+2).LE.0) THEN
            MSGTXT = 'I/O FINISHED OR INACTIVE'
            GO TO 990
            END IF
C                                       Set window
         WIN(1) = FDVEC(FDBLC)
         WIN(2) = FDVEC(FDBLC+1)
         WIN(3) = FDVEC(FDTRC)
         WIN(4) = FDVEC(FDTRC+1)
C                                       Block offset for image
         CALL COMOFF (KICTPN, FDVEC(FDIMSI), FDVEC(FDIMPT+2), BOI,
     *      FDVEC(FDERR))
         BOI = BOI + 1
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 9
            MSGTXT = 'ARRIO: ERROR IN COMOFF COMPUTING BLOCK OFFSET'
            GO TO 990
            END IF
C                                       Init I/O
         CALL MINIT (OPCODE, FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDIMSI),
     *      FDVEC(FDIMSI+1), WIN, IBUFF, FDVEC(FDSZIB), BOI,
     *      FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 5
            WRITE (MSGTXT,1002) FDVEC(FDERR) ,NAME
            GO TO 990
            END IF
C                                       Number of rows to read
         FDVEC(FDNLEF) = ABS (WIN(4) - WIN(2)) + 1
C                                       Set plane pointers for next
C                                       plane.
         NEXT = .TRUE.
         DO 300 LOOP = 3,7
            IF (NEXT .AND. (LOOP.NE.(FDVEC(FDERPL)+1))) THEN
               FDVEC(FDIMPT+LOOP-1) = FDVEC(FDIMPT+LOOP-1) + 1
               NEXT = FDVEC(FDIMPT+LOOP-1) .GT. FDVEC(FDTRC+LOOP-1)
               IF (NEXT) FDVEC(FDIMPT+LOOP-1) = FDVEC(FDBLC+LOOP-1)
               END IF
 300        CONTINUE
C                                       If NEXT still true then done
         IF (NEXT) FDVEC(FDIMPT+2) = -1
         FDVEC(FDIMPT+1) = FDVEC(FDBLC+1)
C                                       Dummy first write
         IF (FDVEC(FDRW).EQ.1) THEN
            CALL MDISK ('WRIT', FDVEC(FDLUNI), FDVEC(FDFTI), IBUFF,
     *         IPNT, FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 6
               WRITE (MSGTXT,1003) FDVEC(FDERR), 'WRIT', NAME
               GO TO 990
               END IF
            END IF
         END IF
C-----------------------------------------------------------------------
C                                       Do I/O operations.
C                                       Check OPCODE
      IF (((OPCODE.EQ.'READ').AND.(FDVEC(FDRW).NE.0)) .OR.
     *   ((OPCODE.EQ.'WRIT').AND.(FDVEC(FDRW).NE.1))) THEN
         IERR = 1
         MSGTXT = 'ARRIO:OPCODE INCOMPATIBLE WITH PREVIOUS USAGE'
         GO TO 990
         END IF
      OPTMP = OPCODE
C                                       Max, min etc for WRIT
      IF (FDVEC(FDRW).EQ.1) THEN
         IMAXV = FDVEC(FDMAX)
         IMINV = FDVEC(FDMIN)
         ISBLNK = FDVEC(FDBLK).EQ.1
         END IF
C                                       Flush buffer?
      IF ((FDVEC(FDNLEF).LE.1) .AND. (OPCODE.EQ.'WRIT')) THEN
         OPTMP = 'FINI'
         END IF
C                                       I/O to image
      ITEMP = IPNT
      CALL MDISK (OPTMP, FDVEC(FDLUNI), FDVEC(FDFTI), IBUFF, IPNT,
     *   FDVEC(FDERR))
      IF (FDVEC(FDERR).NE.0) THEN
         IERR = 6
         WRITE (MSGTXT,1003) FDVEC(FDERR), OPTMP, NAME
         GO TO 990
         END IF
C                                       Max/min etc for 'WRIT'
      IF (FDVEC(FDRW).EQ.1) THEN
         NCHK = FDVEC(FDTRC) - FDVEC(FDBLC) + 1
         NCHK = MIN (NCHK, MAXIMG)
         CALL CHKBLK (NCHK, IBUFF(ITEMP), VALIDX)
         DO 500 LOOP = 1,NCHK
            IF (VALIDX(LOOP)) THEN
               MAXV = MAX (MAXV, IBUFF(ITEMP+LOOP-1))
               MINV = MIN (MINV, IBUFF(ITEMP+LOOP-1))
            ELSE
               ISBLNK = .TRUE.
               END IF
 500        CONTINUE
         END IF
C                                       Decrement line count
      FDVEC(FDNLEF) = FDVEC(FDNLEF) - 1
      FDVEC(FDIMPT+1) = FDVEC(FDIMPT+1) + 1
C                                       Save max, min etc for WRIT
      IF (FDVEC(FDRW).EQ.1) THEN
         FDVEC(FDMAX) = IMAXV
         FDVEC(FDMIN) = IMINV
         FDVEC(FDBLK) = 0
         IF (ISBLNK) FDVEC(FDBLK) = 1
         END IF
C                                       Reset pointers if flushed
C                                       buffers.
      IF (OPTMP.EQ.'FINI') THEN
         IPNT = 1
         FDVEC(FDNLEF) = -1
         END IF
      IERR = 0
      GO TO 999
C----------------------------------------------------------------------
C                                       Close files etc.  Make sure
C                                       actually opened.
 800  IF (FDVEC(FDFTI).GT.0) THEN
C                                       Flush buffer on write if not
C                                       already done.
         IF ((FDVEC(FDRW).EQ.1) .AND. (FDVEC(FDNLEF).GT.1)) THEN
            CALL MDISK ('FINI', FDVEC(FDLUNI), FDVEC(FDFTI), IBUFF,
     *         IPNT,  FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) THEN
               IERR = 6
               WRITE (MSGTXT,1003) FDVEC(FDERR), 'FINI', NAME
               GO TO 990
            END IF
         END IF
         CALL  ZCLOSE (FDVEC(FDLUNI), FDVEC(FDFTI), FDVEC(FDERR))
         IF (FDVEC(FDERR).NE.0) THEN
            IERR = 7
            WRITE (MSGTXT,1801) FDVEC(FDERR)
            GO TO 990
            END IF
C                                       Update Statistics on 'WRIT'
         IF (FDVEC(FDRW).EQ.1) THEN
            IMAXV = FDVEC(FDMAX)
            IMINV = FDVEC(FDMIN)
            IERR = 3
            DIM(1) = 1
            DIM(2) = 1
            DIM(3) = 0
            CALL ARSPUT (NAME, 'DATAMAX', OOARE, DIM, MAXV, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
            CALL ARSPUT (NAME, 'DATAMIN', OOARE, DIM, MINV, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
C                                       Blanking?
            BLANK = 0.0
            IF (FDVEC(FDBLK).EQ.1) BLANK = FBLANK
            DIM(1) = 1
            CALL ARDPUT (NAME, 'BLANK', OOARE, DIM, BLANK, CDUMMY,
     *         FDVEC(FDERR))
            IF (FDVEC(FDERR).NE.0) GO TO 995
            END IF
C                                       Clear FDVEC FTAB pointer
         FDVEC(FDFTI) = 0
         END IF
C                                       Finished
      IERR = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARRIO: ERROR WITH ' // NAME
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1001 FORMAT ('ARRIO: ERROR ',I3,' OPENING ',A)
 1002 FORMAT ('ARRIO: ERROR ',I3,' INITING ',A)
 1003 FORMAT ('ARRIO: ERROR ',I3,1X,A,'ING ',A)
 1801 FORMAT ('ARRIO: ZCLOSE ERROR ',I3)
      END
      SUBROUTINE ARRNEG (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Negate an array and write another.
C   Inputs:
C      IN    C*32  The name of the input object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNOI, BUFNOO,
     *   PNTI, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN.EQ.OUT) THEN
C                                       Create temporary output object
         TIN = 'Temporary IMAGE for ARRNEG'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN = IN
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (TIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (TIN, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARRIO ('READ', TIN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *    BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
C                                       Negate row with blanking
         CALL CHKBLK (LROWC, OBUFFR(PNTI,BUFNOI), VALID1)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBNEG (OBUFFR(PNTI,BUFNOI), VALID1,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBNEG (OBUFFR(PNTI,BUFNOI), VALID1,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       Negate row
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVNEG (OBUFFR(PNTI,BUFNOI), OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVNEG (OBUFFR(PNTI,BUFNOI), OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN(1:32).NE.IN(1:32)) CALL IMGDES (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRNEG: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRNEG: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRADD (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Add two arrays and write a third.
C   Inputs:
C      IN1   C*32  The name of the first object.
C      IN2   C*32  The name of the second object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNI1, BUFNI2,
     *   BUFNOO, PNTI1, PNTI2, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN1*32, TIN2*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN1.EQ.OUT) THEN
C                                       Create temporary output object
         TIN1 = 'Temporary IMAGE 1 for ARRADD'
         CALL IMGCOP (IN1, TIN1, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN1 = IN1
         END IF
      IF (IN1.EQ.IN2) THEN
C                                       Create temporary
         TIN2 = 'Temporary IMAGE 2 for ARRADD'
         CALL IMGCOP (IN2, TIN2, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN2 = IN2
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (TIN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN1, BUFNI1, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input
      CALL ARROPN (TIN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN2, BUFNI2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN1, TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCHK (TIN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Image window
      CALL ARRWIN (TIN1, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN1, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN1, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      CALL ARDGET (TIN2, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = ISBLNK .OR. ((BLANK.NE.0.0) .AND. (IERR.EQ.0))
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input rows
      CALL ARRIO ('READ', TIN1, ARRFDV(1,BUFNI1), OBUFFR(1,BUFNI1),
     *    BUFPNT(BUFNI1), IERR)
      PNTI1 = BUFPNT(BUFNI1)
      IF (IERR.GT.0) GO TO 995
      CALL ARRIO ('READ', TIN2, ARRFDV(1,BUFNI2), OBUFFR(1,BUFNI2),
     *    BUFPNT(BUFNI2), IERR)
      PNTI2 = BUFPNT(BUFNI2)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
         CALL CHKBLK (LROWC, OBUFFR(PNTI1,BUFNI1), VALID1)
         CALL CHKBLK (LROWC, OBUFFR(PNTI2,BUFNI2), VALID2)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBADD (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBADD (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       No blanking
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVADD (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVADD (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN1(1:32).NE.IN1(1:32)) CALL IMGDES (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (TIN2(1:32).NE.IN2(1:32)) CALL IMGDES (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRADD: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'ARRADD: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'ARRADD: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRSUB (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Subtracts array in2 from array in1.
C   Inputs:
C      IN1   C*32  The name of the first object.
C      IN2   C*32  The name of the second object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNI1, BUFNI2,
     *   BUFNOO, PNTI1, PNTI2, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN1*32, TIN2*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN1.EQ.OUT) THEN
C                                       Create temporary output object
         TIN1 = 'Temporary IMAGE 1 for ARRSUB'
         CALL IMGCOP (IN1, TIN1, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN1 = IN1
         END IF
      IF (IN1.EQ.IN2) THEN
C                                       Create temporary
         TIN2 = 'Temporary IMAGE 2 for ARRSUB'
         CALL IMGCOP (IN2, TIN2, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN2 = IN2
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (TIN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN1, BUFNI1, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input
      CALL ARROPN (TIN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN2, BUFNI2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN1, TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCHK (TIN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Image window
      CALL ARRWIN (TIN1, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN1, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN1, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      CALL ARDGET (TIN2, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = ISBLNK .OR. ((BLANK.NE.0.0) .AND. (IERR.EQ.0))
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input rows
      CALL ARRIO ('READ', TIN1, ARRFDV(1,BUFNI1), OBUFFR(1,BUFNI1),
     *    BUFPNT(BUFNI1), IERR)
      PNTI1 = BUFPNT(BUFNI1)
      IF (IERR.GT.0) GO TO 995
      CALL ARRIO ('READ', TIN2, ARRFDV(1,BUFNI2), OBUFFR(1,BUFNI2),
     *    BUFPNT(BUFNI2), IERR)
      PNTI2 = BUFPNT(BUFNI2)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
         CALL CHKBLK (LROWC, OBUFFR(PNTI1,BUFNI1), VALID1)
         CALL CHKBLK (LROWC, OBUFFR(PNTI2,BUFNI2), VALID2)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBSUB (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBSUB (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       No blanking
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVSUB (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVSUB (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN1(1:32).NE.IN1(1:32)) CALL IMGDES (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (TIN2(1:32).NE.IN2(1:32)) CALL IMGDES (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRSUB: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'ARRSUB: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'ARRSUB: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRMUL (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Multiply two arrays and write a third.
C   Inputs:
C      IN1   C*32  The name of the first object.
C      IN2   C*32  The name of the second object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNI1, BUFNI2,
     *   BUFNOO, PNTI1, PNTI2, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN1*32, TIN2*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN1.EQ.OUT) THEN
C                                       Create temporary output object
         TIN1 = 'Temporary IMAGE 1 for ARRMUL'
         CALL IMGCOP (IN1, TIN1, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN1 = IN1
         END IF
      IF (IN1.EQ.IN2) THEN
C                                       Create temporary
         TIN2 = 'Temporary IMAGE 2 for ARRMUL'
         CALL IMGCOP (IN2, TIN2, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN2 = IN2
         END IF
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (TIN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN1, BUFNI1, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input
      CALL ARROPN (TIN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN2, BUFNI2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN1, TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCHK (TIN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Image window
      CALL ARRWIN (TIN1, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN1, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN1, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      CALL ARDGET (TIN2, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = ISBLNK .OR. ((BLANK.NE.0.0) .AND. (IERR.EQ.0))
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input rows
      CALL ARRIO ('READ', TIN1, ARRFDV(1,BUFNI1), OBUFFR(1,BUFNI1),
     *    BUFPNT(BUFNI1), IERR)
      PNTI1 = BUFPNT(BUFNI1)
      IF (IERR.GT.0) GO TO 995
      CALL ARRIO ('READ', TIN2, ARRFDV(1,BUFNI2), OBUFFR(1,BUFNI2),
     *    BUFPNT(BUFNI2), IERR)
      PNTI2 = BUFPNT(BUFNI2)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
         CALL CHKBLK (LROWC, OBUFFR(PNTI1,BUFNI1), VALID1)
         CALL CHKBLK (LROWC, OBUFFR(PNTI2,BUFNI2), VALID2)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBMUL (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBMUL (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       No blanking
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVMUL (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVMUL (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN1(1:32).NE.IN1(1:32)) CALL IMGDES (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (TIN2(1:32).NE.IN2(1:32)) CALL IMGDES (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRMUL: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'ARRMUL: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'ARRMUL: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRDIV (IN1, IN2, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Divides array in1 by array in2.
C   Inputs:
C      IN1   C*32  The name of the first object.
C      IN2   C*32  The name of the second object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN1*(*), IN2*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNI1, BUFNI2,
     *   BUFNOO, PNTI1, PNTI2, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN1*32, TIN2*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN1.EQ.OUT) THEN
C                                       Create temporary output object
         TIN1 = 'Temporary IMAGE 1 for ARRDIV'
         CALL IMGCOP (IN1, TIN1, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN1 = IN1
         END IF
      IF (IN1.EQ.IN2) THEN
C                                       Create temporary
         TIN2 = 'Temporary IMAGE 2 for ARRDIV'
         CALL IMGCOP (IN2, TIN2, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN2 = IN2
         END IF
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (TIN1, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN1, BUFNI1, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input
      CALL ARROPN (TIN2, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN2, BUFNI2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN1, TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCHK (TIN1, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Image window
      CALL ARRWIN (TIN1, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN1, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN1, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      CALL ARDGET (TIN2, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = ISBLNK .OR. ((BLANK.NE.0.0) .AND. (IERR.EQ.0))
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input rows
      CALL ARRIO ('READ', TIN1, ARRFDV(1,BUFNI1), OBUFFR(1,BUFNI1),
     *    BUFPNT(BUFNI1), IERR)
      PNTI1 = BUFPNT(BUFNI1)
      IF (IERR.GT.0) GO TO 995
      CALL ARRIO ('READ', TIN2, ARRFDV(1,BUFNI2), OBUFFR(1,BUFNI2),
     *    BUFPNT(BUFNI2), IERR)
      PNTI2 = BUFPNT(BUFNI2)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
         CALL CHKBLK (LROWC, OBUFFR(PNTI1,BUFNI1), VALID1)
         CALL CHKBLK (LROWC, OBUFFR(PNTI2,BUFNI2), VALID2)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBDIV (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBDIV (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       No blanking
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVDIV (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVDIV (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN1(1:32).NE.IN1(1:32)) CALL IMGDES (TIN1, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (TIN2(1:32).NE.IN2(1:32)) CALL IMGDES (TIN2, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRDIV: FIRST INPUT ' // IN1
      CALL MSGWRT (7)
      MSGTXT = 'ARRDIV: SECOND INPUT ' // IN2
      CALL MSGWRT (7)
      MSGTXT = 'ARRDIV: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRPAD (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy one array to another padding pixels outside of the input array
C   with PADVALUE (default 0.0) and replacing blanks with the pad value.
C   Inputs:
C      IN    C*?   The name of the input array.
C      OUT   C*?   The name of the output array.
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NAXIS(7), BUFNOI, BUFNOO, PNTI, PNTO,
     *   NAXO(7), BLCO(7), TRCO(7), LOOP, OFF, MAXL(7), MINL(7), TYPE,
     *   DIM(7), DUMMY, MSGSAV
      LOGICAL   OK2, OK3, OK4, OK5, OK6, OK7, OKROW, DOCMPL, ISBLNK
      CHARACTER DATYPE*8, CDUMMY*1
      REAL      BLANK, PADVAL
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
      MSGSAV = MSGSUP
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (IN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Windows
      CALL ARRWIN (IN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRWIN (OUT, BLCO, TRCO, NAXO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (IN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
C                                       Blanking?
      CALL ARDGET (IN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
C                                       Pad value
      MSGSUP = 32000
      CALL OGET (IN, 'PADVALUE', TYPE, DIM, PADVAL, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         IERR = 0
         PADVAL = 0.0
         END IF
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility, OUT must be
C                                       at least as large as the window
C                                       in IN.
      DO 10 LOOP = 1,7
         IF (NAXO(LOOP).LT.NAXIS(LOOP)) THEN
            IERR = 2
            WRITE (MSGTXT,1000) LOOP
            GO TO 990
            END IF
C                                       Set max and min loop values to
C                                       copy data
         MINL(LOOP) = NAXO(LOOP)/2 + 1 - NAXIS(LOOP)/2
         MAXL(LOOP) = MINL(LOOP) + NAXIS(LOOP) - 1
 10      CONTINUE
C                                       Loop over array
      LROW = TRCO(1) - BLCO(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = 1,NAXO(7)
         OK7 = ((LOOP7.GE.MINL(7)) .AND. (LOOP7.LE.MAXL(7)))
         DO 600 LOOP6 = 1,NAXO(6)
            OK6 = ((LOOP6.GE.MINL(6)) .AND. (LOOP6.LE.MAXL(6)))
            DO 500 LOOP5 = 1,NAXO(5)
               OK5 = ((LOOP5.GE.MINL(5)) .AND. (LOOP5.LE.MAXL(5)))
               DO 400 LOOP4 = 1,NAXO(4)
                  OK4 = ((LOOP4.GE.MINL(4)) .AND. (LOOP4.LE.MAXL(4)))
                  DO 300 LOOP3 = 1,NAXO(3)
                     OK3 = ((LOOP3.GE.MINL(3)) .AND.
     *                  (LOOP3.LE.MAXL(3)))
                     DO 200 LOOP2 = 1,NAXO(2)
                        OK2 = ((LOOP2.GE.MINL(2)) .AND.
     *                     (LOOP2.LE.MAXL(2)))
                        OKROW = OK2 .AND. OK3 .AND. OK4 .AND. OK5 .AND.
     *                     OK6. AND. OK7
                        OFF = MINL(1) - 1
C                                       Zero fill output
      CALL RFILL (LROWC, PADVAL, OBUFFR(BUFPNT(BUFNOO),BUFNOO))
C                                       Fetch input row and copy
      IF (OKROW) THEN
         CALL ARRIO ('READ', IN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *      BUFPNT(BUFNOI), IERR)
         IF (IERR.GT.0) GO TO 995
         PNTI = BUFPNT(BUFNOI)
C                                       Copy
         IF (ISBLNK) THEN
            CALL RCOPYS (NAXIS(1), BLANK, PADVAL, OBUFFR(PNTI,BUFNOI),
     *         OBUFFR(PNTO+OFF,BUFNOO))
         ELSE
            CALL RCOPY (NAXIS(1), OBUFFR(PNTI,BUFNOI),
     *         OBUFFR(PNTO+OFF,BUFNOO))
            END IF
         END IF
C                                       Store
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARRPAD: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRPAD: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ARRPAD: INPUT ARRAY SIZE LARGER THAN OUTPUT ON AXIS', I3)
      END
      SUBROUTINE ARRSCL (IN, FACTOR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Scale an array with a constant real factor.
C   Inputs:
C      IN     C*32  The name of the input array object.
C      FACTOR R     Scaling factor
C      OUT    C*32  The name of the output array object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      FACTOR
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NAXIS(7), DIM(7), TYPE, BUFNOI, BUFNOO,
     *   PNTI, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      COMPLEX   CFACT
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN.EQ.OUT) THEN
C                                       Create temporary output object
         TIN = 'Temporary IMAGE for ARRSCL'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN = IN
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (TIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex factor
      CFACT = CMPLX (FACTOR, 0.0)
C                                       Window
      CALL ARRWIN (TIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARRIO ('READ', TIN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *    BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
C                                       Scale row with blanking
         CALL CHKBLK (LROWC, OBUFFR(PNTI,BUFNOI), VALID1)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBSCL (OBUFFR(PNTI,BUFNOI), VALID1, CFACT,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBSCL (OBUFFR(PNTI,BUFNOI), VALID1, FACTOR,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       Scale row
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVSCL (OBUFFR(PNTI,BUFNOI), CFACT,
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVSCL (OBUFFR(PNTI,BUFNOI), FACTOR,
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN(1:32).NE.IN(1:32)) CALL IMGDES (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRSCL: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRSCL: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARSSET (IN, IERR)
C-----------------------------------------------------------------------
C   Public
C   Determines array statistics and updates ARRAY_STAT; any windowing
C   (BLC, TRC) and blanking are honored.
C      For complex arrays the statistics are for the modulus of the
C   values.
C   Inputs:
C      IN     C*32 The name of the input object
C   Output:
C      IERR   I    Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP1, LOOP2, LOOP3, LOOP4, LOOP5,
     *   LOOP6, LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNOI, PNTI,
     *   NNDIM, DUMMY
      CHARACTER DATYPE*8, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INTEGER   NUMPIX, PIXMAX(7), PIXMIN(7)
      REAL      VALUE, DATMAX, DATMIN, DATMEN, DATRMS
      DOUBLE PRECISION DATSUM, DATSU2
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open array and get I/O stream
      PNTI = 1
      CALL ARROPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (IN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (IN, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Number of dimensions
      CALL ARDGET (IN, 'NDIM', TYPE, DIM, NNDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (IN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (IN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IF (.NOT.ISBLNK) BLANK = FBLANK
      IERR = 0
C                                       Init statistics
      NUMPIX = 0
      CALL FILL (7, 0, PIXMAX)
      CALL FILL (7, 0, PIXMIN)
      DATMAX = -1.0E25
      DATMIN = 1.0E25
      DATSUM = 0.0D0
      DATSU2 = 0.0D0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       read row
      CALL ARRIO ('READ', IN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *   BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Do statistics
      IF (DOCMPL) THEN
C                                       Complex
         DO 110 LOOP1 = 1,LROW
            IF ((OBUFFR(PNTI,BUFNOI).NE.BLANK) .AND.
     *         (OBUFFR(PNTI+1,BUFNOI).NE.BLANK)) THEN
               VALUE = SQRT ((OBUFFR(PNTI,BUFNOI)**2) +
     *            (OBUFFR(PNTI+1,BUFNOI)**2))
               NUMPIX = NUMPIX + 1
               DATSUM = DATSUM + VALUE
               DATSU2 = DATSU2 + VALUE * VALUE
               IF (VALUE.GT.DATMAX) THEN
                  DATMAX = VALUE
                  PIXMAX(1) = LOOP1 + BLC(1) - 1
                  PIXMAX(2) = LOOP2
                  PIXMAX(3) = LOOP3
                  PIXMAX(4) = LOOP4
                  PIXMAX(5) = LOOP5
                  PIXMAX(6) = LOOP6
                  PIXMAX(7) = LOOP7
                  END IF
               IF (VALUE.LT.DATMIN) THEN
                  DATMIN = VALUE
                  PIXMIN(1) = LOOP1 + BLC(1) - 1
                  PIXMIN(2) = LOOP2
                  PIXMIN(3) = LOOP3
                  PIXMIN(4) = LOOP4
                  PIXMIN(5) = LOOP5
                  PIXMIN(6) = LOOP6
                  PIXMIN(7) = LOOP7
                  END IF
               END IF
            PNTI = PNTI + 2
 110        CONTINUE
      ELSE
C                                       Real
         DO 120 LOOP1 = 1,LROW
            IF (OBUFFR(PNTI,BUFNOI).NE.BLANK) THEN
               VALUE = OBUFFR(PNTI,BUFNOI)
               NUMPIX = NUMPIX + 1
               DATSUM = DATSUM + VALUE
               DATSU2 = DATSU2 + VALUE * VALUE
               IF (VALUE.GT.DATMAX) THEN
                  DATMAX = VALUE
                  PIXMAX(1) = LOOP1 + BLC(1) - 1
                  PIXMAX(2) = LOOP2
                  PIXMAX(3) = LOOP3
                  PIXMAX(4) = LOOP4
                  PIXMAX(5) = LOOP5
                  PIXMAX(6) = LOOP6
                  PIXMAX(7) = LOOP7
                  END IF
               IF (VALUE.LT.DATMIN) THEN
                  DATMIN = VALUE
                  PIXMIN(1) = LOOP1 + BLC(1) - 1
                  PIXMIN(2) = LOOP2
                  PIXMIN(3) = LOOP3
                  PIXMIN(4) = LOOP4
                  PIXMIN(5) = LOOP5
                  PIXMIN(6) = LOOP6
                  PIXMIN(7) = LOOP7
                  END IF
               END IF
            PNTI = PNTI + 1
 120        CONTINUE
         END IF
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close file
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Determine statistics
      IF (NUMPIX.GT.1) THEN
         DATMEN = DATSUM / NUMPIX
      ELSE
         DATMEN = 0.0
         DATMAX = 0.0
         DATMIN = 0.0
         END IF
      IF (NUMPIX.GT.3) THEN
         DATRMS = SQRT ((DATSU2 - (DATSUM*DATSUM/NUMPIX)) / (NUMPIX-1))
      ELSE
         DATRMS = 0.0
         END IF
C                                       Save values in ARRAY_STAT
      DIM(1) = 1
      DIM(2) = 1
      DIM(3) = 3
      CALL ARSPUT (IN, 'DATARMS', OOARE, DIM, DATRMS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (IN, 'DATAMEAN', OOARE, DIM, DATMEN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (IN, 'DATAMAX', OOARE, DIM, DATMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (IN, 'DATAMIN', OOARE, DIM, DATMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (IN, 'NUMPIXEL', OOAINT, DIM, NUMPIX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      DIM(1) = NNDIM
      CALL ARSPUT (IN, 'PIXMAX', OOAINT, DIM, PIXMAX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARSPUT (IN, 'PIXMIN', OOAINT, DIM, PIXMIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARSSET: ERROR DETERMING STAT. FOR ' // IN
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARHIST (IN, NHIS, HMIN, HMAX, HIST, IERR)
C-----------------------------------------------------------------------
C   Public
C   Return histogram of array; any windowing (BLC, TRC) and blanking are
C   honored. For complex arrays the histogram is of the modulus of the
C   values. Values less than HMIN are included in HIST(1) and greater
C   than HMAX in HIST(NHIS).
C   Inputs:
C      IN     C*32 The name of the input object
C      NHIS   I    Number of elements in HIST.
C   Input/Output:
C      HMIN   R    Minimum value for histogram.
C      HMAX   R    Maximum value for histogram.  If HMIN=HMAX=0 then
C                  PIXMIN and PIXMAX from the ARRAY_STAT are used.
C   Output:
C      HIST   I    Histogram of pixel values.
C      IERR   I    Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*)
      REAL      HMAX, HMIN
      INTEGER   NHIS, HIST(NHIS), IERR
C
      INTEGER   BLC(7), TRC(7), LOOP1, LOOP2, LOOP3, LOOP4, LOOP5,
     *   LOOP6, LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNOI, PNTI,
     *   NNDIM, INDX, DUMMY
      CHARACTER DATYPE*8, CDUMMY*1
      REAL      BLANK, DELTA, VALUE
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open array and get I/O stream
      PNTI = 1
      CALL ARROPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (IN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (IN, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Number of dimensions
      CALL ARDGET (IN, 'NDIM', TYPE, DIM, NNDIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (IN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (IN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IF (.NOT.ISBLNK) BLANK = FBLANK
      IERR = 0
C                                       Set up for histogram
      CALL FILL (NHIS, 0, HIST)
C                                       Default range
      IF ((HMAX.EQ.HMIN) .AND. (HMAX.EQ.0.0)) THEN
         CALL ARSGET (IN, 'PIXMIN', TYPE, DIM, HMIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         CALL ARSGET (IN, 'PIXMAX', TYPE, DIM, HMAX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       Check histogram size
      IF (NHIS.LE.1) THEN
         MSGTXT = 'ARHIST: BAD HISTOGRAM SIZE'
         IERR = 1
         GO TO 990
         END IF
C                                       Cellsize
      DELTA = (HMAX - HMIN) / NHIS
      IF (ABS (DELTA).GT.1.0E-20) THEN
         DELTA = 1.0 / DELTA
      ELSE
C                                       Bad range.
         MSGTXT = 'ARHIST: DEGENERATE, NONDEFAULT RANGE'
         IERR = 1
         GO TO 990
         END IF
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       read row
      CALL ARRIO ('READ', IN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *   BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Do histogram
      IF (DOCMPL) THEN
C                                       Complex
         DO 110 LOOP1 = 1,LROW
            IF ((OBUFFR(PNTI,BUFNOI).NE.BLANK) .AND.
     *         (OBUFFR(PNTI+1,BUFNOI).NE.BLANK)) THEN
               VALUE = SQRT ((OBUFFR(PNTI,BUFNOI)**2) +
     *            (OBUFFR(PNTI+1,BUFNOI)**2))
               INDX = ((VALUE - HMIN) * DELTA) + 1
               INDX = MAX (1, MIN (NHIS, INDX))
               HIST(INDX) = HIST(INDX) + 1
               END IF
            PNTI = PNTI + 2
 110        CONTINUE
      ELSE
C                                       Real
         DO 120 LOOP1 = 1,LROW
            IF (OBUFFR(PNTI,BUFNOI).NE.BLANK) THEN
               VALUE = OBUFFR(PNTI,BUFNOI)
               INDX = ((VALUE - HMIN) * DELTA) + 1
               INDX = MAX (1, MIN (NHIS, INDX))
               HIST(INDX) = HIST(INDX) + 1
               END IF
            PNTI = PNTI + 1
 120        CONTINUE
         END IF
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close file
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 990  CALL MSGWRT (7)
 995  MSGTXT = 'ARHIST: ERROR IN HISTOGRAM FOR ' // IN
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE CHKBLK (N, DATA, VALID)
C-----------------------------------------------------------------------
C   Public
C   Returns a blanking array to indicate which elements of an array are
C   blanked.
C   Inputs:
C      N     I     Element count
C      DATA  R(*)  Data array
C   Output:
C      VALID L(*)  True if corresponding element in DATA is valid.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      DATA(*)
      LOGICAL   VALID(*)
C
      INTEGER   LOOP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         VALID(LOOP) = DATA(LOOP).NE.FBLANK
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SETBLK (N, VALID, DATA)
C-----------------------------------------------------------------------
C   Public
C   Blanks elements of an array as indicated by a logical array.
C   Inputs:
C      N     I     Element count
C      VALID L(*)  True if corresponding element in DATA is valid.
C   Input/Output:
C      DATA  R(*)  Data array
C-----------------------------------------------------------------------
      INTEGER   N
      LOGICAL   VALID(*)
      REAL      DATA(*)
C
      INTEGER   LOOP
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      DO 100 LOOP = 1,N
         IF (.NOT.VALID(LOOP)) DATA(LOOP) = FBLANK
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE ARRCOP (IN, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copy the elements of one array to another
C   Inputs:
C      IN    C*32  The name of the input object.
C      OUT   C*32  The name of the output object
C   Output:
C      IERR  I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNOI, BUFNOO,
     *   PNTI, PNTO, DUMMY
      CHARACTER DATYPE*8, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (IN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (IN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (IN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (IN, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (IN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (IN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARRIO ('READ', IN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *    BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Copy row
      IF (DOCMPL) THEN
C                                       Complex
         CALL CVCOPY (OBUFFR(PNTI,BUFNOI), OBUFFR(PNTO,BUFNOO), LROW)
      ELSE
C                                       Real
         CALL RVCOPY (OBUFFR(PNTI,BUFNOI), OBUFFR(PNTO,BUFNOO), LROW)
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (IN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRCOP: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRCOP: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRFIL (SCALAR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Fill an array with a real scalar
C   Inputs:
C      SCALAR R    Scalar to fill
C      OUT    C*32 The name of the output object
C   Output:
C      IERR   I    Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      REAL      SCALAR
      CHARACTER OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NDIM(7), DIM(7), TYPE, BUFNOO, PNTO, DUMMY
      CHARACTER DATYPE*8, CDUMMY*1
      COMPLEX   CSCLR
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Complex scalar
      CSCLR = CMPLX (SCALAR, 0.0)
C                                       Open array and get I/O stream
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Window
      CALL ARRWIN (OUT, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (OUT, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (OUT, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fill row
      IF (DOCMPL) THEN
C                                       Complex
         CALL CVFILL (CSCLR, OBUFFR(PNTO,BUFNOO), LROW)
      ELSE
C                                       Real
         CALL RVFILL (SCALAR, OBUFFR(PNTO,BUFNOO), LROW)
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close file
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRFIL: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRSAD (IN, SCALAR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Add a scalar to the elements of an array.  The scalar is expected to
C   be of the same type (real or complex) as the array
C   Inputs:
C      IN     C*32  The name of the input array object.
C      SCALAR R(?)  Scalar to add, (real, imaginary) for complex.
C      OUT    C*32  The name of the output array object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      SCALAR(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NAXIS(7), DIM(7), TYPE, BUFNOI, BUFNOO,
     *   PNTI, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      COMPLEX   CSCLR
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN.EQ.OUT) THEN
C                                       Create temporary output object
         TIN = 'Temporary IMAGE for ARRSAD'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN = IN
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (TIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex factor
      CSCLR = CMPLX (SCALAR(1), SCALAR(2))
C                                       Window
      CALL ARRWIN (TIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARRIO ('READ', TIN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *    BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
C                                       Scale row with blanking
         CALL CHKBLK (LROWC, OBUFFR(PNTI,BUFNOI), VALID1)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CSVADB (OBUFFR(PNTI,BUFNOI), VALID1, CSCLR,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
         ELSE
C                                       Real
            CALL RSVADB (OBUFFR(PNTI,BUFNOI), VALID1, SCALAR,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       Scale row
         IF (DOCMPL) THEN
C                                       Complex
            CALL CSVADD (OBUFFR(PNTI,BUFNOI), CSCLR,
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RSVADD (OBUFFR(PNTI,BUFNOI), SCALAR,
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN(1:32).NE.IN(1:32)) CALL IMGDES (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRSAD: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRSAD: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRSMU (IN, SCALAR, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   MUltiply a scalar times the elements of an array.  The scalar is
C   expected to be of the same type (real or complex) as the array.
C   Inputs:
C      IN     C*32  The name of the input array object.
C      SCALAR R(?)  Scalar to add, (real, imaginary) for complex.
C      OUT    C*32  The name of the output array object.
C   Output:
C      IERR   I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*)
      REAL      SCALAR(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, LROWC, NAXIS(7), DIM(7), TYPE, BUFNOI, BUFNOO,
     *   PNTI, PNTO, DUMMY
      CHARACTER DATYPE*8, TIN*32, CDUMMY*1
      REAL      BLANK
      LOGICAL   DOCMPL, ISBLNK
      COMPLEX   CSCLR
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       May need temporary object
      IF (IN.EQ.OUT) THEN
C                                       Create temporary output object
         TIN = 'Temporary IMAGE for ARRSMU'
         CALL IMGCOP (IN, TIN, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TIN = IN
         END IF
C                                       Open array and get I/O stream
C                                       pointers
      PNTI = 1
      CALL ARROPN (TIN, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TIN, BUFNOI, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TIN, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex factor
      CSCLR = CMPLX (SCALAR(1), SCALAR(2))
C                                       Window
      CALL ARRWIN (TIN, BLC, TRC, NAXIS, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Complex?
      CALL ARDGET (TIN, 'DATATYPE', TYPE, DIM, DUMMY, DATYPE, IERR)
      DOCMPL = (DATYPE.EQ.'COMPLEX') .AND. (IERR.EQ.0)
      IERR = 0
C                                       Blanking?
      CALL ARDGET (TIN, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      IERR = 0
C                                       Loop over array
      LROW = TRC(1) - BLC(1) + 1
      LROWC = LROW
      IF (DOCMPL) LROWC = LROW * 2
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input row
      CALL ARRIO ('READ', TIN, ARRFDV(1,BUFNOI), OBUFFR(1,BUFNOI),
     *    BUFPNT(BUFNOI), IERR)
      PNTI = BUFPNT(BUFNOI)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
C                                       Scale row with blanking
         CALL CHKBLK (LROWC, OBUFFR(PNTI,BUFNOI), VALID1)
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVBSCL (OBUFFR(PNTI,BUFNOI), VALID1, CSCLR,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
         ELSE
C                                       Real
            CALL RVBSCL (OBUFFR(PNTI,BUFNOI), VALID1, SCALAR,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
            END IF
         CALL SETBLK (LROWC, VALID3, OBUFFR(PNTO,BUFNOO))
      ELSE
C                                       Scale row
         IF (DOCMPL) THEN
C                                       Complex
            CALL CVSCL (OBUFFR(PNTI,BUFNOI), CSCLR,
     *         OBUFFR(PNTO,BUFNOO), LROW)
         ELSE
C                                       Real
            CALL RVSCL (OBUFFR(PNTI,BUFNOI), SCALAR,
     *         OBUFFR(PNTO,BUFNOO), LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TIN(1:32).NE.IN(1:32)) CALL IMGDES (TIN, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRSMU: INPUT ' // IN
      CALL MSGWRT (7)
      MSGTXT = 'ARRSMU: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE ARRBSC (PLUS, MINUS, OUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Combine two beam-switched arrays
C   Inputs:
C      PLUS    C*32  The name of the first object.
C      MINUS   C*32  The name of the second object.
C      OUT     C*32  The name of the output object
C   Output:
C      IERR    I     Error return code, 0=OK, 1=don't match
C-----------------------------------------------------------------------
      CHARACTER PLUS*(*), MINUS*(*), OUT*(*)
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), LOOP2, LOOP3, LOOP4, LOOP5, LOOP6,
     *   LOOP7, LROW, NDIM(7), DIM(7), TYPE, BUFNI1, BUFNI2, BUFNOO,
     *   PNTI1, PNTI2, PNTO, BLORDR, BLWIND(2,10), MSGSAV, NWIND, IC,
     *   BUFCNV, NCROW
      CHARACTER TPLUS*32, TMINUS*32, CDUMMY*1, TCONV*32
      REAL      BLANK, FACTOR, REWT(2), THROWP, THROWM, AXV
      LOGICAL   ISBLNK, FIRST
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'ARRAY.INC'
C-----------------------------------------------------------------------
      FIRST = .TRUE.
      MSGSAV = MSGSUP
C                                       Create temporary output object
      IF (PLUS.EQ.OUT) THEN
         TPLUS = 'Temporary PLUS image for ARRBSC'
         CALL IMGCOP (PLUS, TPLUS, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TPLUS = PLUS
         END IF
C                                       Create temporary
      IF (PLUS.EQ.MINUS) THEN
         TMINUS = 'Temporary minus image for ARRBSC'
         CALL IMGCOP (MINUS, TMINUS, IERR)
         IF (IERR.NE.0) GO TO 995
      ELSE
         TMINUS = MINUS
         END IF
C                                       get another bit of memory
      TCONV = 'Temporary dummy for ARRBSC'
      CALL IMGCOP (PLUS, TCONV, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBOPEN (TCONV, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TCONV, BUFCNV, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Open array and get I/O stream
C                                       pointers
      CALL ARROPN (TPLUS, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TPLUS, BUFNI1, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Second input
      CALL ARROPN (TMINUS, 'READ', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (TMINUS, BUFNI2, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Output
      PNTO = 1
      CALL ARROPN (OUT, 'WRIT', IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBINFO (OUT, BUFNOO, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Clear output window
      CALL ARRCWI (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Check compatibility
      CALL ARRCHK (TPLUS, TMINUS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCHK (TPLUS, OUT, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Image window
      CALL ARRWIN (TPLUS, BLC, TRC, NDIM, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       BS correction parameters
      MSGSUP = 32000
      CALL OGET (TPLUS, 'FACTOR', TYPE, DIM, FACTOR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         FACTOR = 1.0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
      IF (FACTOR.LE.0.0) FACTOR = 1.0
      MSGSUP = 32000
      CALL OGET (TPLUS, 'BLORDER', TYPE, DIM, BLORDR, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         BLORDR = 0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
      BLORDR = MAX (0, MIN (1, BLORDR))
      MSGSUP = 32000
      CALL OGET (TPLUS, 'REWEIGHT', TYPE, DIM, REWT, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         REWT(1) = 1.0
         REWT(2) = 1.0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
      IF (REWT(1).LE.0.0) REWT(1) = 1.0
      IF (REWT(2).LE.0.0) REWT(2) = 1.0
      MSGSUP = 32000
      CALL OGET (TPLUS, 'BLWINDOW', TYPE, DIM, BLWIND, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         IERR = 0
         DIM(1) = 0
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 995
      NWIND = DIM(1) / 2
      IF (NWIND.LE.0) THEN
         NWIND = 1
         BLWIND(1,1) = 1
         BLWIND(2,1) = 8192
         END IF
C                                       BS throws
      CALL OGET (TPLUS, 'BSTHROW', TYPE, DIM, THROWP, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OGET (TMINUS, 'BSTHROW', TYPE, DIM, THROWM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBHGET (OUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
      AXV = CATR(KRCIC) * 3600
      IF (AXV.EQ.0.0) AXV = 1.0
      IC = CATR(KRCRP) + 0.5
      IC = 2 * IC
      THROWP = FACTOR * THROWP / AXV
      THROWM = FACTOR * THROWM / AXV
      LROW = TRC(1) - BLC(1) + 1
      NCROW = MAX (LROW*2, 1024)
      CALL BSCONV (NCROW, THROWP, THROWM, REWT(1), IC, OBUFFR(1,BUFCNV))
C                                       Blanking?
      CALL ARDGET (TPLUS, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = (BLANK.NE.0.0) .AND. (IERR.EQ.0)
      CALL ARDGET (TMINUS, 'BLANK', TYPE, DIM, BLANK, CDUMMY, IERR)
      ISBLNK = ISBLNK .OR. ((BLANK.NE.0.0) .AND. (IERR.EQ.0))
      IERR = 0
C                                       Loop over array
      DO 700 LOOP7 = BLC(7),TRC(7)
         DO 600 LOOP6 = BLC(6),TRC(6)
            DO 500 LOOP5 = BLC(5),TRC(5)
               DO 400 LOOP4 = BLC(4),TRC(4)
                  DO 300 LOOP3 = BLC(3),TRC(3)
                     DO 200 LOOP2 = BLC(2),TRC(2)
C                                       Fetch input rows
      CALL ARRIO ('READ', TPLUS, ARRFDV(1,BUFNI1), OBUFFR(1,BUFNI1),
     *    BUFPNT(BUFNI1), IERR)
      PNTI1 = BUFPNT(BUFNI1)
      IF (IERR.GT.0) GO TO 995
      CALL ARRIO ('READ', TMINUS, ARRFDV(1,BUFNI2), OBUFFR(1,BUFNI2),
     *    BUFPNT(BUFNI2), IERR)
      PNTI2 = BUFPNT(BUFNI2)
      IF (IERR.GT.0) GO TO 995
C                                       Blanking?
      IF (ISBLNK) THEN
         CALL CHKBLK (LROW, OBUFFR(PNTI1,BUFNI1), VALID1)
         CALL CHKBLK (LROW, OBUFFR(PNTI2,BUFNI2), VALID2)
         IF (FIRST) THEN
            CALL RVBBLS (OBUFFR(PNTI1,BUFNI1), VALID1, LROW, BLORDR,
     *         NWIND, BLWIND)
            CALL RVBBLS (OBUFFR(PNTI2,BUFNI2), VALID2, LROW, BLORDR,
     *         NWIND, BLWIND)
            IF (REWT(2).NE.1.0) CALL RVBSCL (OBUFFR(PNTI1,BUFNI1),
     *         VALID1, REWT(2), OBUFFR(PNTI1,BUFNI1), VALID1, LROW)
            CALL RVBSUB (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTI1,BUFNI1),
     *         VALID1, LROW)
            CALL RVBBSC (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW, IC, OBUFFR(1,BUFCNV))
         ELSE
            CALL RVBADD (OBUFFR(PNTI1,BUFNI1), VALID1,
     *         OBUFFR(PNTI2,BUFNI2), VALID2, OBUFFR(PNTO,BUFNOO),
     *         VALID3, LROW)
            CALL RVBSCL (OBUFFR(PNTO,BUFNOO), VALID3, 0.5,
     *         OBUFFR(PNTO,BUFNOO), VALID3, LROW)
            END IF
         CALL SETBLK (LROW, VALID3, OBUFFR(PNTO,BUFNOO))
C                                       No blanking
      ELSE
         IF (FIRST) THEN
            CALL RVBLS (OBUFFR(PNTI1,BUFNI1), LROW, BLORDR, NWIND,
     *         BLWIND)
            CALL RVBLS (OBUFFR(PNTI2,BUFNI2), LROW, BLORDR, NWIND,
     *         BLWIND)
            IF (REWT(2).NE.1.0) CALL RVSCL (OBUFFR(PNTI1,BUFNI1),
     *         REWT(2), OBUFFR(PNTI1,BUFNI1), LROW)
            CALL RVSUB (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTI1,BUFNI1), LROW)
            CALL RVBSC (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTO,BUFNOO),
     *         LROW, IC, OBUFFR(1,BUFCNV))
         ELSE
            CALL RVADD (OBUFFR(PNTI1,BUFNI1), OBUFFR(PNTI2,BUFNI2),
     *         OBUFFR(PNTO,BUFNOO), LROW)
            CALL RVSCL (OBUFFR(PNTO,BUFNOO), 0.5, OBUFFR(PNTO,BUFNOO),
     *         LROW)
            END IF
         END IF
      CALL ARRIO ('WRIT', OUT, ARRFDV(1,BUFNOO), OBUFFR(1,BUFNOO),
     *   BUFPNT(BUFNOO), IERR)
      PNTO = BUFPNT(BUFNOO)
      IF (IERR.GT.0) GO TO 995
 200                    CONTINUE
                     FIRST = .FALSE.
 300                 CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Close files
      CALL ARRCLO (TPLUS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (TMINUS, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL ARRCLO (OUT, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBCLOS (TCONV, IERR)
      IF (IERR.NE.0) GO TO 995
      CALL OBFREE (TCONV, IERR)
      IF (IERR.NE.0) GO TO 995
C                                       Delete temporary if necessary
      IF (TPLUS(1:32).NE.PLUS(1:32)) CALL IMGDES (TPLUS, IERR)
      IF (IERR.NE.0) GO TO 995
      IF (TMINUS(1:32).NE.MINUS(1:32)) CALL IMGDES (TMINUS, IERR)
      IF (IERR.NE.0) GO TO 995
      GO TO 999
C                                       Error
 995  MSGTXT = 'ARRBSC: FIRST INPUT ' // PLUS
      CALL MSGWRT (7)
      MSGTXT = 'ARRBSC: SECOND INPUT ' // MINUS
      CALL MSGWRT (7)
      MSGTXT = 'ARRBSC: OUTPUT ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
