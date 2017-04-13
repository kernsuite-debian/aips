C   Complex Image Class module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Complex Image" class library
C# Map-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2015
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
C
C   A complex image consists of a pair of real images.  The real and
C   imaginary parts of the object are accessed by prepending 'real.' or
C   'imag.' to the element name e.g. 'REAL.ARRAY.ARRAY_DESC.NAXIS' is
C   the name of the dimension array for the real image.  Access to rows
C   of the complex image are to member 'ARRAY' although CIGETX and
C   CIPUTX allow more efficient access.
C
C   Class public members:
C     ARRAY     CX(*,*,*)     Array(s) of complex pixels
C     REAL      image object  Real part of the complex image
C     IMAG      image object  Imaginary part of the complex image
C     REALPART  C*32          Name of REAL
C     IMAGPART  C*32          Name of IMAG
C
C   Class private members:
C     CXIDIM   I(7,MAXIO)    Array dimension per I/O stream
C     IOACTV   L(MAXIO)      If true image I/O is active
C
C   Public functions:
C     CIMCRE (name, real, imag, iret)
C        Creates a complex image object with name "name" and whose real
C        and imaginary components are "real" and "imag".
C     CIMDES (name, ierr)
C        Destroys the image object with name "name"; quasi-permanent
C        forms are unaffected.
C     CIMZAP (name, ierr)
C        Destroys the image object with name "name"; quasi-permanent
C        forms are deleted.
C     CIMCOP (namein, namout, ierr)
C        Copys one object to another.  The same quasi permanent forms
C        are used for both.
C     CIMCLN (namein, namout, ierr)
C        CLONES an object.  The component parts are cloned.
C     CIGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     CIPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C     CIMOPN (name, status, ierr)
C        Opens a complex image for array access
C     CIMCLO (name, ierr)
C        Closes a complex image for array access
C     CIGETX (name, dim, row, ierr)
C        Return complex row.
C     CIPUTX (name, dim, row, ierr)
C        Stores complex row.
C
C   Private functions:
C     GETBAS (name, keywrd, base, object, mem, ierr)
C        Checks base class reference and returns relevant information.
C     CHKBAS (type, bascls, name, ierr)
C         Checks that data type is valid for the base class.
C     CIMIO (name, dim, data, ierr)
C        Fetches or stores a row of a complex image.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'CXIMAGE.INC'
      INCLUDE 'INCS:PMAD.INC'
C                                       Complex Image class I/O
C                                       Requires OBJPARM.INC
      INTEGER   NBASE
C                                       NBASE = .no. base classes.
      PARAMETER (NBASE = 3)
C
      CHARACTER BASE(NBASE)*8, THSCLS*16
      LOGICAL   IOACTV(MAXIO)
      INTEGER   CXIDIM(7,MAXIO)
      REAL      CXIROW(MAXIMG)
      COMMON /CXIIOC/ CXIROW, CXIDIM, IOACTV
      DATA BASE /'REAL', 'IMAG', 'ARRAY'/
      DATA THSCLS /'CX_IMAGE'/
LOCAL END
      SUBROUTINE CIMCRE (NAME, REAL, IMAG, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an image object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C      REAL  C*32  The name of the real component of the object.
C      IMAG  C*32  The name of the imaginary part of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), REAL*32, IMAG*32
      INTEGER   IERR
C
      INTEGER  IDIM(7), DUMMY(1)
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBCREA (NAME, 'CX_IMAGE', IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Store names of the component
C                                       objects.
      IDIM(1) = 32
      IDIM(2) = 1
      IDIM(3) = 0
      CALL CIPUT (NAME, 'REALPART', OOACAR, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIPUT (NAME, 'IMAGPART', OOACAR, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CIMDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the image object with name "name"; quasi-permanent forms
C   are unaffected.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Destroy object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE CIMZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the image object with name "name"; quasi-permanent forms
C   are deleted.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      CHARACTER REAL*32, IMAG*32
      INTEGER   IDIM(7), TYPE, DUMMY(1)
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAME, 'REALPART', TYPE, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAME, 'IMAGPART', TYPE, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ZAP them
      CALL IMGZAP (REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGZAP (IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Destroy object
      CALL CIMDES (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE CIMCOP (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Copys one object to another.  The same quasi permanent forms are
C   used for both.
C   Inputs:
C      NAMEIN  C*32  The name of the input object.
C      NAMOUT  C*32  The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL OBCOPY (NAMEIN, NAMOUT, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CIMCLN (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clones an complex image object.  Each of the components parts is
C   cloned.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER   DIM(7), TYPE, DUMMY(1)
      CHARACTER REALI*32, IMAGI*32, REALO*32, IMAGO*32
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAMEIN, 'REALPART', TYPE, DIM, DUMMY, REALI, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAMEIN, 'IMAGPART', TYPE, DIM, DUMMY, IMAGI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Form output component names by
C                                       perpending R or I to NAMOUT
      REALO = 'R' // NAMOUT
      IMAGO = 'I' // NAMOUT
C                                       Clone parts
      CALL IMGCLN (REALI, REALO, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLN (IMAGI, IMAGO, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CIGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns the dimensionality and value(s) associated with a given
C   keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C   Outputs:
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
      CHARACTER BASCLS*8, BASOBJ*32, MEMBER*32
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look for base class.member name
C                                       in KEYWRD.
      CALL GETBAS (NAME, KEYWRD, BASCLS, BASOBJ, MEMBER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check base class validity.
      CALL CHKBAS (2, BASCLS, NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Call base class access routine
      IF (BASCLS.NE.' ') THEN
         CALL IMGET (BASOBJ, MEMBER, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
C                                       Not base class member
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE CIPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
C   A new non-virtual keyword will be created if necessary.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    R(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
      CHARACTER BASCLS*8, BASOBJ*32, MEMBER*32
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look for base class.member name
C                                       in KEYWRD.
      CALL GETBAS (NAME, KEYWRD, BASCLS, BASOBJ, MEMBER, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check base class validity.
      CALL CHKBAS (2, BASCLS, NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Call base class access routine
      IF (BASCLS.NE.' ') THEN
         CALL IMPUT (BASOBJ, MEMBER, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
      ELSE
C                                       Not base class member
C                                       Lookup NAME
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C
 999  RETURN
      END
      SUBROUTINE CIMOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Sets up to open a complex image file.  Obtains header info etc.
C   For images to be written the validity of the data is checked.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   'READ', 'WRIT', 'DEST' (write but destroy on
C                   failure).
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER   BLC(7), TRC(7), OBJR, OBJI, TYPE, IDIM(5), DUMMY(1)
      CHARACTER REAL*32, IMAG*32
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAME, 'REALPART', TYPE, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAME, 'IMAGPART', TYPE, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open image members
      CALL IMGOPN (REAL, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGOPN (IMAG, STATUS, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Make sure compatible
      CALL IMGCHK (REAL, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer numbers
      CALL OBINFO (REAL, OBJR, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OBINFO (IMAG, OBJI, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Save private data
C                                       Array dimensions
      CALL IMGWIN (REAL, BLC, TRC, CXIDIM(1,OBJR), IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGWIN (IMAG, BLC, TRC, CXIDIM(1,OBJI), IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Init I/O active flag
      IF (OBJR.GT.1) IOACTV(OBJR) = .FALSE.
C
 999  RETURN
      END
      SUBROUTINE CIMCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes complex image updating disk resident information.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      CHARACTER REAL*32, IMAG*32
      INTEGER   BUFNO, TYPE, IDIM(5), DUMMY(1)
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAME, 'REALPART', TYPE, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAME, 'IMAGPART', TYPE, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get buffer numbers
      CALL OBINFO (REAL, BUFNO, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Close any I/O
      IF ((BUFNO.GT.0) .AND. (IOACTV(BUFNO))) THEN
         CALL ARRCLO (REAL, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL ARRCLO (IMAG, IERR)
         IF (IERR.NE.0) GO TO 999
         IOACTV(BUFNO) = .FALSE.
         END IF
C                                       Close image members
      CALL IMGCLO (REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL IMGCLO (IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE CIGETX (NAME, DIM, ROW, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Returns a row from a complex image.
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      DIM      I(*)  Dimensionality of value, (1)=length of row in
C                     complex elements.
C      ROW      CX(*) Complex row
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DIM(*), IERR
      REAL      ROW(*)
C
      CHARACTER REAL*32, IMAG*32
      INTEGER   BUFNO, LROW, LOOP, POINT, TYPE, IDIM(5), DUMMY(1)
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAME, 'REALPART', TYPE, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAME, 'IMAGPART', TYPE, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read real part and copy
      CALL ARREAD (REAL, DIM, CXIROW, IERR)
      IF (IERR.NE.0) GO TO 999
      LROW = DIM(1) * 2
      POINT = 1
      DO 100 LOOP = 1,LROW,2
         ROW(LOOP) = CXIROW(POINT)
         POINT = POINT + 1
 100     CONTINUE
C                                       Read imaginary part and copy
      CALL ARREAD (IMAG, DIM, CXIROW, IERR)
      IF (IERR.NE.0) GO TO 999
      POINT = 1
      DO 200 LOOP = 2,LROW,2
         ROW(LOOP) = CXIROW(POINT)
         POINT = POINT + 1
 200     CONTINUE
C                                       Find object number
      CALL OBINFO (REAL, BUFNO, IERR)
C                                       Set I/O active flag
      IF (BUFNO.GT.0) IOACTV(BUFNO) = .TRUE.
C
 999  RETURN
      END
      SUBROUTINE CIPUTX (NAME, DIM, ROW, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores a row in a complex image.
C   Inputs:
C      NAME     C*?   The name of the object.
C   Outputs:
C      DIM      I(*)  Dimensionality of value, (1)=length of row in
C                     complex elements.
C      ROW      CX(*) Complex row
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   DIM(*), IERR
      REAL      ROW(*)
C
      CHARACTER REAL*32, IMAG*32
      INTEGER   BUFNO, LROW, LOOP, POINT, TYPE, IDIM(5), DUMMY(1)
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get names of component parts
      CALL CIGET (NAME, 'REALPART', TYPE, IDIM, DUMMY, REAL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL CIGET (NAME, 'IMAGPART', TYPE, IDIM, DUMMY, IMAG, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Store real part and copy
      LROW = DIM(1) * 2
      POINT = 1
      DO 100 LOOP = 1,LROW,2
         CXIROW(POINT) = ROW(LOOP)
         POINT = POINT + 1
 100     CONTINUE
      CALL ARRWRI (REAL, DIM, CXIROW, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Store imaginary part and copy
      POINT = 1
      DO 200 LOOP = 2,LROW,2
         CXIROW(POINT) = ROW(LOOP)
         POINT = POINT + 1
 200     CONTINUE
      CALL ARRWRI (IMAG, DIM, CXIROW, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Find buffer number
      CALL OBINFO (REAL, BUFNO, IERR)
C                                       Set I/O active flag
      IF (BUFNO.GT.0) IOACTV(BUFNO) = .TRUE.
C
 999  RETURN
      END
      SUBROUTINE GETBAS (NAME, KEYWRD, BASCLS, OBJECT, MEM, IERR)
C-----------------------------------------------------------------------
C   Private function
C   Determine if keyword is a reference to a known base class and
C   returns the member name in mem.
C   Inputs:
C      NAME    C*?  Input complex image object
C      KEYWRD  C*?  Keyword in form 'base.mem.mem'
C   Outputs:
C      BASCLS  C*8  Name of the base class, blank if none.
C      OBJECT  C*?  Name of the base class object.
C      MEM     C*?  Base class member
C      IERR    I    Return code, 0=>OK else unknown base class.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), BASCLS*(*), OBJECT*(*), MEM*(*)
      INTEGER   IERR
C
      INTEGER   IBASE, LOOP, OBJNUM, POINT, TYPE, IDIM(7), DUMMY(1)
      CHARACTER KEY*16
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'CXIMAGE.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Look for base_class.member_name
C                                       in KEYWRD.
      POINT = INDEX (KEYWRD, '.')
C                                       Base class reference
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
     *         BASCLS
            CALL MSGWRT (7)
            MSGTXT = 'FOR OBJECT = ' // NAME
            CALL MSGWRT (7)
            GO TO 999
            END IF
C                                       Base class member
C                                       'ARRAY' Special case
         IF (BASCLS.NE.'ARRAY') THEN
C                                       Lookup part object name
            KEY = KEYWRD(1:POINT-1) // 'PART'
            CALL OBNAME (NAME, OBJNUM, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OBGET (OBJNUM, KEY, TYPE, IDIM, DUMMY, OBJECT, IERR)
            IF (IERR.NE.0) GO TO 999
         ELSE
            OBJECT = NAME
            END IF
C                                       Save member name
         MEM = KEYWRD(POINT+1:)
C                                       No base class
      ELSE
         BASCLS = '  '
         OBJECT = '  '
         MEM = '  '
         END IF
C
 999  RETURN
      END
      SUBROUTINE CHKBAS (TYPE, BASCLS, NAME, IERR)
C-----------------------------------------------------------------------
C   Private function
C   Checks validity of a given data type for a base class.
C   Base class 'ARRAY' is allowed only for complex data type.
C   Inputs:
C      TYPE    I    Data type, 1=double, 2=real, 3=character, 4=integer,
C                   5=logical, 6=complex
C      BASCLS  C*?  Base class
C      NAME    C*?  Object name, used for error report
C   Outputs:
C      IERR    I    Return code, 0=>OK else invalid request.
C-----------------------------------------------------------------------
      INTEGER   TYPE, IERR
      CHARACTER BASCLS*(*), NAME*(*)
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       No 'ARRAY' base class data for
C                                       this type.
      IF ((BASCLS.EQ.'ARRAY') .AND. (TYPE.NE.6)) THEN
         IERR = 2
         MSGTXT = 'ILLEGAL TYPE FOR COMPLEX IMAGE ARRAY REFERENCE'
         CALL MSGWRT (6)
         MSGTXT = 'OBJECT = ' // NAME
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
      END
