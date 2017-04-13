C   Generic object operations.  Class specific routines are called.
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran Generic member functions
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
C   Available functions:
C      CREATE (name, class, ierr)
C         Creates an object of type class
C      DESTRY (name, ierr)
C         Destroys an object, permanent forms are not affected.
C      ZAP (name, ierr)
C         Zaps an object, permanent forms are destroyed.
C      OCOPY (namein, namout, ierr)
C         Copies an object, permanent forms are unaffected.
C      OCLONE (namein, namout, ierr)
C         Clones an object, new permanent forms are created.
C      OOPEN (name, status, ierr)
C         Opens an object for access.
C      OCLOSE (name, ierr)
C         Closes an object for access.
C      OGET (name, keywrd, type, dim, value, valuec, ierr)
C         Return value associated with keyword.
C      OPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Store value associated with keyword.
C-----------------------------------------------------------------------
      SUBROUTINE CREATE (NAME, CLASS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates an image object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C      CLASS C*8   Class of the object
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), CLASS*(*)
      INTEGER   IERR
C
C-----------------------------------------------------------------------
      IERR = 0
C                                       Class dependent create
C                                       Image
      IF (CLASS.EQ.'IMAGE') THEN
         CALL IMGCRE (NAME, IERR)
C                                       Uvdata
      ELSE IF (CLASS.EQ.'UVDATA  ') THEN
         CALL OUVCRE (NAME, IERR)
C                                       Table
      ELSE IF (CLASS.EQ.'TABLE   ') THEN
         CALL TABCRE (NAME, IERR)
C                                       CLEAN process
      ELSE IF (CLASS.EQ.'CLEAN   ') THEN
         CALL CLNCRE (NAME, IERR)
C                                       Complex Image
      ELSE IF (CLASS.EQ.'CX_IMAGE') THEN
         CALL CIMCRE (NAME, '        ', '        ', IERR)
C                                       Printer
      ELSE IF (CLASS.EQ.'PRINTER') THEN
         CALL PRTCRE (NAME, IERR)
C                                       Edit class
      ELSE IF (CLASS.EQ.'EDIT') THEN
         CALL EDICRE (NAME, IERR)
C                                       TV device
      ELSE IF (CLASS.EQ.'TVDEVICE') THEN
         CALL TVDCRE (NAME, IERR)
C                                       AP device
      ELSE IF (CLASS.EQ.'APDEVICE') THEN
         CALL APDCRE (NAME, IERR)
C                                       Generic create
      ELSE
         CALL OBCREA (NAME, CLASS, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE DESTRY (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the object with name "name"; quasi-permanent forms
C   are unaffected.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent destroy
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGDES (NAME, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVDES (NAME, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABDES (NAME, IERR)
C                                       CLEAN process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNDES (NAME, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMDES (NAME, IERR)
C                                       Printer
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTDES (NAME, IERR)
C                                       Edit clas
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDIDES (NAME, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDDES (NAME, IERR)
C                                       AP
      ELSE IF (CNAME.EQ.'APDEVICE') THEN
         CALL APDDES (NAME, IERR)
C                                       Generic destroy
      ELSE
         CALL OBFREE (NAME, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE ZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the object with name "name"; quasi-permanent forms
C   are deleted.
C   Inputs:
C      NAME  C*32  The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent zap
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGZAP (NAME, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVZAP (NAME, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABZAP (NAME, IERR)
C                                       CLEAN process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNZAP (NAME, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMZAP (NAME, IERR)
C                                       Printer
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTZAP (NAME, IERR)
C                                       Edit class
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDIZAP (NAME, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDZAP (NAME, IERR)
C                                       AP
      ELSE IF (CNAME.EQ.'APDEVICE') THEN
         CALL APDZAP (NAME, IERR)
C                                       Generic destroy
      ELSE
         CALL OBFREE (NAME, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OCOPY (NAMEIN, NAMOUT, IERR)
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
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAMEIN, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent copy
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGCOP (NAMEIN, NAMOUT, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVCOP (NAMEIN, NAMOUT, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TBCOPY (NAMEIN, NAMOUT, IERR)
C                                       CLEAN process - not defined
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         MSGTXT = 'OCOPY: COPY NOT DEFINED FOR CLEAN CLASS'
         CALL MSGWRT (7)
         IERR = 1
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMCOP (NAMEIN, NAMOUT, IERR)
C                                       Generic copy
      ELSE
         CALL OBCOPY (NAMEIN, NAMOUT, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OCLONE (NAMEIN, NAMOUT, IERR)
C-----------------------------------------------------------------------
C   Public
C   Clones an object.  A deep copy is made.
C   Inputs:
C      NAMEIN  C*?   The name of the input object.
C      NAMOUT  C*?   The name of the output object.
C   Output:
C      IERR    I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAMEIN*(*), NAMOUT*(*)
      INTEGER   IERR
C
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAMEIN, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent clone
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGCLN (NAMEIN, NAMOUT, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVCLN (NAMEIN, NAMOUT, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABCLN (NAMEIN, NAMOUT, IERR)
C                                       CLEAN process - not defined
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         IERR = 1
         MSGTXT = 'OCLONE: CLONING NOT DEFINED FOR CLEAN CLASS'
         CALL MSGWRT (7)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMCLN (NAMEIN, NAMOUT, IERR)
C                                       Generic copy
      ELSE
         CALL OBCOPY (NAMEIN, NAMOUT, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OOPEN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens an object for access.  Obtains header info etc.
C   Inputs:
C      NAME   C*32  The name of the object.
C      STATUS C*4   'READ', 'WRIT', 'DEST' (write but destroy on
C                   failure).
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent open
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGOPN (NAME, STATUS, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVOPN (NAME, STATUS, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABOPN (NAME, STATUS, IERR)
C                                       CLEAN process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNOPN (NAME, STATUS, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMOPN (NAME, STATUS, IERR)
C                                       Printer
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTOPN (NAME, STATUS, IERR)
C                                       Edit class
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDIOPN (NAME, STATUS, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDOPN (NAME, STATUS, IERR)
C                                       AP
      ELSE IF (CNAME.EQ.'APDEVICE') THEN
         CALL APDOPN (NAME, STATUS, IERR)
C                                       Generic open
      ELSE
         CALL OBOPEN (NAME, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OCLOSE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes object updating disk resident information.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER  OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent close
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGCLO (NAME, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA') THEN
         CALL OUVCLO (NAME, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABCLO (NAME, IERR)
C                                       CLEAN Process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNCLO (NAME, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIMCLO (NAME, IERR)
C                                       Printer
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTCLO (NAME, IERR)
C                                       Edit class
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDICLO (NAME, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDCLO (NAME, IERR)
C                                       AP
      ELSE IF (CNAME.EQ.'APDEVICE') THEN
         CALL APDCLO (NAME, IERR)
C                                       Generic close
      ELSE
         CALL OBCLOS (NAME, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent close
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       CLEAN process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Printer
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Edit class
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDIGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Generic get
      ELSE
         CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the dimensionality and value(s) associated with a given
C   keyword.
C   Inputs:
C      NAME     C*?   The name of the object.
C      KEYWRD   C*?   The name of the keyword in form 'MEM1.MEM2...'
C      TYPE     I     Data type: 1=D, 2=R, 3=C, 4=I, 5=L
C      DIM      I(*)  Dimensionality of value, an axis dimension of zero
C                     means that that dimension and higher are
C                     undefined.
C      VALUE    ?(*)  The value associated with keyword.
C      VALUEC   C*?   Associated value (character)
C   Outputs:
C      IERR     I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM, CLASNO
      CHARACTER CNAME*8
C-----------------------------------------------------------------------
      IERR = 0
C                                       Object number
      CALL OBNAME (NAME, OBJNUM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Object class
      CALL OBCLAS (OBJNUM, CLASNO, CNAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Class dependent close
C                                       Image
      IF (CNAME.EQ.'IMAGE') THEN
         CALL IMPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       CLEAN process
      ELSE IF (CNAME.EQ.'CLEAN   ') THEN
         CALL CLNPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Complex Image
      ELSE IF (CNAME.EQ.'CX_IMAGE') THEN
         CALL CIPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Table
      ELSE IF (CNAME.EQ.'TABLE   ') THEN
         CALL TABPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       UV data
      ELSE IF (CNAME.EQ.'UVDATA  ') THEN
         CALL OUVPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       PRINTER
      ELSE IF (CNAME.EQ.'PRINTER') THEN
         CALL PRTPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Edit class
      ELSE IF (CNAME.EQ.'EDIT') THEN
         CALL EDIPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       TV
      ELSE IF (CNAME.EQ.'TVDEVICE') THEN
         CALL TVDPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       Generic put
      ELSE
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         END IF
C
 999  RETURN
      END
