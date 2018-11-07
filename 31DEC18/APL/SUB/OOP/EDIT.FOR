C    EDIT uv data Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "EDIT" class library
C# Object-Oriented UV EDITING
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   An EDIT object is an interface to the interactive UV data editing
C   software.  It does require an open TV object to function.
C   Class public members:
C      UVMASTER  C*32  Name of parent UV object
C      UVDATA    C*32  Name of displayed uvdata
C      UVRESID   C*32  Name of displayed uvdata 2nd file matches 1st
C      FGWORK    C*32  Name of output uvdata (gets FG table..)
C      TBEDIT    C*32  Name of table object to edit
C      TVDEVICE  C*32  Name of opened TV-device object
C   Class  private data:
C      MYNAME    C*32  Name of open EDIT
C
C   Public functions:  Object management
C     EDICRE (name, ierr)
C        Creates a Edit object with name "name".
C     EDIDES (name, ierr)
C        Destroys the Edit object with name "name";
C     EDIZAP (name, ierr)
C        Destroys the Edit object with name "name";
C        (Same as EDIDES)
C     EDIOPN (name, status, ierr)
C        Opens a Edit object
C     EDICLO (name, ierr)
C        Closes a Edit object
C     EDIGET (name, keywrd, type, dim, value, valuec, ierr)
C        Return keyword value.
C     EDIPUT (name, keywrd, type, dim, value, valuec, ierr)
C        Store keyword value.
C
C
C   Private functions:
C      EDBGET (name, keywrd, type, dim, value, valuec, ierr)
C         Fetches private member.
C      EDBPUT (name, keywrd, type, dim, value, valuec, ierr)
C         Stores private member.
C-----------------------------------------------------------------------
LOCAL INCLUDE 'EDIT.INC'
C                                       Include for EDIT class.
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   ACTIVE
      COMMON /EDICCM/ ACTIVE
      CHARACTER MYNAME*32
      COMMON /EDIMCC/ MYNAME
C                                                          End EDIT.INC
LOCAL END
      SUBROUTINE EDICRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates a EDIT object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'EDIT.INC'
      INTEGER  IDIM(7), DUMMY(1)
      CHARACTER BNAME*32
      DATA BNAME /'    '/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create AIPS object
      CALL OBCREA (NAME, 'EDIT', IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Initialize - ass. obj.
      IDIM(1) = LEN (BNAME)
      IDIM(2) = 1
      IDIM(3) = 0
      CALL EDIPUT (NAME, 'UVMASTER', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL EDIPUT (NAME, 'UVDATA', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL EDIPUT (NAME, 'TBEDIT', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL EDIPUT (NAME, 'UVRESID', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL EDIPUT (NAME, 'FGWORK', OOACAR, IDIM, DUMMY, BNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL EDIPUT (NAME, 'TVDEVICE', OOACAR, IDIM, DUMMY, BNAME, IERR)
C                                       Error
 990  IF (IERR.NE.0) THEN
         MSGTXT = 'EDICRE: ERROR CREATING ' // NAME
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
      SUBROUTINE EDIDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the EDIT object with name "name";
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C-----------------------------------------------------------------------
      IERR = 0
C                                       Close
      CALL EDICLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE EDIZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the EDIT object with name "name"; same as EDIDES.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'EDIT.INC'
C-----------------------------------------------------------------------
      CALL EDIDES (NAME,IERR)
C
 999  RETURN
      END
      SUBROUTINE EDIOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a EDIT file.  Marks EDIT common as active.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   Not used.
C   Output:
C      IERR  I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'EDIT.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       EDIT common must be inactive
      IF (ACTIVE) THEN
         IERR = 1
         MSGTXT = 'EDIOPN: ATTEMPT TO ACTIVATE SECOND EDIT OBJECT'
         GO TO 990
         END IF
      ACTIVE = .TRUE.
C                                       Save name
      MYNAME = NAME
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'EDIOPN: ERROR OPENING ' // NAME
      CALL MSGWRT (8)
      ACTIVE = .FALSE.
      MYNAME = ' '
C
 999  RETURN
      END
      SUBROUTINE EDICLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes EDIT completing output to Edit if necessary.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'EDIT.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       No action if inactive
      IF (.NOT.ACTIVE) GO TO 999
C                                       Make sure right one
      IF (NAME.NE.MYNAME) THEN
         IERR = 2
         MSGTXT = 'ATTEMPT TO ACCESS CLOSED EDIT OBJECT'
         GO TO 990
         END IF
C                                       Mark as inactive
      MYNAME = ' '
      ACTIVE = .FALSE.
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      MSGTXT = 'EDICLO: ERROR CLOSING ' // NAME
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE EDIGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
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
C-----------------------------------------------------------------------
C     IERR = 0
C                                       Is this a base class?
C     CALL EDBGET (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
C                                       Lookup NAME
C     IF (IERR.EQ.1) THEN
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBGET (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
C        END IF
C
 999  RETURN
      END
      SUBROUTINE EDIPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C-----------------------------------------------------------------------
C   Public function
C   Stores the value (array) associated with a given keyword.
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
      CHARACTER NAME*(*), KEYWRD*(*), VALUEC(*)*(*)
      INTEGER   TYPE, DIM(*), VALUE(*), IERR
C
      INTEGER   OBJNUM
C-----------------------------------------------------------------------
C     IERR = 0
C                                       Is this a base class?
C     CALL EDBPUT (NAME, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
C                                       IERR = 1 means not recognized.
C                                       Lookup NAME
C     IF (IERR.EQ.1) THEN
         CALL OBNAME (NAME, OBJNUM, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL OBPUT (OBJNUM, KEYWRD, TYPE, DIM, VALUE, VALUEC, IERR)
         IF (IERR.NE.0) GO TO 999
C        END IF
C
 999  RETURN
      END
