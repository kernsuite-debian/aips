C    AP device Class Module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "AP" class library
C# AP-util Object-Oriented
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
C-----------------------------------------------------------------------
C   An AP object is a way of reserving the full AP to a specific
C   subroutine to avoid multiple classes assuming that they can use it
C   at the same time.  Only one AP object may be active (open) at a
C   time.  This is mainly a defense against programming error.
C   Usage notes:
C      The class name should be unique to the subroutine "opening" the
C      AP device.  Note that this does not replace the calls to QINIT
C      and QRLSE.
C   Class public members:
C      none
C   Class  private data:
C      MYNAME    C*32  Name of open AP-device
C
C   Public functions:  Object management
C     APDCRE (name, ierr)
C        Creates a AP-device object with name "name".
C     APDDES (name, ierr)
C        Destroys the AP-device object with name "name";
C     APDZAP (name, ierr)
C        Destroys the AP-device object with name "name";
C        (Same as APDDES)
C     APDOPN (name, status, ierr)
C        Opens a AP-device object
C     APDCLO (name, ierr)
C        Closes a AP-device object
C
C   Private functions:
C-----------------------------------------------------------------------
LOCAL INCLUDE 'APDEV.INC'
C                                       Include for AP class.
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   ACTIVE
      CHARACTER MYNAME*32
      COMMON /APDCCM/ ACTIVE
      COMMON /APDMCC/ MYNAME
C                                                          End APDEV.INC
LOCAL END
      SUBROUTINE APDCRE (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Creates a APDEVICE object with name "name"
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'APDEV.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Create AIPS object
      CALL OBCREA (NAME, 'APDEVICE', IERR)
C                                       Error
      IF (IERR.NE.0) THEN
         MSGTXT = 'APDCRE: ERROR CREATING ' // NAME
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
      SUBROUTINE APDDES (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the APDEVICE object with name "name";
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
      CALL APDCLO (NAME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Delete object
      CALL OBFREE (NAME, IERR)
C
 999  RETURN
      END
      SUBROUTINE APDZAP (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Destroys the APDEVICE object with name "name"; same as APDDES.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'APDEV.INC'
C-----------------------------------------------------------------------
      CALL APDDES (NAME,IERR)
C
 999  RETURN
      END
      SUBROUTINE APDOPN (NAME, STATUS, IERR)
C-----------------------------------------------------------------------
C   Public
C   Opens a APDEVICE file.  Marks APDEVICE common as active.
C   Inputs:
C      NAME   C*?   The name of the object.
C      STATUS C*4   used in error message
C   Output:
C      IERR   I     Error return code, 0=OK, 5=data invalid
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STATUS*4
      INTEGER   IERR
C
      INCLUDE 'APDEV.INC'
C-----------------------------------------------------------------------
C                                       APDEVICE common must be inactive
      IF (ACTIVE) THEN
         IERR = 1
         MSGTXT = 'APDOPN: ATTEMPT TO ACTIVATE SECOND AP-DEVICE OBJECT'
         CALL MSGWRT (8)
         MSGTXT = 'APDOPN: STATUS SPECIFIED: ' // STATUS
         CALL MSGWRT (8)
         MSGTXT = 'APDOPN: REQUESTED    ' // NAME
         CALL MSGWRT (8)
         MSGTXT = 'APDOPN: ALREADY HAVE ' // MYNAME
         CALL MSGWRT (8)
C                                       Save name
      ELSE
         IERR = 0
         ACTIVE = .TRUE.
         MYNAME = NAME
         END IF
C
 999  RETURN
      END
      SUBROUTINE APDCLO (NAME, IERR)
C-----------------------------------------------------------------------
C   Public
C   Closes APDEVICE completing output to AP-device if necessary.
C   Inputs:
C      NAME  C*?   The name of the object.
C   Output:
C      IERR  I     Error return code, 0=OK
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INCLUDE 'APDEV.INC'
      INCLUDE 'INCS:CLASSIO.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       No action if inactive
      IF (ACTIVE) THEN
C                                       Make sure right one
         IF (NAME.NE.MYNAME) THEN
            IERR = 2
            MSGTXT = 'ATTEMPT TO ACCESS CLOSED AP OBJECT'
            CALL MSGWRT (8)
            MSGTXT = 'APDCLO: ERROR CLOSING ' // NAME
            CALL MSGWRT (8)
C                                       Mark as inactive
         ELSE
            ACTIVE = .FALSE.
            MYNAME = ' '
            END IF
         END IF
C
 999  RETURN
      END
