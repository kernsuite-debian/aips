C   "Q-routine" utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "Q" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012, 2015
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
C   The routines in this module mostly copy data to and from "AP" memory
C   and will only work for the pseudo AP as they directly access "AP"
C   memory.  The other one implements the recommended method for
C   "reserving" the AP to a particular subroutine.
C   Public functions:
C
C   APOBJ (op, subrou, ierr)
C      Reserve or free the AP for a particular subroutine
C
C   AP2IMG (ap, image, plane, ierr)
C      Copy an image plane from "AP" memory to an image object.
C   IMG2AP (image, plane, ap, ierr)
C      Copy an image plane from an image object to "AP" memory.
C   AP2UV (ap, uv, nv, lv, nr, ierr)
C      Copy a set of visibilities from the "AP" to uv object.
C   UV2AP (uv, ap, nv, lv, nr, ierr)
C      Copy a set of visibilities from a uv object to the "AP".
C
C-----------------------------------------------------------------------
      SUBROUTINE APOBJ (OP, SUBROU, IERR)
C-----------------------------------------------------------------------
C   reserve or free the AP using the APDEVICE object
C   Inputs:
C      OP       C*?   'OPEN' or 'RESE..'  reserve the space
C                     'CLOS..' or 'FREE'  free the space
C      SUBROU   C*?   Subroutine name
C   Outputs:
C      IERR     I     Error code
C-----------------------------------------------------------------------
      CHARACTER OP*(*), SUBROU*(*)
      INTEGER   IERR
C
      CHARACTER APDEVI*32, STATUS*4
      INTEGER   JERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      APDEVI = 'APdevice for ' // SUBROU
C                                       open
      STATUS = OP
      IF ((STATUS.EQ.'OPEN') .OR. (STATUS.EQ.'RESE')) THEN
         CALL APDCRE (APDEVI, IERR)
         IF (IERR.NE.0) CALL APDOPN (APDEVI, STATUS, IERR)
C                                       close
      ELSE IF ((STATUS.EQ.'CLOS') .OR. (STATUS.EQ.'FREE')) THEN
         CALL APDCLO (APDEVI, IERR)
         CALL APDDES (APDEVI, JERR)
         IERR = MAX (IERR, JERR)
C                                       bad op
      ELSE
         MSGTXT = 'APOBJ: UNRECOGNIZED OP = ''' // OP // ''''
         CALL MSGWRT (8)
         IERR = 2
         END IF
C
 999  RETURN
      END
      SUBROUTINE AP2IMG (AP, IMAGE, PLANE, IERR)
C-----------------------------------------------------------------------
C   "Q" utility routine
C   Copy an image plane from "AP" memory to an image object.
C   Inputs:
C      AP      I    0 rel "AP" memory base address of image plane
C      IMAGE   C*?  Name of IMAGE object
C      PLANE   I(7) Plane in image (blc of image)
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   AP, PLANE(7), IERR
C
      LONGINT   IAP
      INTEGER   TYPE, DIM(3), NAXIS(7), BLC(7), TRC(7), DUMMY
      CHARACTER ACCESS*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Set image array access to PLANE
      ACCESS = 'PLANE'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      DIM(3) = 0
      CALL ARPPUT (IMAGE, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       set window for access
      CALL ARDGET (IMAGE, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Plane = BLC
      PLANE(1) = 1
      PLANE(2) = 1
      CALL COPY (7, PLANE, BLC)
      CALL COPY (7, PLANE, TRC)
      TRC(1) = NAXIS(1)
      TRC(2) = NAXIS(2)
C                                       Save new window
      CALL ARDPUT (IMAGE, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (IMAGE, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Transfer
      IAP = AP + PSAPOF
C                                       Plane
      DIM(1) = 10
      DIM(2) = 10
      DIM(3) = 0
      CALL ARDWRI (IMAGE, DIM, APCORE(IAP), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close array
      CALL ARRCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset image array access.
      ACCESS = 'ROW'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      CALL ARPPUT (IMAGE, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'AP2IMG: ERROR COPYING FROM AP:' // IMAGE
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE IMG2AP (IMAGE, PLANE, AP, IERR)
C-----------------------------------------------------------------------
C   "Q" Utility routine
C   Copy an image plane from an image object to "AP" memory.
C   Inputs:
C      AP      I    0 rel "AP" memory base address of image plane
C      IMAGE   C*?  Name of image object
C      PLANE   I(7) Plane in image (blc of image)
C   Output:
C      IERR    I    Error code: 0 => OK
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*)
      INTEGER   AP, PLANE(7), IERR
C
      LONGINT   IAP
      INTEGER   TYPE, DIM(7), NAXIS(7), BLC(7), TRC(7), DUMMY
      CHARACTER ACCESS*8, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Set image array access to plane
      ACCESS = 'PLANE'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      DIM(3) = 0
      CALL ARPPUT (IMAGE, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       set window for access
      CALL ARDGET (IMAGE, 'NAXIS', TYPE, DIM, NAXIS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Plane = BLC
      PLANE(1) = 1
      PLANE(2) = 1
      CALL COPY (7, PLANE, BLC)
      CALL COPY (7, PLANE, TRC)
      TRC(1) = NAXIS(1)
      TRC(2) = NAXIS(2)
C                                       Save new window
      CALL ARDPUT (IMAGE, 'BLC', OOAINT, DIM, BLC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL ARDPUT (IMAGE, 'TRC', OOAINT, DIM, TRC, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Transfer
      IAP = AP + PSAPOF
      CALL ARDRED (IMAGE, DIM, APCORE(IAP), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close array
      CALL ARRCLO (IMAGE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Reset image array access
      ACCESS = 'ROW'
      DIM(1) = LEN (ACCESS)
      DIM(2) = 1
      CALL ARPPUT (IMAGE, 'ACCESS', OOACAR, DIM, DUMMY, ACCESS, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMG2AP: ERROR COPYING TO AP:' // IMAGE
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE AP2UV (AP, UV, NV, LV, NR, IERR)
C-----------------------------------------------------------------------
C   "Q" utility routine
C   Copy a set of visibilities from the "AP" to uv object.
C   Inputs:
C      AP      I    0 rel "AP" memory base address of image plane
C      UV      C*?  Name of uvdata object
C      NV      I    Number of visibilities to transfer.
C      LV      I    Length in words of a visibility record.
C      NR      I    Number of random parameters
C   Output:
C      IERR    I    Error code: 0 => OK
C-----------------------------------------------------------------------
      CHARACTER UV*(*)
      INTEGER   AP, NV, LV, NR, IERR
C
      INTEGER   LOOP, IV
      LONGINT   IAP
      INCLUDE 'INCS:PUVD.INC'
      REAL      RPARM(20), VIS(3*MAXCIF)
      SAVE VIS
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IAP = AP + PSAPOF
      DO 100 LOOP = 1,NV
         DO 10 IV = 1,NR
            RPARM(IV) = APCORE(IAP+IV)
 10         CONTINUE
         DO 20 IV = NR+1,LV
            VIS(IV) = APCORE(IAP+IV)
 20         CONTINUE
         CALL UVWRIT (UV, RPARM, VIS, IERR)
         IF (IERR.NE.0) GO TO 990
         IAP = IAP + LV
 100     CONTINUE
      GO TO 999
C                                       Error
 990  MSGTXT = 'AP2UV: ERROR COPYING FROM AP:' // UV
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE UV2AP (UV, AP, NV, LV, NR, IERR)
C-----------------------------------------------------------------------
C   "Q" utility routine
C   Copy a set of visibilities from the "AP" to uv object.
C   Inputs:
C      UV      C*?  Name of uvdata object
C      AP      I    0 rel "AP" memory base address of image plane
C      NR      I    Number of random parameters
C   Input/Output:
C      NV      I    Number of visibilities to transfer. Reset if end of
C                   data found.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER UV*(*)
      INTEGER   AP, NV, LV, NR, IERR
C
      INTEGER   LOOP, IV
      LONGINT   IAP
      INCLUDE 'INCS:PUVD.INC'
      REAL      RPARM(20), VIS(3*MAXCIF)
      SAVE VIS
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IAP = AP + PSAPOF - 1
      DO 100 LOOP = 1,NV
         CALL UVREAD (UV, RPARM, VIS, IERR)
C                                       End of data?
         IF (IERR.LT.0) GO TO 700
         IF (IERR.NE.0) GO TO 990
         DO 10 IV = 1,NR
            APCORE(IAP+IV) = RPARM(IV)
 10         CONTINUE
         DO 20 IV = NR+1,LV
            APCORE(IAP+IV) = VIS(IV-NR)
 20         CONTINUE
         IAP = IAP + LV
 100     CONTINUE
      GO TO 999
C                                       End of data encountered
 700  NV = LOOP - 1
      IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'UV2AP: ERROR COPYING TO AP:' // UV
      CALL MSGWRT (8)
C
 999  RETURN
      END
