      SUBROUTINE VTTELL (ITER, FINISH, TVFLD, TVPASS, IRET)
C-----------------------------------------------------------------------
C! Tim Cornwell Routine: checks TELL file.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   VTTELL checks the TELL file to see if anything is waiting for VTESS.
C   If so, it picks up the parms (via GTTELL), interprets them, and
C   sends them back to the rest of the task to handle.
C   Input:
C      ITER     I   Current iterations
C   In/out:
C      FINISH   L   T => this is last iteration
C      TVFLD    I   Field to display, <= 0 no TV
C      TVPASS   I   =1 => do TV display with init and ask
C   Output:
C      IRET     I   0 => okay, 99 => abort now
C-----------------------------------------------------------------------
      INTEGER   ITER, TVFLD, TVPASS, IRET
      LOGICAL   FINISH
C
      CHARACTER OPTELL*4
      INTEGER   NPARMS, SCRTCH(256), IROUND, I, ITV
      REAL      XNITER, XERROR(64), XXDOTV, XPRLEV
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /VTTLL/ XNITER, XERROR, XXDOTV, XPRLEV
C-----------------------------------------------------------------------
C                                       Check TELL file
      IRET = 0
      NPARMS = 67
      CALL GTTELL (NPARMS, OPTELL, XNITER, SCRTCH, IERR)
      WRITE (MSGTXT,1000) OPTELL
      IF (IERR.NE.0) CALL MSGWRT (4)
      IF (IERR.EQ.2) FINISH = .TRUE.
      IF (IERR.EQ.3) IRET = 99
      IF (IERR.NE.1) GO TO 999
C                                       install parms
      DO 10 I = 1,NFIELD
         IF (I.LE.64) THEN
            IF (XERROR(I).GT.0.0) FRES(I) = XERROR(I)
         ELSE
            FRES(I) = XERROR(64)
            END IF
 10      CONTINUE
C                                       number iterations, quit
      IF (XNITER.LT.-0.01) DOSTOP = .TRUE.
      IF (ABS(XNITER).GE.1.0) VMLIM = ABS(XNITER) + 0.01
      IF (ITER.GE.VMLIM) FINISH = .TRUE.
      LNGOUT = .FALSE.
      IF (XPRLEV.GT.0.0) LNGOUT = .TRUE.
C                                       TV display: which field
      ITV = IROUND (XXDOTV)
      IF (NTVDEV.LE.0) ITV = 0
      IF (ITV.GT.2) ITV = 1
      IF ((TVPASS.LT.10) .AND. (ITV.NE.TVFLD)) TVPASS = 1
      TVFLD = ITV
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Received ',A4,' operation from TELL')
      END
