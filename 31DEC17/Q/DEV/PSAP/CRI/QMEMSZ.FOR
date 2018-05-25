      SUBROUTINE QMEMSZ (NWORDS)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Find max. value element of a vector.
C# AP-appl
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
C   Manipulates the size of the blank common used for the "AP"
C   memory and vector work space.  The size of the blank common
C   is expanded to NWORDS if that is larger than the current size
C   or reduces the size to zero if NWORDS is zero.
C   Inputs:
C     NWORDS  I    The number of words desired in the blank common
C   Input from common /SPF/ (include D/CAPC.INC)
C     MEMSIZ  I    The current size of the blank common.
C                  -1 => minimum size.
C   Output to common /SPF/
C     MEMSIZ  I    The current size of the blank common.
C                  -1 => minimum size.
C   CRI version (stubbed pending development).
C-----------------------------------------------------------------------
      INTEGER   NWORDS
C
      INTEGER   JWORDS, CHEKIT, IERR
      INCLUDE   'INCS:DAPC.INC'
      DATA CHEKIT /0/
C-----------------------------------------------------------------------
C                                       Check if need to change size
      IF ((NWORDS.LE.MEMSIZ) .AND. (NWORDS.GT.0)) GO TO 999
      IF ((NWORDS.EQ.0) .AND. (MEMSIZ.LT.0)) GO TO 999
C                                       Trap first call (full size).
C                                       NB: May cause trouble for
C                                       overlaying!
         IF (CHEKIT.EQ.0) MEMSIZ = OFF2 + 12 * OFFDEL
         CHEKIT = 1000
C                                       Change size.
         JWORDS = NWORDS - MEMSIZ + 1
         IF (MEMSIZ.LT.0) JWORDS = NWORDS
         IF (NWORDS.EQ.0) JWORDS = - MEMSIZ
C         CALL MEMORY ('UC', JWORDS)
C         CALL HPALLOC (IPTR, JWORDS, IERR, 0)
C         IF (IERR.NE.0) THEN
C            WRITE (6,*) 'QMEMSZ: HPALLOC ERRCODE =', IERR
C            CALL ABORT ()
C            END IF
         IF (NWORDS.GT.0) MEMSIZ = MEMSIZ + JWORDS
         IF (NWORDS.LE.0) MEMSIZ = -1
C
 999  RETURN
      END
