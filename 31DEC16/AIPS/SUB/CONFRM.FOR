      SUBROUTINE CONFRM (IERR)
C-----------------------------------------------------------------------
C! asks user to respond yes or no to some question
C# POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   CONFRM is a subroutine for the AIPS program to carry out the
C   YES/NO confirmation required by various functions.
C   Output:
C      IERR    I        Answer: 0 => yes, 1 => no, 2 => neither
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER PRGNAM*6, SCRTCH*80, LPT*1
      INTEGER   LIUNIT, LN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGNAM /'CONFRM'/
C-----------------------------------------------------------------------
      LPT = IPT
      LIUNIT = IUNIT
C                                       read from user
      IF (IUNIT.EQ.4) IUNIT = 1
      IPT = '?'
      CALL PREAD (SCRTCH)
      IF (ERRNUM.NE.0) GO TO 980
      IUNIT = LIUNIT
      IPT = LPT
      LIUNIT = MIN (80, NCHLIN)
C                                       to message file
      MSGTXT = JBUFF(1:LIUNIT)
      CALL MSGWRT (0)
C                                       what is answer
      CALL CHTRIM (JBUFF, 80, JBUFF, LN)
      CALL CHLTOU (LN, JBUFF)
      IERR = 1
      IF (JBUFF(1:1).EQ.'N') GO TO 999
         IERR = 2
         IF (JBUFF(1:1).EQ.'Y') IERR = 0
         GO TO 999
C                                       AIPS error trace
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
